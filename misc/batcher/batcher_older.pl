#!/usr/bin/perl
# perl script to run sequential arb simulations, changing text in both arb and geo files, and then extracting data from the output files

# usage
# 1) create the perl module batcher_setup within the arb working directory which contains the subs case_setup and output_setup
# see instructions in the example module file included in misc/batcher/examples/parasitic_currents/batcher_setup.pm
# 2) run batcher, while at the same time including this module using
#
#     ./misc/batcher/batcher.pl -m batcher_setup.pm
#
# 3) output will be placed in batcher_output off the working directory
# 4) copy the batcher_output directory to somewhere else before doing the next run, otherwise some of the input files will be clobbered (change this behaviour by using the no-continue flag below)
# 5) touch batcher_stop in the working directory to stop the run (a subsequent touch kill will stop the arb job as quickly as possible too)
#
# large re-write 20/4/15, daltonh

use strict;
use warnings;
use Data::Dumper;
use File::Path qw(mkpath rmtree); # for File::Path version < 2.08, http://perldoc.perl.org/File/Path.html
use File::Copy qw(move copy);
use File::Glob ':glob'; # deals with whitespace better
use Thread; # for simultaneous jobs
use batcher_setup qw(case_setup output_setup $parallel); # brings in the user-written module that defines case-specific data
my @threads;
my $systemcall;
my $output_dir="batcher_output"; # this directory will store all of the output
my $input_dir="$output_dir/input_files"; # this will contain copies of the original arb, geo and msh input files
my $input_file=""; # set this below to a specific file name or glob pattern if you don't want all the *.arb files within the working directory to be used as input
my $geo_file=""; # set this below to a specific file name or glob pattern if you don't want all the *.geo files within the working directory to be used
my $continue=1; # set this to true (1) to allow continuation from a previous run, with new (additional) runs to take place - now this is 1 by default, which will append to previous batcher_output directories
my @case=case_setup(); # array that specifies information about each case that is being run that is initialised using case_setup within batcher_setup.pm
my @output_keys=output_setup(); # create list output keys of variables to be output that is also initialised within batcher_setup.pm
# convert this to output hash that will store the values
our %output=();
for my $key (@output_keys) {
  $output{$key} = '';
}

# sanity check to make sure that we are in the arb working directory
if (! -d "build") { die "BATCHER ERROR: call this script from working directory\n"; }

# remove stopfile from previous run if it exists
our $stopfile="batcher_stop";
if (-e $stopfile) { unlink($stopfile) or die "BATCHER ERROR: could not remove $stopfile stop file from the previous run\n"; }

# make output directory and copy over original arb, geo, msh and pm files
if (-d $output_dir && !($continue)) { # for File::Path version < 2.08, http://perldoc.perl.org/File/Path.html
  die "BATCHER ERROR: batcher will not run unless batcher_output directory is empty or non-existent (this is a safety feature!) or the continue flag is on\n";
} elsif (! -d $output_dir) {
  mkpath($output_dir) or die "BATCHER ERROR: could not create $output_dir\n";
}
if (! -d $input_dir) { mkpath($input_dir) or die "BATCHER ERROR: could not create $input_dir\n"; } # for File::Path version < 2.08, http://perldoc.perl.org/File/Path.html
else { unlink(bsd_glob("$input_dir/*")); } # get rid of old files
# copy over all possible files to input_dir
foreach my $filename (bsd_glob("*.arb"),bsd_glob("*.geo"),bsd_glob("*.msh"),bsd_glob("*.pm")) {
  copy($filename,$input_dir) or die "BATCHER ERROR: could not copy $filename to directory $input_dir\n";
}

# find starting run index if this is a continuation run
my $nstart=0;
my @run_dirs = bsd_glob("$output_dir/run_*");
if (@run_dirs) {
  for my $run_dir ( @run_dirs ) {
    if ($run_dir =~ /run_(\d+)$/) {
      print "BATCHER INFO: found previous run directory $run_dir\n";
      if ($1+1 > $nstart) {$nstart = $1+1;}
    }
  }
}
open(OUTPUT, ">>$output_dir/batch_data.csv"); # open batch_data csv file

#-----------------------------------------------------------------------------------------
print "BATCHER INFO: looping through runs ".$nstart." to ".scalar($#case+$nstart)."\n";

# now loop through each run
for my $n ( 0 .. $#case ) {

  my $ndir = scalar($n+$nstart);
# create output directory to record run
  my $run_record_dir = "$output_dir/run_$ndir";
  mkpath($run_record_dir) or error_stop("could not create $run_record_dir");

# and create msh store directory
  my $msh_store_dir = "$run_record_dir/input_mesh"; 
  mkpath($msh_store_dir) or error_stop("could not create $msh_store_dir");

# check that arbfile contains something, otherwise default to *.arb
  if (!(@{$case[$n]{"arbfile"}})) { push(@{$case[$n]{"arbfile"}},"*.arb"); };

#-----------------
# dump case into into run directory
  open(DUMPER, ">$run_record_dir/batcher_info.txt") or error_stop("can't open batcher_info.txt file in $run_record_dir");
  print DUMPER "run: n = $n: ndir = $ndir\ncase[$n] = ".Dumper($case[$n])."\n";
  close(DUMPER);

#-----------------
# deal with substitutions
# loop through each arbfile, geofile and otherfile in the input_dir, copying them over to the working directory while doing the substitutions (and also copying them to the run_record_dirs)
  foreach my $fffilename ( protectarray(@{$case[$n]{"arbfile"}}), protectarray(@{$case[$n]{"geofile"}}), protectarray(@{$case[$n]{"otherfile"}}) ) { 
    if (!($fffilename)) { next; }
#   print "DEBUG: subtitution files fffilename = $fffilename\n";
    foreach my $ffilename (bsd_glob("$input_dir/".$fffilename)) {
      $ffilename =~ /(.*)\/((.+?)\.(.+?))$/;
      my $filename=$2;
      print "BATCHER INFO: performing substitutions on $filename\n";
      open(INFILE, ">".$filename) or error_stop("can't open substitute input file $filename");
      open(ORIGINAL, "<".$ffilename) or error_stop("can't open original input file $ffilename");
      while (my $line=<ORIGINAL>) {
        if ($case[$n]{"replacements"}) {
          for my $key ( sort(keys(%{$case[$n]{"replacements"}})) ) {
# now stopping replacements if the string is mentioned as a replacement keyword
            if ($line !~ /\s(R|REPLACE)\s+("|'|)\Q$key\E("|'|\s|$)/) {
              $line =~ s/\Q$key/$case[$n]{"replacements"}{"$key"}/g; # substitute value inplace of name if found
            }
          }
        }
        print INFILE $line;
      }
      close(INFILE);
      close(ORIGINAL);
      copy($filename,$run_record_dir) or error_stop("could not copy $filename to run record directory $run_record_dir");
    }
  }

#-----------------
# deal with geo files that need to have msh files generated from, now located within the working directory, with substitutions already taken place
  foreach my $fffilename ( protectarray(@{$case[$n]{"geofile"}}) ) { 
    if (!($fffilename)) { next; }
#   print "DEBUG: geo files fffilename = $fffilename\n";
    foreach my $ffilename (bsd_glob("$fffilename")) {
      $ffilename =~ /((.+)\.(geo))$/;
      my $filename=$1;
      my $mshname=$2.".msh";
      print "BATCHER INFO: creating msh file $mshname from $filename\n";
      $systemcall="./misc/create_msh/create_msh $filename"; #use ./misc/create_mesh/create_mesh script
      (!(system("$systemcall"))) or error_stop("could not $systemcall");
      copy($mshname,$msh_store_dir) or error_stop("could not copy $mshname to msh store directory $msh_store_dir");
    }
  }

#-----------------
# deal with msh files that are listed to be used in this simulation, with no substitutions
  foreach my $fffilename ( protectarray(@{$case[$n]{"mshfile"}}) ) { 
    if (!($fffilename)) { next; }
    print "DEBUG: msh files fffilename = $fffilename\n";
    foreach my $ffilename (bsd_glob("$fffilename")) {
#      $ffilename =~ /((.+)\.(msh))$/;
      my $filename=$ffilename; # as per batcher_setup.pm, mshfile can contain any file that needs to be copied over to working directory without replacements
      print "BATCHER INFO: copying msh file $filename\n";
      copy($filename,$msh_store_dir) or error_stop("could not copy $filename to msh store directory $msh_store_dir");
    }
  }

#-----------------
  
  if ($parallel) { # run arb jobs in parallel
    # copy everything needed to run_record_dir
    $systemcall="rsync -au * $run_record_dir --exclude batcher_output --exclude batcher_setup.pm";
    (!(system("$systemcall"))) or error_stop("could not $systemcall");

    my $t = threads->new(\&arbthread, \@case, $n, $ndir, $run_record_dir);
    push(@threads,$t);
  } else { # run arb jobs in series
    # run only a single thread and wait for it to finish before moving onto the next job
    &arbthread(\@case, $n, $ndir, $run_record_dir);
  }
}


# reset all input files within the working directory, now at the end of each case
copy_back_input_files();

if ($parallel) {
  # wait for all threads to finish
  foreach (@threads) {
    $_->join;
  }
}



close(OUTPUT);
exit;

#-------------------------------------------------------------------------------
# chomp and remove mac linefeads too if present

sub chompm {
  use strict;
  chomp($_[0]);  # remove linefeed from end
  $_[0]=~tr/\r//d; # remove control m from mac files
}

#-------------------------------------------------------------------------------

sub empty {
  use strict;
  if (!(defined($_[0]))) {
    return 1;
  } elsif ($_[0] eq "") {
    return 1;
  } else {
    return 0;
  }
}

#-------------------------------------------------------------------------------
# opposite of empty

sub nonempty {
  use strict;
  if (empty($_[0])) { return 0; } else { return 1; }
}

#-------------------------------------------------------------------------------
# little wrapper to return value, even if a variable is undefined
# for a single scalar

sub protect {
  if (!(defined($_[0]))) {
    return '';
  } else {
    return $_[0];
  }
}

#-------------------------------------------------------------------------------
# little wrapper to return value, even if a variable is undefined
# for an array

sub protectarray {
  if (!(defined($_[0]))) {
    return '';
  } else {
    return @_;
  }
}

#-------------------------------------------------------------------------------
# whatever string is passed to this routine is output as an error message and all files are reset to their original values
sub error_stop {
  print "BATCHER ERROR: $_[0]\n";
  copy_back_input_files();
  die;
}
#-------------------------------------------------------------------------------
sub copy_back_input_files {
  use File::Copy qw(copy);
  foreach my $filename (bsd_glob("*.arb"),bsd_glob("*.geo"),bsd_glob("*.msh"),bsd_glob("*.pm")) {
    unlink($filename) or warn "BATCHER WARNING: could not delete $filename from working directory\n";
  }
  foreach my $filename (bsd_glob("$input_dir/*")) {
    print "BATCHER_DEBUG: before dying copying back input file $filename to working directory\n";
    copy($filename,".") or die "BATCHER ERROR: could not copy $filename back to working directory\n";
  }
}
#-------------------------------------------------------------------------------

sub arbthread {
  my @case = @{$_[0]};
  my $n = $_[1];
  my $ndir = $_[2];
  my $run_record_dir = $_[3];

  # write headers in $output_dir/batch_data.csv
  if (!($n)) {
    my $variable_line = "# run";
    for my $key ( sort(keys(%{$case[$n]{"replacements"}})) ) {
      $variable_line = $variable_line.", \"$key\"";
    }
  # and output
    for my $key ( sort(keys(%output)) ) {
      $variable_line = $variable_line.", \"$key\"";
    }
    print OUTPUT "$variable_line\n"; 
  }


  my $systemcall="./arb --quiet --quiet-make ".protect($case[$n]{"arboptions"});
  for my $ffilename ( @{$case[$n]{"arbfile"}} ) {
    $systemcall=$systemcall." ".bsd_glob($ffilename);
  }
  
  if ($parallel) {
    # parallel jobs run in run_record_dir
    $systemcall = "cd $run_record_dir; ".$systemcall;
    (!(system("$systemcall"))) or error_stop("could not $systemcall");
  } else {
    # series jobs run in working directory
    (!(system("$systemcall"))) or error_stop("could not $systemcall");
  }

#-----------------
# now extract the data

  my $scr_location = "output/output.scr";
  if ($parallel) {
    $scr_location = "$run_record_dir/output/output.scr";
  }

  if (-e $scr_location) {
    open(INPUT,"<$scr_location");
    while (my $line = <INPUT>) {
      chompm($line);
      if ($line =~ /^\s*CELLS: itotal =\s+(\S+): idomain =\s+(\S+):/) { $output{"itotal"}= $1; $output{"idomain"} = $2 }
      if ($line =~ /^TIMING: cpu time to complete setup routines =\s+(\S+)/) { $output{"setuptime"} = $1; }
#     if ($line =~ /^TIMING: cpu time to complete initial update routines =\s+(\S+)/) { $output{"cputime"} = $output{"cputime"} + $1; }
#     if ($line =~ /^TIMING: cpu time to complete update routines =\s+(\S+)/) { $output{"cputime"} = $output{"cputime"} + $1; }
#     if ($line =~ /^TIMING: cpu time to complete mainsolver routines =\s+(\S+)/) { $output{"cputime"} = $output{"cputime"} + $1; }
      if ($line =~ /^TIMING: total wall time =\s+(\S+)\s*: total cpu time =\s+(\S+)/) { $output{"walltime"} = $1; $output{"cputime"} = $2; }
      if ($line =~ /^INFO: the maximum number of dimensions of any region is\s+(\S+)/) { $output{"dimensions"} = $1; }
      if ($line =~ /^INFO: total number of kernel elements =\s+(\S+)/) { $output{"kernel_elements"} = $1; }
      if ($line =~ /^SUCCESS: the simulation finished gracefully/) { $output{"success"} = 1; }
    }
    close(INPUT);
    print "BATCHER INFO: extracted data from $scr_location\n";
  } else {
    print "BATCHER WARNING: file $scr_location not found\n";
  }

  my $stat_location = "output/output.stat";
  if ($parallel) {
    $stat_location = "$run_record_dir/output/output.stat";
  }
  if (-e $stat_location) {
    open(INPUT,"<$stat_location");
    while (my $line = <INPUT>) {
      chompm($line);
      if ($line =~ /^# NEWTSTEP = \s+(\S+)/) { $output{"nstepmax"} = $1; }
      for my $key ( sort(keys(%output)) ) {
        if ($key =~ /^<.+>$/) { # assume that this is a variable or region, outputting maximum value or number of elements, respectively
          if ($line =~ /^\S+ \S+ \Q$key\E:\s+(max|elements)\s+=\s+(\S+?)(\s|:)/) { $output{"$key"} = "$2"; } # \Q starts to escape special characters, \E stops
        }
      }
    }
    close(INPUT);
    print "BATCHER INFO: extracted data from $stat_location\n";
  } else {
    print "BATCHER WARNING: file $stat_location not found\n";
  }

  { 
    my $variable_line = "# run";
    my $value_line = "$ndir";
  # output inputs
    for my $key ( sort(keys(%{$case[$n]{"replacements"}})) ) {
      $variable_line = $variable_line.", \"$key\"";
      $value_line = $value_line.", ".$case[$n]{"replacements"}{"$key"};
    }
  # and output
    for my $key ( sort(keys(%output)) ) {
      $variable_line = $variable_line.", \"$key\"";
      $value_line = $value_line.", $output{$key}";
    }
  
    if (!($output{"success"})) {
      print OUTPUT "# $value_line\n";
      print OUTPUT "# ERROR: arb run $ndir (the above line) was not successful\n";
      print "BATCHER ERROR: arb run $ndir was not successful\n";
    } else {
      print OUTPUT "$value_line\n";
      print "BATCHER INFO: printed summary data for run $ndir to batch_data.csv\n"
    }
  }

  if ($parallel) {
    # remove files/directories from run_record_dir
    # though, anything in the following grep pattern is *retained*
    opendir(RUNDIR, $run_record_dir) or die "BATCHER ERROR: could not open $run_record_dir\n";
    my @to_delete = grep(!/^\.+|output|tmp|input_mesh|batcher_info.txt|\.arb$/, readdir(RUNDIR));
    closedir(RUNDIR);
    print "BATCHER_INFO: cleaning files in $run_record_dir\n";
    for my $entry (@to_delete) {
      if (-f "$run_record_dir/$entry") {
        unlink("$run_record_dir/$entry");
      } else {
        rmtree("$run_record_dir/$entry");
      }
    }
  }

   my @output_search = ("output/output.stat", "output/output.scr", "output/output_step.csv", "output/convergence_details.txt", "tmp/setup/unwrapped_input.arb", "tmp/setup/variable_list.txt", "tmp/setup/region_list.txt");
   if ($parallel) {
     my @output_msh_files = bsd_glob("$run_record_dir/output/output*.msh");
     foreach my $item (@output_msh_files) {
       $item =~ s/$run_record_dir\///g;
     }
     push(@output_search, @output_msh_files);
   } else {
     push(@output_search, bsd_glob("output/output*.msh"));
   }

 # save all output files that are present, including msh files
   for my $output_file (@output_search) {
     if ($parallel) { # parallel
       my $ls_command = "ls $run_record_dir/output";
       system($ls_command);
       if (-e "$run_record_dir/$output_file") {
         print "DEBUG: moving $run_record_dir/$output_file to $run_record_dir\n";
         move("$run_record_dir/$output_file",$run_record_dir) or error_stop("could not copy $run_record_dir/$output_file to run record directory $run_record_dir");
       } 
     } else { # series
       if (-e "$output_file") {
         copy("$output_file",$run_record_dir) or error_stop("could not copy $output_file to run record directory $run_record_dir"); 
       }
     }
   }

   if ($parallel) {
     # remove trace of everything else
     rmtree("$run_record_dir/output");
     rmtree("$run_record_dir/tmp");
   }
   
# clear all output results before starting the next run
  for my $key ( keys(%output) ) { $output{"$key"}=''; }

# look for user created stopfile
  if (-e $stopfile) {print "BATCHER STOPPING: found $stopfile stop file so stopping the batcher run\n"; last;}

}
