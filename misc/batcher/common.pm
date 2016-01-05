package common;

use strict;
use warnings;
use Exporter qw(import);
our @EXPORT_OK = qw(arbthread chompm empty nonempty protect protectarray error_stop);
 
use File::Glob ':glob'; # deals with whitespace better
use File::Copy qw(move copy);
use Data::Dumper; # TODO temporary
use File::Path qw(mkpath rmtree); # for File::Path version < 2.08, http://perldoc.perl.org/File/Path.html
# TODO needs to open and close OUTPUT

#-------------------------------------------------------------------------------
sub arbthread {
  my @case = @{$_[0]};
# could replace this with @main::case to get access, rather than passing a variable reference
  my $n = $_[1];
  my $ndir = $_[2];
  my $run_record_dir = $_[3];
  my %output = %main::output; # local copy
  my $output_dir = $main::output_dir; # local copy
  my $parallel = $main::parallel; # local copy
  my $msh_store_dir = $main::msh_store_dir; # local copy
  my $prune_output_structure = $main::prune_output_structure; # local copy
  my $stopfile = 'batcher_stop';

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
    open(OUTPUT, ">>$output_dir/batch_data.csv"); # open batch_data csv file
    print OUTPUT "$variable_line\n"; 
    close(OUTPUT);
  }

  #-----------------
  # deal with substitutions
  # loop through each arbfile, geofile and otherfile in the input_dir, copying them over to the working directory while doing the substitutions (and also copying them to the run_record_dirs)
  foreach my $fffilename ( protectarray(@{$case[$n]{"arbfile"}}), protectarray(@{$case[$n]{"geofile"}}), protectarray(@{$case[$n]{"otherfile"}}) ) { 
    if (!("$fffilename")) { next; }
    #print "BATCHER DEBUG: subtitution files fffilename = $fffilename\n";
    foreach my $ffilename (bsd_glob("$main::input_dir/".$fffilename)) {
      $ffilename =~ /(.*)\/((.+?)\.(.+?))$/;
      my $filename=$2;
      $filename = "$run_record_dir/".$filename;
      print "BATCHER INFO: performing substitutions on $filename\n";
      #print "BATCHER DEBUG: input \$ffilename = $ffilename\n";
      #print "BATCHER DEBUG: output \$filename = $filename\n";
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
    }
  }

  #-----------------
  # deal with geo files that need to have msh files generated from, now located within the working directory, with substitutions already taken place
  foreach my $fffilename ( protectarray(@{$case[$n]{"geofile"}}) ) { 
    if (!($fffilename)) { next; }
#   print "BATCHER DEBUG: geo files fffilename = $fffilename\n";
    foreach my $ffilename (bsd_glob("$run_record_dir/$fffilename")) {
      $ffilename =~ /((.+)\.(geo))$/;
      my $filename=$1;
      my $mshname=$2.".msh";
      print "BATCHER INFO: creating msh file $mshname from $filename\n";
      #print "BATCHER DEBUG: running create_msh on $filename\n";
      #print "BATCHER DEBUG: \$fffilename = $fffilename\n";
      #print "BATCHER DEBUG: \$ffilename = $ffilename\n";
      #print "BATCHER DEBUG: \$filename = $filename\n";
      my $systemcall="cd $run_record_dir; ../../misc/create_msh/create_msh $fffilename"; #use ./misc/create_mesh/create_mesh script
      (!(system("$systemcall"))) or error_stop("could not $systemcall");

      #print "BATCHER DEBUG: \$mshname = $mshname\n";
      #print "BATCHER DEBUG: \$msh_store_dir = $msh_store_dir\n";

      copy($mshname,$msh_store_dir) or error_stop("could not copy $mshname to msh store directory $msh_store_dir");
    }
  }

  #-----------------
  # deal with msh files that are listed to be used in this simulation, with no substitutions
  foreach my $fffilename ( protectarray(@{$case[$n]{"mshfile"}}) ) { 
    if (!($fffilename)) { next; }
    print "BATCHER DEBUG: msh files fffilename = $fffilename\n";
    foreach my $ffilename (bsd_glob("$fffilename")) {
#     $ffilename =~ /((.+)\.(msh))$/;
#     my $filename=$1;
      my $filename="$run_record_dir/$ffilename"; # mshfile array is not only for .msh files - see batcher_setup.pm
      print "BATCHER INFO: copying msh file $filename\n";
      copy($filename,$msh_store_dir) or error_stop("could not copy $filename to msh store directory $msh_store_dir");
    }
  }

  my $systemcall="./arb --quiet ".protect($case[$n]{"arboptions"});
  for my $ffilename ( @{$case[$n]{"arbfile"}} ) {
    $systemcall=$systemcall." ".bsd_glob($ffilename);
  }
  
  $systemcall = "cd $run_record_dir; ".$systemcall;
  print "INFO: running `$systemcall`\n";
  (!(system("$systemcall"))) or error_stop("could not $systemcall");

#-----------------
# now extract the data
  
  my $scr_location = "$run_record_dir/output/output.scr";

  if (-e $scr_location) {
    open(INPUT,"<$scr_location") or die("couldn't open $scr_location");
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
      if ($line =~ /^INFO: peak memory usage for arb \(rss\) was\s+(\S+)/) { $output{"memoryrss"} = $1; }
      if ($line =~ /^SUCCESS: the simulation finished gracefully/) { $output{"success"} = 1; }
    }
    close(INPUT);
    print "BATCHER INFO: extracted data from $scr_location\n";
  } else {
    print "BATCHER WARNING: file $scr_location not found\n";
  }

  my $stat_location = "$run_record_dir/output/output.stat";
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
    
    open(OUTPUT, ">>$output_dir/batch_data.csv"); # open batch_data csv file
    if (!($output{"success"})) {
      print OUTPUT "# $value_line\n";
      print OUTPUT "# ERROR: arb run $ndir (the above line) was not successful\n";
      print "BATCHER ERROR: arb run $ndir was not successful\n";
    } else {
      print OUTPUT "$value_line\n";
      print "BATCHER INFO: printed summary data for run $ndir to batch_data.csv\n"
    }
    close(OUTPUT);
  }

  if ($prune_output_structure) {
    
    # remove files/directories from run_record_dir
    # though, anything in the following grep pattern is *retained*
    opendir(RUNDIR, $run_record_dir) or die "BATCHER ERROR: could not open $run_record_dir\n";
    my @to_delete = grep(!/^\.+|output|tmp|input_mesh|batcher_info.txt|batcher_pbs_variables.txt|job.pbs|\.arb$/, readdir(RUNDIR));
    closedir(RUNDIR);
    print "BATCHER INFO: cleaning files in $run_record_dir\n";
    for my $entry (@to_delete) {
      if (-f "$run_record_dir/$entry") {
        unlink("$run_record_dir/$entry");
      } else {
        rmtree("$run_record_dir/$entry");
      }
    }
    

    my @output_search = ("output/output.stat", "output/output.scr", "output/output_step.csv", "output/output_process_log.csv", "output/convergence_details.txt", "tmp/setup/current_unwrapped_input.arb", "tmp/setup/variable_list.txt", "tmp/setup/region_list.txt");

    my @output_msh_files = bsd_glob("$run_record_dir/output/output*.msh");
    foreach my $item (@output_msh_files) {
      $item =~ s/$run_record_dir\///g;
    }
    push(@output_search, @output_msh_files);

  # save all output files that are present, including msh files
    for my $output_file (@output_search) {
      #my $ls_command = "ls $run_record_dir/output";
      #system($ls_command);
      if (-e "$run_record_dir/$output_file") {
        #print "BATCHER DEBUG: moving $run_record_dir/$output_file to $run_record_dir\n";
        move("$run_record_dir/$output_file",$run_record_dir) or error_stop("could not copy $run_record_dir/$output_file to run record directory $run_record_dir");
      } 
    }

    # remove trace of everything else
    rmtree("$run_record_dir/output");
    rmtree("$run_record_dir/tmp");
  }
# clear all output results before starting the next run
  for my $key ( keys(%output) ) { $output{"$key"}=''; }

# look for user created stopfile
  if (-e $stopfile) {print "BATCHER STOPPING: found $stopfile stop file so stopping the batcher run\n"; last;}
}

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
  die;
}

#-------------------------------------------------------------------------------

1;
