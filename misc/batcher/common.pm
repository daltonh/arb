# this is the Common module for batcher.pl (arb_batcher)
package Common;

use strict;
use warnings;
use Exporter qw(import);
our @EXPORT_OK = qw(arbthread chompm empty nonempty protect protectarray error_stop);
 
use File::Glob ':glob'; # deals with whitespace better
use File::Copy qw(move copy);
use Data::Dumper;
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
  my $debug = 0;

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
# loop through each arbfile, geofile and includefile specified by the user, copying them over to the directories which are in the same location in each run directory relative to the working directory while doing the substitutions
  foreach my $filetype ( "arbfile", "geofile", "includefile" ) {

# if using string replacements write the batcher_include.arb file with the replacements in it as globals
    if ($filetype eq "arbfile" && $::use_string_variables) {
      print "BATCHER INFO: creating batcher_include.arb file for batcher string variables\n";
      open(INCLUDE_FILE,">$run_record_dir/batcher_include.arb");
      print INCLUDE_FILE "# batcher $run_record_dir string variable replacement file\n";
      if ($case[$n]{"replacements"}) {
        for my $key ( sort(keys(%{$case[$n]{"replacements"}})) ) {
          print INCLUDE_FILE "{{ string_set('$key','$case[$n]{replacements}{$key}','global'); }}\n";
        }
      }
    }

    foreach my $fffilename ( protectarray(@{$case[$n]{$filetype}}) ) { 
      if (!("$fffilename")) { next; }
      if ($debug) {print "BATCHER DEBUG: substitution files fffilename = $fffilename\n";}
      foreach my $ffilename (bsd_glob($fffilename)) {
        if (!($::use_string_variables)) {print "BATCHER INFO: performing substitutions on $filetype $ffilename\n";}
# create directory structure if this isn't going to be placed directly in the run directory
        if ($ffilename =~ /^(.+)\/(.+?)$/) {
          my $create_path = "$run_record_dir/".$1;
          print "BATCHER INFO: create_path = $create_path\n";
          mkpath($create_path) or error_stop("could not create path $create_path required to place $filetype input file $ffilename correctly in the run directory");
        }
# copy over each arb file, doing replacements if necessary
        open(ORIGINAL, "<".$ffilename) or error_stop("can't open the original $filetype input file $ffilename within the working directory for some reason");
        my $filename = "$run_record_dir/".$ffilename;
        if ($debug) {print "BATCHER DEBUG: substitution files filename = $filename\n";}
        open(INFILE, ">".$filename) or error_stop("can't open substitute $filetype input file $filename");
        while (my $line=<ORIGINAL>) {
          if (!($::use_string_variables) && $case[$n]{"replacements"}) {
            for my $key ( sort(keys(%{$case[$n]{"replacements"}})) ) {
# use <<batchercomment>> and <<nobatchercomment>> (see ref: general_replacements in setup_equations) for more consistent and controllable behaviour
              $line =~ s/\Q$key/$case[$n]{"replacements"}{"$key"}/g; # substitute value inplace of name if found
            }
          }
          print INFILE $line;
        }
        close(INFILE);
        close(ORIGINAL);
      }
    }
  }

  #-----------------
  # create msh files from any geofiles using the run directory's local create_mesh script which will place the msh files at the same location as the geo file
  foreach my $fffilename ( protectarray(@{$case[$n]{"geofile"}}) ) { 
    if (!($fffilename)) { next; }
    if ($debug) {print "BATCHER DEBUG: geo files fffilename = $fffilename\n";}
    foreach my $ffilename (bsd_glob("$fffilename")) {
      $ffilename =~ /(.+)\.geo$/;
      my $mshname=$1.".msh";
      print "BATCHER INFO: creating msh file $mshname from $ffilename within $run_record_dir\n";
      my $systemcall="cd $run_record_dir; $::arb_bin_dir/arb_create_msh $ffilename"; # running arb_create_msh (which is actually in misc/create_msh) from record_dir
      (!(system("$systemcall"))) or error_stop("could not $systemcall");

      if ($debug) {print "BATCHER DEBUG: \$mshname = $mshname\n";}
    }
  }

  #-----------------
  # deal with msh files and now whole directories that are listed to be used in this simulation, with no substitutions
  foreach my $fffilename ( protectarray(@{$case[$n]{"mshfile"}}) ) { 
    if (!($fffilename)) { next; }
    if ($debug) {print "BATCHER DEBUG: msh files fffilename = $fffilename\n";}
    foreach my $ffilename (bsd_glob("$fffilename")) {
      print "BATCHER INFO: copying msh file or directory $ffilename\n";
# create directory structure if this isn't going to be placed directly in the run directory
      if ($ffilename =~ /^(.+)\/(.+?)$/) {
        my $create_path = "$run_record_dir/".$1;
        print "BATCHER INFO: create_path = $create_path\n";
        mkpath($create_path) or error_stop("could not create path $create_path required to place mshfile input file or directory $ffilename correctly in the run directory");
      }
      my $filename = "$run_record_dir/".$ffilename;
      my $systemcall="cp -R $ffilename $filename"; # using system cp function instead of the perl one as the perl version on osx can't handle recursive copying
      (!(system("$systemcall"))) or error_stop("could not $systemcall");
    }
  }

  {
    my $systemcall;
    if (nonempty($case[$n]{"runcommand"})) {
      $systemcall=protect($case[$n]{"runcommand"}); # run this (probably) script instead of arb directly
    } else {
# assemble arb command line
      $systemcall="$::arb_script --quiet";
# deal with previous output reuse
# TODO: not working
      if ($::use_previous_build && $n > 0) {
        my $previous_run_record_dir = "../run_".scalar($ndir-1)."/output";
        if ($debug) {print "BATCHER DEBUG: previous_run_record_dir = $previous_run_record_dir\n";}
        $systemcall.=" -po $previous_run_record_dir";
      }
# arboptions
      if (nonempty($case[$n]{"arboptions"})) {$systemcall.=" $case[$n]{arboptions}";};
# if using string_variables add batcher_include.pl
      if ($::use_string_variables) {$systemcall.=" batcher_include.arb";};
# and finally the arbfiles
      for my $ffilename ( @{$case[$n]{"arbfile"}} ) {
        $systemcall.=" ".bsd_glob($ffilename);
      }
    }
    
    $systemcall = "cd $run_record_dir; $systemcall"; # run the arb command
    print "BATCHER INFO: running `$systemcall`\n";
    if (system($systemcall) == -1) { error_stop("error trying to run arb from batcher: stopping all batcher runs: could not $systemcall"); }
  }

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

  if ($::prune_output_structure) {
    # remove files/directories from run_record_dir
    print "BATCHER INFO: pruning files in $run_record_dir\n";
    foreach my $prune_glob (@::prune_files) {
      if ($debug) {print "BATCHER DEBUG: removing files: prune_glob = $prune_glob\n";}
      my @found_files = bsd_glob("$run_record_dir/$prune_glob");
      foreach my $item (@found_files) {
        if ($item =~ /\.\./) {
          print "WARNING: skipping deletion of $item due to repeated .. in pathname\n";
        } elsif ( -e $item ) {
          if ($debug) {print "BATCHER DEBUG: removing files: item = $item\n";}
          rmtree("$item") or print "WARNING: could not delete $item\n";
        }
      }
    }
  }

# clear all output results before starting the next run
  for my $key ( keys(%output) ) { $output{"$key"}=''; }

# look for user created stopfile
  if (-e $::stopfile) {print "BATCHER STOPPING: found $::stopfile stop file so stopping the batcher run\n"; last;}
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
