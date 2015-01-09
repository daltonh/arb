#!/usr/bin/perl -w
# perl script to run sequential arb simulations, changing text in the constants.in and equations.in files, and then extracting data from the output files

# usage
# 1) make a copy of the batcher directory within misc, and edit below to reflect the substitutions you want and data you want extracted (ie, in two places)
# 2) edit an input files to put placeholders where the substitutions should take place - I suggest placeholders such as <<n0>> for variable <n0> - whatever you use make sure the only occurances of these strings in the input file are the ones you want changed.  Note that no INCLUDE'd files will have replacements done, just files within the working directory.  So if you are using an INCLUDE'd input file structure and want to change something in the template files (why would you though???) first create the unwrapped file (grab it from tmp/setup/unwrapped_input.arb) and then use this as the batcher run input file.
# 3) if the input file is the only .arb file in the input directory skip this step.  Otherwise, set $input_file = 'your_input_file_name.arb' ('something.in' will also work)
# 3) run from the working directory of the arb simulation - ie ./misc/batcher/batcher.pl
# 4) output will be placed in batcher_output off the working directory
# 5) copy the batcher_output directory to somewhere else before doing the next run
# 6) touch batcher_stop in the working directory to stop the run

# notes from Lachlan's changes from 151214
# The version attached has these minor changes:
#  1. All .geo files are copied to the working directory before any meshing begins (otherwise errors can occur if there are required .geo files in "include" statements that aren't yet in the working directory)
#  2. There is a new array called @exclude_geo, any .geo files in this won't be meshed (but they will be still available, with appropriate batcher replacements, for use in any include statements). A copy of these .geo files, post replacements, is still stored in $msh_store_dir. You can use this for snippets (eg. free_surface_halfplane_extruded_structured_variables.geo) that need to be included in other .geo files, but not meshed.

use strict;
use File::Path qw(mkpath rmtree); # for File::Path version < 2.08, http://perldoc.perl.org/File/Path.html
use File::Copy qw(move copy);
use File::Glob ':glob'; # deals with whitespace better
my $output_dir="batcher_output"; # this directory will contain details of the runs, including copies of the original input files
my ($line, $n, $key, $systemcall, $filename, $ffilename, $run_record_dir, $output_file, $variable_line, $value_line);
my ($msh_store_dir);
my $arb_options="--omp8 --opt"; # quiet (-q) is already included
my $input_file=""; # set this below to a specific file name or glob pattern if you don't want all the *.arb files within the working directory to be used as input
my $geo_file=""; # set this below to a specific file name or glob pattern if you don't want all the *.geo files within the working directory to be used
my $continue=0; # set this to true (1) to allow continuation from a previous run, with new (additional) runs to take place

# create list (array of hashes) of runs that includes variables to change and values
my @runs=();
my @geo=(); #this will constain .geo-specific replacements
my @exclude_geo=(); #.geo files in this array will not be meshed (useful for segments of .geo code that are "include"d in other .geo files)

# create list (hash) of variables to be output
my %outputs=();

# remove stopfile from previous run if it exists
my $stopfile="batcher_stop";
if (-e $stopfile) { unlink($stopfile) or die "ERROR: could not remove $stopfile stop file from the previous run\n"; }

# find starting run index
my $nrunstart=0;
if ($continue) {
  if (! -d $output_dir) { die "BATCHER ERROR: continue is on in batcher.pl but output directory does not exist\n"; }
# cycle through existing run directories looking for previously used highest
  while (1) {
    $run_record_dir = "$output_dir/run_$nrunstart";
    if (!(-d $run_record_dir)) { print "BATCHER INFO: Continuing on from previous batcher run: Next batcher run will be $nrunstart\n"; last; }
    $nrunstart++;
    if ($nrunstart > 100000) { die "BATCHER ERROR: Continue is on but could not find last previous run directory\n"; }
  }
}
my $m=$nrunstart-1;
####################################################################################
# setup runs array here by specifying strings to replace with appropriate values
# runs is an array (one element for each run) containing hashes in the format string -> substitution

$input_file = 'impacting_volume_of_fluid_drop_in_halfplane.arb';

@exclude_geo = ('free_surface_halfplane_extruded_structured_variables.geo'); # meshing of these .geo files will be skipped (though they will still be made available for use in include statements)

for my $length_extension_factor ( '1.0' ) {
  for my $width_extension_factor ( '1.0', '2.0', '3.0' ) {
      $m++;
      $geo[$m]{"<<length_extension_factor>>"} = $length_extension_factor;
      $geo[$m]{"<<width_extension_factor>>"} = $width_extension_factor;
  }
}

####################################################################################
# setup hash of output variables here
# first there are some standard ones, not delimited by <> meaning that they aren't normal variables
# don't edit these
$outputs{"nstepmax"}='';
#$outputs{"dimensions"}='';
$outputs{"itotal"}='';
$outputs{"idomain"}='';
$outputs{"cputime"}='';
#$outputs{"setuptime"}='';
$outputs{"walltime"}='';
$outputs{"kernel_elements"}='';
$outputs{"success"}='';

# now optional ones (variables) go here
# edit these, add what you want
$outputs{"<dt>"}='';
$outputs{"<CFL>"}='';
$outputs{"<t_out>"}='';
$outputs{"<error_initial_initial1>"}='';
$outputs{"<error_final1>"}='';
$outputs{"<error_final2>"}='';

####################################################################################
# main code below this
my $nruns = $m;

# make output directory and copy over original .arb files
if (! -d "build") { die "BATCHER ERROR: call this script from working directory\n"; }

if (!($input_file)) { $input_file = '*.arb'; }
if (@geo) { if (!($geo_file)) { $geo_file = '*.geo'; } }

if (!($continue)) {
  if (! -d $output_dir) { mkpath($output_dir) or die "BATCHER ERROR: could not create $output_dir\n"; } # for File::Path version < 2.08, http://perldoc.perl.org/File/Path.html
  if (bsd_glob("$output_dir/*")) { die "BATCHER ERROR: batcher will not run unless batcher_output directory is empty or non-existent (this is a safety feature!)\n"; }
  foreach $filename (bsd_glob($input_file)) {
    copy($filename,$output_dir) or die "BATCHER ERROR: could not copy $filename to directory $output_dir\n";
  }
  if (@geo) { # start .geo replacement procedure only if required (i.e. non-empty @geo array)
  # copy the geo file across too (so that we can do replacements on this)
    foreach $filename (bsd_glob($geo_file)) {
      copy($filename,$output_dir) or die "BATCHER ERROR: could not copy $filename to directory $output_dir\n";
    }
  }
}

# open batch_data output file
open(OUTPUT, ">>$output_dir/batch_data.csv"); # open new batch_data file

#-----------------
print "BATCHER INFO: looping through runs $nrunstart to $nruns\n";

# now loop through each run
for $n ( $nrunstart .. $nruns ) {

# create output directory to record run
  $run_record_dir = "$output_dir/run_$n";
  mkpath($run_record_dir) or die "BATCHER ERROR: could not create $run_record_dir\n";

#-----------------
# loop through each input file in the output_dir, copying the file to the working directory while doing the substitutions
  foreach $ffilename (bsd_glob("$output_dir/".$input_file)) {
    ($filename) = $ffilename =~ /$output_dir\/(.+\.(arb|in))/;
    print "BATCHER INFO: creating filename = $filename\n";
    unlink($filename) or die "BATCHER ERROR: could not remove $filename in directory $output_dir\n";

    open(INFILE, ">".$filename) or die "BATCHER ERROR: can't open substitute input file $filename\n";
    open(ORIGINAL, "<".$ffilename) or die "BATCHER ERROR: can't open original input file $ffilename\n";
    while ($line=<ORIGINAL>) {
      for $key ( keys(%{$runs[$n]}) ) {
        $line =~ s/\Q$key/$runs[$n]{"$key"}/g; # substitute value inplace of name if found
      }
      print INFILE $line;
    }
    close(INFILE);
    close(ORIGINAL);
    copy($filename,$run_record_dir) or die "BATCHER ERROR: could not copy $filename to run record directory $run_record_dir\n";
  }

  if (@geo) {
# loop through each geo file in the output_dir, copying the file to the working directory while doing the substitutions
    foreach $ffilename (bsd_glob("$output_dir/".$geo_file)) {
      ($filename) = $ffilename =~ /$output_dir\/(.+\.(geo))/;
      print "BATCHER INFO: creating filename = $filename\n";
      unlink($filename) or die "BATCHER ERROR: could not remove $filename in directory $output_dir\n"; #remove the geo file in the working directory (we have a master copy in output_dir)

      open(INFILE, ">".$filename) or die "BATCHER ERROR: can't open substitute input file $filename\n";
      open(ORIGINAL, "<".$ffilename) or die "BATCHER ERROR: can't open original input file $ffilename\n";
      while ($line=<ORIGINAL>) {
        for $key ( keys(%{$geo[$n]}) ) {
          $line =~ s/\Q$key/$geo[$n]{"$key"}/g; # substitute value inplace of name if found
        }
        print INFILE $line;        
      }
      close(INFILE);
      close(ORIGINAL);
    }
    $msh_store_dir = "$run_record_dir/input_mesh"; 
    mkpath($msh_store_dir) or die "BATCHER ERROR: could not create $msh_store_dir\n";
    foreach $ffilename (bsd_glob($geo_file)) {
      ($filename) = $ffilename =~ /(.+\.(geo))/;
# now do the meshing
      my $gmsh_call='';
#      $gmsh_call="gmsh -2 $filename"; #mesh using gmsh directly
#      $gmsh_call="./misc/create_msh/create_msh 2 1"; #use ./misc/create_mesh/create_mesh script
# new create_msh version accepts filename and works out dimensions for itself
      $gmsh_call="./misc/create_msh/create_msh $filename"; #use ./misc/create_mesh/create_mesh script
      if (!grep(/^$ffilename$/,@exclude_geo)) { # check if current .geo file is not in the list of excluded .geo files
        print "BATCHER INFO: meshing geofile using $gmsh_call\n";
        (!(system($gmsh_call))) or die "ERROR: could not $gmsh_call\n";
        copy($filename,$msh_store_dir) or die "BATCHER ERROR: could not copy $filename to run record directory $run_record_dir\n"; #keep a copy of any .geo files in the run_record_dir
#        copy($filename,$run_record_dir) or die "BATCHER ERROR: could not copy $filename to run record directory $run_record_dir\n"; #keep a copy of any .geo files in the run_record_dir
        $filename =~ s/\.geo$/.msh/;  
        copy($filename,$msh_store_dir) or die "BATCHER ERROR: could not copy $filename to mesh storage directory $run_record_dir\n"; #also keep a copy of any .msh files in msh_store_dir
#     copy($filename,$run_record_dir) or die "BATCHER ERROR: could not copy $filename to mesh storage directory $run_record_dir\n"; #also keep a copy of any .msh files in msh_store_dir
      } else {
      print "INFO: meshing of $ffilename has been bypassed\n";
      copy($filename,$msh_store_dir) or die "BATCHER ERROR: could not copy $filename to run record directory $run_record_dir\n"; #only copy the .geo file (as no mesh has been made for this .geo file)
      }
    }
  }
#-----------------
# now run arb
  $systemcall="./arb -q $arb_options ".bsd_glob($input_file);
  (!(system("$systemcall"))) or die "ERROR: could not $systemcall\n";
 
#-----------------
# now extract the data

  if (-e "output/output.scr") {
    open(INPUT,"<output/output.scr");
    while ($line = <INPUT>) {
      chompm($line);
      if ($line =~ /^\s*CELLS: itotal =\s+(\S+): idomain =\s+(\S+):/) { $outputs{"itotal"}= $1; $outputs{"idomain"} = $2 }
      if ($line =~ /^TIMING: cpu time to complete setup routines =\s+(\S+)/) { $outputs{"setuptime"} = $1; }
#     if ($line =~ /^TIMING: cpu time to complete initial update routines =\s+(\S+)/) { $outputs{"cputime"} = $outputs{"cputime"} + $1; }
#     if ($line =~ /^TIMING: cpu time to complete update routines =\s+(\S+)/) { $outputs{"cputime"} = $outputs{"cputime"} + $1; }
#     if ($line =~ /^TIMING: cpu time to complete mainsolver routines =\s+(\S+)/) { $outputs{"cputime"} = $outputs{"cputime"} + $1; }
      if ($line =~ /^TIMING: total wall time =\s+(\S+)\s*: total cpu time =\s+(\S+)/) { $outputs{"walltime"} = $1; $outputs{"cputime"} = $2; }
      if ($line =~ /^INFO: the maximum number of dimensions of any region is\s+(\S+)/) { $outputs{"dimensions"} = $1; }
      if ($line =~ /^INFO: total number of kernel elements =\s+(\S+)/) { $outputs{"kernel_elements"} = $1; }
      if ($line =~ /^SUCCESS: the simulation finished gracefully/) { $outputs{"success"} = 1; }
    }
    close(INPUT);
    print "BATCHER INFO: extracted data from output/output.scr\n"
  } else {
    print "BATCHER WARNING: file output/output.scr not found\n"
  }

  if (-e "output/output.stat") {
    open(INPUT,"<output/output.stat");
    while ($line = <INPUT>) {
      chompm($line);
      if ($line =~ /^# NEWTSTEP = \s+(\S+)/) { $outputs{"nstepmax"} = $1; }
      for $key ( keys(%outputs) ) {
        if ($key =~ /^<.+>$/) { # assume that this is a variable from constants.in or equations.in
          if ($line =~ /^\S+ \Q$key\E: max\s+(\S+)\s/) { $outputs{"$key"} = "$1"; } # \Q starts to escape special characters, \E stops
        }
      }
    }
    close(INPUT);
    print "BATCHER INFO: extracted data from output/output.stat\n"
  } else {
    print "BATCHER WARNING: file output/output.stat not found\n"
  }

  $variable_line = "# run";
  $value_line = "$n";
# output inputs
  for $key ( keys(%{$runs[$n]}) ) {
    $variable_line = $variable_line.", \"$key\"";
    $value_line = $value_line.", ".$runs[$n]{"$key"};
  }
  if (@geo) {
  # output .geo replacements too
    for $key ( keys(%{$geo[$n]}) ) {
      $variable_line = $variable_line.", \"$key\"";
      $value_line = $value_line.", ".$geo[$n]{"$key"};
    }
  }
# and outputs
  for $key ( sort(keys(%outputs)) ) {
    $variable_line = $variable_line.", \"$key\"";
    $value_line = $value_line.", $outputs{$key}";
  }
  if (!($n)) {print OUTPUT "$variable_line\n"; } # now only print headings at top of file, ie, for first run
  if (!($outputs{"success"})) {
    print OUTPUT "# $value_line\n";
    print OUTPUT "# ERROR: arb run $n (the above line) was not successful\n";
    print "BATCHER ERROR: arb run $n was not successful\n";
  } else {
    print OUTPUT "$value_line\n";
    print "BATCHER INFO: printed summary data for run $n to batch_data.csv\n"
  }

# if (!($outputs{"success"})) {
#   print OUTPUT "# ERROR: arb run did not converge\n";
#   print "BATCHER ERROR: arb run did not converge: success = $outputs{success}\n";
# now keep running instead of dying
#   close(OUTPUT);
#   die;
# } else {
#   $variable_line = "# run";
#   $value_line = "$n";
# output inputs
#   for $key ( keys(%{$runs[$n]}) ) {
#     $variable_line = $variable_line.", $key";
#     $value_line = $value_line.", $runs[$n]{$key}";
#   }
# and outputs
#   for $key ( sort(keys(%outputs)) ) {
#     if (nonempty($outputs{"$key"})) {
#       $variable_line = $variable_line.", $key";
#       $value_line = $value_line.", $outputs{$key}";
#     }
#   }
#   print OUTPUT "$variable_line\n$value_line\n";
# }

# save all output files that are present, including msh files
  for $output_file ("output/output.stat", "output/output.scr", "output/output_step.csv", bsd_glob("output/output*.msh")) {
# save all output files that are present, except for msh
# for $output_file ("output/output.stat", "output/output.scr", "output/output_step.csv") {
    if (-e "$output_file") {
      copy("$output_file",$run_record_dir) or die "BATCHER ERROR: could not copy $output_file to run record directory $run_record_dir\n";
    }
  }

# clear all output results before starting the next run
  for $key ( keys(%outputs) ) { $outputs{"$key"}=''; }

# look for user created stopfile
  if (-e $stopfile) {print "BATCHER STOPPING: found $stopfile stop file so stoping the batcher run\n"; last;}
}

# finally, place originals back in directory again
foreach $ffilename (bsd_glob("$output_dir/".$input_file)) {
  ($filename) = $ffilename =~ /$output_dir\/(.+\.(arb|in))/;
  print "BATCHER INFO: copying back original filename = $filename\n";
  unlink($filename) or die "BATCHER ERROR: could not remove $filename from working directory\n";
  copy($ffilename,".") or die "BATCHER ERROR: could not copy $filename back to working directory\n";
}

if (@geo) {
  foreach $ffilename (bsd_glob("$output_dir/".$geo_file)) { # also copy back .geo files
    ($filename) = $ffilename =~ /$output_dir\/(.+\.(geo))/;
    print "BATCHER INFO: copying back original filename = $filename\n";
    unlink($filename) or die "BATCHER ERROR: could not remove $filename from working directory\n";
    copy($ffilename,".") or die "BATCHER ERROR: could not copy $filename back to working directory\n";
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


