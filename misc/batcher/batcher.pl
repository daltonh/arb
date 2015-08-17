#!/usr/bin/perl -w
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
use batcher_setup qw(case_setup output_setup $parallel $pbs $pbs_jobname $pbs_walltime $pbs_pmem $pbs_queue_name $pbs_module_load_commands); # brings in the user-written module that defines case-specific data

our $run_in_main = 1;
if ($parallel) {
  $run_in_main = 0;
}
if ($pbs) {
  $run_in_main = 0;
  $parallel = 0;
}
if ($parallel) {
  $run_in_main = 0;
}

use lib './misc/batcher';
use common qw(arbthread chompm empty nonempty protect protectarray error_stop copy_back_input_files);

my @threads;
my $systemcall;
our $output_dir="batcher_output"; # this directory will store all of the output
our $input_dir="$output_dir/input_files"; # this will contain copies of the original arb, geo and msh input files
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
  open(PBS, ">$run_record_dir/batcher_pbs_variables.txt") or error_stop("can't open batcher_pbs_variables.txt file in $run_record_dir");
  print PBS "#INFO: code within this file is evaluated by batcher\n";
  print PBS "#INFO: information for present case (likely set by batcher_setup.pm)\n";
  print PBS Data::Dumper->Dump([$case[$n]], ['*present_case']);
  print PBS "#INFO: we're we are:\n";
  print PBS Data::Dumper->Dump([$run_record_dir], ['*run_record_dir']);
  print PBS Data::Dumper->Dump([\%output], ['*output']);
  print PBS Data::Dumper->Dump([$n], ['*n']);
  print PBS Data::Dumper->Dump([$ndir], ['*ndir']);
  close(PBS);

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
      $ffilename =~ /((.+)\.(msh))$/;
      my $filename=$1;
      print "BATCHER INFO: copying msh file $filename\n";
      copy($filename,$msh_store_dir) or error_stop("could not copy $filename to msh store directory $msh_store_dir");
    }
  }

#-----------------
  if (not $run_in_main) {
    $systemcall="rsync -au * $run_record_dir --exclude batcher_output --exclude batcher_setup.pm";
    (!(system("$systemcall"))) or error_stop("could not $systemcall");
  }

  # extract requested number of threads, if it exists
  my $nthreads = 1;
  if ($case[$n]{'arboptions'}) {
    my $specified_arboptions = $case[$n]{'arboptions'};
    if ($specified_arboptions =~ /omp(\d+)/) { $nthreads = $1;}
  }

  my $hostname = `'hostname'`;
  my $module_load_intel = '';
  my $module_load_maxima = '';

  if ($pbs) {
    (my $pbs_job_contents = qq{#!/bin/bash
    #PBS -S /bin/bash
    ##PBS -M skink.notification\@gmail.com
    ##PBS -m bea
    #PBS -N $pbs_jobname\_run\_$ndir
    
    # ensure job spans only one node
    #PBS -l nodes=1:ppn=$nthreads
    #PBS -l walltime=$pbs_walltime
    #PBS -l pmem=$pbs_pmem
    #PBS -q $pbs_queue_name
    
    #send pbs output to batcher_output/run_*
    #PBS -e $run_record_dir/
    #PBS -o $run_record_dir/
    
    
    # even though job will run in eg. \$PBS_O_WORKDIR/batcher_output/run_0
    # the script ./misc/run_and_collect.pl is designed to be called from \$PBS_O_WORKDIR
    rundir=\$PBS_O_WORKDIR/
    
    echo '\$PATH:' \$PATH
    echo '\$SHELL:' \$SHELL
    echo '\$PBS_JOBID: ' \$PBS_JOBID
    echo '\$PBS_O_HOST: ' \$PBS_O_HOST
    echo '\$rundir: ' \$rundir
    echo 'uname -a: ' `uname -a`
    echo 'STARTING DATE ' `date`
    echo

    # load modules if specified
    $pbs_module_load_commands
    

    # move to rundir
    cd \$rundir
    # run job
    ./misc/batcher/run_and_collect.pl $ndir

    echo 'ENDING DATE ' `date`
    echo "FINISHED"
    exit 0
    }) =~ s/^ {4}//mg; # m implies treat as multiple lines (http://perldoc.perl.org/perlre.html#Modifiers)
    my $pbs_job_file = "$run_record_dir/job.pbs";
    open(PBSFILE,">",$pbs_job_file);
    print PBSFILE $pbs_job_contents;
    close(PBSFILE);
    #system("./misc/batcher/run_and_collect.pl $ndir");
    system("qsub $run_record_dir/job.pbs");
    copy_back_input_files();
  } elsif ($parallel) { # run arb jobs in parallel
    print "DEBUG running in non-pbs parallel mode\n";
    # copy everything needed to run_record_dir
    my $t = threads->new(\&arbthread, \@case, $n, $ndir, $run_record_dir);
    push(@threads,$t); 
  } else { # run arb jobs in series
    # run only a single thread and wait for it to finish before moving onto the next job
    print "DEBUG running in series mode\n";
    &arbthread(\@case, $n, $ndir, $run_record_dir);
  }   
}

copy_back_input_files();

if ($parallel) {
  # wait for all threads to finish
  foreach (@threads) {
    $_->join;
  }
}

close(OUTPUT);
exit;
