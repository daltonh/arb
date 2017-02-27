#!/usr/bin/env perl
# perl script to run sequential arb simulations, changing text in both arb and geo files, and then extracting data from the output files
# this file is linked to arb_batcher in bin

# usage
# 1) create the perl module batcher_setup within the arb working directory which contains the subs case_setup and output_setup
# see instructions in the example module file included in misc/batcher/examples/parasitic_currents/batcher_setup.pm
# 2) run batcher, while at the same time including this module using
#
#     arb_batcher -m batcher_setup.pm
#
# 3) output will be placed in batcher_output off the working directory
# 4) copy the batcher_output directory to somewhere else before doing the next run, otherwise some of the input files will be clobbered (change this behaviour by using the no-continue flag below)
# 5) touch batcher_stop in the working directory to stop the run (a subsequent touch kill will stop the arb job as quickly as possible too)
#
# large re-write 20/4/15, daltonh
# another re-write 27/2/17 for v0.57 (roaming), daltonh
# NB: in roaming mode, uses only arb_batcher script location to locate other resources

use strict;
use warnings;
use Data::Dumper;
use File::Path qw(mkpath rmtree); # for File::Path version < 2.08, http://perldoc.perl.org/File/Path.html
use File::Copy qw(move copy);
use File::Glob ':glob'; # deals with whitespace better
use Thread; # for simultaneous jobs
use FindBin;
use lib "$FindBin::RealBin/"; # this will be the script location, with links resolved, which is where Common.pl is
use Common qw(arbthread chompm empty nonempty protect protectarray error_stop);

# batcher_setup should be found in the working_dir, which is the directory from which the script is run
use batcher_setup qw(simulation_setup case_setup output_setup); # brings in the user-written module that defines case-specific data

my $batcher_dir="$FindBin::RealBin"; # batcher_dir is where the batcher.pl script is
my ($arb_dir)=$batcher_dir=~/(^.*)\/misc\/batcher$/; # arb_dir is the arb (root) dir
our $arb_bin_dir="$arb_dir/bin"; # this is the arb bin dir, used to access other arb executables globally
our $arb_script="$arb_bin_dir/arb"; # this is the arb script, user to run arb globally
print "INFO: batcher_dir = $batcher_dir: arb_dir = $arb_dir: arb_bin_dir = $arb_bin_dir: arb_script = $arb_script\n";
our $output_dir="batcher_output"; # this directory will store all of the output, which is off the working dir from which batcher is called 


# whether system using sun grid engine (in place of pbs)
our $sge_variant = 0;
my @threads;
my $systemcall;
our $continue=0; # default value of 0 implies that run will not continue, but 

$parallel $pbs $pbs_jobname $pbs_walltime $pbs_pmem $pbs_queue_name $pbs_module_load_commands $prune_output_structure

simulation_setup(); 

my @case=case_setup(); # array that specifies information about each case that is being run that is initialised using case_setup within batcher_setup.pm
my @output_keys=output_setup(); # create list output keys of variables to be output that is also initialised within batcher_setup.pm
# convert this to output hash that will store the values
our %output=();
for my $key (@output_keys) {
  $output{$key} = '';
}
my $runcommand=''; # if this variable is non-empty, run this command instead of the arb script directly

# remove stopfile from previous run if it exists
our $stopfile="batcher_stop";
if (-e $stopfile) { unlink($stopfile) or die "BATCHER ERROR: could not remove $stopfile stop file from the previous run\n"; }

# make output directory
if (-d $output_dir && !($continue)) { # for File::Path version < 2.08, http://perldoc.perl.org/File/Path.html
  die "BATCHER ERROR: batcher will not run unless batcher_output directory is empty or non-existent (this is a safety feature!) or the continue flag is on\n";
} elsif (! -d $output_dir) {
  mkpath($output_dir) or die "BATCHER ERROR: could not create $output_dir\n";
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
  my $run_record_dir = "$output_dir/run_$ndir";
  if (! -d $run_record_dir) { mkpath($run_record_dir) or die "BATCHER ERROR: could not create $run_record_dir\n"; }

# check that arbfile contains something, otherwise default to *.arb
  if (!(@{$case[$n]{"arbfile"}})) { push(@{$case[$n]{"arbfile"}},"*.arb"); };

#-----------------
# dump case into run directory
  open(DUMPER, ">$run_record_dir/batcher_info.txt") or error_stop("can't open batcher_info.txt file in $run_record_dir");
  print DUMPER "run: n = $n: ndir = $ndir\ncase[$n] = ".Dumper($case[$n])."\n";
  close(DUMPER);
  open(PBS, ">$run_record_dir/batcher_pbs_variables.txt") or error_stop("can't open batcher_pbs_variables.txt file in $run_record_dir");
  print PBS "#INFO: code within this file is evaluated by batcher\n";
  print PBS "#INFO: information for present case (likely set by batcher_setup.pm)\n";
  print PBS Data::Dumper->Dump([$case[$n]], ['*present_case']);
  print PBS "#INFO: where we are:\n";
  print PBS Data::Dumper->Dump([$run_record_dir], ['*run_record_dir']);
  print PBS Data::Dumper->Dump([\%output], ['*output']);
  print PBS Data::Dumper->Dump([$n], ['*n']);
  print PBS Data::Dumper->Dump([$ndir], ['*ndir']);
  print PBS Data::Dumper->Dump([$prune_output_structure], ['*prune_output_structure']);
  close(PBS);

#-----------------

  # extract requested number of threads, if it exists
  my $nthreads = 1;
  if ($case[$n]{'arboptions'}) {
    my $specified_arboptions = $case[$n]{'arboptions'};
    if ($specified_arboptions =~ /omp(\d+)/) { $nthreads = $1;}
  }
  
  # set ppn (number of requested processors) to be half the number of threads
  my $ncores;
  $nthreads = int($nthreads);
  if ($nthreads != 1) {
    $ncores = int($nthreads/2) ;
  }

  my $hostname = `'hostname'`;
  my $module_load_intel = '';
  my $module_load_maxima = '';

  if ($pbs) {
    # check that queue exists
    my $queue_search;
    if ($sge_variant) {
      $queue_search = `qconf -sql | grep $pbs_queue_name`;
    } else {
      $queue_search = `qstat -Q | grep $pbs_queue_name`;
    }
    
    if (not $queue_search) {
      die "BATCHER ERROR: pbs queue $pbs_queue_name could not be found\n"; 
    } 

    my $pbs_job_contents='';
    if (not $sge_variant) {
    ($pbs_job_contents = qq{#!/bin/bash
    #PBS -S /bin/bash
    ##PBS -M skink.notification\@gmail.com
    ##PBS -m bea
    #PBS -N $pbs_jobname\_run\_$ndir
    
    # ensure job spans only one node
    #PBS -l nodes=1:ppn=$ncores
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
    echo '\$rundir: ' \$rundir # needs fixing
    echo 'uname -a: ' `uname -a`
    echo 'STARTING DATE ' `date`
    echo

    # load modules if specified
    $pbs_module_load_commands
    

    # move to rundir - no, now run from working_dir
    # cd \$rundir
    # run job
    ./misc/batcher/run_and_collect.pl $ndir

    echo 'ENDING DATE ' `date`
    echo "FINISHED"
    exit 0
    }) =~ s/^ {4}//mg; # m implies treat as multiple lines (http://perldoc.perl.org/perlre.html#Modifiers)
    }
    
    if ($sge_variant) { # note leave indentation as is for {4} replacement below
    ($pbs_job_contents = qq{#!/bin/sh
    #\$ -S /bin/sh   
    #\$ -N $pbs_jobname\_run\_$ndir
    #\$ -cwd
    #\$ -j y
    
    # ensure job spans only one node
    #\$ -q $pbs_queue_name
    
    # even though job will run in eg. \$PBS_O_WORKDIR/batcher_output/run_0
    # the script ./misc/run_and_collect.pl is designed to be called from \$PBS_O_WORKDIR
    rundir=\$SGE_O_WORKDIR/
    
    echo '\$PATH:' \$PATH
    echo '\$SHELL:' \$SHELL
    echo '\$JOBID: ' \$JOBID
    echo '\$SGE_O_HOST: ' \$SGE_O_HOST
    echo '\$rundir: ' \$rundir # needs fixing
    echo 'uname -a: ' `uname -a`
    echo 'STARTING DATE ' `date`
    echo

    # load modules if specified (not required for borg)

    # move to rundir
    # cd \$rundir
    # run job
    ./misc/batcher/run_and_collect.pl $ndir

    echo 'ENDING DATE ' `date`
    echo "FINISHED"
    exit 0
    }) =~ s/^ {4}//mg; # m implies treat as multiple lines (http://perldoc.perl.org/perlre.html#Modifiers)
    }

    my $pbs_job_file = "$run_record_dir/job.pbs";

    open(PBSFILE,">",$pbs_job_file);
    print PBSFILE $pbs_job_contents;
    close(PBSFILE);

    if ($sge_variant) {
      system("chmod +x $pbs_job_file"); # make executable on borg
    }

    my $qsub_call = '';
    if ($sge_variant) {
      $qsub_call = "qsub -clear -V -q maxima.q -cwd ./$run_record_dir/job.pbs";
    } else {
      $qsub_call = "qsub $run_record_dir/job.pbs";
    }
    system($qsub_call);

  } elsif ($parallel) { # run arb jobs in parallel
    print "BATCHER DEBUG: running in non-pbs parallel mode\n";
    # copy everything needed to run_record_dir
    my $t = threads->new(\&arbthread, \@case, $n, $ndir, $run_record_dir);
    push(@threads,$t); 
  } else { # run arb jobs in series
    # run only a single thread and wait for it to finish before moving onto the next job
# create link to current directory
    if (-e "$output_dir/run_current") { unlink("$output_dir/run_current"); }
    symlink("run_$ndir","$output_dir/run_current");
    print "BATCHER DEBUG: running in series mode\n";
    &arbthread(\@case, $n, $ndir, $run_record_dir);
# and remove the link once we are done
    if (-e "$output_dir/run_current") { unlink("$output_dir/run_current"); }
  }   
}

if ($parallel) {
  # wait for all threads to finish
  foreach (@threads) {
    $_->join;
  }
}

close(OUTPUT);
exit;
