package batcher_setup;
use strict;
use warnings;
 
use Exporter qw(import);
our @EXPORT_OK = qw(case_setup output_setup $parallel $pbs $pbs_jobname $pbs_walltime $pbs_pmem $pbs_queue_name $pbs_module_load_commands);
our $parallel = 0; # default to run arb jobs in series
our $pbs = 1; # whether to use job queueing system

#our $pbs_jobname = "a6_restart_test";
our $pbs_jobname = `basename \$(pwd)`; # pull in dir name automatically
chomp($pbs_jobname);

our $pbs_walltime = '0:25:00';
our $pbs_pmem = '4000mb';

#our $pbs_queue_name = 'batch'; # for skink
#our $pbs_module_load_commands = ''; # for skink

our $pbs_queue_name = 'serial'; # for edward
our $pbs_module_load_commands = 'module load intel; module load maxima'; # for edward

###########################################################################################
# within this subroutine you need to setup the case array of hashes
sub case_setup {
# setup runs array here by specifying strings to replace with appropriate values
# runs is an array (one element for each run) containing hashes in the format string -> substitution

  # now uses case hash which is specific to each run and can accept the following fields (all filenames accept glob patterns and now default to empty if not used):
  # arbfile = array of arb files used for each run, for which replacements will take place
  # geofile = array of geo files used for each run which need to be meshed, for which replacements will take place
  # otherfile = array of other files which are copied over to working directory WITH REPLACEMENTS, commonly used for arb or geo files which are parents of the ones that are specified to arb or create_mesh
  # mshfile = array of other files which are copied over to working directory WITHOUT REPLACEMENTS, commonly used for a premade msh file (actually, non-msh files listed here don't have any actions performed on them)
  # replacements = hash of string replacements, now used across both geo and arb files
  # arboptions = arb options to be used for the specific run, noting that quiet and quiet-make are already included

 
  my @case=();
  for my $z ( '1.0', '2.0', '3.0' ) {
          my $m=$#case+1;
          push(@{$case[$m]{"arbfile"}},'bouncing_ball.arb');
          $case[$m]{"arboptions"}='--omp2 --opt';
          $case[$m]{"replacements"}{"<<var>>"} = $z;
  }

  return @case;
}
###########################################################################################

sub output_setup {
# setup keys that will be required for output variables

  my @output_keys=();

  @output_keys = (
# first there are some standard ones, not delimited by <> meaning that they aren't normal variables
# in general these are always needed, so don't edit these
    "nstepmax",
    "itotal",
    "idomain",
    "cputime",
    "walltime",
    "dimensions",
    "kernel_elements",
    "success",
# now optional ones (variables) go here
# edit these, add what you want
  );

  return @output_keys;
}
###########################################################################################

1;

