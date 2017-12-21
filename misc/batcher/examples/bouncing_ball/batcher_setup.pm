package batcher_setup;
use strict;
use warnings;
 
use Exporter qw(import);
 
our @EXPORT_OK = qw(simulation_setup case_setup output_setup); 

###########################################################################################
# overwrite the default simulation parameters here
sub simulation_setup {

# set this to 1 to allow continuation within the same batcher_output directory, with run directories continuing on from last run_N
# set this to 0 to not run if batcher_output is already there (default)
# set this to -1 to delete batcher_output directory at the start of the run (risky, for testing only)
  $::continue=-1;

# choose either
# parallel = 1 and pbs = 0 for local parallel batch
# parallel = 0 and pbs = 0 for local series batch
# parallel = 0 and pbs = 1 for cluster pbs submission (each job submitted individually to the queue given by $pbs_queue_name below
  $::parallel = 0; # default to run arb jobs in series

  $::prune_output_structure = 0; # clear build and other run files from run_dir

  $::use_string_variables = 1; # if on uses arb runtime global string variables rather than batcher whole of file replacements (default)

  $::use_previous_build = 1; # if on uses build information from previous run

}

###########################################################################################
# within this subroutine you need to setup the case array of hashes
sub case_setup {
# setup runs array here by specifying strings to replace with appropriate values
# runs is an array (one element for each run) containing hashes in the format string -> substitution

  my @case=();
  #$parallel = 1; # run arb jobs in parallel


  # now uses case hash which is specific to each run and can accept the following fields (all filenames accept glob patterns and now default to empty if not used):
  # arbfile = array of arb files used for each run, for which replacements will take place
  # geofile = array of geo files used for each run which need to be meshed, for which replacements will take place
  # includefile = array of other files which are copied over to working directory WITH REPLACEMENTS, commonly used for arb or geo files which are parents of the ones that are specified to arb or create_mesh
  # mshfile = array of other files which are copied over to working directory WITHOUT REPLACEMENTS, commonly used for a premade msh file (actually, non-msh files listed here don't have any actions performed on them)
  # replacements = hash of string replacements, now used across both geo and arb files
  # arboptions = arb options to be used for the specific run, noting that quiet and quiet-make are already included

  for my $z ( '1.0', '2.0', '3.0' ) {
    my $m=$#case+1;

# always include these now, complementing automatic arb strings
    $case[$m]{"replacements"}{"<<batchercomment>>"} = '';
    $case[$m]{"replacements"}{"<<nobatchercomment>>"} = '#';

    push(@{$case[$m]{"arbfile"}},'bouncing_ball_batch.arb');
#   $case[$m]{"arboptions"}='--omp8 --opt';
    $case[$m]{"replacements"}{"<<z_init>>"} = $z;
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
    "dimensions",
    "walltime",
    "kernel_elements",
    "success",
# now optional ones (variables) go here
# edit these, add what you want
  );

  return @output_keys;
}
###########################################################################################

1;
