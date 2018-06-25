package batcher_setup;
use strict;
use warnings;
 
use Exporter qw(import);
 
our @EXPORT_OK = qw(case_setup output_setup $parallel $pbs $pbs_jobname $pbs_walltime $pbs_pmem $pbs_queue_name $pbs_module_load_commands $prune_output_structure);

# choose either
# parallel = 1 and pbs = 0 for local parallel batch
# parallel = 0 and pbs = 0 for local series batch
# parallel = 0 and pbs = 1 for cluster pbs submission (each job submitted individually to the queue given by $pbs_queue_name below

our $parallel = 0; # default to run arb jobs in series
our $pbs = 0; # whether to use job queueing system

# note that for a parallel example, all $pbs_* variables are ignored
our $pbs_jobname = `basename \$(pwd)`; # pull in dir name automatically
chomp($pbs_jobname);

our $pbs_walltime = '0:25:00';
our $pbs_pmem = '4000mb';

#our $pbs_queue_name = 'batch'; # for skink
#our $pbs_module_load_commands = ''; # for skink

our $pbs_queue_name = 'serial'; # for edward
our $pbs_module_load_commands = 'module load intel; module load maxima'; # for edward

our $prune_output_structure = 0; # clear tmp, src, etc. from final run_* directories

###########################################################################################
# within this subroutine you need to setup the case array of hashes
sub case_setup {

# setup case array here, with one element of the case array per each run

# within each case arrays are some subarrays (hashes) which specify what files are required for each run:
# there are four primary file arrays, known by the four hashnames:
# arbfile = array of arb files used for each run, for which replacements will take place
# geofile = array of geo files used for each run which need to be meshed, for which replacements will take place
# mshfile = array of other files which are copied over to working directory WITHOUT REPLACEMENTS, commonly used for a premade msh file (actually, non-msh files listed here don't have any actions performed on them)
# includefile = array of other files which are copied over to working directory WITH REPLACEMENTS, commonly used for arb or geo files which are parents of the ones that are specified to arb or create_mesh that are not passed to these routines, but that require replacements

# matrix attempting to detailing the difference between the four different file arrays:
#                                       | arbfile | geofile | mshfile | includefile |
#---------------------------------------------------------------------------------
# passed to arb script as an input file | yes     | no      | no      | no          |
# string replacements performed         | yes     | yes     | no      | yes         |
# mesh created from file using gmsh     | no      | yes     | no      | no          |
# accepts directory                     | no      | no      | yes     | no          |

# note that the hash names are for convienience - there is no reference to the file extension used for each file, so a mshfile could be any type of file that needs to be copied to each run directory

# in addition to the file arrays, there are also the following hash arrays for each case:
# replacements = hash of string replacements, now used across both geo and arb files
# arboptions = arb options to be used for the specific run, noting that quiet and quiet-make are already included

  my @case=();

# for my $meshfilecells ( qw( 1000 8000 15625 64000 125000 1M 8M ) ) {
  for my $meshfilecells ( qw( 1000 8000 ) ) {

          my $m=$#case+1;

# need all three arb files for this compound run
          push(@{$case[$m]{"arbfile"}},'bone_mesh_090516.arb');
          push(@{$case[$m]{"arbfile"}},'bone_geometry_090516.arb');
          push(@{$case[$m]{"arbfile"}},'bone_flow_090516.arb');

#         $case[$m]{"arboptions"}='--omp --opt -pl'; # using process logging (-pl) here allows memory use (memoryrss) to be reported
# using runcommand instead of options here, noting that path won't be set to local directory
          $case[$m]{"runcommand"}="./batcher_runcommand";
# also need to copy script over
          push(@{$case[$m]{"mshfile"}},"batcher_runcommand");

# always include these now, complementing automatic arb strings
          $case[$m]{"replacements"}{"<<batchercomment>>"} = '';
          $case[$m]{"replacements"}{"<<nobatchercomment>>"} = '#';

# and include mesh file both as a replacement and file to copy
          my $meshfile = "2mm_meshes/BovineMESH2_2_2_$meshfilecells.msh";
          $case[$m]{"replacements"}{"<<mshfile>>"} = $meshfile;
          push(@{$case[$m]{"mshfile"}},$meshfile);

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
    "memoryrss",
# now optional ones (variables) go here
# edit these, add what you want
    "<permeability_mag>",
    "<ndpermeability_mag>",
    "<pressure_gradient_phi_degrees>",
    "<pressure_gradient_theta_degrees>",
    "<pressure_gradient_uphi_av_angle_degrees>",
    "<porosity>",
    "<uphi_av_mag>",
    "<uphi_av_mag>",
    "<fluid volume>",
    "<solid volume>",
    "<total volume>",
    "<porosity>",
    "<interfacial area>",
    "<pore length scale>",
    "<lc_calculated>",
    "<continuity_error>"
  );

  return @output_keys;
}
###########################################################################################

1;
