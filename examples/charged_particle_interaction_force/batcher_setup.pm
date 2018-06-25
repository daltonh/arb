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

  $::prune_output_structure = 0; # clear tmp, src, etc. from final run_* directories

  $::use_string_variables = 1; # if on uses arb runtime global string variables rather than batcher whole of file replacements (default)

# $::pbs = 0; # whether to use job queueing system
# $::pbs_walltime = '0:25:00';
# $::pbs_pmem = '4000mb';
# $::pbs_queue_name = 'batch'; # for skink
# $::pbs_module_load_commands = ''; # for skink
# $::pbs_queue_name = 'serial'; # for edward
# $::pbs_module_load_commands = 'module load intel; module load maxima'; # for edward

}

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
# string replacements performed         | yes     | yes*    | no      | yes         |
# mesh created from file using gmsh     | no      | yes     | no      | no          |
# accepts directory                     | no      | no      | yes     | no          |

# * strings are directly replaced within geofiles, even if $use_string_variables is on (=1)
# note that the hash names are for convienience - there is no reference to the file extension used for each file, so a mshfile could be any type of file that needs to be copied to each run directory

# in addition to the file arrays, there are also the following hash arrays for each case:
# replacements = hash of string replacements, now used across both geo and arb files
# arboptions = arb options to be used for the specific run, noting that quiet and quiet-make are already included

  my @case=();

# for my $innerparticle ( 0 ) { # whether inner particle is included or not
  for my $innerparticle ( 0, 1 ) { # whether inner particle is included or not
  # for my $lcscale ( 0 ) { # 0 is the default mesh, 1 is half as fine
    for my $lcscale ( 0 .. 4 ) { # 0 is the default mesh, 1 is half as fine
      for my $blnstar ( 2, 4 ) { # excess of elements in the boundary layer
    #   for my $domainsize ( 10, 20 ) { # domainsize
        for my $domainsize ( 40, 80, 160 ) { # domainsize
  #       for my $k ( 2, 1, 0.5, 0.25 ) { # k value
          for my $k ( 2 ) { # k value

            my $m=$#case+1;

            push(@{$case[$m]{"arbfile"}},'charged_particle_240518.arb');
            $case[$m]{"arboptions"}='--omp16 --opt -pl'; # using process logging (-pl) here allows memory use (memoryrss) to be reported

    # always include these now, complementing automatic arb strings
            $case[$m]{"replacements"}{"<<batcher>>"} = '1';
            $case[$m]{"replacements"}{"<<batchercomment>>"} = '';
            $case[$m]{"replacements"}{"<<nobatchercomment>>"} = '#';

    # use the gmsh hook to set the mesh size, innerparticle choice and domainsize
            $case[$m]{"replacements"}{"//<<batchergeoreplacements>>"} =
              "lcmultiplier = ".scalar(2**(-$lcscale))."; ".
              "innerparticle = ".$innerparticle."; ".
              "blnstar = ".$blnstar."; ".
              "domainsize = ".$domainsize."; ";

            $case[$m]{"replacements"}{"<<K>>"} = $k;
            $case[$m]{"replacements"}{"<<innerparticle>>"} = $innerparticle;

            push(@{$case[$m]{"geofile"}},'particle_semi_structured.geo');

          }
        }
      }
    }
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
    "<K>",
    "<S>",
    "<Re>",
    "<Sc>",
    "<B>",
    "<F_pressure_total[l=2]>",
    "<F_charge_total[l=2]>",
    "<F_total_total[l=2]>",
    "<area_interface>",
    "<area_total>",
    "<area_analytical>",
    "<area_error_normalised>"
  );

  return @output_keys;
}
###########################################################################################

1;
