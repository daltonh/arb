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
# string replacements performed         | yes     | yes     | no      | yes         |
# mesh created from file using gmsh     | no      | yes     | no      | no          |
# accepts directory                     | no      | no      | yes     | no          |

# note that the hash names are for convienience - there is no reference to the file extension used for each file, so a mshfile could be any type of file that needs to be copied to each run directory

# in addition to the file arrays, there are also the following hash arrays for each case:
# replacements = hash of string replacements, now used across both geo and arb files
# arboptions = arb options to be used for the specific run, noting that quiet and quiet-make are already included

  my @case=();

  for my $meshtype ( "structured", "unstructured" ) {
  #for my $meshtype ( "structured" ) {
    for my $lcscale ( 0 .. 0 ) { # 0 is the default mesh, 1 is half as fine
      for my $rescale ( 0 .. 0 ) { # re is simply 10*$rescale
        for my $sscale ( 0 .. 0 ) { # s is simply 10*$sscale

          my $m=$#case+1;

          push(@{$case[$m]{"arbfile"}},'parasitic_currents_batch.arb');
          $case[$m]{"arboptions"}='--omp2 --opt -pl'; # using process logging (-pl) here allows memory use (memoryrss) to be reported

# always include these now, complementing automatic arb strings
          $case[$m]{"replacements"}{"<<batcher>>"} = '1';
          $case[$m]{"replacements"}{"<<batchercomment>>"} = '';
          $case[$m]{"replacements"}{"<<nobatchercomment>>"} = '#';

          $case[$m]{"replacements"}{"<<batchercartesiancomment>>"} = '';
          $case[$m]{"replacements"}{"<<batchercylindricalcomment>>"} = '#';
          $case[$m]{"replacements"}{"//lc = <<lc>>;"} = "lc = ".scalar(0.125/(2**$lcscale)).";";
          $case[$m]{"replacements"}{"<<batcherRe>>"} = 10.**$rescale;
          $case[$m]{"replacements"}{"<<batcherS>>"} = 10.**$sscale;
          $case[$m]{"replacements"}{"<<batcherrho>>"} = "1.d0";
          $case[$m]{"replacements"}{"<<batchermu>>"} = "1.d0";
          $case[$m]{"replacements"}{"<<batcherdudtstop>>"} = "1.d-3";
          $case[$m]{"replacements"}{"<<batcherinterfacewidth>>"} = "2.d0";
          if ($meshtype eq "structured") {
            $case[$m]{"replacements"}{"<<batcherstructureddomaincomment>>"} = "";
            $case[$m]{"replacements"}{"<<batcherunstructureddomaincomment>>"} = "#";
            push(@{$case[$m]{"includefile"}},'free_surface_quadrant_extruded_structured_variables.geo');
            push(@{$case[$m]{"geofile"}},'free_surface_quadrant_extruded_structured_fluid.geo');
            push(@{$case[$m]{"geofile"}},'free_surface_quadrant_extruded_structured_free_surface.geo');
          } else {
            $case[$m]{"replacements"}{"<<batcherstructureddomaincomment>>"} = "#";
            $case[$m]{"replacements"}{"<<batcherunstructureddomaincomment>>"} = "";
#           push(@{$case[$m]{"includefile"}},'free_surface_quadrant_nonextruded_unstructured_variables.geo');
#           push(@{$case[$m]{"geofile"}},'free_surface_quadrant_nonextruded_unstructured_both.geo');
# as a demonstration, use previously created msh file instead
            push(@{$case[$m]{"mshfile"}},'free_surface_quadrant_nonextruded_unstructured_both.msh');
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
    "<dt>",
    "<CFL[r=1]>",
    "<t>",
    "<t_out>",
    "<t_save>",
    "<stretch[l=1]>",
    "<stretch[l=2]>",
    "<centroid[l=1]>",
    "<centroid[l=2]>",
    "<u_f_max>",
    "<u_f_max[r=1]>",
    "<absdudt>",
    "<absdudt[r=1]>",
    "<phi error>",
    "<p error>",
    "<p rel error>",
    "<p initial>",
    "<vof_phi_max all cells>",
    "<vof_phi_min all cells>",
    "<integrated volume>",
    "<initial integrated volume>",
    "<integrated volume error>",
    "<vof_interface_mask[r=1]>",
    "<parasitic_mac2_u_V>",
    "<parasitic_mac2_u_A>",
    "<parasitic_mac2_u_T>",
    "<parasitic_mac2_u_p>"
  );

  return @output_keys;
}
###########################################################################################

1;
