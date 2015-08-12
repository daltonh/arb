package batcher_setup;
use strict;
use warnings;
 
use Exporter qw(import);
 
our @EXPORT_OK = qw(case_setup output_setup $parallel);
our $parallel = 0; # default to run arb jobs in series

###########################################################################################
# within this subroutine you need to setup the case array of hashes
sub case_setup {
# setup runs array here by specifying strings to replace with appropriate values
# runs is an array (one element for each run) containing hashes in the format string -> substitution

  my @case=();

  for my $meshtype ( "structured", "unstructured" ) {
  #for my $meshtype ( "structured" ) {
    for my $lcscale ( 0 .. 0 ) { # 0 is the default mesh, 1 is half as fine
      for my $rescale ( 0 .. 0 ) { # re is simply 10*$rescale
        for my $sscale ( 0 .. 0 ) { # s is simply 10*$sscale

  # now uses case hash which is specific to each run and can accept the following fields (all filenames accept glob patterns and now default to empty if not used):
  # arbfile = array of arb files used for each run, for which replacements will take place
  # geofile = array of geo files used for each run which need to be meshed, for which replacements will take place
  # otherfile = array of other files which are copied over to working directory WITH REPLACEMENTS, commonly used for arb or geo files which are parents of the ones that are specified to arb or create_mesh
  # mshfile = array of other files which are copied over to working directory WITHOUT REPLACEMENTS, commonly used for a premade msh file (actually, non-msh files listed here don't have any actions performed on them)
  # replacements = hash of string replacements, now used across both geo and arb files
  # arboptions = arb options to be used for the specific run, noting that quiet and quiet-make are already included

          my $m=$#case+1;

          push(@{$case[$m]{"arbfile"}},'parasitic_currents_200415.arb');
          $case[$m]{"arboptions"}='--omp2 --opt';

          $case[$m]{"replacements"}{"//lc = <<lc>>;"} = "lc = ".scalar(0.125/(2**$lcscale)).";";
          $case[$m]{"replacements"}{"<<batcherRe>>"} = 10.**$rescale;
          $case[$m]{"replacements"}{"<<batcherS>>"} = 10.**$sscale;
          if ($meshtype eq "structured") {
            $case[$m]{"replacements"}{"<<batcherstructureddomaincomment>>"} = "";
            $case[$m]{"replacements"}{"<<batcherunstructureddomaincomment>>"} = "#";
            push(@{$case[$m]{"otherfile"}},'free_surface_quadrant_extruded_structured_variables.geo');
            push(@{$case[$m]{"geofile"}},'free_surface_quadrant_extruded_structured_fluid.geo');
            push(@{$case[$m]{"geofile"}},'free_surface_quadrant_extruded_structured_free_surface.geo');
          } else {
            $case[$m]{"replacements"}{"<<batcherstructureddomaincomment>>"} = "#";
            $case[$m]{"replacements"}{"<<batcherunstructureddomaincomment>>"} = "";
#           push(@{$case[$m]{"otherfile"}},'free_surface_quadrant_nonextruded_unstructured_variables.geo');
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
