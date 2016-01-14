#!/usr/bin/env perl

# this little script reads a msh (v2) file produced by engrid, and modifies it by renaming and reordering the physical regions in it to correspond to arb-suitable names
# usage: engridtoarb inputfile.msh outputfile.msh

use strict;
use warnings;
my ($line, $nelement, $n, $nregion, $gtype, $region_number, $following, $m, $dimension, $name);
# define an array of hashes specifying how physical region numbers in the engrid file are to be renamed
my @regions = ();
push (@regions, { "number" => 0, "name" => "<default>" } ); # 0 region is named <default> here, but need not be
# you can specify individual regions here
#push (@regions, { "number" => 999, "name" => "<top>" } );
#push (@regions, { "number" => 1000, "name" => "<walls>" } );
#push (@regions, { "number" => 1001, "name" => "<bottom>" } );
# or alternatively, if regions aren't specified then they will be given default names

# create an array relating the element type to the dimension of the element type
my @element_dimension=();
$element_dimension[1]=1; # line
$element_dimension[2]=2; # triangle
$element_dimension[3]=2; # quad
$element_dimension[4]=3; # tet
$element_dimension[5]=3; # 
$element_dimension[6]=3; # 
$element_dimension[7]=3; # 
$element_dimension[15]=0; # point

# dump entire input file in an array
open(INPUT,"<$ARGV[0]") or die "ERROR: could not open first file specified on the command line for reading: input filename = $ARGV[0]\n";
my @lines = <INPUT>;
close(INPUT);

# read elements lines, looking for any region numbers and finding their dimension
my $reading_elements=0;
my $nline = 0;
for $n ( 0 .. $#lines) {
  $line = $lines[$n];
  if ($reading_elements) {
# if this is an element entry that only has one tag, then find the region number and dimension
    if ($line =~ /\$EndElements/) {
      $reading_elements = 0;
    } elsif ($line =~ /^\s*(\d+)\s+(\d+)\s+1\s+(\d+)\s+/) { # $1 is element number, $2 is element type, $3 is region number
      $nelement = $1;
      $gtype = $2;
      $region_number = $3; # this is the region number as listed in the engrid msh file
      $following = $';
      if (empty($element_dimension[$gtype])) {die "ERROR: element $nelement of type $gtype is not known to the dimension array on line ".scalar($nline+1)."\n";}
      $dimension = $element_dimension[$gtype];
      $nregion = ''; # this will be the new region number, used for both the physical and elementary region in the new arb-compatible msh file
# find the regions array element with this number and same dimensions, otherwise creating new
      foreach $m ( 0 .. $#regions ) {
        if ($regions[$m]{"number"} == $region_number) {
          if (empty($regions[$m]{"dimension"})) {
            $regions[$m]{"dimension"} = $dimension;
            print "INFO: setting region $regions[$m]{name} to dimension $dimension\n";
          }
          if ($regions[$m]{"dimension"} == $dimension) { # region is only the same if it has the same number of dimensions as previously, otherwise new region is created
            $nregion = $m; last;
          }
        }
      }
      if (empty($nregion)) { # region not previously defined
        $name = "<default_num".$region_number."_dim".$dimension.">"; # define a new name
        push (@regions, { "number" => $region_number, "name" => $name ,"dimension" => $dimension} );
        $nregion = $#regions;
        print "INFO: creating region number $region_number $name of dimension $dimension\n"
      }
      $lines[$n] = "$nelement $gtype 2 ".scalar($nregion+1)." ".scalar($nregion+1)." $following"; # finally change the element entry, making the new region number the array index+1 (to avoid the 0 region)
    }
  } elsif ($line =~ /^\$Element/) {
    $reading_elements = 1;
  } elsif ($n == 1 && $line =~ /^2\.0/) {
    $lines[$n] = "2.2 0 8\n"; # also need to update file version number
  }
}

# now output
open(OUTPUT,">$ARGV[1]") or die "ERROR: could not open second file specified on the command line for writing: output filename = $ARGV[1]\n";
for $line (@lines) {
  print OUTPUT $line;
  if ($line =~ /\$EndMeshFormat/) { # dump physical region details here
    print OUTPUT "\$PhysicalNames\n";
    print OUTPUT scalar($#regions+1)."\n";
    foreach $m ( 0 .. $#regions ) {
      print OUTPUT $regions[$m]{"dimension"}." ".scalar($m+1)." \"$regions[$m]{name}\"\n";
    }
    print OUTPUT "\$EndPhysicalNames\n";
  }
}
close(OUTPUT);

#-------------------------------------------------------------------------------
# chomp and remove mac linefeads too if present

sub chompm {
  use strict;
  chomp($_[0]);  # remove linefeed from end
  $_[0]=~tr/\r//d; # remove control m from mac files
}

#-------------------------------------------------------------------------------
# little subroutine that tests whether a variable has been defined and/or holds anything

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
