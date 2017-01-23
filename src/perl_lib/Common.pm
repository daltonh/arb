# non-specific routines

package Common;

use strict;
use warnings;
use Exporter 'import';
#our $VERSION = '1.00';
our @EXPORT  = qw(chompm empty nonempty ijkstring error_stop); # list of subroutines and variables that will by default be made available to calling routine

#-------------------------------------------------------------------------------
# chomp and remove mac linefeads too if present

sub chompm {
  chomp($_[0]);  # remove linefeed from end
  $_[0]=~tr/\r//d; # remove control m from mac files
}

#-------------------------------------------------------------------------------
# little subroutine that tests whether a variable has been defined and/or holds anything

sub empty {
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
  if (empty($_[0])) { return 0; } else { return 1; }
}

#-------------------------------------------------------------------------------
# this returns the correct ijk index letter corresponding to the passed in centring
# or a 1 if none centring

sub ijkstring {
  if ($_[0] eq "cell") {
    return "i";
  } elsif ($_[0] eq "face") {
    return "j";
  } elsif ($_[0] eq "node") {
    return "k";
  } else {
    return "1";
  }
}
  
#-------------------------------------------------------------------------------
# whatever string is passed to this routine is output as an error message to both screen
#  and DEBUG file, and the script then dies
# also deal with previous successful run

sub error_stop {

  use File::Path qw(rmtree); # for File::Path version < 2.08, http://perldoc.perl.org/File/Path.html
  print "ERROR: $_[0]\n";
  print ::DEBUG "ERROR: $_[0]\n";

# also remove traces of last successful run
  print ::DEBUG "INFO: removing last successful setup data from $::setup_creation_dir\n";
  print "INFO: removing last successful setup data from $::setup_creation_dir\n";
  if (-d $::setup_creation_dir) { rmtree($::setup_creation_dir) or print "ERROR: could not remove existing $::setup_creation_dir\n"; } # using rmtree for older File::Path compatibility
  if (-f $::setup_equation_file) {unlink($::setup_equation_file) or print "ERROR: could not remove existing $::setup_equation_file\n"; }

  exit 1; # signifies that there was an error

}
#-------------------------------------------------------------------------------

1;
