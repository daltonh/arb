# routines to read arb input files

package ScopeTest;

use strict;
use warnings;
use Exporter 'import';
#our $VERSION = '1.00';
our @EXPORT_OK  = qw(test_variable_scope print_scope_test);
our $scope_test;



sub test_variable_scope {
  $scope_test = 3;
  print "scope_test = $::scope_test\n";
  $::scope_test = 2;
  print "changing scope_test in sub\n";
  print "scope_test = $::scope_test\n";
  print "scope_test = $scope_test\n";
}

sub print_scope_test {
  print "scope_test = $::scope_test\n";
  print "scope_test = $scope_test\n";
}

1;
