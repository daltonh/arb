# file src/perl_lib/StringCode.pm
#
# Copyright 2009-2015 Dalton Harvie (daltonh@unimelb.edu.au)
# 
# This file is part of arb finite volume solver, referred to as `arb'.
# 
# arb is a software package designed to solve arbitrary partial
# differential equations on unstructured meshes using the finite volume
# method.  Primarily it consists of fortran source code, perl source
# code and shell scripts.  arb replies on certain third party software
# to run, most notably the computer algebra system maxima
# <http://maxima.sourceforge.net/> which is released under the GNU GPL.
# 
# The copyright of arb is held by Dalton Harvie.
# 
# arb is released under the GNU GPL.  arb is free software: you can
# redistribute it and/or modify it under the terms of the GNU General
# Public License (version 3) as published by the Free Software Foundation.
# You should have received a copy of the GNU General Public Licence
# along with arb (see file licence/gpl.txt after unpacking).  If not,
# see <http://www.gnu.org/licences/>.
# 
# arb is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public Licence
# for more details.
# 
# For full details of arb's licence see the licence directory.
# 
# The current homepage for the arb finite volume solver project is
# <http://people.eng.unimelb.edu.au/daltonh/downloads/arb>.
#
#-------------------------------------------------------------------------
# this package deals with parsing any string code within the input files

package StringCode;

use strict;
use warnings;
use Exporter 'import';
#our $VERSION = '1.00';
our @EXPORT  = qw(parse_string_code string_setup set_transient_simulation); # list of subroutines and variables that will by default be made available to calling routine
use Common;
use Data::Alias 'alias';

#-------------------------------------------------------------------------------
sub parse_string_code {

  my ($buffer) = @_;

  print ::DEBUG "INFO: start of StringCode::parse_string_code: buffer = $buffer\n";

# removing lead and trailing double delimiters
  ($buffer) = $buffer =~ /^\{\{(.*)\}\}$/;

  my $eval_return = eval($buffer."; return ''");
  if ($@) {
    syntax_problem("error in evaluating the string code $buffer","error");
  } else { $buffer = $eval_return; }

  print ::DEBUG "INFO: end of StringCode::parse_string_code: buffer = $buffer\n";

  $_[0] = $buffer;
}

#-------------------------------------------------------------------------------
# this sub sets a string variable with name to a value, for N string name/value pairs
# on entry:
#  $_[2*n-2)] = name for string n = 1 .. N
#  $_[2*n-1)] = value for string n = 1 .. N
#  $_[2*N] = comma separated list of options (string) to be applied for all strings:
#    - replace: this string name will be searched for in solver code and replaced with its value, which is default for most string names
#    - noreplace: opposite of replace, which is the default for strings whose names start with $, as in "$a"
#    - default: only set the string to this value if the string is not already defined
#    - global: set the string in the root code_blocks ($code_blocks[0]) so that it is available even after the current block has closed - ie, globally
#    - if both global and default are specified, search and set is only done on root code_blocks[0]

sub string_set {

  my @name_value_pairs = @_; # place all arguments in this array to start

# if the number of arguments is odd, then the last argument is the list of options, so pop this off
  my $options = '';
  if ($#name_value_pairs % 2 == 0) { $options = pop(@name_value_pairs); } # eg, 3/2=1 -> no options, 4/2=0 -> options

  print ::DEBUG "INFO: entering string_set: name_value_pairs = @name_value_pairs: options = $options\n";

  alias my @code_blocks = @ReadInputFiles::code_blocks;

  while (@name_value_pairs) {

# see if string already exists, either in all code_blocks, or in code_blocks[0] for global

    my ($code_block_found,$string_variable_found);
    if ($options =~ /(^|,|\s)global($|,|\s)/) {
      ($code_block_found,$string_variable_found) = string_search($name_value_pairs[0],'global');
    } else {
      ($code_block_found,$string_variable_found) = string_search($name_value_pairs[0]);
    }

  print ::DEBUG "INFO: in string_set, dealing with: name = $name_value_pairs[0]: value = $name_value_pairs[1]: code_block_found = $code_block_found: string_variable_found = $string_variable_found\n";

    if ($options =~ /(^|,|\s)default($|,|\s)/ && $code_block_found >= 0) { # if string is found and default option is on, we are done
      print ::DEBUG "INFO: found string when default option is on, so no action required\n";
    } else {

# as we are needing to set this variable, create a temporary hash to push on to the stack
      my %string_variable = ( "name" => $name_value_pairs[0], "value" => $name_value_pairs[1] );
      $string_variable{"replace"} = 1;
      if ($options =~ /(^|,|\s)(no|)replace($|,|\s)/) {
        if (nonempty($2)) { $string_variable{"replace"} = 0; }
      } elsif ($name_value_pairs[0] =~ /^$/) {
        $string_variable{"replace"} = 0;
      }

      if ($string_variable_found >= 0) {
        $code_blocks[$code_block_found]{"string_variables"}[$string_variable_found] = %string_variable;
      } elsif ($options =~ /(^|,|\s)global($|,|\s)/) {
        push(@{$code_blocks[0]{"string_variables"}},%string_variable);
      } else {
        push(@{$code_blocks[$#code_blocks]{"string_variables"}},%string_variable);
      }

    }

  }

}

#-------------------------------------------------------------------------------
sub string_setup {

  use Data::Dumper;
# for convienience create an alias to just the string_variables part of code_blocks
# alias my @string_variables = @{$ReadInputFiles::code_blocks[$#ReadInputFiles::code_blocks]{"string_variables"}};
  alias my @string_variables = @{$ReadInputFiles::code_blocks[0]{"string_variables"}};

# ref: string variables
# setup default string_variables
# loose convention is that replacement strings be delimited by <<>>, however any strings can (and will) be matched/valued
# convention is that valuement names that end with "comment" are meant to preceed statements in the files, converting them to comments if they are not relevant
# this string is for batcher integration - if a file is run through batcher, this string will be valued by an empty string, so can be used to precede arb lines that are specific to the batcher runs
  string_set("<<batchercomment>>","#","global");
  string_set("<<nobatchercomment>>","","global");
# geometry and equation related
  string_set("<<dim1comment>>","","global");
  string_set("<<dim2comment>>","","global");
  string_set("<<dim3comment>>","","global");
  string_set("<<steadystatecomment>>","","global"); # default is steady-state
  string_set("<<transientcomment>>","#","global");
  string_set("<<cartesiancomment>>","","global"); # default is cartesian
  string_set("<<cylindricalcomment>>","#","global");
# convention is that valuement names that end with "flag" are either on (1) or off (0), so can be used within expressions
  string_set("<<steadystateflag>>","1","global"); # default is steady-state
  string_set("<<transientflag>>","0","global");
  string_set("<<cartesianflag>>","1","global"); # default is cartesian
  string_set("<<cylindricalflag>>","0","global");
# these two should be overwritten by the relevant radius in the input file if using cylindrical coordinates: eg R "<<radius_c>>" W "<cellx[l=1]>" R "<<radius_f>>" W "<facex[l=1]>"
  string_set("<<radius_c>>","1.d0","global");
  string_set("<<radius_f>>","1.d0","global");
  string_set("<<radius_n>>","1.d0","global");
  string_set("<<radiusdim1flag>>","0","global"); # for 2D cylindrical coordinates, set the radius dimension flag to 1 to include (for example) the hoop stress in that dimension
  string_set("<<radiusdim2flag>>","0","global");
  string_set("<<radiusdim3flag>>","0","global");
  string_set("<<radialdim>>","0","global"); # for 2D cylindrical this is the radial coordinate direction
  string_set("<<axialdim>>","0","global"); # for 2D cylindrical this is the axial coordinate direction
# these strings should be overwritten by the normal coordinate directions of any reflection boundaries in the domain: eg R "<<reflect=1>>" W "reflect=1"
  string_set("<<reflect=1>>","","global");
  string_set("<<reflect=2>>","","global");
  string_set("<<reflect=3>>","","global");

  print ::DEBUG "INFO: initial string_variables = ".Dumper(@string_variables)."\n";

}

#--------------------------------------------------------------
# search through string_variables for search string

sub string_search {

  my $name = $_[0]; # on input, name of string to find
  my $options = $_[1]; # on input, a list of possible options, right now only global, which implies only look in code_blocks[0]
  my $string_variable_found = -1; # on output returns -1 if not found, or string_variables index if found
  my $code_block_found = -1; # on output returns -1 if not found, or code_blocks index if found

  alias my @code_blocks = @ReadInputFiles::code_blocks;

  my $code_block_upper = $#code_blocks;
  if ($options =~ /(^|,|\s)global($|,|\s)/) { $code_block_upper = 0; }

  CODE_BLOCK_LOOP: for my $m ( reverse( 0 .. $code_block_upper ) ) {
    for my $n ( reverse( 0 .. $#{$code_blocks[$m]{"string_variables"}} ) ) {
      if ($name eq $code_blocks[$m]{"string_variables"}[$n]{"name"}) { # found existing general replacements
        $string_variable_found = $n;
        $code_block_found = $m;
        last CODE_BLOCK_LOOP;
      }
    }
  }

  return ($code_block_found,$string_variable_found);

}
#-------------------------------------------------------------------------------
# based on passed variable, set or unset transient simulation status, including comment strings

sub set_transient_simulation {

  $::transient_simulation = $_[0];
  if ($::transient_simulation) {
    string_set("<<transientcomment>>","","global");
    string_set("<<steadystatecomment>>","#","global");
    string_set("<<transientflag>>","1","global");
    string_set("<<steadystateflag>>","0","global");
  } else {
    string_set("<<transientcomment>>","#","global");
    string_set("<<steadystatecomment>>","","global");
    string_set("<<transientflag>>","0","global");
    string_set("<<steadystateflag>>","1","global");
  }

}

#-------------------------------------------------------------------------------

1;
