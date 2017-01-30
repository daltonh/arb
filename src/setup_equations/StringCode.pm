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
#    - global: set the string in the root code_block so that it is available even after the current block has closed - ie, globally

# 
# 
# start with two strings
# needs to be changed so that only does found on current block
# otherwise creates new in current block, which will be found before anything in preceeding blocks

sub string_set {

  my @name_value_pairs = @_; # place all arguments in this array to start

# if the number of arguments is odd, then the last argument is the list of options, so pop this off
  my $options = '';
  if ($#name_value_pairs % 2 == 0) { $options = pop(@name_value_pairs); } # eg, 3/2=1 -> no options, 4/2=0 -> options

  alias my @code_blocks = @ReadInputFiles::code_blocks;

# UP TO HERE
  while (@name_value_pairs) {

# %{$string_variables[$#string_variables+1]} = ( search => "<<batchercomment>>", replace => "#" );
  
    my ($code_block_found,$string_variable_found) = string_search($name);

    if ($code_block_found >= 0 && $string_variable_found >= 0) {
      $code_blocks[$code_block_found]{"string_variables"}[$string_variable_found]{"value"} = $value;
    } else {
      %{$code_blocks[$#code_blocks]{"string_variables"}[$#{$code_blocks[$#code_blocks]{"string_variables"}}+1]} = 
        ( name => $name, value => $value );
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
  %{$string_variables[$#string_variables+1]} = ( name => "<<batchercomment>>", value => "#" );
  %{$string_variables[$#string_variables+1]} = ( name => "<<nobatchercomment>>", value => "" );
# geometry and equation related
  %{$string_variables[$#string_variables+1]} = ( name => "<<dim1comment>>", value => "" );
  %{$string_variables[$#string_variables+1]} = ( name => "<<dim2comment>>", value => "" );
  %{$string_variables[$#string_variables+1]} = ( name => "<<dim3comment>>", value => "" );
  %{$string_variables[$#string_variables+1]} = ( name => "<<steadystatecomment>>", value => "" ); # default is steady-state
  %{$string_variables[$#string_variables+1]} = ( name => "<<transientcomment>>", value => "#" );
  %{$string_variables[$#string_variables+1]} = ( name => "<<cartesiancomment>>", value => "" ); # default is cartesian
  %{$string_variables[$#string_variables+1]} = ( name => "<<cylindricalcomment>>", value => "#" );
# convention is that valuement names that end with "flag" are either on (1) or off (0), so can be used within expressions
  %{$string_variables[$#string_variables+1]} = ( name => "<<steadystateflag>>", value => "1" ); # default is steady-state
  %{$string_variables[$#string_variables+1]} = ( name => "<<transientflag>>", value => "0" );
  %{$string_variables[$#string_variables+1]} = ( name => "<<cartesianflag>>", value => "1" ); # default is cartesian
  %{$string_variables[$#string_variables+1]} = ( name => "<<cylindricalflag>>", value => "0" );
# these two should be overwritten by the relevant radius in the input file if using cylindrical coordinates: eg R "<<radius_c>>" W "<cellx[l=1]>" R "<<radius_f>>" W "<facex[l=1]>"
  %{$string_variables[$#string_variables+1]} = ( name => "<<radius_c>>", value => "1.d0" );
  %{$string_variables[$#string_variables+1]} = ( name => "<<radius_f>>", value => "1.d0" );
  %{$string_variables[$#string_variables+1]} = ( name => "<<radius_n>>", value => "1.d0" );
  %{$string_variables[$#string_variables+1]} = ( name => "<<radiusdim1flag>>", value => "0" ); # for 2D cylindrical coordinates, set the radius dimension flag to 1 to include (for example) the hoop stress in that dimension
  %{$string_variables[$#string_variables+1]} = ( name => "<<radiusdim2flag>>", value => "0" );
  %{$string_variables[$#string_variables+1]} = ( name => "<<radiusdim3flag>>", value => "0" );
  %{$string_variables[$#string_variables+1]} = ( name => "<<radialdim>>", value => "0" ); # for 2D cylindrical this is the radial coordinate direction
  %{$string_variables[$#string_variables+1]} = ( name => "<<axialdim>>", value => "0" ); # for 2D cylindrical this is the axial coordinate direction
# these strings should be overwritten by the normal coordinate directions of any reflection boundaries in the domain: eg R "<<reflect=1>>" W "reflect=1"
  %{$string_variables[$#string_variables+1]} = ( name => "<<reflect=1>>", value => "" );
  %{$string_variables[$#string_variables+1]} = ( name => "<<reflect=2>>", value => "" );
  %{$string_variables[$#string_variables+1]} = ( name => "<<reflect=3>>", value => "" );

  print ::DEBUG "INFO: initial string_variables = ".Dumper(@string_variables)."\n";

}

#--------------------------------------------------------------
# search through string_variables for search string

sub string_search {

  my $name = $_[0]; # on input, name of string to find
  my $string_variable_found = -1; # on output returns -1 if not found, or string_variables index if found
  my $code_block_found = -1; # on output returns -1 if not found, or code_blocks index if found

  alias my @code_blocks = @ReadInputFiles::code_blocks;

  CODE_BLOCK_LOOP: for my $m ( reverse( 0 .. $#code_blocks ) ) {
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
