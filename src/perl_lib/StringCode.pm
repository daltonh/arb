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
our @EXPORT  = qw(parse_string_code setup_string_variables set_transient_simulation); # list of subroutines and variables that will by default be made available to calling routine
use Common;
use Data::Alias 'alias';

# create alias' to make a few variables more accessible
#alias local @code_blocks = @ReadInputFiles::code_blocks;
#alias my $transient_simulation = $::transient_simulation;
#alias local $newtient_simulation = $::newtient_simulation;

#-------------------------------------------------------------------------------
sub parse_string_code {

  my ($buffer) = @_;

  print "INFO: in StringCode::parse_string_code with buffer = $buffer\n";

# removing lead and trailing double delimiters
  ($buffer) = $buffer =~ /^\{\{(.*)\}\}$/;

  my $eval_return = eval($buffer."; return ''");
  if ($@) {
    syntax_problem("error in evaluating the string code $buffer","error");
  } else { $buffer = $eval_return; }

  print "INFO: return from StringCode::parse_string_code with buffer = $buffer\n";
# my $transient_simulation;
# alias my $transient_simulation = $::transient_simulation;
# print "::transient_simulation = $::transient_simulation\n";
# print "transient_simulation = $transient_simulation\n";

  $_[0] = $buffer;
}

#-------------------------------------------------------------------------------
# 
# start with two strings
# needs to be changed so that only does found on current block
# otherwise creates new in current block, which will be found before anything in preceeding blocks
sub set_string {

  my ($name, $value) = @_;
  alias my @code_blocks = @ReadInputFiles::code_blocks;

# %{$string_variables[$#string_variables+1]} = ( search => "<<batchercomment>>", replace => "#" );
  
  my ($code_block_found,$string_variable_found) = search_string_variables($name);

  if ($code_block_found >= 0 && $string_variable_found >= 0) {
    $code_blocks[$code_block_found]{"string_variables"}[$string_variable_found]{"value"} = $value;
  } else {
    %{$code_blocks[$#code_blocks]{"string_variables"}[$#{$code_blocks[$#code_blocks]{"string_variables"}}+1]} = 
      ( name => $name, value => $value );
  }

}

#-------------------------------------------------------------------------------
sub setup_string_variables {

  use Data::Dumper;
# for convienience create an alias to just the string_variables part of code_blocks
  alias my @string_variables = @{$ReadInputFiles::code_blocks[$#ReadInputFiles::code_blocks]{"string_variables"}};

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

sub search_string_variables {

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
    set_string("<<transientcomment>>","");
    set_string("<<steadystatecomment>>","#");
    set_string("<<transientflag>>","1");
    set_string("<<steadystateflag>>","0");
  } else {
    set_string("<<transientcomment>>","#");
    set_string("<<steadystatecomment>>","");
    set_string("<<transientflag>>","0");
    set_string("<<steadystateflag>>","1");
  }

}

#-------------------------------------------------------------------------------

1;
