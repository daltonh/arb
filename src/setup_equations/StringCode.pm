# file src/perl_lib/StringCode.pm
#
# Copyright 2009-2017 Dalton Harvie (daltonh@unimelb.edu.au)
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
# The original copyright of arb is held by Dalton Harvie, however the
# project is now under collaborative development.
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
our @EXPORT  = qw(parse_string_code string_setup string_set string_delete string_option string_eval string_set_transient_simulation); # list of subroutines and variables that will by default be made available to calling routine
use Common;
use Data::Alias 'alias';

#-------------------------------------------------------------------------------
sub parse_string_code {

  my ($buffer) = @_;
  my $debug = 0;

  if ($debug) { print ::DEBUG "INFO: start of StringCode::parse_string_code: buffer = $buffer\n"; }

# removing lead and trailing double delimiters
  $buffer =~ s/(\{\{|\}\})//g;
  if ($debug) { print ::DEBUG "INFO: after removing string code delimiters: buffer = $buffer\n"; }

  my $eval_return = eval($buffer.";\nreturn ''"); # a linefeed is added here incase the last entry was a comment
  if ($@) {
    syntax_problem("error in evaluating the string code $buffer: $ReadInputFiles::filelinelocator");
  } else { $buffer = $eval_return; }

  if ($debug) { print ::DEBUG "INFO: end of StringCode::parse_string_code: buffer = $buffer\n"; }

  $_[0] = $buffer;
}

#-------------------------------------------------------------------------------
# sets options for a string
# on input
#  $_[0] = string name
#  $_[1] = comma separated list of options (optional)
# options include:
#  (no|)replace = whether to treat this string variable as a replacement in the following arb code
#  (no|)checkexists = whether to issue an error if the string doesn't exist (default nocheckexists)
# on output
#  @_ = unchanged
sub string_option {

  alias my @code_blocks = @ReadInputFiles::code_blocks;
  my $name = $_[0];
  my $options = ''; if (defined($_[1])) { $options = $_[1]; }; # options is optional

  my ($code_block_found,$string_variable_found) = string_search($name);
  if ($code_block_found == -1) {
    if ($options =~ /(^|,)\s*checkexists\s*(,|$)/) {
      $options = $`.','.$'; # remove this string from the options
      syntax_problem("string variable $name not found during string_options: $ReadInputFiles::filelinelocator");
    }
    
  } else {

    while (nonempty($options)) {
      if ($options =~ /^\s*(((no|)replace)|)\s*(,|$)/) {
        $options = $';
        if (nonempty($2)) {
          if (empty($3)) {
            $code_blocks[$code_block_found]{"string_variables"}[$string_variable_found]{"replace"} = 1;
          } else {
            $code_blocks[$code_block_found]{"string_variables"}[$string_variable_found]{"replace"} = 0;
          }
        }
      } else {
        syntax_problem("unknown string within string_options options of $options: $ReadInputFiles::filelinelocator");
      }
    }
  }
  
}
#-------------------------------------------------------------------------------
# deletes a string
# on input
#  $_[0] = string name
# on output
#  $_[0] = unchanged
sub string_delete {

  alias my @code_blocks = @ReadInputFiles::code_blocks;
  my @names = @_; # string_delete now accepts multiple strings
  foreach my $name ( reverse( @names ) ) {
    my ($code_block_found,$string_variable_found) = string_search($name);
    if ($code_block_found == -1) {
      syntax_problem("string variable $name not found during string_delete: $ReadInputFiles::filelinelocator","warning");
    } else {
      splice(@{$code_blocks[$code_block_found]{"string_variables"}},$string_variable_found,1);
    }
  }
  
}
#-------------------------------------------------------------------------------
# finds the value of a string
# on input
#  $_[0] = string name
#  $_[1] = options (optional)
# possible options:
#  list = return the string as a list containing the string split at commas (useful for using within foreach loops etc)
#  (no|)checkexists = whether to issue an error if the string doesn't exist (default nocheckexists)
# on output
#  $_[0] = unchanged
# returns string value if found, or dies or returns if string is not found

sub string_eval {

  alias my @code_blocks = @ReadInputFiles::code_blocks;
  my $name = $_[0];
  my $options = ''; if (defined($_[1])) { $options = $_[1]; }; # options is optional

  my ($code_block_found,$string_variable_found) = string_search($name);

  if ($code_block_found == -1) {
    if ($options =~ /(^|,)\s*checkexists\s*(,|$)/) {
      $options = $`.','.$'; # remove this string from the options
      syntax_problem("string variable $name not found during string_set: $ReadInputFiles::filelinelocator");
    }
    return ''; # return empty string if the variable doesn't exist
    
  } else {
    if ($options =~ /(^|,)list($|,)/) {
      return split(/,/,$code_blocks[$code_block_found]{"string_variables"}[$string_variable_found]{"value"});
    } else {
      return $code_blocks[$code_block_found]{"string_variables"}[$string_variable_found]{"value"};
    }
  }
  
}
#-------------------------------------------------------------------------------
# tests whether a string variable is equal to a particular test string, returning '1' or ''
# on input
#  $_[0] = string variable name
#  $_[1] = test string
# on output
#  $_[0] = '1' if test string is given and string variable is defined and equal to the value of test string
#        = '1' if test string isn't given by variable is defined
#        = '' otherwise

sub string_test {

  alias my @code_blocks = @ReadInputFiles::code_blocks;
  my $name = $_[0];
  my ($code_block_found,$string_variable_found) = string_search($name);

  if (!(defined($_[1]))) { # no test string was passed to routine, so just return whether string is defined
    if ($code_block_found == -1) { return ''; } else { return '1'; }
  } else { # need to check
    if ($code_block_found == -1) { return ''; }
    elsif ($code_blocks[$code_block_found]{"string_variables"}[$string_variable_found]{"value"} eq $_[1]) { return '1'; }
    else { return ''; }
  }
  
}
#-------------------------------------------------------------------------------
# takes an array of code lines and repeats them for each dimension in the <<dimensions>> range, replacing $l with each relevant index
# on input
#  @_ = list of code template strings, containing $l references and no carriage returns
# on output
#  $_[0] = expanded code string

sub vector_expand {

  my @templates = @_;
  my $code = '';
  foreach my $l ( string_eval('<<dimensions>>','list') ) {
    foreach my $template ( @templates ) {
      my $snippet = $template;
      $snippet =~ s/\$l/$l/g; # do the $l replacement
      $code .= $snippet."\n"; # add to code with a linefeed too
    }
  }
  return $code;
  
}
#-------------------------------------------------------------------------------
# takes an array of code lines and repeats them for each dimension in the <<dimensions>> range, replacing $l with each relevant index
# same as vector_expand but does tensors, using $l1 and $l2 references, with $l1 varying slowest
# on input
#  @_ = list of code template strings, containing $l1 and $l2 references and no carriage returns
# on output
#  $_[0] = expanded code string

sub tensor_expand {

  my @templates = @_;
  my $code = '';
  foreach my $l1 ( string_eval('<<dimensions>>','list') ) {
    foreach my $l2 ( string_eval('<<dimensions>>','list') ) {
      foreach my $template ( @templates ) {
        my $snippet = $template;
        $snippet =~ s/\$l1/$l1/g; # do the $l1 replacement
        $snippet =~ s/\$l2/$l2/g; # do the $l2 replacement
        $code .= $snippet."\n"; # add to code with a linefeed too
      }
    }
  }
  return $code;
  
}
#-------------------------------------------------------------------------------
# this sub sets a string variable with name to a value, for N string name/value pairs
# on entry:
#  $_[2*n-2)] = name for string n = 1 .. N
#  $_[2*n-1)] = value for string n = 1 .. N
#  $_[2*N] = comma separated list of options (string) to be applied for all strings:
#    - replace: this string name will be searched for in solver code and replaced with its value, which is default for most string names
#    - noreplace: opposite of replace, which is the default for strings whose names start with $, as in "$a"
#    - default: only set the string if the string is not already defined anywhere
#          - search is done over all code blocks, irrespective of global setting
#          - so global and replace or noreplace options are only relevant here if the string variable is not already set
#    - global: set the string in the root code_blocks ($code_blocks[0]) so that it is available even after the current block has closed - ie, globally

# on entry if number of arguments is:
# 1 -> name of string
# 2 -> name and value of string
# 3 -> name and value of string, plus options
# even but >= 2 -> name and value of pairs of strings
# odd but >= 3 -> name and value of pairs of strings, plus options

sub string_set {

  alias my @code_blocks = @ReadInputFiles::code_blocks;
  my @name_value_pairs = @_; # place all arguments in this array to start

# if there is only one value passed in, it is the name, so set the value to an empty string
  if ($#name_value_pairs == 0) { $name_value_pairs[1] = ''; }

# if the number of arguments is odd now, then the last argument is the list of options, so pop this off
  my $options = '';
  if ($#name_value_pairs % 2 == 0) { $options = pop(@name_value_pairs); } # eg, 3/2=1 -> no options, 4/2=0 -> options

  print ::DEBUG "INFO: entering string_set: name_value_pairs = @name_value_pairs: options = $options\n";

# check validity of options
  my $options_save = $options;
  while (nonempty($options)) {
    if ($options =~ /^\s*((no|)replace|default|global|)\s*(,|$)/) {
      $options = $';
    } else {
      syntax_problem("unknown string within string_set options of $options: $ReadInputFiles::filelinelocator");
    }
  }
  $options = $options_save;

  while (@name_value_pairs) {

# see if string already exists, either in all code_blocks, or in code_blocks[0] for global

    print ::DEBUG "INFO: in string_set, dealing with: name = $name_value_pairs[0]: value = $name_value_pairs[1]\n";

# first deal with default option
    if ($options =~ /(^|,|\s)default($|,|\s)/) { # if string is found and default option is on, we are done
      my ($code_block_found,$string_variable_found) = string_search($name_value_pairs[0]);
      print ::DEBUG "INFO: in string_set, with default option: code_block_found = $code_block_found: string_variable_found = $string_variable_found\n";
      if ($code_block_found >= 0) {
        print ::DEBUG "INFO: found string, moving to next\n";
        shift(@name_value_pairs);
        shift(@name_value_pairs);
        next;
      }
    }

# if we are here then the string will be set or reset, so
# 1. search for string name in the relevant code block (to be reset if it is found there), or
# 2. if it doesn't exist in that block, find the next available indicies in that block and set it

    my $code_block_search = $#code_blocks; # the block to limit our activities to
    if ($options =~ /(^|,|\s)global($|,|\s)/) { $code_block_search = 0; }
    my ($code_block_set,$string_variable_set) = string_search($name_value_pairs[0],$code_block_search);
    print ::DEBUG "INFO: in string_set, after string search: code_block_search = $code_block_search: code_block_set = $code_block_set: string_variable_set = $string_variable_set\n";
    if ($code_block_set == -1) { # string was not found, so set to next available indicies
      ($code_block_set,$string_variable_set) = ($code_block_search,$#{$code_blocks[$code_block_search]{"string_variables"}}+1);
    }
    print ::DEBUG "INFO: in string_set, about to set string: code_block_search = $code_block_search: code_block_set = $code_block_set: string_variable_set = $string_variable_set\n";

# if we are here, then the new or reused indices are ready to go
    $code_blocks[$code_block_set]{"string_variables"}[$string_variable_set]{"name"} = shift(@name_value_pairs);
    $code_blocks[$code_block_set]{"string_variables"}[$string_variable_set]{"value"} = shift(@name_value_pairs);

# also set replace variable
    $code_blocks[$code_block_set]{"string_variables"}[$string_variable_set]{"replace"} = 1; # default value
    if ($options =~ /(^|,|\s)(no|)replace($|,|\s)/) {
      if (nonempty($2)) {
        $code_blocks[$code_block_set]{"string_variables"}[$string_variable_set]{"replace"} = 0;
      }
    } elsif ($code_blocks[$code_block_set]{"string_variables"}[$string_variable_set]{"name"} =~ /^$/) {
      $code_blocks[$code_block_set]{"string_variables"}[$string_variable_set]{"replace"} = 0;
    }

  }

}

#-------------------------------------------------------------------------------
sub string_setup {

  use Data::Dumper;

# ref: string system variables
# setup default string_variables
# loose convention is that replacement strings be delimited by <<>>, however any strings can (and will) be matched/valued
# convention is that names that end with "comment" are meant to precede statements in the files, converting them to comments if they are not relevant
# this string is for batcher integration - if a file is run through batcher, this string will be valued by an empty string, so can be used to precede arb lines that are specific to the batcher runs
  string_set("<<batchercomment>>","#","global");
  string_set("<<nobatchercomment>>","","global");
# geometry and equation related
  string_set("<<dim1comment>>","","global");
  string_set("<<dim2comment>>","","global");
  string_set("<<dim3comment>>","","global");
  string_set("<<cartesiancomment>>","","global"); # default is cartesian
  string_set("<<cylindricalcomment>>","#","global");
# convention is that names that end with "flag" are either on (1) or off (0), so can be used within expressions
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
# dimensions that are being used
  string_set("<<dimensions>>","1,2,3","global"); # defaults to 3D, and should be accessed using the list form of string_eval('<<dimensions>>','list') to be used in (say) a foreach loop
# set the transient versus steady_state strings using the sub
  string_set_transient_simulation(0); # default is steady_state

  print ::DEBUG "INFO: initial string_variables = ".Dumper(@{$ReadInputFiles::code_blocks[0]{"string_variables"}})."\n";

}

#--------------------------------------------------------------
# search through string_variables for search string
# on input
#  $_[0] = name of string to find
#  $_[1] = if defined, is the specific code block in which is searched, otherwise if not defined, all code blocks are searched (from last = $#code_blocks to root = 0)
# on output, @_ unchanged
# on output, returns ($code_block_found,$string_variable_found), default both to -1 if not found

sub string_search {

  alias my @code_blocks = @ReadInputFiles::code_blocks;

  my $name = $_[0];
  my ($code_block_lower,$code_block_upper) = (0,$#code_blocks);
  if (defined($_[1])) { $code_block_lower = $_[1]; $code_block_upper = $_[1]; }
    
  my $string_variable_found = -1; # on output returns -1 if not found, or string_variables index if found
  my $code_block_found = -1; # on output returns -1 if not found, or code_blocks index if found

  CODE_BLOCK_LOOP: for my $m ( reverse( $code_block_lower .. $code_block_upper ) ) {
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
# based on passed variable, set or unset transient simulation replacement strings
# note that this is now separated from $::transient_simulation

sub string_set_transient_simulation {

  my $simulation = $_[0];
  if ($simulation) {
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
