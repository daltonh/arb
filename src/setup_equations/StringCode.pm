# file src/perl_lib/StringCode.pm
#
# Copyright 2009-2018 Dalton Harvie (daltonh@unimelb.edu.au)
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
our @EXPORT  = qw(parse_string_code string_setup string_set string_delete string_option string_eval string_examine string_debug string_set_transient_simulation arb_defined); # list of subroutines and variables that will by default be made available to calling routine
use Common;

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
# options are passed to string search so supports global, noglobal and local options
# on output
#  @_ = unchanged
sub string_option {

  my $name = $_[0];
  my $options = ''; if (defined($_[1])) { $options = $_[1]; }; # options is optional

  my ($code_block_found,$string_variable_found) = string_search($name,$options);
  if ($code_block_found == -1) {
    if ($options =~ /(^|,)\s*checkexists\s*(,|$)/) {
      $options = $`.','.$'; # remove this string from the options
      syntax_problem("string variable $name not found during string_options: $ReadInputFiles::filelinelocator");
    }
    
  } else {

    while (nonempty($options)) {
      if ($options =~ /^\s*(((no|)replace)|)\s*(,|$)/i) {
        $options = $';
        if (nonempty($2)) {
          if (empty($3)) {
            $ReadInputFiles::code_blocks[$code_block_found]{"string_variables"}[$string_variable_found]{"replace"} = 1;
          } else {
            $ReadInputFiles::code_blocks[$code_block_found]{"string_variables"}[$string_variable_found]{"replace"} = 0;
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
#  $_[0] = string name (can be an list of strings, but in this case options must be set, possibly to the empty string)
#  $_[$#_] = options, passed to string_search (ie global, noglobal and local, suffix|prefix removes the equivalent stripped variable name, as defined in string_set, not accessible from legacy string code)
# on output
#  $_[0] = unchanged
sub string_delete {

  my @names = @_; # string_delete now accepts multiple strings
  my $options = ''; if (defined($_[1])) { # if the number of arguments is greater than one, then the last will be options
    $options = pop(@names); # pop off these options
  };
  foreach my $name ( reverse( @names ) ) {

# change strings if we are requesting either a prefix or suffix
    if ($options =~ /(^|,|\s)prefix|suffix($|,|\s)/i) {
      if ($name =~ /^(<.*\[).*\]>$/) {
        $name = $1;
      } elsif ($name =~ /^(<.*>)$/) {
        $name = $1;
      }
      print ::DEBUG "INFO: in string_delete, having dealt with suffix/prefix options: name = $name\n";
    }

    my ($code_block_found,$string_variable_found) = string_search($name,$options);
    if ($code_block_found == -1) {
      syntax_problem("string variable $name not found during string_delete: $ReadInputFiles::filelinelocator","warning");
    } else {
      splice(@{$ReadInputFiles::code_blocks[$code_block_found]{"string_variables"}},$string_variable_found,1);
    }
  }
  
}
#-------------------------------------------------------------------------------
# finds the value of a string or performs replacements on a line of text (option text)
# ref: string_eval
# on input
#  $_[0] = string name
#  $_[1] = options (optional)
# possible options:
#  list = return the string as a list containing the string split at commas (useful for using within foreach loops etc)
#  (no|)checkexists = whether to issue an error if the string doesn't exist (default nocheckexists, meaning that it the string doesn't exist we just return an empty string)
#  text = rather than look for an individual string, just perform string replacements on the passed in text.  This will hence perform nested replacements
#  commaseparatedcount|count = related to commaseparated|list, returns the number of items in the list
#  spaceseparatedcount: count items in list separated by spaces
#  variablelist|regionlist: list of these items, ie, delimited by < and > and spaces
#  
# options also passed to string_search so accepts local, global and noglobal
# on output
#  $_[0] = unchanged
# returns string value if found, or dies or returns if string is not found

sub string_eval {

  my $name = $_[0];
  my $options = ''; if (defined($_[1])) { $options = $_[1]; }; # options is optional

  if ($options =~ /(^|,)(text)($|,)/i) {
    ReadInputFiles::perform_string_replacements($name); # use sub from ReadInputFiles to perform string replacements on entire line
    return $name;

  } else {
    my ($code_block_found,$string_variable_found) = string_search($name,$options);

    if ($code_block_found == -1) {
      if ($options =~ /(^|,)\s*checkexists\s*(,|$)/i) {
        $options = $`.','.$'; # remove this string from the options
        syntax_problem("string variable $name not found during string_set: $ReadInputFiles::filelinelocator");
      }
      return ''; # return empty string if the variable doesn't exist
      
    } else {

      if ($options =~ /(^|,)((commaseparated|spaceseparated|)count)($|,)/i) {
# return the number of items in the list
        my $option = $3."list";
        return scalar(string_split($ReadInputFiles::code_blocks[$code_block_found]{"string_variables"}[$string_variable_found]{"value"},$option));
      } elsif ($options =~ /(^|,)((commaseparated|spaceseparated|variable|region|)list)($|,)/i) {
# return as an array of items
        my $option = $2;
        return string_split($ReadInputFiles::code_blocks[$code_block_found]{"string_variables"}[$string_variable_found]{"value"},$option);

      } else {
        return $ReadInputFiles::code_blocks[$code_block_found]{"string_variables"}[$string_variable_found]{"value"};
      }
    }
  }
  
}
#-------------------------------------------------------------------------------
# splits a string that is a list into an array of elements, with type of list determined by options
# regardless of comma, space or variable item delimiting, quoting with " or ' also works to preserve items
# input:
# $_[0]: string to split
# $_[1]: options, optional:
#  list|commaseparatedlist: items are separated by commas
#  spaceseparatedlist: items are separated by spaces
#  variablelist|regionlist: list of these items, ie, delimited by < and > and spaces
# output:
# $_[0]: unchanged
# returns list of elements contained within string
sub string_split {
  my $string = $_[0];
  my $ostring = $string;
  my $options = '';
  my $debug=1;
  if (defined($_[1])) { $options = $_[1]; }
  my @elements = (); # this is what will be passed out
  my $element_delimiter=" "; # default element delimiter is a space
  if ($options =~ /(^|,)(commaseparated|)list($|,)/i) { $element_delimiter = ","; }

  if ($debug) { print ::DEBUG "STRING_SPLIT: entering with: string = $string: options = $options\n"; }
  while ($string ne '') {
    $string =~ s/^\s*//; # remove leading spaces using greedy match
    if ($debug) { print ::DEBUG "STRING_SPLIT: after space clear, start of loop: string = $string: elements = @elements\n"; }
    if ($string =~ /^(["'])/) {
      my $delimiter = $1;
      if ($string =~ /^$delimiter(.*?)$delimiter\s*($element_delimiter|$)/) {
        $string = $';
        push(@elements,$1);
      } else {
        error_stop("problem with delimiting of string_split $ostring: $ReadInputFiles::filelinelocator");
      }
    } elsif ($options =~ /(^|,)(variable|region)list($|,)/i) {
      if ($string =~ /^(<.*?>)\s*($element_delimiter|$)/) {
        $string = $';
        push(@elements,$1);
      } else {
        error_stop("problem with delimiting of string_split $ostring: $ReadInputFiles::filelinelocator");
      }
    } else {
      if ($string =~ /^(.*?)\s*($element_delimiter|$)/) {
        $string = $';
        push(@elements,$1);
      } else {
        error_stop("problem with delimiting of string_split $ostring: $ReadInputFiles::filelinelocator");
      }
    }
    if ($debug) { print ::DEBUG "STRING_SPLIT: at end of loop: string = $string: elements = @elements\n"; }
  }
  if ($debug) { print ::DEBUG "STRING_SPLIT: exiting with: string = $string: elements = @elements\n"; }

  return @elements;

}
#-------------------------------------------------------------------------------
# tests whether a string variable is equal to a particular test string, returning '1' or ''
# ref: string_test
# on input
#  $_[0] = string variable name, or line of text (option text)
#  $_[1] = value of name or text that represents a true result (ie, that is compared against)
#  $_[2] = options, which is also passed to string_search for notext, so supports global, noglobal and local
#    option 'text' means that string replacements are performed on first argument, and this is compared against value - handed for (eg) testing true value of nested replacements, whereas notext simply tests on what the string replacement is for the given variable
# on output
#  $_[0] = '1' if test string is given and string variable is defined and equal to the value of test string
#        = '0' otherwise
# now throws an error if test string is not provided or string is not found, unless text option is given
# see string_examine for checking on existence of strings

sub string_test {

  my $name = $_[0];
  my $options = '';

  if (!(defined($_[1]))) { # no test string was passed to routine, which is a terminal error
    error_stop("no test string has been passed to string_test: name = $name");
  }
  my $result = $_[1];
  if (defined($_[2])) { $options = $_[2]; }

  if ($options =~ /(^|,|\s)text($|,|\s)/i) { # text option is handled by string_eval
    if (string_eval($name,'text') eq $result) { return 1; } else { return 0; }
  } else { # otherwise we are looking at checking the value of a single string
    my ($code_block_found,$string_variable_found) = string_search($name,$options);

    if ($code_block_found == -1) {
      error_stop("test string has been passed to string_test was not found: name = $name");
    }
    
    if ($ReadInputFiles::code_blocks[$code_block_found]{"string_variables"}[$string_variable_found]{"value"} eq $result) { return 1; } else { return 0; }

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
#
# the following control whether the string active replaces solver code:
#    - replace: this string name will be searched for in solver code and replaced with its value, which is default for most string names
#    - noreplace: opposite of replace, which is the default for strings whose names start with $, as in "$a"
#
# the following control the scope of the string variable:
#    - without default, substitute or global:
#          - search only in current code block for the string, and if not found, set it in the current code block
#    - default: only set the string if the string is not already defined anywhere
#          - search is done over all code blocks (including the global one)
#          - global, replace and noreplace options are only relevant here if the string variable is not already set
#          - substitute option doesn't make sense here
#    - substitute: opposite of default, only set the string if it is set already, otherwise exit with error
#          - search is done over all code blocks (including the global one)
#          - global and default options don't make sense here
#    - global: set the string in the root code_blocks ($ReadInputFiles::code_blocks[0]) so that it is available even after the current block has closed - ie, globally
#    - suffix|prefix: create a string replacement that will change the variable|region names in this manner, dealing with presence of [l=:etc] brackets

# on entry if number of arguments is:
# 1 -> name of string
# 2 -> name and value of string
# 3 -> name and value of string, plus options
# even but >= 2 -> name and value of pairs of strings
# odd but >= 3 -> name and value of pairs of strings, plus options

sub string_set {

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
    if ($options =~ /^\s*((no|)replace|default|global|substitute|suffix|prefix|)\s*(,|$)/i) { # note trailing | to match just the delimiter
      $options = $';
    } else {
      syntax_problem("unknown string within string_set options of $options: $ReadInputFiles::filelinelocator");
    }
  }
  $options = $options_save;

# sanity check on requested options
  if ($options =~ /(^|,|\s)substitute($|,|\s)/i) {
    if ($options =~ /(^|,|\s)default($|,|\s)/i) {
      error_stop('both default and substitute options requested for string variable $name_value_pairs[0]');
    } elsif ($options =~ /(^|,|\s)global($|,|\s)/i) {
      error_stop('both global and substitute options requested for string variable $name_value_pairs[0]');
    }
  }

  while (@name_value_pairs) {

# see if string already exists, either in all code_blocks, or in code_blocks[0] for global

    print ::DEBUG "INFO: in string_set, dealing with: name = $name_value_pairs[0]: value = $name_value_pairs[1]\n";

# change strings if we are requesting either a prefix or suffix
    if ($options =~ /(^|,|\s)prefix|suffix($|,|\s)/i) {
      if ($options =~ /(^|,|\s)prefix($|,|\s)/i) {
        if ($name_value_pairs[0] =~ /^(<(.*\[)).*\]>$/) {
          $name_value_pairs[0] = $1;
          $name_value_pairs[1] = "<".$name_value_pairs[1].$2;
        } elsif ($name_value_pairs[0] =~ /^(<(.*>))$/) {
          $name_value_pairs[0] = $1;
          $name_value_pairs[1] = "<".$name_value_pairs[1].$2;
        } else {
          $name_value_pairs[1] = "$name_value_pairs[1]$name_value_pairs[0]";
        }
      } else {
        if ($name_value_pairs[0] =~ /^((<.*)\[).*\]>$/) {
          $name_value_pairs[0] = $1;
          $name_value_pairs[1] = $2.$name_value_pairs[1]."[";
        } elsif ($name_value_pairs[0] =~ /^((<.*)>)$/) {
          $name_value_pairs[0] = $1;
          $name_value_pairs[1] = $2.$name_value_pairs[1].">";
        } else {
          $name_value_pairs[1] = "$name_value_pairs[0]$name_value_pairs[1]";
        }
      }
      print ::DEBUG "INFO: in string_set, having dealt with suffix/prefix options: name = $name_value_pairs[0]: value = $name_value_pairs[1]\n";

    }

# first deal with default option
    if ($options =~ /(^|,|\s)default($|,|\s)/i) { # if string is found and default option is on, we are done
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
# 1. search for string name in the global or all code blocks (to be reset wherever it is found), or
# 2. if it doesn't exist in that block(s), find the next available indicies in the global or local block and set it

    my ($code_block_set,$string_variable_set) = (-1,-1); # define scope of variables
    if ($options =~ /(^|,|\s)global($|,|\s)/i) {
      ($code_block_set,$string_variable_set) = string_search($name_value_pairs[0],'global'); # for global only look in global block
    } elsif ($options =~ /(^|,|\s)substitute($|,|\s)/i) {
      ($code_block_set,$string_variable_set) = string_search($name_value_pairs[0]); # with the substitute option, we look anywhere for string
      if ($code_block_set == -1) { # not finding the string here is an error as it indicates that the user does not know at what block the string was to be set - too dangerous for now
        error_stop("substitute option given for string replacement $name_value_pairs[0] but the string has not been previously set")
      }
    } else {
      ($code_block_set,$string_variable_set) = string_search($name_value_pairs[0],'local'); # without no substitute/global option, we only look locally for the string
    }
    print ::DEBUG "INFO: in string_set, after string search: code_block_set = $code_block_set: string_variable_set = $string_variable_set: options = $options\n";
    if ($code_block_set == -1) { # string was not found, so set to next available indicies
      print ::DEBUG "INFO: in string_set, string $name_value_pairs[0] not found: creating new string\n";
      if ($options =~ /(^|,|\s)global($|,|\s)/i) {
        $code_block_set = 0; # for global string
      } else {
        $code_block_set = $#ReadInputFiles::code_blocks; # otherwise use current block
      }
      $string_variable_set = $#{$ReadInputFiles::code_blocks[$code_block_set]{"string_variables"}}+1; # increment index in the relevant block
    }
    print ::DEBUG "INFO: in string_set, about to set string: code_block_set = $code_block_set: string_variable_set = $string_variable_set: options = $options\n";

# if we are here, then the new or reused indices are ready to go
    $ReadInputFiles::code_blocks[$code_block_set]{"string_variables"}[$string_variable_set]{"name"} = shift(@name_value_pairs);
    $ReadInputFiles::code_blocks[$code_block_set]{"string_variables"}[$string_variable_set]{"value"} = shift(@name_value_pairs);

# also set replace variable
    $ReadInputFiles::code_blocks[$code_block_set]{"string_variables"}[$string_variable_set]{"replace"} = 1; # default value
    if ($options =~ /(^|,|\s)(no|)replace($|,|\s)/i) {
      if (nonempty($2)) {
        $ReadInputFiles::code_blocks[$code_block_set]{"string_variables"}[$string_variable_set]{"replace"} = 0;
      }
    } elsif ($ReadInputFiles::code_blocks[$code_block_set]{"string_variables"}[$string_variable_set]{"name"} =~ /^$/) {
      $ReadInputFiles::code_blocks[$code_block_set]{"string_variables"}[$string_variable_set]{"replace"} = 0;
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
#
# this string is for batcher integration - if a file is run through batcher, this string will be valued by an empty string, so can be used to precede arb lines that are specific to the batcher runs
  string_set("<<batcher>>","0","global"); # (0 not using batcher, 1 using batcher, to be set in batcher_setup.m) use this from now on over the comment strings
  string_set("<<batchercomment>>","#","global");
  string_set("<<nobatchercomment>>","","global");
# geometry and equation related
# choice of coordinate system
  string_set("<<cylindrical>>","0","global"); # for a cylindrical simulation set this to one
  string_set("<<azimuthal>>","0","global"); # for a cylindrical simulation that includes velocity in the azimuthal direction, set this (and <<cylindrical>>) to 1
# choice of which axis corresponds to which direction, used for cylindrical and cylindrical+azimuthal
  string_set("<<radialdim>>","0","global"); # for 2D cylindrical this is the radial coordinate direction
  string_set("<<axialdim>>","0","global"); # for 2D cylindrical this is the axial coordinate direction
  string_set("<<azimuthaldim>>","0","global"); # for 2D cylindrical this is the azimuthal coordinate direction, which should be set for any cylindrical simulation now
# these three should be overwritten by the relevant radius in the input file if using cylindrical coordinates: eg R "<<radius_c>>" W "<cellx[l=1]>" R "<<radius_f>>" W "<facex[l=1]>"
  string_set("<<radius_c>>","1.d0","global");
  string_set("<<radius_f>>","1.d0","global");
  string_set("<<radius_n>>","1.d0","global");
# legacy comments strings, do not use, use <<cylindrical>> instead
  string_set("<<cartesiancomment>>","","global"); # default is cartesian, try to use above <<cylindrical>> flag instead, hoping to eventually remove this
  string_set("<<cylindricalcomment>>","#","global"); # again, preference is to use <<cylindrical>> variable instead, but this could remain as a automatically generated string based on <<cylindrical>>
# these strings should be overwritten by the normal coordinate directions of any reflection boundaries in the domain: eg R "<<reflect=1>>" W "reflect=1"
  string_set("<<reflect=1>>","","global");
  string_set("<<reflect=2>>","","global");
  string_set("<<reflect=3>>","","global");
# dimensions that are being used
  string_set("<<dimensions>>","1,2,3","global"); # defaults to 3D, and should be accessed using the list form of string_eval('<<dimensions>>','list') to be used in (say) a foreach loop
# legacy comment strings, do not use, use <<dimensions>> instead
  string_set("<<dim1comment>>","","global"); # try to use and set <<dimensions>> where possible, with the idea that eventually the comment strings may go
  string_set("<<dim2comment>>","","global");
  string_set("<<dim3comment>>","","global");
# set the transient versus steady_state strings using the sub
  string_set_transient_simulation(0); # default is steady_state

  print ::DEBUG "INFO: initial string_variables = ".Dumper(@{$ReadInputFiles::code_blocks[0]{"string_variables"}})."\n";

}

#--------------------------------------------------------------
# search through string_variables for search string
# on input
#  $_[0] = name of string to find
#  $_[1] = is a list of options determining the scope of the search
#    - local: only in $#ReadInputFiles::code_blocks local block
#    - global: only in 0 global code_block
#    - noglobal: in all code blocks except for 0
# on output, @_ unchanged
# on output, returns ($code_block_found,$string_variable_found), default both to -1 if not found

sub string_search {

  my $name = $_[0];
  my ($code_block_lower,$code_block_upper) = (0,$#ReadInputFiles::code_blocks);
  my $options = '';
  if (defined($_[1])) { $options = $_[1]; }
  if ($options =~ /(^|,)\s*global\s*($|,)/i) {
    $code_block_upper = 0;
  } elsif ($options =~ /(^|,)\s*local\s*($|,)/i) {
    $code_block_lower = $#ReadInputFiles::code_blocks;
  } elsif ($options =~ /(^|,)\s*nolocal\s*($|,)/i) {
    $code_block_lower = 1;
  }
    
  my $string_variable_found = -1; # on output returns -1 if not found, or string_variables index if found
  my $code_block_found = -1; # on output returns -1 if not found, or code_blocks index if found

  CODE_BLOCK_LOOP: for my $m ( reverse( $code_block_lower .. $code_block_upper ) ) {
    for my $n ( reverse( 0 .. $#{$ReadInputFiles::code_blocks[$m]{"string_variables"}} ) ) {
      if ($name eq $ReadInputFiles::code_blocks[$m]{"string_variables"}[$n]{"name"}) { # found existing general replacements
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
    string_set("<<transientcomment>>","","global"); # use <<transientsimulation>> instead
    string_set("<<steadystatecomment>>","#","global"); # use <<transientsimulation>> instead
    string_set("<<transientsimulation>>","1","global");
    string_set("<<relsteps>>","0,1","global"); # default list of relsteps for a transient simulation
    string_set("<<transientflag>>","1","global"); # do not use, this string to be removed in the future
    string_set("<<steadystateflag>>","0","global"); # do not use, this string to be removed in the future
  } else {
    string_set("<<transientcomment>>","#","global"); # use <<transientsimulation>> instead
    string_set("<<steadystatecomment>>","","global"); # use <<transientsimulation>> instead
    string_set("<<transientsimulation>>","0","global");
    string_set("<<relsteps>>","0","global"); # default list of relsteps for a steady-state simulation
    string_set("<<transientflag>>","0","global"); # do not use, this string to be removed in the future
    string_set("<<steadystateflag>>","1","global"); # do not use, this string to be removed in the future
  }

}

#-------------------------------------------------------------------------------
# little routine that returns 1 if a string is defined, or 0 otherwise
# ref: string_examine
# second argument is list of options, taken straight from string_search:
# global = only returns 1 if string is found and is a global
# noglobal = only returns 1 if string is found and is not a global
# local = only returns 1 if string is found and local to the current code block
# string_test can do a similar thing, but without the options (but with the test functionality)
sub string_examine {

  my $string = $_[0];
  my $options = '';
  if (defined($_[1])) { $options = $_[1]; }

# options string can be passed straight to string_search which knows global, noglobal and local
  my ($code_block_found,$string_variable_found) = string_search($string,$options);
  if ($code_block_found ge 0) { return 1; } else { return 0; };

}
#-------------------------------------------------------------------------------
# prints out a formatted list of the string replacements
# first argument is list of options, including the string search ones of global, noglobal and local
# use it in arb code as eg print string_debug;

sub string_debug {
  
# this is the string that we will form (to be printed, as in {{ print string_debug('local') }})
  my $debug_output = "STRING DEBUG OUTPUT (a listing of the string replacement strings at this point in time):\n";

# deal with options
  my $options = '';
  if (defined($_[1])) { $options = $_[1]; }
  $debug_output .= "Called with: options = $options\n";

  my ($code_block_lower,$code_block_upper) = (0,$#ReadInputFiles::code_blocks);
  if ($options =~ /(^|,)\s*global\s*($|,)/i) {
    $code_block_upper = 0;
  } elsif ($options =~ /(^|,)\s*local\s*($|,)/i) {
    $code_block_lower = $#ReadInputFiles::code_blocks;
  } elsif ($options =~ /(^|,)\s*nolocal\s*($|,)/i) {
    $code_block_lower = 1;
  }
    
  CODE_BLOCK_LOOP: for my $m ( reverse( $code_block_lower .. $code_block_upper ) ) {
    if ($m eq 0) { $debug_output .= "Global "; }
    if ($m eq $#ReadInputFiles::code_blocks) { $debug_output = $debug_output."Local "; }
    $debug_output .= "Code Block $m:\n";
    for my $n ( reverse( 0 .. $#{$ReadInputFiles::code_blocks[$m]{"string_variables"}} ) ) {
      $debug_output .= "  String Variable $n: name = $ReadInputFiles::code_blocks[$m]{string_variables}[$n]{name}: value = $ReadInputFiles::code_blocks[$m]{string_variables}[$n]{value}: replace = $ReadInputFiles::code_blocks[$m]{string_variables}[$n]{replace}\n";
    }
  }

  return $debug_output;

}
#-------------------------------------------------------------------------------
# subroutine that checks on existence, or provides information about, an arb variable or region name (NB, defined is already a perl routine...)
# on input:
# $_[0] = name of variable/region to be checked
# $_[1] = comma separated list of options
#   options: more to be included in the future during reorder rewrite, getting rid of asread_variable rubbish
# on output:
# $_[0] = REGION|VARIABLE|0, depending on whether name is an existing variable, region, or not defined (with no options)
sub arb_defined {

  my $name = $_[0];
  my $options = '';
  if (defined($_[1])) { $options = $_[1]; }
  my $exists = 0; # set this to whether name is a region or variable, or doesn't exist

  if (!($options =~ /(^|,|\s)region($|,|\s)/i)) {
# check whether we have a variable from the asread_variables
    my $variable_name = examine_name($name,"name");
# see if this name has already been defined, and if so, find position of the variable in the input file
    my $masread = -1; # variable masread starts at 0 if any variables are defined
    foreach my $mcheck ( 0 .. $#::asread_variable ) {
      if ($::asread_variable[$mcheck]{"name"} eq $variable_name) { # variable has been previously defined, and position in file is based on first definition
        $masread = $mcheck;
        last;
      }
    }
    if ($masread ne -1) { $exists = 'VARIABLE'; }
    if (!($exists)) { # also check the variable array for system variables
      my $masread = -1;
#     foreach my $mcheck ( 0 .. $#{@::variable{"system"}} ) { # interpreter advises against this
      foreach my $mcheck ( 1 .. $#{$::variable{"system"}} ) { # variable array starts from 1
        if ($::variable{"system"}[$mcheck]{"name"} eq $variable_name) { # variable has been previously defined, and position in file is based on first definition
          $masread = $mcheck;
          last;
        }
      }
      if ($masread ne -1) { $exists = 'VARIABLE'; }
    }
  }

  if ((!($options =~ /(^|,|\s)variable($|,|\s)/i)) && !($exists)) {
# check whether we have a region
    my $region_name = examine_name($name,"regionname");
    if (find_region($region_name) ne -1) { $exists = 'REGION'; }
  }

  return $exists;

}
#-------------------------------------------------------------------------------

1;
