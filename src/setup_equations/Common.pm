# file src/perl_lib/Common.pm
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
# non-specific routines

package Common;

use strict;
use warnings;
use Exporter 'import';
#our $VERSION = '1.00';
our @EXPORT  = qw(chompm empty nonempty ijkstring error_stop examine_name extract_first syntax_problem syntax_file); # list of subroutines and variables that will by default be made available to calling routine

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
# this subroutine takes a user written <> delimited name ($_[0]) and provides info about it,
#  most importantly the standardised name

# design is not the most computationally efficient, but is convienient

# input variables are:
# $_[0] = name
# $_[1] = action = name|compoundname|basename|nrank|rank|lindex|rindex|all|regionname

# output variable is a single item corresponding to the action, or for all, an array of all of the items
# name - <> delimited and standardised (see below)
# compoundname - <> delimited, and has only r index if r>0
# basename - no indices and no <> delimiters
# nrank - 1|3|9
# rank - scalar|vector|tensor
# lindex (expressed as 1->9)
# rindex (>=0)
# regionname - can have both rindices and lindices (witness the kernel regions) so is exactly the same as name, except now checks and corrects deprecated names

# the standarised variable name obeys:
# if the variable is a scalar then no l index is given in the consistent name
# if the variable is for the current timestep then no r index is given in the consistent name
# eitherway the l index always preceeds the r index

sub examine_name {

  my ($name,$action,$compoundname,$basename,$nrank,$rank,$indices,$lindex,$rindex,$lindices);

  $action = $_[1];
  ($name) = $_[0] =~ /^<(.*)>$/;
  if (!($name)) { error_stop("an empty variable was passed to sub consistent_name: $_[0]"); }

# set default (no index) options
  $basename = $name;
  $lindex = 1; # default is a scalar
  $nrank = 1;
  $rank = "scalar";
  $rindex = 0; # default is at the current timestep (relative timestep = 0)
  $lindices = "";
  if ($name =~ /^(.+?)\[(.+?)\]$/) {
    ($basename,$indices) = ($1, $2);
    if ($indices =~ /(^|\,)\s*l\s*=\s*([123])\s*,\s*([123])\s*($|\,)/) {
      $nrank = 9;
      $rank = "tensor";
      $lindex = ($2-1)*3 + $3; # l = (j-1)*3+i where j = row number, i = col number
      $lindices = "$2,$3";
    } elsif ($indices =~ /(^|\,)\s*l\s*=\s*([0123])\s*($|\,)/) {
      if ($2) { # l=0 indicates a scalar
        $nrank = 3;
        $rank = "vector";
        $lindex = $2;
        $lindices = "$2";
      }
    }
    if ($indices =~ /(^|\,)\s*r\s*=\s*(\d+)\s*($|\,)/) {
      $rindex = $2;
    }
  }

# now assemble (consistent) name and compoundname
  $name = "<".$basename;
  if ($lindices || $rindex) { $name = $name."["; }
  if ($lindices) {
    $name = $name."l=".$lindices;
    if ($rindex) {$name = $name.",";}
  }
  if ($rindex) { $name = $name."r=$rindex"; }
  if ($lindices || $rindex) { $name = $name."]"; }
  $name = $name.">";
  $compoundname = "<".$basename;
  if ($rindex) { $compoundname = $compoundname."[r=$rindex]"; }
  $compoundname = $compoundname.">";

# $_[1] = action = name|compoundname|basename|nrank|lindex|rindex|all
  if ($action eq "name") { return ($name); }
  elsif ($action eq "compoundname") { return ($compoundname); }
  elsif ($action eq "basename") { return ($basename); }
  elsif ($action eq "nrank") { return ($nrank); }
  elsif ($action eq "rank") { return ($rank); }
  elsif ($action eq "lindex") { return ($lindex); }
  elsif ($action eq "rindex") { return ($rindex); }
  elsif ($action eq "all") { return ($compoundname,$rank,$nrank,$lindex,$rindex); }
  elsif ($action eq "regionname") {
    if ($name =~ /<(all|domain|boundary) (cells|faces|nodes)>/) {
      print ::DEBUG "WARNING: spaces in system names have been deprecated: $name has been replaced with <$1$2>\n";
      $name = "<$1$2>";
    }
    if ($name eq "<boundaryfaces>") { $name = "<boundaries>"; }
    if ($name eq "<domaincells>") { $name = "<domain>"; }
    return ($name);
  }
}

#-------------------------------------------------------------------------------
# simple subroutine to extract the first string entry from a line of text, possibly removing any delimiters at the same time
# delimiter is governed by what starts the string:
#  if it is a " or ', string is delimited by the closest matching delimiter, with no escaping of characters in the middle
#  otherwise the string is delimited by the closest space (which becomes the delimiter)
# input
# $_[0] = string that we want something extracted from
# output
# first return = extracted string removed from the front, dedelimited
# second return = error flag (0 or 1)
# $_[0] = remainder of string, with leading spaces removed

sub extract_first {

  my ($input) = @_;

  my $remainder=$input;
  my $string="";
  my $delimiter="";
  my $error=0;
  
  if (nonempty($remainder)) {
    $remainder=~s/^\s*//; #remove leading spaces
    ($delimiter)=$remainder=~/^(['"])/;
    if (nonempty($delimiter)) {
      if ($remainder=~/^$delimiter(.*?)$delimiter/) {
        $string=$1; # $string is whatever is between closest delimiters
        $remainder=$';
      } else { print "WARNING: matching delimiters not found in the following string: $input\n"; $error=1; }
    } else {
      $remainder=~/^(.+?)(\s|$)/; # $string is whatever is before closest space
      $string=$1;
      $remainder=$';
    }
    $remainder=~s/^\s*//; #remove leading spaces
    $remainder=~s/\s*$//; #remove trailing spaces too now
  } else {
    $remainder = ''; # remainder could have been blank, so set it to nothing explicitly
  }

# print "string = $string: remainder = $remainder: delimiter = $delimiter\n";
# place remainder of string back in $_[0];
  $_[0]=$remainder;
# return the extracted string and error
  return ($string, $error);
}

#-------------------------------------------------------------------------------
# report and take action with any syntax problems
# these are handled a bit differently to normal messages so that user can see what problems there are with their syntax

# on entry:
# if three values given, then second is debug message
# if two, then message and action
# if one, then only message, and action is error

# message that goes with this problem
# syntax_action could be info, warning or error (which implies a stop and is the default if no syntax_action is given)
# default is an error, so becomes a drop-in replacement for error_stop subroutine

sub syntax_problem {

  my ($message, $debug_message, $syntax_action) = @_;
  if (!($debug_message)) {
    $debug_message = $message;
    $syntax_action = "error";
  } elsif (!($syntax_action)) {
    $syntax_action = $debug_message;
    $debug_message = $message;
  }

#   

# if ($#_ == 2) {
#   ($message, $debug_message, $syntax_action) = @_;
# } else {
#   ($message, $syntax_action) = @_;
#   if ($#_ == 0) { $syntax_action = "error"; }
#   $debug_message = $message;
# }

  print SYNTAX "\U$syntax_action: "."$debug_message\n";
  if ($syntax_action eq "error") {
    error_stop($message) # already writes to output and debug files
  } else {
    print "\U$syntax_action: "."$message\n";
    print ::DEBUG "\U$syntax_action: "."$debug_message\n";
  }
    
}

#-------------------------------------------------------------------------------
sub syntax_file {
  my ($action) = @_;

  if ($action eq "open") {
    open(SYNTAX, ">$::syntax_problems_file"); # this file is specifically for syntax problems in the input files and is written to by sub syntax_problem
  } elsif ($action eq "close") {
    close(SYNTAX);
  } else {
    error_stop("internal error: unknown syntax_file action $action\n");
  }
}

#-------------------------------------------------------------------------------

1;
