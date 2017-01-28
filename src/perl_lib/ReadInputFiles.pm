# file src/perl_lib/ReadInputFiles.pm
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
# routines to read arb input files contained in a perl module

# general notes about code parsing:

# parsing logic:

# 1. read in line and place in raw_buffer
# 2. remove comments and place in comments
# 3. add raw_buffer to buffer
# 4. check if buffer ends in continuation symbol (&).  If it does:
#   - remove continuation symbol
#   - get new raw_buffer
#   - check if raw_buffer starts with &.  If it does remove it and preceding space
#   - add to buffer and repeat 4
# 5. replace any deprecated string replacement code in buffer with new string code: ie
#   -  'GENERAL_REPLACEMENTS REPLACE "<<a>>" W "2.d0" D "<<b>>" W "#"' becomes '{{ "<<a>>" =g "2.d0" ; "<<b>>" =dg "#" }}'
#   -  'INCLUDE "an_arb_file" R "<<a>>" WITH "2.d0"' becomes 'INCLUDE "an_arb_file" {{ "<<a>>" = "2.d0" }}'

# notes:
# * actual algorithm loops through asking for processed code_lines for each code_block
# * buffers deal with linefeeds (\n), although remove all (used on windows in addition, as in \r\n) carriage returns (\r)


# 1. if string ends with continuation symbol (&) and then possibly a comment (space separated):
#    * remove comment and possibly continuation
#    * read in next line (place in raw_buffer) , apply 1->3, concatenate and check again
# 1. look for deprecated string related code and replace with new syntax, writing syntax message at the same time
# 2. do any string replacements on a per-file-line basis, except within any string_code sections delimited by {{ and }} (remembering code type over carriage returns)
# 3. now strip comments, concatenating onto comments 
# 5. possibly deal with filelinelocator?
# 6. now split input line at any ; characters, being cognisant of solver syntax and ignoring any string code
# 7. finally parse the line as solver code
#   1) {{ }} indicates string code, handled by separate sub
#   2) INCLUDE statements start new code block, with calling string code added to start of new code block's buffer
#   3) write unwrapped line (buffer) to unwrapped input file, which includes no string code or references (ie, straight solver code)

# code_block variables to maintain:
# raw_buffer = line as read from file: move to buffer until empty
# buffer = line which has had string replacements done, comments removed and concatenation dealt with: move to code_line until empty
# code_line = single solver code line ready to parse with sub parse_code_line: remove during parsing
# skip = 


package ReadInputFiles;

use strict;
use warnings;
use Common;
use Exporter 'import';
#our $VERSION = '1.00';
our @EXPORT  = qw(read_input_files); # list of subroutines and variables that will by default be made available to calling routine

# define variables common to all of these subs
my @string_variables = (); # is an array/hash of the replacement strings
my @code_blocks; # this will become a stack of recursively called arb code blocks (which could correspond to a new input file), starting with the root_input.arb file created by the arb script that contains INPUT_WORKING links to the arb files called by the user from the arb script

my $filelinelocator; # holds generic locator of current line for message purposes corresponding to $#code_blocks

my $unwrapped_indent = "   "; # amount to indent the unwrapped input file for each level of file inclusion

#--------------------------------------------------------------
# parse all *.arb files - this is called from main
sub read_input_files {

  setup_string_variables(); # create the default general replacements

# open unwrapped input file that will be used as a record only, and can be used for subsequent runs
  open(UNWRAPPED_INPUT, ">$::unwrapped_input_file");

  open(SYNTAX, ">$::syntax_problems_file"); # this file is specifically for syntax problems in the input files and is written to by sub syntax_problem

# push the first [0] code block (from the root_input.arb) onto the code_blocks array and prep for reading (open)
  push_code_block("root_input.arb","$::build_dir/root_input.arb");

  my $raw_buffer='';
  my $comments='';
    
  while (@code_blocks) { # we keep forming the buffer and parsing the code until all code blocks are destroyed

# buffer is ready to be processed
    if ( nonempty($code_blocks[$#code_blocks]{"buffer"}) && $code_blocks[$#code_blocks]{"buffer"} !~ /&\s*\n$/ ) { # buffer is ready to be processed to code_line as it is full and doesn't end with a continuation symbol
      $code_blocks[$#code_blocks]{"buffer"} =~ s/\n$//; # remove trailing linefeed

# set message line locator string
      $filelinelocator = "file = $code_blocks[$#code_blocks]{include_name}: linenumber = $.: line = \'$code_blocks[$#code_blocks]{buffer}\'";

# check and correct any deprecated string replacement comments here
      correct_deprecated_string_replacement_code($code_blocks[$#code_blocks]{"buffer"}); # here buffer has no comments, and no linefeed

      $code_blocks[$#code_blocks]{"buffer"} .= $comments; # and add on concatenated comments
      $comments = '';

# string replacements are done on complete line including comments, except for string code sections delimited by {{ and }}
# TODO



# this will become area that parses code
#     print UNWRAPPED_INPUT $unwrapped_indent x $#code_blocks,"#prior to parse_solver_code_line: $code_blocks[$#code_blocks]{buffer}\n";
      parse_solver_code_line($code_blocks[$#code_blocks]{"buffer"}); # processes the buffer

# buffer needs more raw_buffer from this point down
    } elsif (empty($raw_buffer)) { # if raw buffer is empty, then we need to fill this first
        my $handle = $code_blocks[$#code_blocks]{"handle"};
        if (defined($raw_buffer = <$handle>)) { # defined is required (as advised by warnings) as without file read could be '0', which while valid (and defined) is actually false.  Apparently the while (<>) directive does this automatically.
          $raw_buffer=~s/\r//g; # remove extra dos linefeeds
          if ($raw_buffer =~ /^([^#]*(&|))(\s*#.*)\n$/) { # remove comments with preceeding greedy space match
            $raw_buffer = $1."\n"; # replace linefeed
            $comments .= $3; # concatenate onto comment string
          }
        } else {
          pop_code_block(); # exit code_block if end of file is reached
          $raw_buffer = '';
        }

    } else {
# raw_buffer contains something from this point down

# if raw_buffer is to be a continuation need to deal with trailing and possibly leading continuation symbols
      if (nonempty($code_blocks[$#code_blocks]{"buffer"}) && $code_blocks[$#code_blocks]{"buffer"} =~ /&\s*\n$/ ) {
        $code_blocks[$#code_blocks]{"buffer"} = $`; # strip off continuation symbol, any trailing spaces from buffer and linefeed
        if ($raw_buffer =~ /^\s*&/) {$raw_buffer = $';} # strip off leading continuation symbol from raw_buffer
      }

      $code_blocks[$#code_blocks]{"buffer"} .= $raw_buffer; # buffer is either empty or ready for continuation here
      $raw_buffer = '';
    }

    if (@code_blocks) {
      print ::DEBUG "INFO: code_blocks loop".
        ": #code_blocks = $#code_blocks".
        ": raw_buffer = $raw_buffer".
        ": buffer = $code_blocks[$#code_blocks]{buffer}".
        ": skip = $code_blocks[$#code_blocks]{skip}\n";
    } else {
      print ::DEBUG "INFO: code_blocks loop finished\n";
    }

  } # end of loop for this input file

  close(UNWRAPPED_INPUT);
  close(SYNTAX);

# dump all of the simulation info into the fortran file, and output to the screen and debug
  $::sub_string{"simulation_info"} = '';
  foreach my $key ( keys(%::simulation_info)) {
    print ::DEBUG "SIMULATION INFO: "."\U$key"." = $::simulation_info{$key}\n";
    if ($key ne "description") { print "SIMULATION INFO: "."\U$key"." = $::simulation_info{$key}\n"; }
    print ::FORTRAN_INPUT "INFO_\U$key"." \"$::simulation_info{$key}\"\n"; # also tell the fortran program about this
    $::sub_string{"simulation_info"} = $::sub_string{"simulation_info"}."! SIMULATION INFO: "."\U$key"." = $::simulation_info{$key}\n"; # and write the same to equation_module.f90
  }

}
#--------------------------------------------------------------

sub setup_string_variables {

# ref: general replacements
# setup default string_variables
# loose convention is that replacement strings be delimited by <<>>, however any strings can (and will) be matched/replaced
# convention is that replacement names that end with "comment" are meant to preceed statements in the files, converting them to comments if they are not relevant
# this string is for batcher integration - if a file is run through batcher, this string will be replaced by an empty string, so can be used to precede arb lines that are specific to the batcher runs
  %{$string_variables[$#string_variables+1]} = ( search => "<<batchercomment>>", replace => "#" );
  %{$string_variables[$#string_variables+1]} = ( search => "<<nobatchercomment>>", replace => "" );
# geometry and equation related
  %{$string_variables[$#string_variables+1]} = ( search => "<<dim1comment>>", replace => "" );
  %{$string_variables[$#string_variables+1]} = ( search => "<<dim2comment>>", replace => "" );
  %{$string_variables[$#string_variables+1]} = ( search => "<<dim3comment>>", replace => "" );
  %{$string_variables[$#string_variables+1]} = ( search => "<<steadystatecomment>>", replace => "" ); # default is steady-state
  %{$string_variables[$#string_variables+1]} = ( search => "<<transientcomment>>", replace => "#" );
  %{$string_variables[$#string_variables+1]} = ( search => "<<cartesiancomment>>", replace => "" ); # default is cartesian
  %{$string_variables[$#string_variables+1]} = ( search => "<<cylindricalcomment>>", replace => "#" );
# convention is that replacement names that end with "flag" are either on (1) or off (0), so can be used within expressions
  %{$string_variables[$#string_variables+1]} = ( search => "<<steadystateflag>>", replace => "1" ); # default is steady-state
  %{$string_variables[$#string_variables+1]} = ( search => "<<transientflag>>", replace => "0" );
  %{$string_variables[$#string_variables+1]} = ( search => "<<cartesianflag>>", replace => "1" ); # default is cartesian
  %{$string_variables[$#string_variables+1]} = ( search => "<<cylindricalflag>>", replace => "0" );
# these two should be overwritten by the relevant radius in the input file if using cylindrical coordinates: eg R "<<radius_c>>" W "<cellx[l=1]>" R "<<radius_f>>" W "<facex[l=1]>"
  %{$string_variables[$#string_variables+1]} = ( search => "<<radius_c>>", replace => "1.d0" );
  %{$string_variables[$#string_variables+1]} = ( search => "<<radius_f>>", replace => "1.d0" );
  %{$string_variables[$#string_variables+1]} = ( search => "<<radius_n>>", replace => "1.d0" );
  %{$string_variables[$#string_variables+1]} = ( search => "<<radiusdim1flag>>", replace => "0" ); # for 2D cylindrical coordinates, set the radius dimension flag to 1 to include (for example) the hoop stress in that dimension
  %{$string_variables[$#string_variables+1]} = ( search => "<<radiusdim2flag>>", replace => "0" );
  %{$string_variables[$#string_variables+1]} = ( search => "<<radiusdim3flag>>", replace => "0" );
  %{$string_variables[$#string_variables+1]} = ( search => "<<radialdim>>", replace => "0" ); # for 2D cylindrical this is the radial coordinate direction
  %{$string_variables[$#string_variables+1]} = ( search => "<<axialdim>>", replace => "0" ); # for 2D cylindrical this is the axial coordinate direction
# these strings should be overwritten by the normal coordinate directions of any reflection boundaries in the domain: eg R "<<reflect=1>>" W "reflect=1"
  %{$string_variables[$#string_variables+1]} = ( search => "<<reflect=1>>", replace => "" );
  %{$string_variables[$#string_variables+1]} = ( search => "<<reflect=2>>", replace => "" );
  %{$string_variables[$#string_variables+1]} = ( search => "<<reflect=3>>", replace => "" );

  print ::DEBUG "INFO: initial string_variables = ".Dumper(@string_variables)."\n";

}

#-------------------------------------------------------------------------------
# search through string_variables for search string

sub search_string_variables {

  my $search = $_[0]; # on input, search string
  my $nfound = -1; # on output returns -1 if not found, or general_replacments index if found

  for my $n ( 0 .. $#string_variables ) {
    if ($search eq $string_variables[$n]{"search"}) { # found existing general replacements
      $nfound = $n;
      last;
    }
  }

  return $nfound;

}
#-------------------------------------------------------------------------------
# based on passed variable, set or unset transient simulation status, including comment strings

sub set_transient_simulation {

  $::transient_simulation = $_[0];
  for my $n ( 0 .. $#string_variables ) {
    if ($string_variables[$n]{"search"} eq "<<steadystatecomment>>") {
      if ($::transient_simulation) { $string_variables[$n]{"replace"} = "#" } else { $string_variables[$n]{"replace"} = "" }
    }
    if ($string_variables[$n]{"search"} eq "<<transientcomment>>") {
      if ($::transient_simulation) { $string_variables[$n]{"replace"} = "" } else { $string_variables[$n]{"replace"} = "#" }
    }
  }

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
sub create_external_file {
# finds a external file, and parses it for new externals
# on input
# $_[0] = name of file, based on the working directory
  my $filename = $_[0];
  my $search = 'preamble';
  my $current = '';
  my ($line);
  
  open (EXTERNAL, "<$::working_dir/$filename") or error_stop("Could not find external file $filename");
  
  my ($name) = $filename =~ /(.*)\.(f90|f|for)/;
  push(@::externals,{name => $name, preamble => '', contents => '', setup => '', used => 0}); # push a new hash onto this array
  print ::DEBUG "EXTERNAL: found new external file: name = $name: filename = $filename\n";
  
  while($line=<EXTERNAL>) {
  	chompm($line);
    if ($line =~ /^\s*arb_external_(\S+)($|\s)/) {
      if ($1 eq 'preamble' || $1 eq 'setup' || $1 eq 'contents') {
        $current = $1;
      } elsif ($1 eq 'operator') {
        if ($line =~ /^\s*arb_external_operator\s+(\S+)($|\s)/) { # also form a list of the operators that are within this file
          print ::DEBUG "EXTERNAL: found operator $1\n";
          push(@{$::externals[$#::externals]{"operators"}},$1);
          $::external_operators{$1}=$#::externals;
        } else {
          error_stop("missing arb_external_operator name in external file $filename");
        }
      } else {
        error_stop("unknown arb_external_$1 statement in external file $filename");
      }
  	} elsif (nonempty($current)) {
  		$::externals[$#::externals]{$current} = $::externals[$#::externals]{$current}."\n".$line;
  	}
  }
  close(EXTERNAL);
  print ::DEBUG "EXTERNAL: file $filename contains the following operators: @{$::externals[$#::externals]{operators}}\n";
  print "INFO: external file $filename contains the following operators: @{$::externals[$#::externals]{operators}}\n";
# print ::DEBUG "EXTERNAL PREAMBLE:\n".$::externals[$#::externals]{preamble}."\n";
# print ::DEBUG "EXTERNAL SETUP:\n".$::externals[$#::externals]{setup}."\n";
# print ::DEBUG "EXTERNAL CONTENTS:\n".$::externals[$#::externals]{contents}."\n";

}
#-------------------------------------------------------------------------------
# little sub to determine whether input location is an arbfile or directory

sub check_for_arbfile_or_dir {

# input
  my $search_file = $_[0];
# output: ($found_name,$found_type)
  my $found_name = '';
  my $found_type = '';

  if (-d $search_file) {
# if a directory is found
    $found_name = $search_file;
    $found_type = 'directory';
  } elsif ($search_file !~ /\/$/) {
# if no trailing slash is present, then this could be a file
    if (-f $search_file) {
# and it could already have the appropriate extension
      $found_name = $search_file;
      $found_type = 'file';
    } elsif ($search_file !~ /\.(arb|in)$/ && -f $search_file.'.arb') {
# or if it has no extension then try searching for the name with the .arb extension
      $found_name = $search_file.'.arb';
      $found_type = 'file';
    }
  }
# remove any trailing slashes from found_name
  $found_name =~ s/\/$//;
  print ::DEBUG "INFO: at end of check_for_arbfile_or_dir: search_file = $search_file: found_name = $found_name: found_type = $found_type\n";

  return ($found_name,$found_type);
}

#-------------------------------------------------------------------------------
# subroutine that parses one code_line of solver code
sub parse_solver_code_line {
# processes buffer, using information also about the file contained in $#code_block
# on input:
# $_[0] = line to process - includes comments but not linefeed
# on output:
# $_[0] = line after processing - the default is to process string and clear the buffer, however if a new file is included there may be associated string code which is placed in the buffer for processing

  use List::Util qw( min max );
  use Data::Dumper;
  use Storable qw(dclone);
  use File::Find; # for find

  my $line = $_[0]; # set line to local variable

  my $buffer = ''; # buffer is the variable that is going to be returned in $_[0]

  my %region_list = (); # contains the centring and REGION_LIST most recently specified in the input file (as used for REGION_CONSTANT)
  my $default_options = ""; # default options prepended to each statement read in
  my $override_options = ""; # override options appended to each statement read in

  my $oline=$line; # save line as (original) line
  my $comments;
  ($line,$comments)=$line=~/^\s*(.*?)\s*(#.*|)$/; # split off comments and extra leading/trailing spaces (noting that string replacements would have occurred since buffer processing)

  my $file = $code_blocks[$#code_blocks]{"include_name"}; # and grab filename for messaging purposes

# keep a record of what arb is doing in UNWRAPPED_INPUT, commenting out any INCLUDE or GENERAL_REPLACMENTS statements so that this file could be read again by arb directly
  if ($line =~ /^((INCLUDE(|_[A-Z]+))|\{\{)($|#|\s)/i) { print UNWRAPPED_INPUT $unwrapped_indent x $#code_blocks,"#(hash added during unwrap)$oline\n"; } else { print UNWRAPPED_INPUT $unwrapped_indent x $#code_blocks,"$oline\n"; }

# now process guts of statement
# this whole subroutine is one large if statement
# $line has no leading (or trailing) spaces, and no comments

#-------------------
# first check whether skip is active or there is a COMMENTS|SKIP statement or there is an empty line
  if ($code_blocks[$#code_blocks]{"skip"} && $line =~ /^((STOP|END)_(COMMENT(S){0,1}|SKIP))$/i) { print "INFO: found \L$1\E statement in $file\n"; $code_blocks[$#code_blocks]{"skip"}=0; }
  elsif ($code_blocks[$#code_blocks]{"skip"} || empty($line)) { } # do nothing!
  elsif ($line =~ /^((START|BEGIN)_(COMMENT(S){0,1}|SKIP))$/i) { print "INFO: found \L$1\E statement in $file\n"; $code_blocks[$#code_blocks]{"skip"}=1; }

#-------------------
# look for delimited sections, and END statement
  elsif ($line =~ /^(START|BEGIN)$/i) { print "INFO: found opening code block \L$1\E statement in $file\n"; push_code_block(); }
  elsif ($line =~ /^(STOP|END)$/i) { print "INFO: found closing code block \L$1\E statement in $file\n"; pop_code_block(); }
# not sure about {} delimiters
# elsif ($line =~ /^(START|BEGIN|{)$/i) { print "INFO: found opening code block \L$1\E statement in $file\n"; push_code_block(); }
# elsif ($line =~ /^(STOP|END|})$/i) { print "INFO: found closing code block \L$1\E statement in $file\n"; pop_code_block(); }

#-------------------
# TODO add IF blah and END_IF statements here, opening new block

#-------------------
# look for string code
  elsif ($line =~ /^\{\{(.*)\}\}$/i) {
#   print "FOUND string code: $&\n";
    my $string_code = $1;
#   print "INFO: processing string code $& from $file\n";
    print "INFO: processing string code $string_code from $file\n";
    my $x = 0;
    print "before: x = $x\n";
    my $eval_return = eval($string_code."; return ''");
    if ($@) { print "ERROR: error in $string_code\n"; }
    print "eval_return = $eval_return\n";

#   my $ok = eval {
#       $string_code;
#       print "within eval: x = $x\n";
#       return 1;
#   } or do {
#       # error handler code
#       my $error = $@;
#
#       ...
#       $line = "Error in executing the following string code: $line\n"
#   };

    print "after: x = $x\n";

#   $buffer = $line;

# TODO
#   push_code_block();
  }

#-------------------
# check for include statement, possibly opening new file
# ref: include ref: include_template ref: include_local ref: include_absolute ref: include_working ref: include_last
  elsif ($line =~ /^INCLUDE(|_([A-Z]+))($|\s)/i) {
    my $include_type = '';
    if (nonempty($2)) {$include_type = "\L$2";}
# note to user re deprecation of INCLUDE_ROOT
    if ($include_type eq "root" || $include_type eq "from") {
      syntax_problem("INCLUDE_"."\U$include_type"." has been deprecated.  Use INCLUDE_TEMPLATE instead which searches through the templates directory tree for a specific file, and at the same time, adds the file's path to the include_path stack.  Or, if the include_path stack already includes the path for the template file, you can just use the INCLUDE command as this searches through the include_path stack.  INCLUDE_"."\U$include_type"." has been replaced by INCLUDE_TEMPLATE in this instance: $filelinelocator","warning");
      $include_type = "template";
    }
    if ($3 =~ /#/) {$line = '';} else {$line = $';}
    my ($new_file,$error) = extract_first($line); # extract filename from line of text
    if ($error) { error_stop("a valid file or directory name could not be determined from the following: $filelinelocator\n"); }

#-------
    if (empty($new_file)) {
      if (nonempty($include_type)) { syntax_problem("the include_path stack can only have its top level removed (popped) using the generic INCLUDE statement, otherwise some error occurred in: $filelinelocator"); }
# an empty include statement means to remove one level off the search_path
      if ($#{$code_blocks[$#code_blocks]{"include_path"}} > 0) {
# this means to pull an item from the stack
        pop(@{$code_blocks[$#code_blocks]{"include_path"}});
        print "INFO: an INCLUDE statement is removing (popping) an include_path from the stack, leaving: include_path = $code_blocks[$#code_blocks]{include_path}[0]\n";
        print UNWRAPPED_INPUT $unwrapped_indent x $#code_blocks,"#(comment generated during unwrap) after one has been removed, currently: include_path = @{$code_blocks[$#code_blocks]{include_path}}\n";
      } else {
        print "WARNING: an INCLUDE statement is attempting to remove an include_path from the stack, but there is only the local path left which cannot be removed: include_path = $code_blocks[$#code_blocks]{include_path}[0]\n";
        print UNWRAPPED_INPUT $unwrapped_indent x $#code_blocks,"#(comment generated during unwrap) after failed removal attempt, only the single local (unremovable) path is left: include_path = $code_blocks[$#code_blocks]{include_path}[0]\n";
      }
    } else {

#-------
  # otherwise we are creating either a new include file or adding to the current file's include_paths
      my $found_name = ''; # this will hold the found file or directory
      my $found_type = ''; # this will specify whether what was found was a file or directory
      my $found_depth = 1; # this is used only for file::find used within the template include
      if (empty($include_type)) {
  # if this is a plain INCLUDE statement then we cycle through the list of paths looking for the file or directory
# TODO: make include path search also recursive through code_blocks
        for my $search_path ( reverse( @{$code_blocks[$#code_blocks]{"include_path"}} ) ) {
          ($found_name,$found_type) = check_for_arbfile_or_dir($search_path.'/'.$new_file);
          if (nonempty($found_name)) {
            print ::DEBUG "INFO: found include $found_type $new_file at $found_name\n";
            last;
          }
        }
        if (empty($found_name)) {
          error_stop("could not find $new_file that is referenced in an INCLUDE statement in any of the current include paths (ie, the include path stack): $filelinelocator");
        }
      } elsif ($include_type eq "template") {
  # the following would only check in the templates directory
  #         ($found_name,$found_type) = check_for_arbfile_or_dir($::template_dir.'/'.$new_file);
  # whereas this cycles through all subdirectories of the templates directory, using a depth-prioritised search
  # as file paths are relative to the build directory, don't chdir is required to reference filename
        find ({ wanted => sub { wanted($new_file,$found_name,$found_type,$found_depth); }, no_chdir => 1},$::template_dir);
        if (empty($found_name)) {
          error_stop("could not find $new_file that is referenced in an INCLUDE_TEMPLATE statement in any of template directories: $filelinelocator");
        }
  #         if (-f $found_name) { $found_type = 'file'; } elsif (-d $found_name) { $found_type = 'directory'; }
      } else {
        if ($include_type eq "absolute") {
          ($found_name,$found_type) = check_for_arbfile_or_dir('/'.$new_file);
        } elsif ($include_type eq "local") {
          ($found_name,$found_type) = check_for_arbfile_or_dir($code_blocks[$#code_blocks]{"include_path"}[0].'/'.$new_file);
        } elsif ($include_type eq "last") {
          ($found_name,$found_type) = check_for_arbfile_or_dir($code_blocks[$#code_blocks]{"include_path"}[$#{$code_blocks[$#code_blocks]{"include_path"}}].'/'.$new_file);
        } elsif ($include_type eq "working") {
          ($found_name,$found_type) = check_for_arbfile_or_dir($::working_dir.'/'.$new_file);
        } else {
          error_stop("keyword INCLUDE_"."\U$include_type"." is not understood: $filelinelocator");
        }
        if (empty($found_name)) {
          error_stop("could not find $new_file that is referenced in an INCLUDE_"."\U$include_type"." statement: $filelinelocator");
        }
      }
      print "INFO: found the following include $found_type $new_file at $found_name\n";
      print ::DEBUG "INFO: found the following include $found_type $new_file at $found_name referenced from: $filelinelocator\n";

  # here we extract the path from the full found_name if we have found a file, or remove found_name (and store in found_dir) if we have found a directory
      my $found_dir = '';
      if ($found_type eq 'file' && $found_name =~ /\//) {
        ($found_dir) = $found_name =~ /^(.*)\/.*?$/;
      } elsif ($found_type eq 'directory') {
        $found_dir = $found_name;
        $found_name = '';
      }

  # check whether this path is on the top of the current files's include_path list, and if not, add it
      if (nonempty($found_dir)) {
        if ($found_dir ne $code_blocks[$#code_blocks]{"include_path"}[$#{$code_blocks[$#code_blocks]{"include_path"}}]) {
          push(@{$code_blocks[$#code_blocks]{"include_path"}},$found_dir);
          print "INFO: adding new path $found_dir to the include_path stack, making: include_path = @{$code_blocks[$#code_blocks]{include_path}}\n";
          print ::DEBUG "INFO: adding new path $found_dir to the include_path stack, making: include_path = @{$code_blocks[$#code_blocks]{include_path}}\n";
          print UNWRAPPED_INPUT $unwrapped_indent x $#code_blocks, "#(comment generated during unwrap) adding new include_path $found_dir, making include_path stack = @{$code_blocks[$#code_blocks]{include_path}}\n";
        } else {
          print UNWRAPPED_INPUT $unwrapped_indent x $#code_blocks, "#(comment generated during unwrap) not adding new include_path $found_dir, as it is already on the top of include_path stack = @{$code_blocks[$#code_blocks]{include_path}}\n";
        }
      }
          
# now open the file and set new buffer
      if (nonempty($found_name)) {

        push_code_block($new_file,$found_name);

# add any remainder on line back to buffer for processing next time
        $buffer = $line;
      }

    }
#-------

#-------------------
# extract any general replacements, pushing them onto the back of the existing list
# replacements are performed in reverse order, so latest replacement definitions take precedence
#   } elsif ($line =~ /^(GENERAL_|)REPLACEMENTS($|\s*#|\s)/i) {
#     print "INFO: found GENERAL_REPLACEMENTS statement in file = $file\n";
#     if ($2 =~ /#/) {$line = '';} else {$line = $';}
#     while (!($line=~/^\s*$/ || $line =~ /^\s*#/)) {
#       ($search,$replace,$cancel,$default) = extract_replacements($line,$file,$oline);
#       if (nonempty($search)) {
# # if the replacement is a default replacement, first need to check whether string is part of an existing input file replacement
#         if ($default) {
#           foreach my $n1 ( reverse( 0 .. $#code_blocks ) ) {
#             foreach my $n2 ( 0 .. $#{$code_blocks[$n1]{"replacements"}} ) {
#               if ($search eq $code_blocks[$n1]{"replacements"}[$n2]{"search"}) { # found search string in input file replacements
#                 print "INFO: general replacement (default) search string $search cancelled as the string is already present as an input file string replacement\n";
#                 last;
#               }
#             }
#           }
#         }
# # v0.42 general replacements of each search string is unique
#         my $n = search_string_variables($search);
#         if ($n < 0) { # indicates that string is not found in string_variables
#           if ($cancel) {
#             print "WARNING: general replacement search string $search cancelled before being allocated\n";
#           } else { # add a new one
#             %{$string_variables[$#string_variables+1]} = ( search => $search, replace => $replace );
#             print "INFO: added general replacements search and replace pair: search = $search: replace = $replace\n";
#           }
#         } else { # found search as an existing general replacement
#           if ($cancel) {
#             splice(@string_variables,$n,1);
#             print "INFO: cancelling previous general replacements search string: search = $search\n";
#           } elsif ($default) {
#             print "INFO: general replacement (default) search string $search cancelled as the string is already present as a general string replacement\n";
#           } else {
#             $string_variables[$n]{"replace"}=$replace;
#             print "INFO: replaced general replacements search and replace pair: search = $search: replace = $replace\n";
#           }
#         }

#       }
#     }
#     print ::DEBUG "INFO: just processed GENERAL_REPLACEMENTS statement: string_variables = ".Dumper(@string_variables)."\n";

#-------------------
# remove any in-line comments now (so from now on, do not have to consider # in any string matching)
# if ($line =~ /^(.*?)#/) { $comments = $'; $line = $1 } else { $comments = ''; }

# check for EXTERNALS statement, which accepts a list of external fortran external files
  } elsif ($line =~ /^EXTERNAL(|S)(\s+)/i) { 
    $line = $';
    while ( ( my ($tmp,$error) = extract_first($line) )[0] ) {  # the ()[0] is slice notation, which extracts the first element of the list, which is evaluated as a conditional
# check that there weren't any errors, and that text was properly quoted
      if ($error) { error_stop("some type of syntax problem with the EXTERNAL statement.  Should the text be quoted?: $filelinelocator"); }
      create_external_file($tmp);
    }

#---------------------
# ref: deprecated syntax
# check for deprecated syntax and correct if possible
  } elsif ($line =~ /^(CELL_|FACE_|NODE_|NONE_|)(INDEPENDENT|FIELD|DEPENDENT)($|\s)/i) {
    my $deprecatedtype = "\U$2";
    my $type;
    if ($deprecatedtype eq "DEPENDENT") {
      $type = "DERIVED";
    } else {
      $type = "UNKNOWN";
    }
#   $line = $`.$1.$type.$3.$';
    syntax_problem("$deprecatedtype type has been deprecated, use $type instead: $filelinelocator");
  } elsif ( $line =~ /^(READ_GMSH)($|\s)/i ) {
    syntax_problem("READ_GMSH keyword has been deprecated, use MSH_FILE instead: $filelinelocator");
  } elsif ( $line =~ /^(LINEAR_SOLVER)($|\s)/i ) {
    syntax_problem("LINEAR_SOLVER keyword has been deprecated, use SOLVER_OPTIONS linearsolver=default (eg) instead: $filelinelocator");

#---------------------
# check for the user ERROR or WARNING statements that signifies that there is a known problem with a file
  } elsif ($line =~ /^(ERROR|WARNING|INFO)(\s|$)/i) {
    my $key="\L$1"; $line=$';
# pull out any string that follows this keyword if there is one
    my ($tmp,$error) = extract_first($line);
    if (empty($tmp)) { $tmp = "no explanation provided"; }
    if ($key eq "error") { error_stop("user error statement found in $file: $tmp"); }
    else { print "\U$key",": user ","\L$key"," statement found in $file: $tmp\n"; }

#---------------------
# check on file version
  } elsif ($line =~ /^\s*VERSION\s+(\S*)/i) {
    my $file_version = $1;
    if (abs($file_version - $::version) > 1.e-7) {
      if ($file_version < $::minimum_version) {
        syntax_problem("version mismatch between $file and the current version (of setup_equations.pl)\n".
            "  You may be able to increase the version number in $file (from $file_version to $::version), however some features of the language ".
            "syntax have changed since version $file_version so you should check the input files: $filelinelocator");
      } else {
        syntax_problem("version mismatch between $file and the current version (of setup_equations.pl)\n".
            "  You should be able to increase the version number in $file (from $file_version to $::version) safely without altering the input ".
            "file syntax, however this error indicates that additional language features are now available: $filelinelocator","warning");
      }
    }
    print ::FORTRAN_INPUT "VERSION $file_version\n"; # also tell the fortran program about this

#---------------------
# ref: INFO
# look for userable simulation info statements
  } elsif ($line =~ /^INFO_(TITLE|DESCRIPTION|AUTHOR|DATE|VERSION)(|\+|-)\s+/i) {
    my $key="\L$1"; $line=$';
    my $append;
    if ($2 eq "+") { $append=1; } elsif ($2 eq "-") {$append=-1;} else { $append=0; }
    my ($tmp,$error) = extract_first($line);
# check that there weren't any errors, and that text was properly quoted
    if ($error || $line =~ /\S/) { error_stop("some type of syntax problem with the INFO_"."\U$key"." string.  Should the text be quoted?: $filelinelocator"); }
# a plus signifies to append the string to the current value
# a minus means set the string only if it is empty.
# Otherwise, set the string to the new value ($append=0)
    if ($append eq 1) {
      $::simulation_info{$key} = $::simulation_info{$key}.$tmp;
    } elsif (!($append) || empty($::simulation_info{$key})) {
      $::simulation_info{$key} = $tmp;
    }

#-------------------
# look for transient/steady-state simulation keyword
  } elsif ($line =~ /^(TRANSIENT|STEADY-STATE|STEADYSTATE|NONTRANSIENT)_SIMULATION($|\s)/i) {
    print ::DEBUG "INFO: $1_SIMULATION set directly: $filelinelocator\n";
    print "INFO: $1_SIMULATION set directly\n";
    if ($1 =~ /^TRANSIENT$/i) { set_transient_simulation(1); } else { set_transient_simulation(0); }

#-------------------
# look for newtient simulation keyword
  } elsif ($line =~ /^\s*((NON|)NEWTIENT)_SIMULATION($|\s)/i) {
    print ::DEBUG "INFO: $1_SIMULATION set directly: $filelinelocator\n";
    print "INFO: $1_SIMULATION set directly\n";
    if ($1 =~ /^NEWTIENT$/i) { $::newtient_simulation=1; } else { $::newtient_simulation=0; }

#-------------------
# ref: default options
# set or reset any default options for variables (these go before the individual options so any relevant individual options take precedence over these)
# also, each DEFAULT_OPTIONS statement clears previous DEFAULT_OPTIONS statements
  } elsif ($line =~ /^DEFAULT_OPTIONS\s*($|\s)/i) {
    $default_options = $';
    ($default_options) = $default_options =~ /^\s*(.*?)\s*$/; # greedy space matches at the front and back remove all leading and trailing space
    if (empty($default_options)) {
      print ::DEBUG "INFO: default options have been removed via: $filelinelocator\n";
      print "INFO: default options have been removed\n";
    } else {
      print ::DEBUG "INFO: default options have been set to $default_options via: $filelinelocator\n";
      print "INFO: default options have been set to $default_options\n";
    }

#-------------------
# ref: override options
# set or reset any override options (these go at the end of the individual options so override any individual options)
# also, each OVERRIDE_OPTIONS statement clears previous OVERRIDE_OPTIONS statements
  } elsif ($line =~ /^OVERRIDE_OPTIONS\s*($|\s)/i) {
    $override_options = $';
    ($override_options) = $override_options =~ /^\s*(.*?)\s*$/; # greedy space matches at the front and back remove all leading and trailing space
    if (empty($override_options)) {
      print ::DEBUG "INFO: override options have been removed via: $filelinelocator\n";
      print "INFO: override options have been removed\n";
    } else {
      print ::DEBUG "INFO: override options have been set to $override_options via: $filelinelocator\n";
      print "INFO: override options have been set to $override_options\n";
    }

#-------------------
# these are commands that need to be transferred unaltered to the arb input file
  } elsif ( $line =~ /^(MSH_FILE|((KERNEL|SOLVER|GENERAL)(|_OPTION(|S)))|ITERRESTOL|ITERRESRELTOL|ITERSTEP(MAX|CHECK)|NEWTRESTOL|NEWTSTEP(MAX|MIN|OUT|DEBUGOUT)|TIMESTEP(MAX|MIN|OUT|ADDITIONAL)|TIMESTEPSTART|NEWTSTEPSTART|GLUE_FACES|((TIME|NEWT)STEP_REWIND))($|\s)/i ) {
    my $keyword = "\U$1";
    $line = $'; $line =~ s/^\s*//;
    if ($keyword =~ /^((KERNEL|SOLVER|GENERAL)(|_OPTION(|S)))$/) {$keyword = "$2_OPTIONS";} # standardise the statement for fortran input
# if this is a GLUE_FACES then check if reflect=? has been specified and if so, set general_replacement string automatically
# not setting this in the arb input file has caught me out so many times that it is now automatic, but can be overwritten after the GLUE_FACES command if need be
    if ($keyword =~ /^GLUE_FACES/i) {
      my $tmp = $line;
# strip any region definitions from line, just in case a region contains a reflect=N statement in it...
      while ($tmp =~ /\s*<(.+?)>\s*/) { $tmp = $`." ".$'; }
      while ($tmp =~ /\s*reflect\s*=\s*(\d)\s*/i) {
        my $search="<<reflect=$1>>";
        my $replace="reflect=$1";
        $tmp = $tmp = $`." ".$';
        my $n = search_string_variables($search);
        if ($n < 0) { error_stop("some type of error with the reflect specification in the following: $filelinelocator"); }
        else { $string_variables[$n]{"replace"}=$replace; }
        print "INFO: based on a GLUE_FACES statement setting $search string_variables string to $replace\n";
      }
    }
    print ::FORTRAN_INPUT $keyword; if (nonempty($line)) {print ::FORTRAN_INPUT " ".$line}; print ::FORTRAN_INPUT "\n"; # print line to fortran input file

#-------------------
# read in region_list
  } elsif ($line =~ /^(CELL_|FACE_|NODE_|)REGION_LIST($|\s)/i) {
    $line = $';
# reset most recently read region list
    %region_list = ();
# find centring
    $region_list{"centring"} = '';
    if ($1) { $region_list{"centring"} = "\L$1"; ($region_list{"centring"}) = $region_list{"centring"} =~ /^(\S+?)_/; }
# read in regions and store in an array
    while ($line =~ /^\s*(<.+?>)\s*/) { 
      $line = $';
      push(@{$region_list{"regions"}},examine_name($1,'regionname')); # the regions hash contains an array of region names
    }
    if ($line !~ /^\s*$/) {error_stop("there is a syntax error in the following REGION_LIST statement: $filelinelocator");}
    if (empty($region_list{"regions"})) {error_stop("the following REGION_LIST statement contains no regions: $filelinelocator");}
# print some info about this
    my $tmp = "INFO: found ";
    if (nonempty($region_list{"centring"})) { $tmp = $tmp."$region_list{centring} centred"; } else { $tmp = $tmp."unknown centring"; }
    $tmp = $tmp." REGION_LIST containing the regions: @{$region_list{regions}}\n";
    print $tmp; print ::DEBUG $tmp;

#-----------------------
# user variables, by type and name
# ref: VARIABLE
  } elsif ($line =~ /^(CELL_|FACE_|NODE_|NONE_|)(CONSTANT|REGION_CONSTANT|TRANSIENT|NEWTIENT|DERIVED|UNKNOWN|EQUATION|OUTPUT|CONDITION|LOCAL|VARIABLE)($|\s)/i) {
    $line = $';
    my $type = "\L$2"; # NB: $2 cannot be empty
    if ($type eq "variable") { $type = ''; }; # the variable keyword is just a placeholder - set type to an empty string
    my $region_constant;
    if ($type eq "region_constant") { $type = "constant"; $region_constant = 1; } else { $region_constant = 0; }
    my $centring = ""; # now centring can be grabbed from last definition
    if ($1) { $centring = "\L$1"; ($centring) = $centring =~ /^(\S+?)_/; }

    my $name='';
    if ($line =~ /^\s*(<.+?>)($|\s)/) { $name = $1; $line = $'; }
    else { error_stop("problem reading in the variable name from the following line: $filelinelocator");}
    print ::DEBUG "INFO: found user variable in input file: name = $name: type = $type: centring = $centring\n";
    $name = examine_name($name,"name");
    print ::DEBUG "  coverting user defined name to consistent name = $name\n";

# see if this name has already been defined, and if so, find position of the variable in the input file
    my $masread = -1; # variable masread starts at 0 if any variables are defined
    foreach my $mcheck ( 0 .. $#::asread_variable ) {
      if ($::asread_variable[$mcheck]{"name"} eq $name) { # variable has been previously defined, and position in file is based on first definition
        $masread = $mcheck;
        last;
      }
    }

#-------
# check for CANCEL keyword
    if ($line =~ /^(\s*)CANCEL(\s|$)/) {
      if ($masread >= 0) {
        print "INFO: cancelling variable $name\n";
        print ::DEBUG "INFO: cancelling variable $name\n";
        splice(@::asread_variable, $masread, 1);
# also have to adjust the reference to the variables from the regions
        for my $nregion ( 0 .. $#::region ) {
          if ($::region[$nregion]{"last_variable_masread"} >= $masread) { $::region[$nregion]{"last_variable_masread"}=$::region[$nregion]{"last_variable_masread"}-1; }
        }
# TODO: get rid of options if variable is cancelled
      } else {
        print "WARNING: attempting to cancel variable $name that hasn't been defined yet - CANCEL ignored\n";
        print ::DEBUG "WARNING: attempting to cancel variable $name that hasn't been defined yet - CANCEL ignored\n";
      }
#-------
# otherwise we are either updating or creating a variable
    } else {

  # now create or update variable type and centring
      if ($masread >= 0) {
        $::asread_variable[$masread]{"redefinitions"}++; 
        print "INFO: a secondary definition statement (repeat number $::asread_variable[$masread]{redefinitions}) for variable $name has been found in file = $file\n";
        print ::DEBUG "INFO: a secondary definition statement (repeat number $::asread_variable[$masread]{redefinitions}) for variable $name has been found based on: $filelinelocator\n";
  # a variable has been identified, now check whether the type has changed
        if (nonempty($type)) {
          if ($type ne $::asread_variable[$masread]{"type"} && nonempty($::asread_variable[$masread]{"type"})) {
            print "NOTE: changing variable $name from type $::asread_variable[$masread]{type} to $type\n";
            print ::DEBUG "NOTE: changing variable $name from type $::asread_variable[$masread]{type} to $type based on: $filelinelocator\n";
            $::asread_variable[$masread]{"typechanges"}++;
          }
          $::asread_variable[$masread]{"type"} = $type;
        } else {
          $type = $::asread_variable[$masread]{"type"};
        }
  # a variable has been identified, now check whether the centring has changed
        if (nonempty($centring)) {
          if ($centring ne $::asread_variable[$masread]{"centring"} && nonempty($::asread_variable[$masread]{"centring"})) {
            print "NOTE: changing the centring of variable $name from $::asread_variable[$masread]{centring} to $centring\n";
            print ::DEBUG "NOTE: changing the centring of variable $name from $::asread_variable[$masread]{centring} to $centring based on: $filelinelocator\n";
            $::asread_variable[$masread]{"centringchanges"}++;
  # also clear previous region
            if (nonempty($::asread_variable[$masread]{"region"})) {
              print "NOTE: during change of centring type region specification of $::asread_variable[$masread]{region} deleted for variable $name\n";
              print ::DEBUG "NOTE: during change of centring type region specification of $::asread_variable[$masread]{region} deleted for variable $name\n";
              $::asread_variable[$masread]{"region"} = '';
            }
          }
          $::asread_variable[$masread]{"centring"} = $centring;
        } else {
          $centring = $::asread_variable[$masread]{"centring"};
        }
        $::asread_variable[$masread]{"comments"}=$::asread_variable[$masread]{"comments"}." ".$comments;
        $::asread_variable[$masread]{"filename"}=$::asread_variable[$masread]{"filename"}." ".$file;
        $::asread_variable[$masread]{"absfilename"}=$::asread_variable[$masread]{"absfilename"}." ".$code_blocks[$#code_blocks]{"abs_name"};
      } else {
        print "INFO: a primary definition statement for variable $name has been found in file = $file\n";
        print ::DEBUG "INFO: a primary definition statement for variable $name has been found based on: $filelinelocator\n";
  # otherwise create a new variable
        $masread=$#::asread_variable+1;
        print ::DEBUG "INFO: creating new variable number $masread with name $name based on: $filelinelocator\n";
  # and set basic info, empty if necessary
        $::asread_variable[$masread]{"name"}=$name;
        $::asread_variable[$masread]{"type"}=$type;
        if (empty($centring)) { $centring = 'none'; } # default centring if not previously set
        $::asread_variable[$masread]{"centring"}=$centring;
        $::asread_variable[$masread]{"rindex"}=examine_name($name,"rindex"); # this is based on name so doesn't change with repeat definitions
        $::asread_variable[$masread]{"comments"}=$comments;
        $::asread_variable[$masread]{"region"}='';
        foreach my $repeats (keys(%::statement_repeats)) {
          $::asread_variable[$masread]{$repeats}=0;
        }
        $::asread_variable[$masread]{"options"} = '';
        $::asread_variable[$masread]{"filename"}=$file;
        $::asread_variable[$masread]{"absfilename"}=$code_blocks[$#code_blocks]{"abs_name"};
      }

  # units and multiplier (optional)
  # first determine whether any specification has been made
      my $units = ''; my $multiplier = '';
      if ($line =~ /^\s*\[(.*?)\]/) {
  # units specification is present to determine what they are and strip them from the front of $line
        my $cunits = $1;
        $line = $';
  # split units if multiplier is present
        if ($cunits =~ /\*/) { ($multiplier,$units) = $cunits =~ /(.*?)\*(.*)/; }
        else { $multiplier=""; $units=$cunits;}
        $multiplier =~ s/e|E|D/d/; # convert single and floats to double precision, regardless of case
        if (nonempty($units)) { $::asread_variable[$masread]{"units"}=$units; }
        if (nonempty($multiplier)) { $::asread_variable[$masread]{"multiplier"}=$multiplier; }
      }
      if (!($::asread_variable[$masread]{"units"})) { $::asread_variable[$masread]{"units"} = "1"; }
      if (!($::asread_variable[$masread]{"multiplier"})) { $::asread_variable[$masread]{"multiplier"} = "1.d0"; }

  # if type is a transient or newtient make sure that the simulation type is consistent
      if ($type eq "transient") { set_transient_simulation(1); }
      if ($type eq "newtient") { $::newtient_simulation = 1; }
      
  # equations or numerical constants
  # look for either a single (CONSTANT) or list (REGION_CONSTANT) of numbers, or otherwise an expression for this variable
  # first delete any orphaned initial_equations or constant_lists
      if (!($type eq "transient" || $type eq "newtient") && nonempty($::asread_variable[$masread]{"initial_equation"})) {
        print "NOTE: deleting initial equation for $type variable $name that must have been left over from a previous definition\n";
        print ::DEBUG "NOTE: deleting initial equation for $type variable $name that must have been left over from a previous definition\n";
        delete $::asread_variable[$masread]{"initial_equation"};
      }
      if ($type ne "constant" && nonempty($::asread_variable[$masread]{"constant_list"})) {
        print "NOTE: deleting constant list (numerical value) for $type variable $name that must have been left over from a previous constant definition\n";
        print ::DEBUG "NOTE: deleting constant list (numerical value) for $type variable $name that must have been left over from a previous constant definition\n";
        delete $::asread_variable[$masread]{"region_list"};
        delete $::asread_variable[$masread]{"constant_list"};
      }
  # look for numerical constants which must start with either +-. or a digit
      if ( $type eq "constant" && $line =~ /^\s*[\+\-\d\.]/ ) { # to use a numerical constant value the type must be known at read-in time
        print ::DEBUG "INFO: assuming a numerical constant is entered in the following: $filelinelocator\n";
        if (nonempty($::asread_variable[$masread]{"equation"}) || nonempty($::asread_variable[$masread]{"initial_equation"})) {
          print "NOTE: resetting CONSTANT $name from an equation form to a numerical form\n";
          delete $::asread_variable[$masread]{"equation"}; # preference is to delete these key/values as they then won't be included in %variable (for restart purposes)
          delete $::asread_variable[$masread]{"initial_equation"};
        }
        my $n = 1;
        delete $::asread_variable[$masread]{"region_list"};
        delete $::asread_variable[$masread]{"constant_list"};
        if ($region_constant) {
          if (empty($region_list{"regions"})) { syntax_problem("a $centring REGION_CONSTANT appears before a REGION_LIST has been defined: $filelinelocator");}
          if ($centring eq "none") { syntax_problem("attempting to set a none centred constant $name using a REGION_CONSTANT statement: use a NONE_CONSTANT statement instead: $filelinelocator");}
          if (nonempty($region_list{"centring"}) && $region_list{"centring"} ne $centring) { syntax_problem("the $centring centring of a REGION_CONSTANT is not consistent with the $region_list{centring} centring of the preceeding REGION_LIST: $filelinelocator");}
          @{$::asread_variable[$masread]{"region_list"}} = @{$region_list{"regions"}}; # set region_list to that of most recent REGION_LIST
          $n = scalar(@{$region_list{"regions"}}); # returning the number of elements in this array
        }
        while ($line =~ /^\s*([\+\-\d\.][\+\-\ded\.]*)(\s+|$)/i) { # numbers must start with either +-. or a digit, so options cannot start with any of these
          $line = $';
          my $match = "\L$1";
          if ($match !~ /\d/) { syntax_problem("a numerical constant was not valid in the following line, indicating some type of syntax error: $filelinelocator"); };
  # make sure all constants are written in double precision
          $match =~ s/e/d/;
          if ($match !~ /d/) { $match = $match."d0"; }
          if ($match !~ /\./) { $match =~ s/d/.d/; }
          push(@{$::asread_variable[$masread]{"constant_list"}},$match); # assemble list of numerical constants
        }
        if (empty($::asread_variable[$masread]{"constant_list"})) { syntax_problem("no numerial constants were read in from the following line, indicating some type of syntax error: $filelinelocator"); };
        print ::DEBUG "INFO: found the following constant_list for $name: @{$::asread_variable[$masread]{constant_list}}\n";
        if ($region_constant) { print ::DEBUG "INFO: found the following region_list for $name: @{$::asread_variable[$masread]{region_list}}\n"; }
        if ($n ne @{$::asread_variable[$masread]{"constant_list"}} ) {
          if ($region_constant) {
            syntax_problem("the following REGION_CONSTANT line has ".scalar(@{$::asread_variable[$masread]{constant_list}})." numerical entries, whereas the preceeding REGION_LIST has $n entries - these should match: $filelinelocator");
          } else { syntax_problem("a single numerical constant could not be read from the following CONSTANT line: $filelinelocator"); }
        }
      } elsif ( $line =~ /^\s*["']/ ) {
        print ::DEBUG "INFO: assuming an expression (rather than a numerical constant) is entered in the following: $filelinelocator\n";
        delete $::asread_variable[$masread]{"region_list"};
        delete $::asread_variable[$masread]{"constant_list"};
  # read in expressions, noting that only if the expression is nonempty do we overide previously stored expression
  # this allows initial_equation to be reset independently of the equation for transient/newtient variables
        my ($tmp1,$error) = extract_first($line);
        if ($error) { syntax_problem("some type of syntax problem with the (first) expression in the following $type $name variable definition: $filelinelocator"); }
        if (($type eq "transient" || $type eq "newtient") && $line =~ /^\s*["']/) { # to set the intial expression the type must be known at read-in time
          my ($tmp2,$error) = extract_first($line);
          if ($error) { syntax_problem("some type of syntax problem with the second expression in the following $type $name variable definition: $filelinelocator"); }
  # if we are here then tmp1 corresponds to the initial_equation, and tmp2 to the equation
  # Note that later when these equations are processed (in organise_user_variables), empty and undef now have different meanings
  # depending on variable type and r index, empty ("") now means to repeat the full equation, whereas undef means to give it a value of zero
          my $previous_equation = ""; if (nonempty($::asread_variable[$masread]{"initial_equation"})) { $previous_equation = $::asread_variable[$masread]{"initial_equation"}; }
          $::asread_variable[$masread]{"initial_equation"} = expand_equation($tmp1,$::asread_variable[$masread]{"name"},$previous_equation,$oline,$::asread_variable[$masread]{"selfreferences"});
          print ::DEBUG "INFO: setting the $type $name initial_equation to $::asread_variable[$masread]{initial_equation} based on: $filelinelocator\n";
  # incase we need to only set the initial_equation of a variable, keeping the previous equation value, only set equation if it is actually nonempty (ie, not "")
          if (nonempty($tmp2)) {
            my $previous_equation = ""; if (nonempty($::asread_variable[$masread]{"equation"})) { $previous_equation = $::asread_variable[$masread]{"equation"}; }
            $::asread_variable[$masread]{"equation"} = expand_equation($tmp2,$::asread_variable[$masread]{"name"},$previous_equation,$oline,$::asread_variable[$masread]{"selfreferences"});
            print ::DEBUG "INFO: setting the $type $name equation to $::asread_variable[$masread]{equation} based on: $filelinelocator\n";
          }
        } else {
  # if we are here then tmp1 corresponds to the equation
  # save previous equation for possible selfreference replacement, passing an empty string if it hasn't been previously defined
          my $previous_equation = ""; if (nonempty($::asread_variable[$masread]{"equation"})) { $previous_equation = $::asread_variable[$masread]{"equation"}; }
          $::asread_variable[$masread]{"equation"} = expand_equation($tmp1,$::asread_variable[$masread]{"name"},$previous_equation,$oline,$::asread_variable[$masread]{"selfreferences"});
          print ::DEBUG "INFO: setting the $type $name equation to $::asread_variable[$masread]{equation} based on: $filelinelocator\n";
        }
  # check/set defaults for these later, after all variable definitions have been read in
      }

  # region
      if ( $line =~ /^\s*ON(\s*)(<.+?>)\s*/i ) {
        if ($2 eq "<none>" ) { # the '<none>' region cancels the previously defined region
          $::asread_variable[$masread]{"region"} = '';
        } else {
          $::asread_variable[$masread]{"region"} = examine_name($2,'regionname');
        }
        $line = $';
      }

  # store raw options in the ::asread_variable array now
  # variable and compound option lists will be assembled later
      $line =~ s/^\s*//; # remove any leading space from the line
      if (nonempty($line) || nonempty($default_options) || nonempty($override_options)) {
        $line = $default_options.','.$line.','.$override_options;
        $line =~ s/(^\,+\s*)|(\s*\,+$)//;
        $line =~ s/\s*\,+\s*/,/g;
        if (empty($::asread_variable[$masread]{"options"})) {
          $::asread_variable[$masread]{"options"} = $line;
        } else {
          $::asread_variable[$masread]{"options"} = $::asread_variable[$masread]{"options"}.",".$line;
        }
        print ::DEBUG "INFO: adding options $line to: name = $name: masread = $masread: options = $::asread_variable[$masread]{options}\n";
  # now clean up by removing any leading, repeated or trailing commas
        $line = ''; # nothing is now left in the line
      }
    }
#-------

#-----------------------
# now processing user regions too, in much the same way as the user variables
# ref: REGION

  } elsif ( $line =~ /^((FACE|CELL|NODE)_|)((STATIC|SETUP|GMSH|CONSTANT|TRANSIENT|NEWTIENT|DERIVED|EQUATION|OUTPUT|UNKNOWN)_|)REGION($|\s)/i ) {
    $line = $';
    my $centring = ""; # now centring can be grabbed from last definition
    if ($2) { $centring = "\L$2"; }
    my $type = ""; # as can type
    if ($4) { $type = "\L$4";}
    if ($type eq "static") {$type="setup";} # setup is the name used in the fortran and perl to denote a user region that is not dynamic, but for the end user, static is easier to comprehend (for the fortran and perl static means !dynamic, which ewals setup, gmsh and system types)

# grab region name
    my $name;
    if ($line =~ /^\s*(<.+?>)($|\s)/) { $name = $1; $line = $'; }
    else { syntax_problem("problem reading in the region name from the following line: $filelinelocator");}
    print ::DEBUG "INFO: found user region in input file: name = $name: centring = $centring: type = $type\n";
    $name = examine_name($name,"regionname");
    print ::DEBUG "  coverting user defined name to consistent name = $name\n";

# see if this name has already been defined, and if so, find its index
    my $masread = find_region($name);
    if ($masread >= 0 && ( $::region[$masread]{"type"} eq "system" || $::region[$masread]{"type"} eq "internal" ) ) {
      syntax_problem("an attempt is being made to define a region which has a name ($name) that is reserved for system or internal region, in the following: $filelinelocator");
    }

#       $masread = -1; # variable masread starts at 0 if any variables are defined
#       foreach $mcheck ( 0 .. $#region ) {
#         if ($::region[$mcheck]{"name"} eq $name) { # region has been previously defined, and position in file is based on first definition
#           $masread = $mcheck;
#           last;
#         }
#       }

#-------
# check for CANCEL keyword
    if ($line =~ /^(\s*)CANCEL(\s|$)/) {
      if ($masread >= 0) {
        print "INFO: cancelling region $name\n";
        print ::DEBUG "INFO: cancelling region $name\n";
        splice(@::region, $masread, 1)
      } else {
        print "WARNING: attempting to cancel region $name that hasn't been defined yet - CANCEL ignored\n";
        print ::DEBUG "WARNING: attempting to cancel region $name that hasn't been defined yet - CANCEL ignored\n";
      }

#-------
# now create or update region type and centring
    } else {
      if ($masread >= 0) {
        $::region[$masread]{"redefinitions"}++; 
        print "INFO: a secondary definition statement (number $::region[$masread]{definitions}) for region $name has been found in file = $file\n";
        print ::DEBUG "INFO: a secondary definition statement (number $::region[$masread]{definitions}) for region $name has been found based on: $filelinelocator\n";
  # a variable has been identified, now check whether the centring has changed
        if (nonempty($centring)) {
          if ($centring ne $::region[$masread]{"centring"} && nonempty($::region[$masread]{"centring"})) {
            print "NOTE: changing the centring of region $name from $::region[$masread]{centring} to $centring\n";
            print ::DEBUG "NOTE: changing the centring of region $name from $::region[$masread]{centring} to $centring based on: $filelinelocator\n";
          }
          $::region[$masread]{"centring"} = $centring;
        } else {
          $centring = $::region[$masread]{"centring"};
        }
  # same with type
        if (nonempty($type)) {
          if ($type ne $::region[$masread]{"type"} && nonempty($::region[$masread]{"type"})) {
            print "NOTE: changing the type of region $name from $::region[$masread]{type} to $type\n";
            print ::DEBUG "NOTE: changing the type of region $name from $::region[$masread]{type} to $type based on: $filelinelocator\n";
          }
          $::region[$masread]{"type"} = $type;
        } else {
          $type = $::region[$masread]{"type"};
        }
        $::region[$masread]{"comments"}=$::region[$masread]{"comments"}." ".$comments;
        $::region[$masread]{"filename"}=$::region[$masread]{"filename"}." ".$file;
        $::region[$masread]{"absfilename"}=$::region[$masread]{"absfilename"}." ".$code_blocks[$#code_blocks]{"abs_name"};
      } else {
        print "INFO: a primary definition statement for region $name has been found in file = $file\n";
        print ::DEBUG "INFO: a primary definition statement for region $name has been found based on: $filelinelocator\n";
  # otherwise create a new region
        $masread=$#::region+1;
        print ::DEBUG "INFO: creating new region number $masread with name $name based on \n:file = $file: line = $oline\n";
  # and set basic info, empty if necessary
        $::region[$masread]{"name"}=$name;
        $::region[$masread]{"centring"}=$centring; # maybe blank
        $::region[$masread]{"type"}=$type; # maybe blank
        $::region[$masread]{"comments"}=$comments;
        $::region[$masread]{"redefinitions"}=0;
        $::region[$masread]{"part_of"}='';
        $::region[$masread]{"options"}='';
        $::region[$masread]{"location"}{"description"}='';
        $::region[$masread]{"initial_location"}{"description"}='';
        $::region[$masread]{"last_variable_masread"}=$#::asread_variable; # this determines when a region will be evaluated, for dynamic regions - it will be -1 if no variables are defined yet
        $::region[$masread]{"filename"}=$file;
        $::region[$masread]{"absfilename"}=$code_blocks[$#code_blocks]{"abs_name"};
      }

  # extract the location string, and if two are present, also an initial_location string (to be used for transient and newtient dynamic regions)
      if ( $line =~ /^\s*["']/ ) {
        my ($tmp1,$error) = extract_first($line);
        if ($error) { error_stop("some type of syntax problem with a location string in the following region definition: $filelinelocator"); }
        if (nonempty($::region[$masread]{"location"}{"description"})) {
          print "NOTE: changing the location of region $name\n";
          print ::DEBUG "NOTE: changing the location of region $name\n";
        }
        if ( $line =~ /^\s*["']/ ) {
          my ($tmp2,$error) = extract_first($line);
          if ($error) { error_stop("some type of syntax problem with a location string in the following region definition: $filelinelocator"); }
          if (nonempty($::region[$masread]{"initial_location"}{"description"})) {
            print "NOTE: changing the initial_location of region $name\n";
            print ::DEBUG "NOTE: changing the initial_location of region $name\n";
          }
          $::region[$masread]{"location"}{"description"} = $tmp2;
          $::region[$masread]{"initial_location"}{"description"} = $tmp1;
          print ::DEBUG "INFO: extracting region $name location and initial_location string from the following: $filelinelocator\n";
        } else {
          $::region[$masread]{"location"}{"description"} = $tmp1;
          print ::DEBUG "INFO: extracting region $name location string from the following: $filelinelocator\n";
        }
      }

  # ON keyword
      if ($line =~ /ON(\s+(<.+?>)($|\s+)|($|\s+))/i) {
        $line = $`.$';
        if ($2) {
          my $tmp = examine_name($2,'regionname'); # standardise name here
          if (nonempty($::region[$masread]{"part_of"})) {
            print "NOTE: changing the ON region for region $name to $tmp\n";
            print ::DEBUG "NOTE: changing the ON region for region $name to $tmp\n";
          }
          $::region[$masread]{"part_of"} = $tmp;
          print ::DEBUG "INFO: found ON region $tmp for region $name\n";
        }
        else { $::region[$masread]{"part_of"} = ''; print ::DEBUG "INFO: cancelling any possible ON region for region $name\n"; }
      }
        
  # region options
      $line =~ s/^\s*//; # remove any leading space from the line
      if (nonempty($line)) {
        $::region[$masread]{"options"} = $::region[$masread]{"options"}.",".$line;
        $::region[$masread]{"options"} =~ s/(^\,+\s*)|(\s*\,+$)//;
        $::region[$masread]{"options"} =~ s/\s*\,+\s*/,/g;
        print ::DEBUG "INFO: adding options to: region = $::region[$masread]{name}: masread = $masread: options = $::region[$masread]{options}\n";
        $line = ''; # nothing is now left in the line
      }

      print "INFO: region statement has been read: name = $name: number = $masread: centring = $::region[$masread]{centring}: ".
        "type = $::region[$masread]{type}: location = $::region[$masread]{location}{description}: ".
        "initial_location = $::region[$masread]{initial_location}{description}: part_of = $::region[$masread]{part_of}\n"; 
      print ::DEBUG "INFO: region statement has been read: name = $name: number = $masread: centring = $::region[$masread]{centring}: ".
        "type = $::region[$masread]{type}: location = $::region[$masread]{location}{description}: ".
        "initial_location = $::region[$masread]{initial_location}{description}: part_of = $::region[$masread]{part_of}\n"; 

#-------
    }

#-----------------------
  } else {
# finally if the line doesn't match any of the above, then stop - it may mean that something is not as intended
    syntax_problem("the following line in $file makes no sense: $filelinelocator");
  }

  $_[0] = $buffer;

} 
#-------------------------------------------------------------------------------
# examines buffer line and corrects any replace/with pairs, either in GENERAL_REPLACEMENTS or INCLUDE* statements
# from v0.57 onwards, string coding has changed considerably
# these next two subroutines utilise legacy coding to rewrite the older string replacement syntax with the new
sub correct_deprecated_string_replacement_code {
  my $input = $_[0];
  my $oinput = $input; # save input string for messages
  my $buffer = '';
  my $in_string_code = 0; # 0 means no string code, 1 means the next statement could be string code but the last wasn't, and 2 means the next could be and the last was
  my $file = 'fixme';
  my $oline = 'fixme';
  my $warn = 0;
  my $global = 0; # indicates that we are doing a general replacement

# look for keywords that could indicate potential string code
# roughly in line with legacy language, only <<>> delimited replacement strings could appear before these statements
# this should match any INCLUDE statement that could possibly preceed a deprecated replace/with pair
  if ($input =~ /^\s*(<<.*>>|)\s*(INCLUDE(|_[A-Z]+))\s+\S+\s*/i || # space delimited filename
      $input =~ /^\s*(<<.*>>|)\s*(INCLUDE(|_[A-Z]+))\s*".*?"\s*/i || # " delimited filename
      $input =~ /^\s*(<<.*>>|)\s*(INCLUDE(|_[A-Z]+))\s*'.*?'\s*/i ) { # ' delimited filename
#   print ::DEBUG "INFO: in correct_deprecated_string_replacement_code: if 1\n";
    $buffer .= $&;
    $input = $';
    $in_string_code = 1;

# this should match any REPLACEMENTS statement that preceeds a deprecated replace/with pair
  } elsif ($input =~ /^(\s*(<<.*>>|)\s*)((GENERAL_|)REPLACEMENTS)(\s*)/i ) { # GENERAL_REPLACEMENTS type keywords
#   print ::DEBUG "INFO: in correct_deprecated_string_replacement_code: if 2\n";
    $buffer .= $1.$5;
    $input = $';
    $in_string_code = 1;
    $global = 1;

  }

  if ($in_string_code) {
    while (nonempty($input) || $in_string_code) {

      print ::DEBUG "INFO: in correct_deprecated_string_replacement_code loop with: input = $input: buffer = $buffer: in_string_code = $in_string_code\n";
      if ($in_string_code) {
#       print ::DEBUG "INFO: in correct_deprecated_string_replacement_code: if 3\n";
        my ($search,$replace,$cancel,$default) = extract_deprecated_replacements($input);
#       if ($cancel) { die "cancel not done yet\n"; }
#       if ($default) { die "default not done yet\n"; }
        if (nonempty($search)) {
          if ($in_string_code == 1) {
            $buffer .= '{{ ';
            $in_string_code = 2;
            $warn = 1;
          } else { # $in_string_code == 2
            $buffer .= '; ';
          }
          if ($cancel) {
            $buffer .= "cancel(\'$search\') "; # add this cancel operation
          } else {
            $buffer .= "\'$search\' ="."g" x $global."d" x $default." \'$replace\' "; # add this found search/replace pair
          }
        } else {
#         print ::DEBUG "INFO: in correct_deprecated_string_replacement_code: if 3.2\n";
          if ($in_string_code == 2) { $buffer .= '}} '; }
          $in_string_code = 0;
        }

      } else {
        print ::DEBUG "INFO: in correct_deprecated_string_replacement_code: if 4\n";
        $buffer .= $input;
        $input = '';
      }
    }

# tell the user about the change if there was one
    if ($warn) { syntax_problem("legacy string code has been found and replaced by newer string code at $filelinelocator",
      "legacy string code has been found and replaced by newer string code at $filelinelocator\n".
      "  legacy string code = $oinput\n  replaced string code = $buffer","warning"); }

    $_[0] = $buffer;
  } else {
    $_[0] = $oinput; # no strings found, line returned as original input
  }

}
#-------------------------------------------------------------------------------
sub extract_deprecated_replacements {
# extracts a search and replace pair from a deprecated string code line
# on input
# $_[0] = line of text
# exit with two strings and two flags
#  ( search, replace, cancel, default )
# $_[3] = cancel = 0,1, indicates whether the CANCEL "string" was used
# $_[4] = default = 0,1, indicates whether the REPLACE* or R* or DEFAULT "string" was used, which indicates that string replacement is only set if it isn't set already
# if search is empty then no string was found

  my $line = $_[0];
  my $error = 0;
  my $search = '';
  my $replace = '';
  my $cancel = 0;
  my $default = 0;

  if ($line =~ /^((R|REPLACE)|(R\*|REPLACE\*|DEFAULT|D))\s+/i) { # found a replacement
    print ::DEBUG "found a replace statement specified as $1: $'\n";
    if (nonempty($3)) { $default=1; }
    $line = $';
    ($search,$error) = extract_first($line);
    if (empty($search) || $error) {
      syntax_problem("one search string not identified from: $filelinelocator");
    } else {
      $line =~s/^(W|WITH)\s+//i;
      ($replace,$error) = extract_first($line);
      if ($error) { syntax_problem("replacement string corresponding to search string $search not identified from: $filelinelocator"); }
    }
    print ::DEBUG "search = $search: replace = $replace: default = $default\n";
  } elsif ($line =~ /^(C|CANCEL)\s+/i) { # found a string to cancel
    print ::DEBUG "found a cancel statement: $'\n";
    $line = $';
    ($search,$error) = extract_first($line);
    if (!($search) || $error) {
      syntax_problem("one cancel string not identified from: $filelinelocator");
    } else {
      $cancel = 1;
    }
  }

  $_[0] = $line;
  return ($search,$replace,$cancel,$default);

}

#-------------------------------------------------------------------------------
# adds a code block to the top of the code_blocks array
# if a name and actual name are provided, assumes this is a file and opens that too (new_file = 1)
# otherwise assumes that block is in the same file (new_file = 0)
# 
# on input
# _[0] = name of file from include statement (include_name)|undef
# _[1] = actual name of file as found and to be opened|undef
sub push_code_block {

  use FileHandle;
  use File::Spec; # for rel2abs

  my ($include_name,$name) = @_;

# add new array element, starting with flag that indicates whether this file needs to be opened, or already is
  if (defined($name)) {
    $code_blocks[$#code_blocks+1]{"new_file"}=1;
    $code_blocks[$#code_blocks]{"include_name"}=$include_name;
    $code_blocks[$#code_blocks]{"name"}=$name;
    $code_blocks[$#code_blocks]{"abs_name"} = File::Spec->rel2abs($name); # abs_name is always the absolute path to the file
    $code_blocks[$#code_blocks]{"handle"} = FileHandle->new(); # make a new filehandle for the file (taken from http://docstore.mik.ua/orelly/perl/cookbook/ch07_17.htm)

# open file
    my $handle = $code_blocks[$#code_blocks]{"handle"};
    open($handle, "<$code_blocks[$#code_blocks]{name}") or error_stop("problem opening arb input file $code_blocks[$#code_blocks]{name}");

  } else {
    if ($#code_blocks) { error_stop("internal problem with push_code_block"); }
    $code_blocks[$#code_blocks+1]{"new_file"}=0;
    $code_blocks[$#code_blocks]{"include_name"}=$code_blocks[$#code_blocks-1]{"include_name"};
    $code_blocks[$#code_blocks]{"name"}=$code_blocks[$#code_blocks-1]{"name"};
    $code_blocks[$#code_blocks]{"abs_name"}=$code_blocks[$#code_blocks-1]{"abs_name"};
    $code_blocks[$#code_blocks]{"handle"}=$code_blocks[$#code_blocks-1]{"handle"};
  }

  if ($#code_blocks) {
# set to last include_path of previous code_block, which is where calling file must be
    $code_blocks[$#code_blocks]{"include_path"}[0] = $code_blocks[$#code_blocks-1]{"include_path"}[$#{$code_blocks[$#code_blocks-1]{"include_path"}}];
  } else {
# initial include_path used for searching for the file is the working directory
# so, always the initial search path for files is the working_dir (even though root_input.arb sits in the build directory)
# drag this value out of main
    $code_blocks[$#code_blocks]{"include_path"}[0] = $::working_dir;
  }

  $code_blocks[$#code_blocks]{"buffer"} = ''; # buffer created from raw_buffer
  $code_blocks[$#code_blocks]{"skip"} = 0; # flag to indicate whether we are in a comments section or not

  print UNWRAPPED_INPUT $unwrapped_indent x $#code_blocks,"#(comment generated during unwrap)++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n",
    $unwrapped_indent x $#code_blocks,"#(comment generated during unwrap): new block = $code_blocks[$#code_blocks]{include_name}: new_file = $code_blocks[$#code_blocks]{new_file}\n";

# ref: FILENAME
# set simulation_info filename based on the first included file (which is the one that will be listed in root_input.arb)
  if (empty($::simulation_info{"filename"}) && $#code_blocks) {
    $::simulation_info{"filename"} = $code_blocks[$#code_blocks]{"include_name"};
    $::simulation_info{"absfilename"} = $code_blocks[$#code_blocks]{"abs_name"};
  }

}

#-------------------------------------------------------------------------------
# removes a code block from the top of the code_blocks array
# 
sub pop_code_block {

  print UNWRAPPED_INPUT $unwrapped_indent x $#code_blocks,"#(comment generated during unwrap)--------------------------------------------------------\n";

  if ($code_blocks[$#code_blocks]{"new_file"}) {
    my $handle = $code_blocks[$#code_blocks]{"handle"};
    close($handle);
    print ::DEBUG "INFO: closing file $code_blocks[$#code_blocks]{name}\n";
  }

  pop(@code_blocks);
}

#-------------------------------------------------------------------------------

# do file-specific replacements on file before anything else
# foreach my $n1 ( reverse( 0 .. $#code_blocks ) ) {
#   foreach my $n2 ( 0 .. $#{$code_blocks[$n1]{"replacements"}} ) {
#     replace_substrings($oline,$code_blocks[$n1]{"replacements"}[$n2]{"search"},$code_blocks[$n1]{"replacements"}[$n2]{"replace"});
#   }
# }
# foreach my $n1 ( reverse( 0 .. $#string_variables ) ) {
#   replace_substrings($oline,$string_variables[$n1]{"search"},$string_variables[$n1]{"replace"});
# }

1;
