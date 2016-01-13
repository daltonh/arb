#!/usr/bin/env perl
# file src/setup_equations.pl
#
# Copyright 2009-2014 Dalton Harvie (daltonh@unimelb.edu.au)
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
# perl script to create equation f90 module for arb

use strict;
use warnings;
use File::Basename;
#use File::Path qw(make_path);
use File::Path qw(mkpath); # for File::Path version < 2.08, http://perldoc.perl.org/File/Path.html
use File::Copy qw(move copy);
use File::Glob ':glob'; # deals with whitespace better
#use Time::Piece; # removed for portability
use Sys::Hostname;

my $version="0.54";
my $minimum_version="0.40";

# now called from build directory
my $working_dir="..";
my $src_dir="$working_dir/src";
my $build_dir=".";
my $template_dir="$working_dir/templates";
my $tmp_dir="$working_dir/tmp/setup";
my $variable_list_file = "$tmp_dir/variable_list.txt";
my $variable_arb_file = "$tmp_dir/variable_list.arb";
my $region_file = "$tmp_dir/region_list.txt";
my $tmp_file_number=0;
our $maxima_bin='maxima'; # use this if the maxima executable is in your path
#our $maxima_bin='/sw/bin/maxima'; # if all else fails specify the actual location - fink
#our $maxima_bin='/usr/local/bin/maxima'; # if all else fails specify the actual location
#our $maxima_bin='../misc/maxima_OsX/maxima'; # or the supplied script for the OsX binary, relative to the build directory
my $fortran_input_file="$build_dir/fortran_input.arb"; # input file for the fortran executable, which is in a slightly different format to the user written input files
my $current_unwrapped_input_file="$tmp_dir/current_unwrapped_input.arb"; # this is an unwrapped version of the user input file, which can be used for debugging, or alternatively used directly for future runs
my $unwrapped_input_file="$tmp_dir/unwrapped_input.arb"; # this is an unwrapped version of the user input file, which can be used for debugging, or alternatively used directly for future runs
my $current_debug_info_file="$tmp_dir/current_debugging_info.txt"; # this is where all the debugging info is dumped from this current call to setup_equations.pl
my $debug_info_file="$tmp_dir/debugging_info.txt"; # this is where all the debugging info is stored from the last setup
my $version_file="$working_dir/licence/version";

# create and clear out tmp directory of any files
#if (! -d $tmp_dir) { make_path($tmp_dir) or die "ERROR: could not create $tmp_dir\n"; }
if (! -d $tmp_dir) { mkpath($tmp_dir) or die "ERROR: could not create $tmp_dir\n"; } # for File::Path version < 2.08, http://perldoc.perl.org/File/Path.html
my $filename;
foreach $filename (bsd_glob("$tmp_dir/*")) {
  if ($filename eq $variable_arb_file || $filename eq $variable_list_file || $filename eq $region_file || $filename eq $debug_info_file || $filename eq $unwrapped_input_file) { next; } # don't delete the files which are a record of the previous setup run
  if (-f $filename) {unlink($filename) or die "ERROR: could not remove $filename in directory $tmp_dir\n";}
}

# remove stopfile from previous run if it exists
my $stopfile="$working_dir/stop";
if (-e $stopfile) { unlink($stopfile) or die "ERROR: could not remove $stopfile from previous run\n"; }

open(DEBUG, ">$current_debug_info_file");
print "\nperl setup_equations script to create f90 subroutines for arb\n";
print "dalton harvie, v$version\n\n";
print DEBUG "\nperl setup_equations script to create f90 subroutines for arb\n";
print DEBUG "dalton harvie, v$version\n\n";

# this will become a stack of recursively called arb input files, starting with the root_input.arb file created by the arb script that contains INPUT_WORKING links to the arb files called by the user from the arb script
my @input_files;
$input_files[0]{"ref_name"}="root_input.arb";
$input_files[0]{"name"}="$build_dir/root_input.arb";

# user_types are variables that can be defined by the user
my @user_types = ("constant","transient","newtient","unknown","derived","equation","output","condition","local");

my %m=(); # is a hash of the maximum number of variables of each type
my $type;
foreach $type (@user_types,"initial_transient","initial_newtient","someloop","system","empty","compound","user") {
  $m{$type}=0;
}

my %variable=(); # is a hash/array/hash of all the variables with associated data of each type and number and datatype
my @asread_variable = (); # this is a list of variables in the order that they are read from the file - new for v0.51 to allow change of variable type

my @region_link=(); # is an array/hash of all the region links that need to calculated at run time

my %sub_string=(); # is a hash of strings that we need to assemble for substitution in the equations.f90 routine
my %operator_contents=(); # this hash will contain the contents of each operator as read in, with keys as the name of the operator, global variable used in mequation_interpolation and subs

my %basename;
foreach $type ( @user_types ) {
  $basename{$type}="var";
}
$basename{"compound"}="compound";
$basename{"someloop"}="someloop";
$basename{"system"}="sys";

my $mseparation_list = 0; # total number of separation_lists required (ie, number of separation loops being used)
my $lengthunit="m"; # do this better in the future

my $transient=0; # this indicates whether the simulation is transient or not
my $newtient=0; # this indicates whether the simulation is newtient or not (ie, uses newtient variables which are evaluated outside of the newton loop)
my $number_of_lousysubstitutes = 0; # number of lousy substitutions that had to be performed (unwrapping derived and equations)

my $reuse_maxima_results = 1; # set to 1 to reuse these - faster
my @maxima_simplify_results = (); # this will be an array of all maxima simplification results, for reuse purposes
my @maxima_fortran_results = (); # this will be an array of all maxima fortran results, for reuse purposes

# setup the simulation info, including grabbing the current code runversion, rundate and runhost
my %simulation_info = ( "title" => '', "description" => '', "filename" => '', "author" => '', "date" => '', "version" => ''); # this hash will store some general info about the simulation
$simulation_info{"runversion"}='unknown';
if (-e $version_file) {
  open(VERSION_FILE,"<$version_file") or die "could not open $version_file\n";
  $simulation_info{"runversion"}=<VERSION_FILE>;
  chompm($simulation_info{"runversion"});
  close(VERSION_FILE);
}
#$simulation_info{"rundate"} = Time::Piece->new; 
$simulation_info{"rundate"} = localtime();
$simulation_info{"runhost"} = hostname;

# now create an array of hashes, each for a different fortran external file
my @externals=();
# and a hash of external operators contained within these files
my %external_operators=();

# the replacement strings
my @general_replacements = ();

my %statement_repeats = ( 'definitions' => 0, 'typechanges' => 0, 'centringchanges' => 0 );

# kernels are now only calculated when needed
# these settings can be overwritten (only making true, not turning off) in general_module.f90
my %kernel_availability = ( 'cellave' => 0, 'cellgrad' => 0, 'cellfromnodeave' => 0, 'cellfromnodegrad' => 0, 'faceave' => 0, 'facegrad' => 0, 'nodeave' => 0, 'nodegrad' => 0 );

my @region = (); # list of regions
my $fortran_regions = 0; # number (ie, highest fortran index) of the regions that need fortran allocations
#--------------------------------------------------------------
# read through setup files storing all the information

create_system_variables(); # create variables generated by the system

setup_general_replacements(); # create the default general replacements

open(FORTRAN_INPUT, ">$fortran_input_file"); # open input file for the fortran executable, used in the next two subroutines

read_input_files(); # read input files, generating user defined variables

organise_user_variables(); # create variable structure from raw asread_variable array

close(FORTRAN_INPUT);

organise_regions(); # initialise all of the regions not already read in, organise them, and check on regions used by variables

create_compounds(); # create vector and tensor allocations

dump_variable_setup_info(); # output all variable information into the debug file

#write_latex(); # produce a latexable summary file - NEEDS UPDATING!

check_variable_status(); # dumps variable list to file (removing options and comments) and checks whether we need to proceed with equation construction

read_maxima_results_files(); # read in any old maxima results stored in files from previous runs

create_mequations(); # create maxima type equations

process_regions(); # finalise all of the regions, checking any that haven't been included as variable regions, and create fortran allocations etc for them

create_allocations();  # create allocation statements

create_fortran_equations(); # generate fortran equations

write_sub_strings(); # write out sub_strings and creating new fortran files

write_maxima_results_files(); # write out maxima results for subsequent runs

dump_variable_dependency_info(); # dump all variable dependency info into the debug file, and also dump the raw variable arrays for possible post-processing

output_variable_list(); # dump info about variables in a single file

output_region_list(); # dump info about regions in a single file

# final warning about multiple definitions
if ($statement_repeats{"definitions"} > 1) { print "NOTE: at least one variable was defined multiple times.  Was this your intention?  Details are given above (or in the file $debug_info_file).\n"; }
if ($statement_repeats{"typechanges"} > 1) { print "NOTE: at least one variable had its type changed.  Was this your intention?  Details are given above (or in the file $debug_info_file).\n"; }
if ($statement_repeats{"centringchanges"} > 1) { print "NOTE: at least one variable had its centring changed.  Was this your intention?  Details are given above (or in the file $debug_info_file).\n"; }
if ($number_of_lousysubstitutes > 0) { print "WARNING: some variable substitutions had to be performed that may result in very inefficient code being generated.  These lousy substitutions ".
  "could be avoided by a careful reordering of some of the input statements.  Details are given above (or in the file $debug_info_file by searching for 'lousy substitution').\n"; }
print "SUCCESS: equation_module.f90 has been created\n";
print DEBUG "SUCCESS: equation_module.f90 has been created\n";

# finally move equation data to build directory as a record of the successful run
print DEBUG "INFO: moving setup_equation_data to build directory\n";
move("$tmp_dir/setup_equation_data","$build_dir/last_setup_equation_data") or die "could not move $tmp_dir/setup_equation_data to $build_dir/last_setup_equation_data\n";
# and also keep a copy of the debugging and unwrapped file which acts as a record from the last successful setup
copy("$current_debug_info_file","$debug_info_file") or  die "could not save $current_debug_info_file as $debug_info_file\n";
copy("$current_unwrapped_input_file","$unwrapped_input_file") or  die "could not save $current_unwrapped_input_file as $unwrapped_input_file\n";

close(DEBUG);

exit;

#-------------------------------------------------------------------------------
# dump all of the info about the variables that was read in for debugging purposes

sub dump_variable_setup_info {

  use strict;
  use Data::Dumper;
  my ($type, $mvar, $n);

  $Data::Dumper::Terse = 1;
  $Data::Dumper::Indent = 0;
# $Data::Dumper::Pad = "  ";
  print DEBUG "=================================================================\n";
  print DEBUG "SUMMARY OF ALL VARIABLES AND COMPOUNDS BEFORE EQUATION PROCESSING\n";
  foreach $type (sort(keys %variable)) {
    foreach $mvar ( 1 .. $m{$type} ) {
# multiline
#     print DEBUG "-----------------------------------------------------------------------------------------------\nTYPE = $type: MVAR = $mvar:\n";
#     print DEBUG Dumper($variable{$type}[$mvar])."\n";
# single line
      print DEBUG "$type $mvar $variable{$type}[$mvar]{name}: ".Dumper($variable{$type}[$mvar])."\n";
    }
  }
  print DEBUG "=================================================================\n";

}
#-------------------------------------------------------------------------------
# dump all of the dependency info about the variables to the debug file, and also to a raw data file for post-processing

sub dump_variable_dependency_info {

  use strict;
  use Data::Dumper;
  use Storable qw(freeze store); # routine for collapsing data structures into a single string
  my ($type, $mvar, $n, $dtype, $dmvar, $stype, $smvar, $deriv);

  $Data::Dumper::Terse = 1;
  $Data::Dumper::Indent = 0;
# $Data::Dumper::Pad = "  ";
  print DEBUG "=================================================================\n";
  print DEBUG "SUMMARY OF DEPENDENCY INFORMATION FOR ALL EQUATION-BASED VARIABLES\n";
  foreach $type ( @user_types,"initial_newtient","initial_transient","someloop" ) {
    foreach $mvar ( 1 .. $m{$type} ) {
      print DEBUG "$type $mvar $variable{$type}[$mvar]{name}\n";
      foreach $n ( 0 .. $#{$variable{$type}[$mvar]{"dependency"}} ) {
        $dtype = $variable{$type}[$mvar]{"dependency"}[$n]{"type"};
        $dmvar = $variable{$type}[$mvar]{"dependency"}[$n]{"mvar"};
        $deriv = $variable{$type}[$mvar]{"dependency"}[$n]{"deriv"};
        print DEBUG "  primary dependency = $dtype $dmvar $variable{$dtype}[$dmvar]{name}: deriv = $deriv";
        if (nonempty($variable{$type}[$mvar]{"dependency"}[$n]{"substituted_type"})) {
          $stype = $variable{$type}[$mvar]{"dependency"}[$n]{"substituted_type"};
          $smvar = $variable{$type}[$mvar]{"dependency"}[$n]{"substituted_mvar"};
          print DEBUG ": substituted dependency = $stype $smvar $variable{$stype}[$smvar]{name}";
#         print DEBUG ": substituted dependency = $stype $smvar";
        }
        print DEBUG "\n";
      }
# single line
#     print DEBUG "$type $mvar $variable{$type}[$mvar]{name}: ".Dumper($variable{$type}[$mvar])."\n";
    }
  }
  print DEBUG "=================================================================\n";

# also dump all data to last_setup_dependency_data file for latex post-processing
# my $new_data = freeze( \%variable ); # collapse variable hash
# open(DUMP_DATA, ">$build_dir/last_setup_dependency_data") or die "ERROR: problem opening $build_dir/last_setup_dependency_data\n";
# print DUMP_DATA $new_data;
# close(DUMP_DATA);

  if (nonempty(eval{store(\%variable, "$build_dir/last_setup_dependency_data")})) {
    print "INFO: written last_setup_dependency_data to file for possible reuse\n";
  } else {
    print "WARNING: unable to write last_setup_dependency_data to file\n";
  }
  
}

#-------------------------------------------------------------------------------
# dump the variables as an ordered list, and now also as a list in the arb format

sub output_variable_list {

  use strict;
  open(VARIABLE, ">$variable_list_file") or die "ERROR: problem opening temporary variable file $variable_list_file: something funny is going on: check permissions??\n";
  print VARIABLE "# List of the variables:\n";
  for my $key ( keys(%variable) ) {
    print VARIABLE "-" x 80,"\n";
    print VARIABLE "List of $key variables:\n";
    for my $mvar ( 1 .. $#{$variable{$key}} ) {
      print VARIABLE "$mvar";
        for my $infokey ( qw( name units centring region rank fortran_number component_list equation masread hasderiv deriv newtstepmax newtstepmin )) {
          print VARIABLE ": $infokey = ";
          if (empty($variable{$key}[$mvar]{$infokey})) {
            print VARIABLE "empty"
          } else {
            print VARIABLE "$variable{$key}[$mvar]{$infokey}";
          }
        }
      print VARIABLE "\n";
    }
  }
  print VARIABLE "-" x 80,"\n";
  close(VARIABLE);

  open(VARIABLE, ">$variable_arb_file") or die "ERROR: problem opening temporary variable file $variable_arb_file: something funny is going on: check permissions??\n";
  print VARIABLE "# Reconstructed list of the variables in arb format:\n";
  for my $key ( @user_types ) {
    print VARIABLE "#","-" x 80,"\n";
    print VARIABLE "# $key variables:\n";
    for my $mvar ( 1 .. $#{$variable{$key}} ) {
      print VARIABLE "\U$variable{$key}[$mvar]{centring}_$key "."$variable{$key}[$mvar]{name} [$variable{$key}[$mvar]{units}]";
      if ($key =~ /ient$/) { print VARIABLE " \"".$variable{"initial_$key"}[$mvar]{equation}."\""; }
      if ($key eq "constant" && !($variable{$key}[$mvar]{equation})) { print VARIABLE " \"numerical constant rather than an equation\"" } else { print VARIABLE " \"$variable{$key}[$mvar]{equation}\"" };
      if ($variable{$key}[$mvar]{centring} ne "none") { print VARIABLE " ON $variable{$key}[$mvar]{region}"; }
      print VARIABLE " # other information:";
      for my $infokey ( qw( deriv newtstepmax newtstepmin comments )) {
        print VARIABLE ": $infokey = ";
        if (empty($variable{$key}[$mvar]{$infokey})) {
          print VARIABLE "empty"
        } else {
          print VARIABLE "$variable{$key}[$mvar]{$infokey}";
        }
      }
      print VARIABLE "\n";
    }
  }
  print VARIABLE "#","-" x 80,"\n";
  close(VARIABLE);

}
#-------------------------------------------------------------------------------
# dump the regions as an ordered list

sub output_region_list {

  use strict;
  use Data::Dumper;
  my @types=(qw( system setup gmsh constant transient newtient derived equation output condition )); # list of region types
  open(REGION, ">$region_file") or die "ERROR: problem opening temporary region file $region_file: something funny is going on: check permissions??\n";

  for my $type ( @types ) {
    print REGION "-" x 80,"\n";
    print REGION "List of $type regions:\n";
    for my $n ( 0 .. $#region ) {
      if ($region[$n]{"type"} ne $type) { next; }
      print REGION "$n";
      for my $key (qw(name type centring user dynamic part_of parent rindex fortran part_of_fortran parent_fortran last_variable_masread definitions location initial_location options newtstepmax newtstepmin)) {
        print REGION ": $key = ";
        if (empty($region[$n]{$key})) {
          print REGION "empty";
        } elsif ($key eq "location" || $key eq "initial_location") {
          print REGION Dumper($region[$n]{$key});
        } else {
          print REGION "$region[$n]{$key}";
        }
      }
      print REGION "\n";
    }
  }
  print REGION "-" x 80,"\n";
  close(REGION);

}
#-------------------------------------------------------------------------------
# run through each variable checking whether anything has changed that requires the script to be rerun

sub check_variable_status {

  use strict;
  use Storable qw(freeze); # routines for collapsing data structures into a single string
  my $same=1; # flag to indicate whether this run and last are the same
  my ($type, $mvar, $n);

# first remove options, comments and numerical constants from variable structure, which are not used in the perl
  foreach $type (sort(keys %variable)) {
    foreach $mvar ( 1 .. $m{$type} ) {
      delete $variable{$type}[$mvar]{"options"}; # delete removes value and key for a hash
      delete $variable{$type}[$mvar]{"comments"}; # delete removes value and key for a hash
      delete $variable{$type}[$mvar]{"constant_list"}; # delete removes value and key for a hash
      delete $variable{$type}[$mvar]{"definitions"}; # delete removes value and key for a hash
    }
  }

# also do similar for regions
  foreach $n ( 1 .. $#region ) {
    delete $region[$n]{"options"}; # delete removes value and key for a hash
    delete $region[$n]{"comments"}; # delete removes value and key for a hash
    delete $region[$n]{"definitions"}; # delete removes value and key for a hash
  }

  $Storable::canonical=1; # the data structure will have its keys sorted before being created

# my $new_data = freeze( \%variable ); # collapse variable hash and now externals array
# tack any single variables on the end
# $new_data = "$new_data\n$transient\n$newtient";
  my $new_data = freeze( \%variable)."\n".freeze( \@externals )."\n".freeze( \@region )."\n$transient\n$newtient";
# my $new_data = '';

# open old file and compare string created last time
  open(OLD_CHECK, "<$build_dir/last_setup_equation_data") or $same=0; # if file doesn't exist
  if ($same) {
    my $old_data = "";
    while (<OLD_CHECK>) {
      $old_data = $old_data.$_; # read file into a single string
    }
    if ($new_data ne $old_data) { $same=0; }
#   print DEBUG "old_data = $old_data\n";
#   print DEBUG "new_data = $new_data\n";
  }

  close(OLD_CHECK);
      
# write out new data string for comparison during the next time
  open(NEW_CHECK, ">$tmp_dir/setup_equation_data") or die "ERROR: problem opening $tmp_dir/setup_equation_data\n";
  print NEW_CHECK "$new_data";
  close(NEW_CHECK);

  if ($same) {
    print "INFO: setup_equations data has not changed since the last run: exiting immediately\n";
    print DEBUG "INFO: setup_equations data not has changed since the last run: exiting immediately\n";
# final warning about multiple statements
    if ($statement_repeats{"definitions"} > 1) { print "NOTE: at least one variable was defined multiple times.  Was this your intention?  Details are given above (or in the file $debug_info_file).\n"; }
    if ($statement_repeats{"typechanges"} > 1) { print "NOTE: at least one variable had its type changed.  Was this your intention?  Details are given above (or in the file $debug_info_file).\n"; }
    if ($statement_repeats{"centringchanges"} > 1) { print "NOTE: at least one variable had its centring changed.  Was this your intention?  Details are given above (or in the file $debug_info_file).\n"; }
    print "SUCCESS: equation_module.f90 was already up-to-date\n";
    print DEBUG "SUCCESS: equation_module.f90 was already up-to-date\n";
    exit;
  } else {
    print "INFO: setup_equations data has changed since the last run: recreating fortran\n";
    print DEBUG "INFO: setup_equations data has changed since the last run: recreating fortran\n";
  }

}

#-------------------------------------------------------------------------------
# now write out sub_strings to new fortran files

sub write_sub_strings {

  use strict;
  use File::Glob ':glob'; # deals with whitespace better
  my @f90_files=bsd_glob("$src_dir/*_template.f90");
  my ($keyword, $indent, $line, $string, @strings, $n, $key);
  my $linelength = 100;

# set transient and newtient substrings based on the simulation type
  $sub_string{"transient_simulation"} = "transient_simulation = ".fortran_logical_string($transient);
  $sub_string{"newtient_simulation"} = "newtient_simulation = ".fortran_logical_string($newtient);

# setup kernel_availability string
	print DEBUG "KERNEL_AVAILABILITY as calculated:\n";
	print "INFO: calculated kernel_availability:\n";
	$sub_string{"kernel_availability"} = '';
	foreach $key (keys(%kernel_availability)) {
		if ($kernel_availability{$key}) {
   	  $sub_string{"kernel_availability"} = $sub_string{"kernel_availability"}."kernel_availability_".$key." = .true.\n";
		}
#  	$sub_string{"kernel_availability"} = $sub_string{"kernel_availability"}.
# 		"if (.not.kernel_availability_$key) kernel_availability_".$key." = ".fortran_logical_string($kernel_availability{$key})."\n";
		print DEBUG "$key = $kernel_availability{$key}\n";
		print "  $key = $kernel_availability{$key}\n";
	}

  foreach my $in_file (@f90_files) {

    open(INFILE, "<$in_file");
    basename($in_file) =~ /_template\.f90$/;
    my $out_file=$build_dir."/$`.f90";

    open(OUTFILE, ">$out_file"); #overwrite any existing files
    print "INFO: writing from equation src file $in_file to output file $out_file\n";

    while ($line=<INFILE>) { chompm($line);
      if (($indent,$keyword) = $line =~ /^\!(\s*?)<sub_string:(.+)>/) { # substring lines are now commented out by default
        print "INFO: found sub_string marker in fortran file with keyword $keyword\n";
        if (empty($sub_string{$keyword})) {
          print "NOTE: sub_string corresponding to keyword $keyword not set\n";
        } else {
          @strings = split("\n",$sub_string{$keyword});
          $sub_string{$keyword} = "";
          foreach $string (@strings) {
            $string =~ s/^\s*//; # remove any space from front of string
            if (!($string =~ /^\!/) || $string =~ /^\!\$/) {
              if ($string =~ /^\s*end\s*?if/ || $string =~ /^\s*end\s*?do/) {substr($indent,-2,2,"");}  # decrease indent
              if ($string =~ /^\s*else/) {substr($indent,-2,2,"");}  # decrease indent
# split string every $linelength characters and put in continuation mark if not a comment
              $n=1;
              while (length($string) > $n*$linelength ) { # $#string should be the number of the last element of $string
                substr($string,$n*$linelength-1,0,"&\n  $indent&");  # cut string and put in &, CR with extra indent
                $n++;
              }
              $string =~ s/^/$indent/;
              if ($string =~ /^\s*if .+?then/ || $string =~ /^\s*do/ || $string =~ /^\s*\S+:\s*do/) {$indent = $indent.'  ';} # increase indent
              if ($string =~ /^\s*else/) {$indent = $indent.'  ';} # increase indent
            } 
            $sub_string{$keyword} = $sub_string{$keyword}.$string."\n" # comments are printed with no indent
          }
          
          $line =~ s/^\!\s*?<sub_string:$keyword>.*?$/$sub_string{$keyword}/;
        }
      } elsif ($line =~ /^\s*\!\s*<arb_external_(contents|setup|preamble)>/) { # externals to be included in equation_module.f90 are read in here
        $line = '';
        for my $external ( 0 .. $#externals ) {
          if ($externals[$external]{"used"}) {
            $line = $line."\n\n! The following $1 is included from fortran external file $externals[$external]{name}:\n".$externals[$external]{$1};
          }
        }
      }

      print OUTFILE "$line\n";
    }

    close(INFILE);
    close(OUTFILE);
  }
}

#--------------------------------------------------------------

sub setup_general_replacements {

  use strict;
# ref: general replacements
# setup default general_replacements
# loose convention is that replacement strings be delimited by <<>>, however any strings can (and will) be matched/replaced
# convention is that replacement names that end with "comment" are meant to preceed statements in the files, converting them to comments if they are not relevant
  %{$general_replacements[$#general_replacements+1]} = ( search => "<<dim1comment>>", replace => "" );
  %{$general_replacements[$#general_replacements+1]} = ( search => "<<dim2comment>>", replace => "" );
  %{$general_replacements[$#general_replacements+1]} = ( search => "<<dim3comment>>", replace => "" );
  %{$general_replacements[$#general_replacements+1]} = ( search => "<<steadystatecomment>>", replace => "" ); # default is steady-state
  %{$general_replacements[$#general_replacements+1]} = ( search => "<<transientcomment>>", replace => "#" );
  %{$general_replacements[$#general_replacements+1]} = ( search => "<<cartesiancomment>>", replace => "" ); # default is cartesian
  %{$general_replacements[$#general_replacements+1]} = ( search => "<<cylindricalcomment>>", replace => "#" );
# convention is that replacement names that end with "flag" are either on (1) or off (0), so can be used within expressions
  %{$general_replacements[$#general_replacements+1]} = ( search => "<<steadystateflag>>", replace => "1" ); # default is steady-state
  %{$general_replacements[$#general_replacements+1]} = ( search => "<<transientflag>>", replace => "0" );
  %{$general_replacements[$#general_replacements+1]} = ( search => "<<cartesianflag>>", replace => "1" ); # default is cartesian
  %{$general_replacements[$#general_replacements+1]} = ( search => "<<cylindricalflag>>", replace => "0" );
# these two should be overwritten by the relevant radius in the input file if using cylindrical coordinates: eg R "<<radius_c>>" W "<cellx[l=1]>" R "<<radius_f>>" W "<facex[l=1]>"
  %{$general_replacements[$#general_replacements+1]} = ( search => "<<radius_c>>", replace => "1.d0" );
  %{$general_replacements[$#general_replacements+1]} = ( search => "<<radius_f>>", replace => "1.d0" );
  %{$general_replacements[$#general_replacements+1]} = ( search => "<<radius_n>>", replace => "1.d0" );
  %{$general_replacements[$#general_replacements+1]} = ( search => "<<radiusdim1flag>>", replace => "0" ); # for 2D cylindrical coordinates, set the radius dimension flag to 1 to include (for example) the hoop stress in that dimension
  %{$general_replacements[$#general_replacements+1]} = ( search => "<<radiusdim2flag>>", replace => "0" );
  %{$general_replacements[$#general_replacements+1]} = ( search => "<<radiusdim3flag>>", replace => "0" );
  %{$general_replacements[$#general_replacements+1]} = ( search => "<<radialdim>>", replace => "0" ); # for 2D cylindrical this is the radial coordinate direction
  %{$general_replacements[$#general_replacements+1]} = ( search => "<<axialdim>>", replace => "0" ); # for 2D cylindrical this is the axial coordinate direction
# these strings should be overwritten by the normal coordinate directions of any reflection boundaries in the domain: eg R "<<reflect=1>>" W "reflect=1"
  %{$general_replacements[$#general_replacements+1]} = ( search => "<<reflect=1>>", replace => "" );
  %{$general_replacements[$#general_replacements+1]} = ( search => "<<reflect=2>>", replace => "" );
  %{$general_replacements[$#general_replacements+1]} = ( search => "<<reflect=3>>", replace => "" );

  print DEBUG "INFO: initial general_replacements = ".Dumper(@general_replacements)."\n";

}

#--------------------------------------------------------------
# read through constants.in and equations.in files storing all info

sub read_input_files {

  use strict;
  use FileHandle;
  use List::Util qw( min max );
  use Data::Dumper;
  use Storable qw(dclone);
  my ($file, $oline, $line, $type, $name, $cunits, $units, $multiplier, $mvar, $file_version,
    $mcheck, $typecheck, $tmp, $keyword, $centring, $otype, $match, $tmp1, $tmp2,
    $handle, $try_dir, $search, $replace, $working, $comments, $error, $region_constant,
    $condition, $key, $append, $cancel, $default, $masread);

  my %region_list = (); # contains the centring and REGION_LIST most recently specified in the input file (as used for REGION_CONSTANT)
  my $default_options = ""; # default options prepended to each statement read in
  my $override_options = ""; # override options appended to each statement read in
  my $skip = 0; # flag to indicate whether we are in comments section or not

# open unwrapped input file that will be used as a record only, and can be used for subsequent runs
  open(UNWRAPPED_INPUT, ">$current_unwrapped_input_file");

  $input_files[$#input_files]{"include_root"} = ''; # initial include_root is blank
  $input_files[$#input_files]{"handle"} = FileHandle->new(); # make a filehandle for the first file (taken from http://docstore.mik.ua/orelly/perl/cookbook/ch07_17.htm)
  $handle = $input_files[$#input_files]{"handle"};
  open($handle, "<$input_files[$#input_files]{name}") or die "ERROR: problem opening arb input file $input_files[$#input_files]{name}\n";;

  while (@input_files) {

# make sure that the handle corresponds to the last file on the input_files stack
    $handle = $input_files[$#input_files]{"handle"};
    $file = $input_files[$#input_files]{"ref_name"};

    while ($oline=<$handle>) { chompm($oline); if ($oline) { $oline=~s/^\s*// }; # remove linefeed from end and space from the start

# do file-specific replacements on file before anything else
# for v0.42 and onwards not doing replacements on any statements that define replacements (including include statements)
      if (!($oline =~ /^\s*((INCLUDE(|_ROOT|_WORKING))|((GENERAL_|)REPLACEMENTS))($|#|\s)/i)) {
        foreach my $n1 ( reverse( 0 .. $#input_files ) ) {
          foreach my $n2 ( 0 .. $#{$input_files[$n1]{"replacements"}} ) {
            replace_substrings($oline,$input_files[$n1]{"replacements"}[$n2]{"search"},$input_files[$n1]{"replacements"}[$n2]{"replace"});
          }
        }
      }
      
# now do general replacements, with latest defined replacements taking precedence
# don't do general replacements on general_replacement or include lines
      if (!($oline =~ /^\s*((INCLUDE(|_ROOT|_WORKING))|((GENERAL_|)REPLACEMENTS))($|#|\s)/i)) {
        foreach my $n1 ( reverse( 0 .. $#general_replacements ) ) {
          replace_substrings($oline,$general_replacements[$n1]{"search"},$general_replacements[$n1]{"replace"});
        }
      }

      $line = $oline;
# keep a record of what arb is doing in UNWRAPPED_INPUT, commenting out any INCLUDE or GENERAL_REPLACMENTS statements so that this file could be read again by arb directly
      if ($line =~ /^\s*((INCLUDE(|_ROOT|_WORKING))|((GENERAL_|)REPLACEMENTS))($|#|\s)/i) { print UNWRAPPED_INPUT "#$line\n"; } else { print UNWRAPPED_INPUT "$line\n"; }

# now process guts of statement
# for the time being, have to handle trailing comments on the input lines

# first check whether skip is active or there is a COMMENTS|SKIP statement or there is an empty line
      if ($skip && $line =~ /^\s*((STOP|END)_(COMMENT(S){0,1}|SKIP))($|#|\s)/i) { print "INFO: found \L$1\E statement in $file\n"; $skip=0; next; }
      elsif ($skip || $line =~ /^\s*($|#)/) {next;}
      elsif ($line =~ /^\s*((START|BEGIN)_(COMMENT(S){0,1}|SKIP))($|#|\s)/i) { print "INFO: found \L$1\E statement in $file\n"; $skip=1; next; }

# check for include statement, possibly opening new file
      elsif ($line =~ /^\s*INCLUDE(|_WORKING)($|(\s*#)|\s)/i) {
        if ($2 =~ /#/) {$line = '';} else {$line = $';}
        if ($1) { $working=1; } else { $working=0; } # if the working flag is on then we only look in the working directory for the file
        $input_files[$#input_files+1]{"ref_name"} = extract_first($line,$error); # extract filename from line of text
        if (empty($input_files[$#input_files]{ref_name}) || $error) {
          error_stop("a valid filename could not be determined from the following:\nfile = $file: line = $oline");
        }
        $input_files[$#input_files]{"include_root"} = $input_files[$#input_files-1]{"include_root"}; # copy over include_root from previous file
        $input_files[$#input_files]{"name"} = '';
# append arb suffix to ref_name if it isn't already there - for v0.52 onwards, only arb or in suffixes are allowed
        if ( $input_files[$#input_files]{"ref_name"} !~ /.+\.(arb|in)$/ ) {
          $input_files[$#input_files]{"ref_name"} = $input_files[$#input_files]{"ref_name"}.'.arb';
        }
# ref: include working
        if ($working) {
# look only in working directory for the file
          if (-f "$working_dir/$input_files[$#input_files]{ref_name}") {
            $input_files[$#input_files]{"name"} = "$working_dir/$input_files[$#input_files]{ref_name}";
          }
        } else {
# prepend include_root directory to ref_name - no, now not looking in the working directory
#          if (nonempty($include_root)) { $input_files[$#input_files]{"ref_name"} = $include_root."/".$input_files[$#input_files]{"ref_name"}; }
# find location of the file, first looking in the specific template directory, then in all second and third level subsequent (sub)directories
# TODO: look at perl's Find::File for the really lazy users who don't write out template directory names in full and like to gamble on where the included files come from....
# ref: include
          my $include_root = $input_files[$#input_files]{"include_root"};
          foreach $try_dir ("$template_dir/$include_root",bsd_glob("$template_dir/*/$include_root"),bsd_glob("$template_dir/*/*/$include_root")) {
            if (-f "$try_dir/$input_files[$#input_files]{ref_name}") {
              $input_files[$#input_files]{"name"} = "$try_dir/$input_files[$#input_files]{ref_name}";
              last;
            }
          }
        }
        if (empty($input_files[$#input_files]{"name"})) {
          error_stop("cannot find the input file $input_files[$#input_files]{ref_name} referenced:\nfile = $file :line = $oline");
        } else {
          print "INFO: found INCLUDE $input_files[$#input_files]{ref_name} statement in file = $file: include file identified as $input_files[$#input_files]{name}\n";
          print DEBUG "INFO: found INCLUDE $input_files[$#input_files]{ref_name} statement in file = $file: include file identified as $input_files[$#input_files]{name}\n";
          print UNWRAPPED_INPUT "#++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n# the following is INCLUDED from $input_files[$#input_files]{name}";
# ref: FILENAME
# set simulation_info filename based on the first included file (which is the one that will be listed in root_input.arb)
          if (empty($simulation_info{"filename"})) {
            $simulation_info{"filename"} = $input_files[$#input_files]{"ref_name"};
          }
        }
# now extract replacements
        while (!($line=~/^\s*(|#.*)$/)) {
          ($search,$replace,$cancel,$default) = extract_replacements($line,$file,$oline);
          if ($cancel) { error_stop("string replacements for individual files cannot be cancelled:\nfile = $file :line = $oline"); }
          if ($default) { error_stop("default string replacements for individual files are not implemented:\nfile = $file :line = $oline"); }
          if (nonempty($search)) {
            %{$input_files[$#input_files]{"replacements"}[$#{$input_files[$#input_files]{"replacements"}}+1]} = ( search => $search, replace => $replace );
          }
        }

        if ($input_files[$#input_files]{"replacements"}) {
          print "INFO: using the following search/replace combinations during the include of $input_files[$#input_files]{ref_name}";
          print UNWRAPPED_INPUT " with the following search/replace combinations";
          my $n1 = $#input_files;
          foreach my $n2 ( 0 .. $#{$input_files[$#input_files]{"replacements"}} ) {
            print ": replace $input_files[$n1]{replacements}[$n2]{search} with $input_files[$n1]{replacements}[$n2]{replace}";
            print UNWRAPPED_INPUT ": replace $input_files[$n1]{replacements}[$n2]{search} with $input_files[$n1]{replacements}[$n2]{replace}";
          }
          print "\n";
          print UNWRAPPED_INPUT "\n";
        } else {
          print "INFO: not using any search/replace combinations during the include of $input_files[$#input_files]{ref_name}\n";
          print UNWRAPPED_INPUT " without any search/replace combinations\n";
        }
# now setup handle and open file
        $input_files[$#input_files]{"handle"} = FileHandle->new(); # make a filehandle for this file
        $handle = $input_files[$#input_files]{"handle"};
        $file = $input_files[$#input_files]{"ref_name"};
        open($handle, "<$input_files[$#input_files]{name}") or die "ERROR: problem opening arb input file $input_files[$#input_files]{name}\n";;
        next; # skip to reading the next line (in the included file)

# set include_root
# ref: include root
      } elsif ($line =~ /^\s*INCLUDE_(ROOT|FROM)\s*(($|#)|(.+))/i) {
        if ($3) { # no string follows, so reset include_root back to parent value
          if ($#input_files == 0) {
            $input_files[$#input_files]{"include_root"} = '';
          } else {
            $input_files[$#input_files]{"include_root"} = $input_files[$#input_files-1]{"include_root"} 
          }
        } else {
          $line = $4;
          $input_files[$#input_files]{"include_root"} = extract_first($line,$error);
          if ($error) {error_stop("matching delimiters not found in the following:\nfile = $file: line = $oline")}
        }
        print UNWRAPPED_INPUT "# INFO: setting include root directory to $input_files[$#input_files]{include_root}\n";
        next;

# extract any general replacements, pushing them onto the back of the existing list
# replacements are performed in reverse order, so latest replacement definitions take precedence
      } elsif ($line =~ /^\s*(GENERAL_|)REPLACEMENTS($|\s*#|\s)/i) {
        print "INFO: found GENERAL_REPLACEMENTS statement in file = $file\n";
        if ($2 =~ /#/) {$line = '';} else {$line = $';}
        while (!($line=~/^\s*$/ || $line =~ /^\s*#/)) {
          ($search,$replace,$cancel,$default) = extract_replacements($line,$file,$oline);
          if (nonempty($search)) {
# if the replacement is a default replacement, first need to check whether string is part of an existing input file replacement
            if ($default) {
              foreach my $n1 ( reverse( 0 .. $#input_files ) ) {
                foreach my $n2 ( 0 .. $#{$input_files[$n1]{"replacements"}} ) {
                  if ($search eq $input_files[$n1]{"replacements"}[$n2]{"search"}) { # found search string in input file replacements
                    print "INFO: general replacement (default) search string $search cancelled as the string is already present as an input file string replacement\n";
                    last;
                  }
                }
              }
            }
# v0.42 general replacements of each search string is unique
            my $n = search_general_replacements($search);
            if ($n < 0) { # indicates that string is not found in general_replacements
              if ($cancel) {
                print "WARNING: general replacement search string $search cancelled before being allocated\n";
              } else { # add a new one
                %{$general_replacements[$#general_replacements+1]} = ( search => $search, replace => $replace );
                print "INFO: added general replacements search and replace pair: search = $search: replace = $replace\n";
              }
            } else { # found search as an existing general replacement
              if ($cancel) {
                splice(@general_replacements,$n,1);
                print "INFO: cancelling previous general replacements search string: search = $search\n";
              } elsif ($default) {
                print "INFO: general replacement (default) search string $search cancelled as the string is already present as a general string replacement\n";
              } else {
                $general_replacements[$n]{"replace"}=$replace;
                print "INFO: replaced general replacements search and replace pair: search = $search: replace = $replace\n";
              }
            }

          }
        }
        print DEBUG "INFO: just processed GENERAL_REPLACEMENTS statement: general_replacements = ".Dumper(@general_replacements)."\n";
        next;
      }

# remove any in-line comments now (so from now on, do not have to consider # in any string matching)
      if ($line =~ /^(.*?)#/) { $comments = $'; $line = $1 } else { $comments = ''; }

# check for EXTERNALS statement, which accepts a list of external fortran external files
      if ($line =~ /^\s*EXTERNAL(|S)(\s+)/i) { 
				$line = $';
				while ($tmp = extract_first($line,$error)) {
# check that there weren't any errors, and that text was properly quoted
          if ($error) { error_stop("some type of syntax problem with the EXTERNAL statement.  Should the text be quoted?:\nfile = $file: line = $oline"); }
					create_external_file($tmp);
				}
				next;
			}

# check for END statement
      if ($line =~ /^\s*END($|\s)/i) { print "INFO: found END statement in file = $file\n"; last; }

# check on file version
      elsif ($line =~ /^\s*VERSION\s+(\S*)/i) {
        $file_version = $1;
        if (abs($file_version - $version) > 1.e-7) {
          if ($file_version < $minimum_version) {
            error_stop("version mismatch between $file and the current version (of setup_equations.pl)\n".
                "  You may be able to increase the version number in $file (from $file_version to $version), however some features of the language ".
                "syntax have changed since version $file_version so you should check the input files");
          } else {
            print "WARNING: version mismatch between $file and the current version (of setup_equations.pl)\n".
                "  You should be able to increase the version number in $file (from $file_version to $version) safely without altering the input ".
                "file syntax, however this error indicates that additional language features are now available\n";
          }
        }
        print FORTRAN_INPUT "VERSION $file_version\n"; # also tell the fortran program about this
        next;
      }

# ref: INFO
# look for userable simulation info statements
      elsif ($line =~ /^\s*INFO_(TITLE|DESCRIPTION|AUTHOR|DATE|VERSION)(|\+|-)\s+/i) {
        $key="\L$1"; $line=$';
        if ($2 eq "+") { $append=1; } elsif ($2 eq "-") {$append=-1;} else { $append=0; }
        $tmp = extract_first($line,$error);
# check that there weren't any errors, and that text was properly quoted
        if ($error || $line =~ /\S/) { error_stop("some type of syntax problem with the INFO_"."\U$key"." string.  Should the text be quoted?:\nfile = $file: line = $oline"); }
# a plus signifies to append the string to the current value
# a minus means set the string only if it is empty.
# Otherwise, set the string to the new value ($append=0)
        if ($append eq 1) {
          $simulation_info{$key} = $simulation_info{$key}.$tmp;
        } elsif (!($append) || empty($simulation_info{$key})) {
          $simulation_info{$key} = $tmp;
        }
        next;
      }

# look for transient/steady-state simulation keyword
      elsif ($line =~ /^\s*(TRANSIENT|STEADY-STATE|STEADYSTATE|NONTRANSIENT)_SIMULATION($|\s)/i) {
        print DEBUG "INFO: $1_SIMULATION set from line: $oline\n";
        print "INFO: $1_SIMULATION set directly\n";
        if ($1 =~ /^TRANSIENT$/i) { set_transient_simulation(1); } else { set_transient_simulation(0); }
        next;
      }

# look for newtient simulation keyword
      elsif ($line =~ /^\s*((NON|)NEWTIENT)_SIMULATION($|\s)/i) {
        print DEBUG "INFO: $1_SIMULATION set from line: $oline\n";
        print "INFO: $1_SIMULATION set directly\n";
        if ($1 =~ /^NEWTIENT$/i) { $newtient=1; } else { $newtient=0; }
        next;
      }

# ref: default options
# set or reset any default options (these go before the individual options so any relevant individual options take precedence over these)
# also, each DEFAULT_OPTIONS statement clears previous DEFAULT_OPTIONS statements
      elsif ($line =~ /^\s*DEFAULT_OPTIONS\s*($|\s)/i) {
        $default_options = $';
        ($default_options) = $default_options =~ /^\s*(.*?)\s*$/; # greedy space matches at the front and back remove all leading and trailing space
        if (empty($default_options)) {
          print DEBUG "INFO: default options have been removed via:\nfile = $file: line = $oline\n";
          print "INFO: default options have been removed\n";
        } else {
          print DEBUG "INFO: default options have been set to $default_options via:\nfile = $file: line = $oline\n";
          print "INFO: default options have been set to $default_options\n";
        }
        next;
      }

# ref: override options
# set or reset any override options (these go at the end of the individual options so override any individual options)
# also, each OVERRIDE_OPTIONS statement clears previous OVERRIDE_OPTIONS statements
      elsif ($line =~ /^\s*OVERRIDE_OPTIONS\s*($|\s)/i) {
        $override_options = $';
        ($override_options) = $override_options =~ /^\s*(.*?)\s*$/; # greedy space matches at the front and back remove all leading and trailing space
        if (empty($override_options)) {
          print DEBUG "INFO: override options have been removed via:\nfile = $file: line = $oline\n";
          print "INFO: override options have been removed\n";
        } else {
          print DEBUG "INFO: override options have been set to $override_options via:\nfile = $file: line = $oline\n";
          print "INFO: override options have been set to $override_options\n";
        }
        next;
      }

      elsif ( $line =~ /^\s*(MSH_FILE|((KERNEL|SOLVER|GENERAL)(|_OPTION(|S)))|NEWTRESTOL|NEWTSTEP(MAX|MIN|OUT|DEBUGOUT)|TIMESTEP(MAX|MIN|OUT|ADDITIONAL)|TIMESTEPSTART|NEWTSTEPSTART|GLUE_FACES)($|\s)/i ) {
# these are commands that need to be transferred unaltered to the arb input file
        $keyword = "\U$1";
        $line = $'; $line =~ s/^\s*//;
        if ($keyword =~ /^((KERNEL|SOLVER|GENERAL)(|_OPTION(|S)))$/) {$keyword = "$2_OPTIONS";} # standardise the statement for fortran input
# if this is a GLUE_FACES then check if reflect=? has been specified and if so, set general_replacement string automatically
# not setting this in the arb input file has caught me out so many times that it is now automatic, but can be overwritten after the GLUE_FACES command if need be
        if ($keyword =~ /^GLUE_FACES/i) {
          $tmp = $line;
# strip any region definitions from line, just in case a region contains a reflect=N statement in it...
          while ($tmp =~ /\s*<(.+?)>\s*/) { $tmp = $`." ".$'; }
          while ($tmp =~ /\s*reflect\s*=\s*(\d)\s*/i) {
            my $search="<<reflect=$1>>";
            my $replace="reflect=$1";
            $tmp = $tmp = $`." ".$';
            my $n = search_general_replacements($search);
            if ($n < 0) { error_stop("some type of error with the reflect specification in the following: $oline"); }
            else { $general_replacements[$n]{"replace"}=$replace; }
            print "INFO: based on a GLUE_FACES statement setting $search general_replacements string to $replace\n";
          }
        }
        print FORTRAN_INPUT $keyword; if (nonempty($line)) {print FORTRAN_INPUT " ".$line}; print FORTRAN_INPUT "\n"; # print line to fortran input file
        next;
      }

# check for deprecated syntax
      elsif ($line =~ /^\s*(CELL_|FACE_|NODE_|NONE_|)(INDEPENDENT|FIELD)($|\s)/i) {
        $type = "\U$2";
        error_stop("$type type has been deprecated, use UNKNOWN instead.\nfile = $file: line = $oline\n");
      }

      elsif ($line =~ /^\s*(CELL_|FACE_|NODE_|NONE_|)DEPENDENT($|\s)/i) {
        error_stop("DEPENDENT type has been deprecated, use DERIVED instead.\nfile = $file: line = $oline\n");
      }

      elsif ( $line =~ /^\s*(READ_GMSH)($|\s)/i ) {
        error_stop("READ_GMSH keyword has been deprecated, use MSH_FILE instead.\nfile = $file: line = $oline\n");
      }

      elsif ( $line =~ /^\s*(LINEAR_SOLVER)($|\s)/i ) {
        error_stop("LINEAR_SOLVER keyword has been deprecated, use SOLVER_OPTIONS linearsolver=default (eg) instead.\nfile = $file: line = $oline\n");
      }

# # read in glue_face
# # glue_faces are processed in reverse order, and once a face is glued it won't be glued to another
# # so for duplicate glue_face commands, the last one takes precedence, although all are stored in both the perl and fortran
# # for options they are processes as per variable options, moving from left to right - hence the rightmost takes precedence, and the order is important
#       elsif ($line =~ /^\s*GLUE_FACES($|\s)/i) {
#         $line = $';
#         if ($line =~ /^\s*(<.+?>)\s*/) {
#           $glue_face[$#glue_face+1]{"region"}[1] = $1;
#           $line = $';
#         } else { error_stop("a valid region name was not recognised for the following GLUE_FACES definition\nfile = $file: line = $oline"); }
# # see if this glue_face has been defined before, and if so, overwrite previous definition
#         if ($line =~ /^\s*(<.+?>)\s*/) {
#           $glue_face[$#glue_face]{"region"}[2] = $1;
#           $line = $';
#         } else { $glue_face[$#glue_face]{"region"}[2] = $glue_face[$#glue_face]{"region"}[1]; } # if no second region is specified then we set this to the first region (ie, for reflection boundaries)
#         ($glue_face[$#glue_face]{"options"}) = $line =~ /^\s*(.*)\s*$/;
# # now clean up by removing any leading, repeated or trailing commas
#         $glue_face[$#glue_face]{"options"} =~ s/(^\,+\s*)|(\s*\,+$)//;
#         $glue_face[$#glue_face]{"options"} =~ s/\s*\,+\s*/,/g;
#         print "INFO: GLUE_FACE definition between $glue_face[$#glue_face]{region}[1] and $glue_face[$#glue_face]{region}[2] with options $glue_face[$#glue_face]{options} processed\n";
#         print DEBUG "INFO: GLUE_FACE definition between $glue_face[$#glue_face]{region}[1] and $glue_face[$#glue_face]{region}[2] with options $glue_face[$#glue_face]{options} processed\n";
#       }

# read in region_list
      elsif ($line =~ /^\s*(CELL_|FACE_|NODE_|)REGION_LIST($|\s)/i) {
        $line = $';
# reset most recently read region list
        %region_list = ();
# find centring
        $region_list{"centring"} = '';
        if ($1) { $region_list{"centring"} = "\L$1"; ($region_list{"centring"}) = $region_list{"centring"} =~ /^(\S+?)_/; }
# read in regions and store in an array
        while ($line =~ /^\s*(<.+?>)\s*/) { 
          $line = $';
          push(@{$region_list{"regions"}},$1); # the regions hash contains an array of region names
        }
        if ($line !~ /^\s*$/) {error_stop("there is a syntax error in the following REGION_LIST statement:\nfile = $file: line = $oline");}
        if (empty($region_list{"regions"})) {error_stop("the following REGION_LIST statement contains no regions:\nfile = $file: line = $oline");}
# print some info about this
        $tmp = "INFO: found ";
        if (nonempty($region_list{"centring"})) { $tmp = $tmp."$region_list{centring} centred"; } else { $tmp = $tmp."unknown centring"; }
        $tmp = $tmp." REGION_LIST containing the regions: @{$region_list{regions}}\n";
        print $tmp; print DEBUG $tmp;
        next;
      }

#-----------------------
# user variables, by type and name
# ref: VARIABLE
      elsif ($line =~ /^\s*(CELL_|FACE_|NODE_|NONE_|)(CONSTANT|REGION_CONSTANT|TRANSIENT|NEWTIENT|DERIVED|UNKNOWN|EQUATION|OUTPUT|CONDITION|LOCAL|VARIABLE)($|\s)/i) {
        $line = $';
        $type = "\L$2"; # NB: $2 cannot be empty
        if ($type eq "variable") { $type = ''; }; # the variable keyword is just a placeholder - set type to an empty string
        if ($type eq "region_constant") { $type = "constant"; $region_constant = 1; } else { $region_constant = 0; }
        $centring = ""; # now centring can be grabbed from last definition
        if ($1) { $centring = "\L$1"; ($centring) = $centring =~ /^(\S+?)_/; }

        if ($line =~ /^\s*(<.+?>)($|\s)/) { $name = $1; $line = $'; }
        else { error_stop("problem reading in the variable name from the following line:\nfile = $file: line = $oline");}
        print DEBUG "INFO: found user variable in input file: name = $name: type = $type: centring = $centring\n";
        $name = examine_name($name,"name");
        print DEBUG "  coverting user defined name to consistent name = $name\n";

# see if this name has already been defined, and if so, find position of the variable in the input file
        $masread = -1; # variable masread starts at 0 if any variables are defined
        foreach $mcheck ( 0 .. $#asread_variable ) {
          if ($asread_variable[$mcheck]{"name"} eq $name) { # variable has been previously defined, and position in file is based on first definition
            $masread = $mcheck;
            last;
          }
        }

# check for CANCEL keyword
        if ($line =~ /^(\s*)CANCEL(\s|$)/) {
          if ($masread >= 0) {
            print "INFO: cancelling variable $name\n";
            print DEBUG "INFO: cancelling variable $name\n";
            splice(@asread_variable, $masread, 1);
# also have to adjust the reference to the variables from the regions
            for my $nregion ( 0 .. $#region ) {
              if ($region[$nregion]{"last_variable_masread"} >= $masread) { $region[$nregion]{"last_variable_masread"}=$region[$nregion]{"last_variable_masread"}-1; }
            }
# TODO: get rid of options if variable is cancelled
          } else {
            print "WARNING: attempting to cancel variable $name that hasn't been defined yet - CANCEL ignored\n";
            print DEBUG "WARNING: attempting to cancel variable $name that hasn't been defined yet - CANCEL ignored\n";
          }
          next;
        }

# now create or update variable type and centring
        if ($masread >= 0) {
          $asread_variable[$masread]{"definitions"}++; 
          print "INFO: a secondary definition statement (number $asread_variable[$masread]{definitions}) for variable $name has been found in file = $file\n";
          print DEBUG "INFO: a secondary definition statement (number $asread_variable[$masread]{definitions}) for variable $name has been found based on:\nfile = $file: line = $oline\n";
# a variable has been identified, now check whether the type has changed
          if (nonempty($type)) {
            if ($type ne $asread_variable[$masread]{"type"} && nonempty($asread_variable[$masread]{"type"})) {
              print "NOTE: changing variable $name from type $asread_variable[$masread]{type} to $type\n";
              print DEBUG "NOTE: changing variable $name from type $asread_variable[$masread]{type} to $type based on:\nfile = $file: line = $oline\n";
              $asread_variable[$masread]{"typechanges"}++;
            }
            $asread_variable[$masread]{"type"} = $type;
          } else {
            $type = $asread_variable[$masread]{"type"};
          }
# a variable has been identified, now check whether the centring has changed
          if (nonempty($centring)) {
            if ($centring ne $asread_variable[$masread]{"centring"} && nonempty($asread_variable[$masread]{"centring"})) {
              print "NOTE: changing the centring of variable $name from $asread_variable[$masread]{centring} to $centring\n";
              print DEBUG "NOTE: changing the centring of variable $name from $asread_variable[$masread]{centring} to $centring based on \nfile = $file: line = $oline\n";
              $asread_variable[$masread]{"centringchanges"}++;
# also clear previous region
              if (nonempty($asread_variable[$masread]{"region"})) {
                print "NOTE: during change of centring type region specification of $asread_variable[$masread]{region} deleted for variable $name\n";
                print DEBUG "NOTE: during change of centring type region specification of $asread_variable[$masread]{region} deleted for variable $name\n";
                $asread_variable[$masread]{"region"} = '';
              }
            }
            $asread_variable[$masread]{"centring"} = $centring;
          } else {
            $centring = $asread_variable[$masread]{"centring"};
          }
          $asread_variable[$masread]{"comments"}=$asread_variable[$masread]{"comments"}." ".$comments;
        } else {
          print "INFO: a primary definition statement for variable $name has been found in file = $file\n";
          print DEBUG "INFO: a primary definition statement for variable $name has been found based on:\nfile = $file: line = $oline\n";
# otherwise create a new variable
          $masread=$#asread_variable+1;
          print DEBUG "INFO: creating new variable number $masread with name $name based on \n:file = $file: line = $oline\n";
# and set basic info, empty if necessary
          $asread_variable[$masread]{"name"}=$name;
          $asread_variable[$masread]{"type"}=$type;
          if (empty($centring)) { $centring = 'none'; } # default centring if not previously set
          $asread_variable[$masread]{"centring"}=$centring;
          $asread_variable[$masread]{"rindex"}=examine_name($name,"rindex"); # this is based on name so doesn't change with repeat definitions
          $asread_variable[$masread]{"comments"}=$comments;
          $asread_variable[$masread]{"region"}='';
          $asread_variable[$masread]{"definitions"}=1;
          $asread_variable[$masread]{"typechanges"}=0;
          $asread_variable[$masread]{"centringchanges"}=0;
          $asread_variable[$masread]{"options"} = '';
        }

# units and multiplier (optional)
# first determine whether any specification has been made
        $units = ''; $multiplier = '';
        if ($line =~ /^\s*\[(.*?)\]/) {
# units specification is present to determine what they are and strip them from the front of $line
          $cunits = $1;
          $line = $';
# split units if multiplier is present
          if ($cunits =~ /\*/) { ($multiplier,$units) = $cunits =~ /(.*?)\*(.*)/; }
          else { $multiplier=""; $units=$cunits;}
          $multiplier =~ s/e|E|D/d/; # convert single and floats to double precision, regardless of case
          if (nonempty($units)) { $asread_variable[$masread]{"units"}=$units; }
          if (nonempty($multiplier)) { $asread_variable[$masread]{"multiplier"}=$multiplier; }
        }
        if (!($asread_variable[$masread]{"units"})) { $asread_variable[$masread]{"units"} = "1"; }
        if (!($asread_variable[$masread]{"multiplier"})) { $asread_variable[$masread]{"multiplier"} = "1.d0"; }

# equations or numerical constants
# look for either a single (CONSTANT) or list (REGION_CONSTANT) of numbers, or otherwise an expression for this variable
# first delete any orphaned initial_equations or constant_lists
        if (!($type eq "transient" || $type eq "newtient") && nonempty($asread_variable[$masread]{"initial_equation"})) {
          print "NOTE: deleting initial equation for $type variable $name that must have been left over from a previous definition\n";
          print DEBUG "NOTE: deleting initial equation for $type variable $name that must have been left over from a previous definition\n";
          delete $asread_variable[$masread]{"initial_equation"};
        }
        if ($type ne "constant" && nonempty($asread_variable[$masread]{"constant_list"})) {
          print "NOTE: deleting constant list (numerical value) for $type variable $name that must have been left over from a previous constant definition\n";
          print DEBUG "NOTE: deleting constant list (numerical value) for $type variable $name that must have been left over from a previous constant definition\n";
          delete $asread_variable[$masread]{"region_list"};
          delete $asread_variable[$masread]{"constant_list"};
        }
# look for numerical constants which must start with either +-. or a digit
        if ( $type eq "constant" && $line =~ /^\s*[\+\-\d\.]/ ) { # to use a numerical constant value the type must be known at read-in time
          print DEBUG "INFO: assuming a numerical constant is entered in the following:\nfile = $file: line = $oline\n";
          if (nonempty($asread_variable[$masread]{"equation"}) || nonempty($asread_variable[$masread]{"initial_equation"})) {
            print "NOTE: resetting CONSTANT $name from an equation form to a numerical form\n";
            delete $asread_variable[$masread]{"equation"}; # preference is to delete these key/values as they then won't be included in %variable (for restart purposes)
            delete $asread_variable[$masread]{"initial_equation"};
          }
          my $n = 1;
          delete $asread_variable[$masread]{"region_list"};
          delete $asread_variable[$masread]{"constant_list"};
          if ($region_constant) {
            if (empty($region_list{"regions"})) { error_stop("a $centring REGION_CONSTANT appears before a REGION_LIST has been defined:\nfile = $file: line = $oline");}
            if ($centring eq "none") { error_stop("attempting to set a none centred constant $name using a REGION_CONSTANT statement: use a NONE_CONSTANT statement instead:\nfile = $file: line = $oline");}
            if (nonempty($region_list{"centring"}) && $region_list{"centring"} ne $centring) { error_stop("the $centring centring of a REGION_CONSTANT is not consistent with the $region_list{centring} centring of the preceeding REGION_LIST:\nfile = $file: line = $oline");}
            @{$asread_variable[$masread]{"region_list"}} = @{$region_list{"regions"}}; # set region_list to that of most recent REGION_LIST
            $n = scalar(@{$region_list{"regions"}}); # returning the number of elements in this array
          }
          while ($line =~ /^\s*([\+\-\d\.][\+\-\ded\.]*)(\s+|$)/i) { # numbers must start with either +-. or a digit, so options cannot start with any of these
            $line = $'; $match = "\L$1";
            if ($match !~ /\d/) { error_stop("a numerical constant was not valid in the following line, indicating some type of syntax error:\nfile = $file: line = $oline"); };
# make sure all constants are written in double precision
            $match =~ s/e/d/;
            if ($match !~ /d/) { $match = $match."d0"; }
            if ($match !~ /\./) { $match =~ s/d/.d/; }
            push(@{$asread_variable[$masread]{"constant_list"}},$match); # assemble list of numerical constants
          }
          if (empty($asread_variable[$masread]{"constant_list"})) { error_stop("no numerial constants were read in from the following line, indicating some type of syntax error:\nfile = $file: line = $oline"); };
          print DEBUG "INFO: found the following constant_list for $name: @{$asread_variable[$masread]{constant_list}}\n";
          if ($region_constant) { print DEBUG "INFO: found the following region_list for $name: @{$asread_variable[$masread]{region_list}}\n"; }
          if ($n ne @{$asread_variable[$masread]{"constant_list"}} ) {
            if ($region_constant) {
              error_stop("the following REGION_CONSTANT line has ".scalar(@{$asread_variable[$masread]{constant_list}})." numerical entries, whereas the preceeding REGION_LIST has $n entries - these should match:\nfile = $file: line = $oline");
            } else { error_stop("a single numerical constant could not be read from the following CONSTANT line:\nfile = $file: line = $oline"); }
          }
        } elsif ( $line =~ /^\s*["']/ ) {
          print DEBUG "INFO: assuming an expression (rather than a numerical constant) is entered in the following:\nfile = $file: line = $oline\n";
          delete $asread_variable[$masread]{"region_list"};
          delete $asread_variable[$masread]{"constant_list"};
# read in expressions, noting that only if the expression is nonempty do we overide previously stored expression
# this allows initial_equation to be reset independently of the equation for transient/newtient variables
          $tmp1 = extract_first($line,$error);
          if ($error) { error_stop("some type of syntax problem with the (first) expression in the following variable definition:\nfile = $file: line = $oline"); }
          if (($type eq "transient" || $type eq "newtient") && $line =~ /^\s*["']/) { # to set the intial expression the type must be known at read-in time
            $tmp2 = extract_first($line,$error);
            if ($error) { error_stop("some type of syntax problem with the second expression in the following variable definition:\nfile = $file: line = $oline"); }
# if we are here then tmp1 corresponds to the initial_equation, and tmp2 to the equation
#           if (nonempty($tmp1)) { $asread_variable[$masread]{"initial_equation"} = $tmp1; print DEBUG "INFO: setting the $type variable initial_equation to $tmp1 based on:\nfile = $file: line = $oline\n"; }
#           if (nonempty($tmp2)) { $asread_variable[$masread]{"equation"} = $tmp2; print DEBUG "INFO: setting the $type variable equation to $tmp2 based on:\nfile = $file: line = $oline\n"; }
# empty and undef now have different meanings - empty ("") now means to repeat the full equation, whereas undef means to give it a value of zero
            $asread_variable[$masread]{"initial_equation"} = $tmp1; print DEBUG "INFO: setting the $type variable initial_equation to $tmp1 based on:\nfile = $file: line = $oline\n";
#           $asread_variable[$masread]{"equation"} = $tmp2; print DEBUG "INFO: setting the $type variable equation to $tmp2 based on:\nfile = $file: line = $oline\n";
# incase we need to only set the initial_equation of a variable, keeping the previous equation value, only set equation if it is actually nonempty (ie, not "")
            if (nonempty($tmp2)) { $asread_variable[$masread]{"equation"} = $tmp2; print DEBUG "INFO: setting the $type variable equation to $tmp2 based on:\nfile = $file: line = $oline\n"; }
          } else {
# if we are here then tmp1 corresponds to the equation
#           if (nonempty($tmp1)) { $asread_variable[$masread]{"equation"} = $tmp1; print DEBUG "INFO: setting the $type variable equation to $tmp1 based on:\nfile = $file: line = $oline\n"; }
            $asread_variable[$masread]{"equation"} = $tmp1; print DEBUG "INFO: setting the $type variable equation to $tmp1 based on:\nfile = $file: line = $oline\n";
          }
# check/set defaults for these later, after all variable definitions have been read in
        }

# region
        if ( $line =~ /^\s*ON(\s*)(<.+?>)\s*/i ) {
          if ($2 eq "<none>" ) { # the '<none>' region cancels the previously defined region
            $asread_variable[$masread]{"region"} = '';
          } else {
            $asread_variable[$masread]{"region"} = examine_name($2,'regionname');
          }
          $line = $';
        }

# store raw options in the asread_variable array now
# variable and compound option lists will be assembled later
        $line =~ s/^\s*//; # remove any leading space from the line
        if (nonempty($line) || nonempty($default_options) || nonempty($override_options)) {
          $line = $default_options.','.$line.','.$override_options;
          $line =~ s/(^\,+\s*)|(\s*\,+$)//;
          $line =~ s/\s*\,+\s*/,/g;
          if (empty($asread_variable[$masread]{"options"})) {
            $asread_variable[$masread]{"options"} = $line;
          } else {
            $asread_variable[$masread]{"options"} = $asread_variable[$masread]{"options"}.",".$line;
          }
          print DEBUG "INFO: adding options $line to: name = $name: masread = $masread: options = $asread_variable[$masread]{options}\n";
# now clean up by removing any leading, repeated or trailing commas
          $line = ''; # nothing is now left in the line
        }
      }

#-----------------------
# now processing user regions too, in much the same way as the user variables
# ref: REGION

      elsif ( $line =~ /^\s*((FACE|CELL|NODE)_|)((STATIC|SETUP|GMSH|CONSTANT|TRANSIENT|NEWTIENT|DERIVED|EQUATION|OUTPUT|UNKNOWN)_|)REGION($|\s)/i ) {
        $line = $';
        $centring = ""; # now centring can be grabbed from last definition
        if ($2) { $centring = "\L$2"; }
        $type = ""; # as can type
        if ($4) { $type = "\L$4";}
        if ($type eq "static") {$type="setup";} # setup is the name used in the fortran and perl to denote a user region that is not dynamic, but for the end user, static is easier to comprehend (for the fortran and perl static means !dynamic, which ewals setup, gmsh and system types)

# grab region name
        if ($line =~ /^\s*(<.+?>)($|\s)/) { $name = $1; $line = $'; }
        else { error_stop("problem reading in the region name from the following line:\nfile = $file: line = $oline");}
        print DEBUG "INFO: found user region in input file: name = $name: centring = $centring: type = $type\n";
        $name = examine_name($name,"regionname");
        print DEBUG "  coverting user defined name to consistent name = $name\n";

# see if this name has already been defined, and if so, find its index
        $masread = -1; # variable masread starts at 0 if any variables are defined
        foreach $mcheck ( 0 .. $#region ) {
          if ($region[$mcheck]{"name"} eq $name) { # region has been previously defined, and position in file is based on first definition
            $masread = $mcheck;
            last;
          }
        }

# check for CANCEL keyword
        if ($line =~ /^(\s*)CANCEL(\s|$)/) {
          if ($masread >= 0) {
            print "INFO: cancelling region $name\n";
            print DEBUG "INFO: cancelling region $name\n";
            splice(@region, $masread, 1)
          } else {
            print "WARNING: attempting to cancel region $name that hasn't been defined yet - CANCEL ignored\n";
            print DEBUG "WARNING: attempting to cancel region $name that hasn't been defined yet - CANCEL ignored\n";
          }
          next;
        }

# now create or update region type and centring
        if ($masread >= 0) {
          $region[$masread]{"definitions"}++; 
          print "INFO: a secondary definition statement (number $region[$masread]{definitions}) for region $name has been found in file = $file\n";
          print DEBUG "INFO: a secondary definition statement (number $region[$masread]{definitions}) for region $name has been found based on:\nfile = $file: line = $oline\n";
# a variable has been identified, now check whether the centring has changed
          if (nonempty($centring)) {
            if ($centring ne $region[$masread]{"centring"} && nonempty($region[$masread]{"centring"})) {
              print "NOTE: changing the centring of region $name from $region[$masread]{centring} to $centring\n";
              print DEBUG "NOTE: changing the centring of region $name from $region[$masread]{centring} to $centring based on \nfile = $file: line = $oline\n";
            }
            $region[$masread]{"centring"} = $centring;
          } else {
            $centring = $region[$masread]{"centring"};
          }
# same with type
          if (nonempty($type)) {
            if ($type ne $region[$masread]{"type"} && nonempty($region[$masread]{"type"})) {
              print "NOTE: changing the type of region $name from $region[$masread]{type} to $type\n";
              print DEBUG "NOTE: changing the type of region $name from $region[$masread]{type} to $type based on \nfile = $file: line = $oline\n";
            }
            $region[$masread]{"type"} = $type;
          } else {
            $type = $region[$masread]{"type"};
          }
          $region[$masread]{"comments"}=$region[$masread]{"comments"}." ".$comments;
        } else {
          print "INFO: a primary definition statement for region $name has been found in file = $file\n";
          print DEBUG "INFO: a primary definition statement for region $name has been found based on:\nfile = $file: line = $oline\n";
# otherwise create a new region
          $masread=$#region+1;
          print DEBUG "INFO: creating new region number $masread with name $name based on \n:file = $file: line = $oline\n";
# and set basic info, empty if necessary
          $region[$masread]{"name"}=$name;
          $region[$masread]{"centring"}=$centring; # maybe blank
          $region[$masread]{"type"}=$type; # maybe blank
          $region[$masread]{"comments"}=$comments;
          $region[$masread]{"definitions"}=1;
          $region[$masread]{"part_of"}='';
          $region[$masread]{"options"}='';
          $region[$masread]{"location"}{"description"}='';
          $region[$masread]{"initial_location"}{"description"}='';
          $region[$masread]{"last_variable_masread"}=$#asread_variable; # this determines when a region will be evaluated, for dynamic regions - it will be -1 if no variables are defined yet
        }

# extract the location string, and if two are present, also an initial_location string (to be used for transient and newtient dynamic regions)
        if ( $line =~ /^\s*["']/ ) {
          $tmp1 = extract_first($line,$error);
          if ($error) { error_stop("some type of syntax problem with a location string in the following region definition:\nfile = $file: line = $oline"); }
          if (nonempty($region[$masread]{"location"}{"description"})) {
            print "NOTE: changing the location of region $name\n";
            print DEBUG "NOTE: changing the location of region $name\n";
          }
          if ( $line =~ /^\s*["']/ ) {
            $tmp2 = extract_first($line,$error);
            if ($error) { error_stop("some type of syntax problem with a location string in the following region definition:\nfile = $file: line = $oline"); }
            if (nonempty($region[$masread]{"initial_location"}{"description"})) {
              print "NOTE: changing the initial_location of region $name\n";
              print DEBUG "NOTE: changing the initial_location of region $name\n";
            }
            $region[$masread]{"location"}{"description"} = $tmp2;
            $region[$masread]{"initial_location"}{"description"} = $tmp1;
            print DEBUG "INFO: extracting region $name location and initial_location string from the following:\nfile = $file: line = $oline\n";
          } else {
            $region[$masread]{"location"}{"description"} = $tmp1;
            print DEBUG "INFO: extracting region $name location string from the following:\nfile = $file: line = $oline\n";
          }
        }

# ON keyword
        if ($line =~ /ON(\s+(<.+?>)($|\s+)|($|\s+))/i) {
          $line = $`.$';
          if ($2) {
            $tmp = examine_name($2,'regionname'); # standardise name here
            if (nonempty($region[$masread]{"part_of"})) {
              print "NOTE: changing the ON region for region $name to $tmp\n";
              print DEBUG "NOTE: changing the ON region for region $name to $tmp\n";
            }
            $region[$masread]{"part_of"} = $tmp;
            print DEBUG "INFO: found ON region $tmp for region $name\n";
          }
          else { $region[$masread]{"part_of"} = ''; print DEBUG "INFO: cancelling any possible ON region for region $name\n"; }
        }
          
# region options
        $line =~ s/^\s*//; # remove any leading space from the line
        if (nonempty($line)) {
          $region[$masread]{"options"} = $region[$masread]{"options"}.",".$line;
          $region[$masread]{"options"} =~ s/(^\,+\s*)|(\s*\,+$)//;
          $region[$masread]{"options"} =~ s/\s*\,+\s*/,/g;
          print DEBUG "INFO: adding options to: region = $region[$masread]{name}: masread = $masread: options = $region[$masread]{options}\n";
          $line = ''; # nothing is now left in the line
        }

        print "INFO: region statement has been read: name = $name: number = $masread: centring = $region[$masread]{centring}: ".
          "type = $region[$masread]{type}: location = $region[$masread]{location}{description}: ".
          "initial_location = $region[$masread]{initial_location}{description}: part_of = $region[$masread]{part_of}\n"; 
        print DEBUG "INFO: region statement has been read: name = $name: number = $masread: centring = $region[$masread]{centring}: ".
          "type = $region[$masread]{type}: location = $region[$masread]{location}{description}: ".
          "initial_location = $region[$masread]{initial_location}{description}: part_of = $region[$masread]{part_of}\n"; 

        next;

#-----------------------
      } else {
# finally if the line doesn't match any of the above, then stop - it may mean that something is not as intended
        error_stop("the following line in $file makes no sense:\n line = $oline");
      }

    } # end of loop for this input file

    close($handle);
    if ($#input_files) { print UNWRAPPED_INPUT "# INCLUDE FINISHED for $input_files[$#input_files]{name}\n#--------------------------------------------------------\n"; }
    pop(@input_files);
  } # end of loop for all input files

  close(UNWRAPPED_INPUT);

# dump all of the simulation info into the fortran file, and output to the screen and debug
  $sub_string{"simulation_info"} = '';
  foreach $key ( keys(%simulation_info)) {
    print DEBUG "SIMULATION INFO: "."\U$key"." = $simulation_info{$key}\n";
    if ($key ne "description") { print "SIMULATION INFO: "."\U$key"." = $simulation_info{$key}\n"; }
    print FORTRAN_INPUT "INFO_\U$key"." \"$simulation_info{$key}\"\n"; # also tell the fortran program about this
    $sub_string{"simulation_info"} = $sub_string{"simulation_info"}."! SIMULATION INFO: "."\U$key"." = $simulation_info{$key}\n"; # and write the same to equation_module.f90
  }

}

#-------------------------------------------------------------------------------
# here we add all of the SYSTEM, INTERNAL and GMSH (as known) regions to the region array

sub organise_regions {

  use strict;
  my ($n);

# ref: regions
# what this all means:

# type       dynamic  user  fortran  location  initial_location  description
#---------------------------------------------------------------------------------
# constant   X        X     X        X                           updated as the constant variables are updated (in the order of definition)
# transient  X        X     X        X         X                 updated as the transient and initial transient variables are updated (in the order of definition)
# newtient   X        X     X        X         X                 updated as the newtient and initial newtient variables are updated (in the order of definition)
# derived    X        X     X        X                           updated as the derived variables are updated (in the order of definition)
# equation   X        X     X        X                           updated as the equation variables are updated (in the order of definition)
# output     X        X     X        X                           updated as the output variables are updated (in the order of definition)
# setup               X     X        X                           updated at the start of a simulation during the setup routines, only once, using location information
# gmsh                      X        GMSH                        read in from a gmsh file, although centring and name can be declared within arb file (using no location string or 'GMSH')
# system                    X        SYSTEM                      regions defined by the system, and available to users (such as <all cells>)
# internal                           INTERNAL                    regions that are special-cased within the fortran (such as <adjacentcellicells>)
#---------------------------------------------------------------------------------
# dynamic means that this is actually a region mask, which must have a parent region, and is evaluated in the chain of the same type of variable updates
# user means that the location string will be interpreted to find the elements within the region
# fortran means that this region will be specifically defined within the fortran program
# location is the string that will be interpreted to find relevant region elements for user variables
# initial_location X is the string that will be interpreted to find relevant region elements, on the initial update of transients and newtients
#---------------------------------------------------------------------------------

#-------------
# user regions are those that are specifically defined in the arb input file, unless this is for gmsh centring specification
# only USER regions can be dynamic, although not all USER regions are dynamic
# set some user-specific flags - user and dynamic - while checking type and centring
  foreach $n ( 0 .. $#region ) {

    if (empty($region[$n]{'location'}{'description'}) || $region[$n]{'location'}{'description'} =~ /^\s*gmsh/i || 
      (nonempty($region[$n]{"type"}) && $region[$n]{"type"} eq "gmsh") ) {
# initialise any gmsh regions
      if (nonempty($region[$n]{"type"}) && $region[$n]{"type"} ne "gmsh") {
        print "WARNING: based on not having any location specified, region $region[$n]{name} is being set to gmsh type and is expected to be read in from a msh file";
        print DEBUG "WARNING: based on not having any location specified, region $region[$n]{name} is being set to gmsh type and is expected to be read in from a msh file";
      }
      $region[$n]{'type'} = 'gmsh';
      $region[$n]{"dynamic"} = 0;
      $region[$n]{"user"} = 0;
      if ((nonempty($region[$n]{'location'}{'description'}) && $region[$n]{'location'}{'description'} !~ /^\s*gmsh/i) ||
        empty($region[$n]{'location'}{'description'})) { $region[$n]{'location'}{"description"} = "gmsh from arb file"; }
    } elsif (empty($region[$n]{'type'}) || $region[$n]{'type'} eq 'setup') {
# initialise any setup regions
      $region[$n]{'type'} = 'setup';
      $region[$n]{"dynamic"} = 0;
      $region[$n]{"user"} = 1;
    } else {
# anything left must be dynamic: initialise
      $region[$n]{"dynamic"} = 1;
      $region[$n]{"user"} = 1;
      if ($region[$n]{"type"} eq "transient") { set_transient_simulation(1); print DEBUG "INFO: setting simulation type to transient based on the detection of at least one transient region $region[$n]{name}\n"; }
      if ($region[$n]{"type"} eq "newtient") { $newtient=1; print DEBUG "INFO: setting simulation type to newtient based on detection of at least one newtient region $region[$n]{name}\n"; }
    }
# deal with user regions having no centring defined
    if ($region[$n]{"user"} && empty($region[$n]{'centring'})) { error_stop("$region[$n]{type} region $region[$n]{name} has no centring defined: all regions (except gmsh) ".
      "entered in the arb file must have a centring defined"); }
# remove deprecated PART OF statements from with location strings
    print DEBUG "looking for PART OF in description: name = $region[$n]{name}: description = $region[$n]{location}{description}\n";
    if ($region[$n]{"type"} ne "gmsh") {
      for my $key ( "location", "initial_location" ) {
        if (nonempty($region[$n]{$key}{"description"}) && $region[$n]{$key}{"description"} =~ /(^|\s+)PART( |_)OF\s+(<(.+?)>)(\s+|$)/i) {
          print "WARNING: using a PART OF specification within the location description string is deprecated (found in region $region[$n]{name}): place ON afterwards instead\n";
          print DEBUG "WARNING: using a PART OF specification within the location description string is deprecated (found in region $region[$n]{name}): place ON afterwards instead\n";
          if (empty($region[$n]{"part_of"})) {
            $region[$n]{"part_of"} = examine_name($3,"regionname");
          } else {
            print "WARNING: part_of region already set: ignoring anyway\n";
            print DEBUG "WARNING: part_of region already set: ignoring anyway\n";
          }
          $region[$n]{$key}{"description"} = $`." ".$';
        }
      }
    }
# process options - from now on the options string can be wiped and does not determine whether equations need re-running
    $region[$n]{"newtstepmin"}='';
    $region[$n]{"newtstepmax"}='';
    my $type = $region[$n]{"type"};
    if (nonempty($region[$n]{"options"})) {
      my $tmp = $region[$n]{"options"};
      $region[$n]{"options"} = '';
      print DEBUG "INFO: before processing region options $type $region[$n]{name} has loaded (unchecked) options of $tmp\n";
      while ($tmp =~ /(^|\,)\s*([^\,]+?)\s*(\,|$)/i) {
        my $option = $2; $tmp = $`.','.$';
        if ($option =~ /^(newtstep(max|min))(\s*=\s*([\+\-\d][\+\-\de]*))$/i) { # integer max/min of newtsteps during which this variable should be updated
          my $option_name = "\L$1";
          my $match;
          if (empty($4)) { 
            $match = "-1";
          } else { 
            $match = "\L$4"; # match is the magnitude of the variable, which needs to be an integer
          }
          if (($type eq "derived" || $type eq "equation") && $region[$n]{"dynamic"}) { # newtstep limiting is only done on equations or deriveds right now
            if ($match < 0) { # a negative values clears this option
              $region[$n]{$option_name} = '';
            } else {
              $region[$n]{$option_name} = $match;
            }
          } else { error_stop("option $option specified for region $type $region[$n]{name} cannot be used for this type of region"); }
        }
      }
    }
  }

#-------------
# add the SYSTEM regions to the start of the region array
# SYSTEM regions are those that would be commonly used by a user and which have a corresponding fortran region entity
  
  unshift(@region,{ name => '<boundary nodes>', type => 'system', centring => 'node' });
  unshift(@region,{ name => '<domain nodes>', type => 'system', centring => 'node' });
  unshift(@region,{ name => '<all nodes>', type => 'system', centring => 'node' });
  unshift(@region,{ name => '<boundaries>', type => 'system', centring => 'face' });
  unshift(@region,{ name => '<domain faces>', type => 'system', centring => 'face' });
  unshift(@region,{ name => '<all faces>', type => 'system', centring => 'face' });
  unshift(@region,{ name => '<boundary cells>', type => 'system', centring => 'cell' });
  unshift(@region,{ name => '<domain>', type => 'system', centring => 'cell' });
  unshift(@region,{ name => '<all cells>', type => 'system', centring => 'cell' });

#-------------
# now enter all INTERNAL regions, now at the end of the array
# INTERNAL regions do not require fortran entities but are hard-coded into the create_fortran sub
# may be used in variable checking
# NOTE!!!! the names for internals are actually perl regexs for these regions, so []'s have to be escaped
  push(@region,{ name => '<celljfaces>', type => 'internal', centring => 'face' });
  push(@region,{ name => '<nobcelljfaces>', type => 'internal', centring => 'face' });
  push(@region,{ name => '<cellicells>', type => 'internal', centring => 'cell' });
  push(@region,{ name => '<cellknodes>', type => 'internal', centring => 'node' });
  push(@region,{ name => '<faceicells>', type => 'internal', centring => 'cell' });
  push(@region,{ name => '<faceknodes>', type => 'internal', centring => 'node' });
  push(@region,{ name => '<nodeicells>', type => 'internal', centring => 'cell' });
  push(@region,{ name => '<adjacentcellicells>', type => 'internal', centring => 'cell' });
  push(@region,{ name => '<nocadjacentcellicells>', type => 'internal', centring => 'cell' });
  push(@region,{ name => '<adjacentfaceicells>', type => 'internal', centring => 'cell' });
  push(@region,{ name => '<adjacentfaceupcell>', type => 'internal', centring => 'cell' });
  push(@region,{ name => '<adjacentfacedowncell>', type => 'internal', centring => 'cell' });
  push(@region,{ name => '<adjacentfaceothercell>', type => 'internal', centring => 'cell' });
  push(@region,{ name => '<upwindfaceicells>', type => 'internal', centring => 'cell' });
  push(@region,{ name => '<downwindfaceicells>', type => 'internal', centring => 'cell' });
  push(@region,{ name => '<glueface>', type => 'internal', centring => 'face' });
  push(@region,{ name => '<lastface>', type => 'internal', centring => 'face' });
  push(@region,{ name => '<noloop>', type => 'internal', centring => '' }); # this special case has no centring
  push(@region,{ name => '<cellkernelregion\[l=0\]>', type => 'internal', centring => 'face' });
  push(@region,{ name => '<cellkernelregion\[l=([123])\]>', type => 'internal', centring => 'cell' });
  push(@region,{ name => '<cellkernelregion\[l=([4567])\]>', type => 'internal', centring => 'node' });
  push(@region,{ name => '<facekernelregion\[l=([0123456])\]>', type => 'internal', centring => 'cell' });
  push(@region,{ name => '<nodekernelregion\[l=([0123])\]>', type => 'internal', centring => 'cell' });
  push(@region,{ name => '<separationcentre(\d*)>', type => 'internal', centring => 'cell' });

#-------------
# set fortran numbers now for the above user, system and internal variables

  $fortran_regions = 0; # this is the total number of regions that need to be allocated by this script within the fortran
  foreach $n ( 0 .. $#region ) {
    if ($region[$n]{'type'} eq 'internal') { $region[$n]{"fortran"} = 0; } else { $fortran_regions++; $region[$n]{"fortran"} = $fortran_regions; } # only internal regions don't have a corresponding region in the fortran code
    if (empty($region[$n]{"newtstepmin"})) { $region[$n]{"newtstepmin"}=''; }
    if (empty($region[$n]{"newtstepmax"})) { $region[$n]{"newtstepmax"}=''; }
  }

#-------------
# run through user regions adding any unrecognised part_of regions that have been specifically specified to the list of regions
# NB: part_of names have also been previously run through examine_name
# also finalise user variable

  foreach $n ( 0 .. $#region ) {
    if (empty($region[$n]{"user"})) { $region[$n]{"user"} = 0; }
    if (nonempty($region[$n]{"part_of"}) && $region[$n]{"user"}) {
      my $nfound = check_region_and_add_if_not_there($region[$n]{"part_of"},$region[$n]{"centring"},"part_of region from $region[$n]{centring} centred region $region[$n]{name}");
      if ($region[$nfound]{"type"} eq "internal") { error_stop("a part_of region cannot be an internal region: region = $region[$n]{name}: part_of region = $region[$nfound]{name}"); }
    } elsif (nonempty($region[$n]{"part_of"})) {
      error_stop("a part_of region ($region[$n]{part_of}) cannot be specified for region type $region[$n]{type}, which is being attempted for region $region[$n]{name}");
    }
  }
      
}
#-------------------------------------------------------------------------------
# here we first run through all of the variables checking that no regions have been left out, and then process the description strings for the variables

sub process_regions {

  use strict;
  use Data::Dumper;
  $Data::Dumper::Terse = 1;
  $Data::Dumper::Indent = 0;
  my ($n, $n2, $type, $mvar);

#-------------
# run through variables finding any other regions that have not been previously specified and hence must be brought in via gmsh
# NB: the region name here was previously run through examine_name when the region was specified within the variable definition line
# at the same time record region fortran_number with variable

  foreach $type (@user_types,"someloop") {
    foreach $mvar ( 1 .. $m{$type} ) {
      if (empty($variable{$type}[$mvar]{"region"})) { next; }
      if ($variable{$type}[$mvar]{"centring"} eq 'none') { next; }
      print DEBUG "INFO: search for $variable{$type}[$mvar]{centring} region $variable{$type}[$mvar]{region} on which variable $variable{$type}[$mvar]{name} is defined:\n";
      my $nfound = check_region_and_add_if_not_there($variable{$type}[$mvar]{"region"},$variable{$type}[$mvar]{"centring"},"region associated with $variable{$type}[$mvar]{centring} $type variable $variable{$type}[$mvar]{name}");
    }
  }

#-------------
# now process the location statements, at this stage setting location types and looking for any other regions
# one problem with regions extracted from location statements is that we can't work out their centring

  foreach $n ( 0 .. $#region ) {
    if (!($region[$n]{"user"})) { next; } # this will ignore any regions that shouldn't have atleast a valid location description
# deal with user regions having no location description defined
    if (empty($region[$n]{'location'}{'description'})) { error_stop("$region[$n]{type} region $region[$n]{name} has no location description defined: all regions (except gmsh) ".
      "entered in the arb file must have a location description"); }
# set initial_location if required
    $type = $region[$n]{'type'};
    if (empty($region[$n]{"initial_location"}{"description"}) ) {
      if ($type eq "transient" || $type eq "newtient") {
        $region[$n]{"initial_location"}{"description"} = $region[$n]{"location"}{"description"};
        print "INFO: initial_location of $type region $region[$n]{name} not set - defaulting to location $region[$n]{location}{description}\n";
        print DEBUG "INFO: initial_location of $type region $region[$n]{name} not set - defaulting to location $region[$n]{location}{description}\n";
      } else {
        $region[$n]{"initial_location"}{"description"} = "";
      }
    }

# cycle through (possibly both) location description strings
    for my $key ( "location", "initial_location" ) {
      if (empty($region[$n]{$key}{"description"})) { next; }
      $region[$n]{$key}{"type"} = location_description_scan($region[$n]{$key}{"description"},"type",$n);
      @{$region[$n]{$key}{"regionnames"}} = location_description_scan($region[$n]{$key}{"description"},"regionnames",$n);
      @{$region[$n]{$key}{"regioncentrings"}} = location_description_scan($region[$n]{$key}{"description"},"regioncentrings",$n);
      @{$region[$n]{$key}{"floats"}} = location_description_scan($region[$n]{$key}{"description"},"floats",$n);
      @{$region[$n]{$key}{"integers"}} = location_description_scan($region[$n]{$key}{"description"},"integers",$n);
      @{$region[$n]{$key}{"variablenames"}} = location_description_scan($region[$n]{$key}{"description"},"variablenames",$n);
      @{$region[$n]{$key}{"variablecentrings"}} = location_description_scan($region[$n]{$key}{"description"},"variablecentrings",$n);

# check that location type is consistent with dynamic
      if (!($region[$n]{"dynamic"}) && ($region[$n]{$key}{"type"} eq "variable" || $region[$n]{$key}{"type"} eq "separation")) {
        error_stop("region location type $region[$n]{$key}{type} used in $key is inconsistent with the static region $region[$n]{name}")
      }

# now look at the region names, checking that these regions are known (adding as gmsh if not) and checking on centring
      if (@{$region[$n]{$key}{"regionnames"}}) {
        for $n2 ( 0 .. $#{$region[$n]{$key}{"regionnames"}}) {
          my $match_name = $region[$n]{$key}{"regionnames"}[$n2];
          my $match_centring = $region[$n]{$key}{"regioncentrings"}[$n2];
          my $nfound = check_region_and_add_if_not_there($match_name,$match_centring,"$key description string for $region[$n]{centring} region $region[$n]{name}"); 
          if ($region[$nfound]{"type"} eq "internal") { error_stop("a region within a location description cannot be an internal region: region = $region[$n]{name}: region within location description = $region[$nfound]{name}"); }
          push(@{$region[$n]{$key}{"regions"}},$region[$nfound]{"fortran"}); # add this fortran region number to list of location_regions
        }
      }

# also need to check that variable centrings are consistent
      if (@{$region[$n]{$key}{"variablenames"}}) {
        for $n2 ($#{$region[$n]{$key}{"variablenames"}}) {
          my $match_name = $region[$n]{$key}{"variablenames"}[$n2];
          my $match_centring = $region[$n]{$key}{"variablecentrings"}[$n2];
          $mvar = -1;
          TYPE_LOOP: foreach $type (@user_types) {
            foreach my $mvarsearch ( 1 .. $m{$type} ) {
              if ($variable{$type}[$mvarsearch]{"name"} ne $match_name) { next; } else { $mvar = $mvarsearch; last TYPE_LOOP; }
            }
          }
          if ($mvar < 0) { error_stop("the variable $match_name that is used in a location statement for region $region[$n]{name} is not known"); }
          if (nonempty($match_centring) && $match_centring ne $variable{$type}[$mvar]{"centring"}) {
            error_stop("the variable $match_name that is used in a location statement for region $region[$n]{name} has inconsistent centring: ".
              "region location context centring = $match_centring: variable centring = $variable{$type}[$mvar]{centring}");
          }
          push(@{$region[$n]{$key}{"variables"}},$variable{$type}[$mvar]{"fortran_number"}); # add this fortran number to list of location_variables
        }
      }

# now check some requirements of each location type
# look at floats for at, within or normal location types
      if ($region[$n]{$key}{"type"} eq "at") {
        if ($#{$region[$n]{$key}{"floats"}} > 2) {
          error_stop("more than 3 floats are specified in an $region[$n]{$key}{type} statement for region $region[$n]{name}");
        } elsif ($#{$region[$n]{$key}{"floats"}} < 2) {
          print "WARNING: less than 3 floats are specified in an $region[$n]{$key}{type} statement for region $region[$n]{name}: using zero for the uninitialised ones\n";
          while ($#{$region[$n]{$key}{"floats"}} < 2) { push(@{$region[$n]{$key}{"floats"}},"0.d0"); }
        }
      }
      if ($region[$n]{$key}{"type"} eq "withinbox") {
        if ($#{$region[$n]{$key}{"floats"}} > 5) {
          error_stop("more than 6 floats are specified in a $region[$n]{$key}{type} statement for region $region[$n]{name}");
        } elsif ($#{$region[$n]{$key}{"floats"}} < 5) {
          print "WARNING: less than 6 floats are specified in a $region[$n]{$key}{type} statement for region $region[$n]{name}: using zero for the uninitialised ones\n";
          while ($#{$region[$n]{$key}{"floats"}} < 5) { push(@{$region[$n]{$key}{"floats"}},"0.d0"); }
        }
      }
# a normal region is defined by the three normal components plus a maximum dot-product deviation from this vector
      if ($region[$n]{$key}{"type"} eq "normal") {
        if ($#{$region[$n]{$key}{"floats"}} > 3) {
          error_stop("more than 4 floats are specified in a $region[$n]{$key}{type} statement for region $region[$n]{name}");
        } 
        if ($#{$region[$n]{$key}{"floats"}} < 2) {
          print "WARNING: less than 3 floats are specified in a $region[$n]{$key}{type} statement for region $region[$n]{name}: using zero for the uninitialised normal components\n";
          while ($#{$region[$n]{$key}{"floats"}} < 2) { push(@{$region[$n]{$key}{"floats"}},"0.d0"); }
        }
        if ($#{$region[$n]{$key}{"floats"}} == 2) {
          print "INFO: a normal deviation float (the fourth component) is not specified in a $region[$n]{$key}{type} statement for region $region[$n]{name}: using 1.d-3 for this component\n";
          push(@{$region[$n]{$key}{"floats"}},"1.d-3");
        }
        if ($region[$n]{'centring'} ne 'face') {
          error_stop("the normal region $region[$n]{name} must be face centred");
        }
      }
# look at variable type
      if ($region[$n]{$key}{"type"} eq "variable") {
        if (@{$region[$n]{$key}{"variables"}} != 1) {
          error_stop("there is not a single variable specified in a $region[$n]{$key}{type} statement for region $region[$n]{name}: variables = @{$region[$n]{$key}{variables}}");
        }
      }

    }

  }

#-------------
# check that no user defined regions have the names of system or internal regions

  foreach $n ( 0 .. $#region ) {
    if ($region[$n]{"type"} eq "internal" || $region[$n]{"type"} eq "system") { next; }
    foreach $n2 ( 0 .. $#region ) {
      if ($region[$n]{"type"} ne "internal" && $region[$n]{"type"} ne "system") { next; }
# if we are here then $n is a user or gmsh region, and $n2 is a internal or system region
      if (match_region($n2,$region[$n]{"name"})) { # use match_region that copes with the (possible) internal region $n2 being a regex
        error_stop("an attempt is being made to name a region using a $region[$n2]{type} region name: region = $region[$n]{name}: type = $region[$n]{type}");
      }
    }
  }

#-------------
# update any missing variables, set part_of region and fortran number

  foreach $n ( 0 .. $#region ) {
    $type = $region[$n]{'type'};
    if (empty($region[$n]{"location"}{"description"})) { $region[$n]{"location"}{"description"} = "$type"; } # user regions have already been checked
    if (empty($region[$n]{"user"})) { $region[$n]{"user"} = 0; } # already set for user regions
    if (empty($region[$n]{"dynamic"})) { $region[$n]{"dynamic"} = 0; } # already set for user regions
# check that only user region names have relative step indices
    if ($type eq 'gmsh' && ( examine_name($region[$n]{"name"},'regionname') ne $region[$n]{"name"} ||
      examine_name($region[$n]{"name"},'rindex') != 0 )) { error_stop("gmsh region names cannot have any r indices specified: name = $region[$n]{name}"); }
    if (empty($region[$n]{'part_of'}) && $region[$n]{"user"}) {
# part_of regions default to largest static region based on size if not specified
# note, these system regions will come at the start so don't need when each is calculated
      if ($region[$n]{'centring'} eq 'cell') {
        $region[$n]{'part_of'} = '<all cells>';
      } elsif ($region[$n]{'centring'} eq 'face') {
        $region[$n]{'part_of'} = '<all faces>';
      } else {
        $region[$n]{'part_of'} = '<all nodes>';
      }
    }
# and find fortran number for part_of region
    if ($region[$n]{"user"}) {
      my $nfound = find_region($region[$n]{'part_of'});
      if ($nfound < 0) { error_stop("(INTERNAL ERROR): the ON region $region[$n]{part_of} for region $region[$n]{name} is not a valid region"); }
# check order and suitability of part_of region update for static regions
      if (!($region[$n]{"dynamic"})) { 
        if ($region[$nfound]{"dynamic"}) {
          error_stop("cannot use a dynamic region $region[$nfound]{name} as a ON for a static region $region[$n]{name}");
        }
        if ($region[$nfound]{"user"}) {
# if we are here then the region and its part_of region are both static user regions - check order of evaluation
          if ($nfound > $n) {
            error_stop("the ON region $region[$nfound]{name} for static region $region[$n]{name} is being evaluated after it: you need to reverse the order");
          }
        }
      }
      $region[$n]{"part_of_fortran"}=$region[$nfound]{"fortran"};
    }
  }

#-------------
# find parent regions
# for static regions, the parent is the same as the part_of, both of which must be static
# for dynamic regions, part_of can be dynamic, but parent must be the inclusive static region

  foreach $n ( 0 .. $#region ) {
    if (!($region[$n]{"user"})) { next; }
    if ($region[$n]{"dynamic"}) {
      my $nlast = $n;
      while ($region[$nlast]{"dynamic"}) {
        my $nfound = find_region($region[$nlast]{'part_of'});
				if ($nfound < 0) { die "INTERNAL ERROR: the ON region $region[$nlast]{part_of} for dynamic region $region[$n]{name} is not a valid region"; }
        $nlast = $nfound;
        print DEBUG "INFO: in process of finding parent region for $region[$n]{name}: found part_of region $region[$nfound]{name}\n";
      }
      print "INFO: found parent region $region[$nlast]{name} for dynamic region $region[$n]{name}\n";
      print DEBUG "INFO: found parent region $region[$nlast]{name} for dynamic region $region[$n]{name}\n";
      $region[$n]{"parent"} = $region[$nlast]{"name"};
      $region[$n]{"parent_fortran"} = $region[$nlast]{"fortran"};
    } else {
# for static regions parent=part_of
# order dependencies have been checked already as part_ofs
      $region[$n]{"parent"} = $region[$n]{"part_of"};
      $region[$n]{"parent_fortran"} = $region[$n]{"part_of_fortran"};
    }
  }

#-------------
# now place known region indices back into variables

  foreach $type (@user_types,"initial_transient","initial_newtient","someloop","compound") {
    foreach $mvar ( 1 .. $m{$type} ) {
      $variable{$type}[$mvar]{"update_region_number"} = 0;
      $variable{$type}[$mvar]{"update_region"} = '';
      if (empty($variable{$type}[$mvar]{"region"}) || $variable{$type}[$mvar]{"centring"} eq 'none') {
        $variable{$type}[$mvar]{"region_number"} = 0;
      } else {
        my $nfound = find_region($variable{$type}[$mvar]{"region"});
        if ($region[$nfound]{"dynamic"}) { # if the region is dynamic, store this in the update_region and swap the region for the parent region
          if ($type =~ /unknown|equation/) { error_stop("cannot define a $type variable ($variable{$type}[$mvar]{name}) on a dynamic region ($region[$nfound]{name})"); }
          $variable{$type}[$mvar]{"update_region"} = $variable{$type}[$mvar]{"region"};
          $variable{$type}[$mvar]{"update_region_number"} = $region[$nfound]{"fortran"};
          $variable{$type}[$mvar]{"region"} = $region[$nfound]{"parent"};
          $variable{$type}[$mvar]{"region_number"} = $region[$nfound]{"parent_fortran"};
        } else {
          $variable{$type}[$mvar]{"region_number"} = $region[$nfound]{"fortran"};
# now update_region is set generally to be equal to region for static regions, and different for dynamic regions (update_region is the dynamic region)
          $variable{$type}[$mvar]{"update_region"} = $variable{$type}[$mvar]{"region"};
          $variable{$type}[$mvar]{"update_region_number"} = $variable{$type}[$mvar]{"region_number"};
        }
      }
    }
  }

#-------------
# create region sub_string for all regions that require fortran allocations

  if ($fortran_regions > 0) {
    $sub_string{"allocate_regions"}="allocate(region($fortran_regions))\n";
    foreach $n ( 0 .. $#region ) {
      my $m = $region[$n]{"fortran"};
      if ($m) {
        $sub_string{"allocate_regions"}=$sub_string{"allocate_regions"}.
          "\n! region $region[$n]{name} of $region[$n]{type} type\n".
          "region($m)%name = \"$region[$n]{name}\"\n".
          "region($m)%centring = \"$region[$n]{centring}\"\n". # every region written to fortran has a centring
          "region($m)%type = \"$region[$n]{type}\"\n".
          "region($m)%relstep = ".examine_name($region[$n]{"name"},"rindex")."\n".
          "region($m)%dynamic = ".fortran_logical_string($region[$n]{"dynamic"})."\n"; # dynamic logical

        if ($region[$n]{"user"}) {
          $sub_string{"allocate_regions"}=$sub_string{"allocate_regions"}.
            "region($m)%part_of = $region[$n]{part_of_fortran}\n".
            "region($m)%parent = $region[$n]{parent_fortran}\n";
        }

        for my $key ( "location", "initial_location" ) {
          if (nonempty($region[$n]{$key}{"description"})) {
            $sub_string{"allocate_regions"}=$sub_string{"allocate_regions"}.
              "region($m)%".$key."%active = .true.\n".
              "region($m)%".$key."%description = \"$region[$n]{$key}{description}\"\n";
            if (nonempty($region[$n]{$key}{"type"})) {
              $sub_string{"allocate_regions"}=$sub_string{"allocate_regions"}.
                "region($m)%".$key."%type = \"$region[$n]{$key}{type}\"\n";
            }
            for my $key2 ( "variables", "regions", "integers", "floats" ) {
              if (nonempty(@{$region[$n]{$key}{$key2}})) {
                $sub_string{"allocate_regions"}=$sub_string{"allocate_regions"}.
                  "allocate(region($m)%".$key."%".$key2."(".scalar($#{$region[$n]{$key}{$key2}}+1)."))\n".
                  "region($m)%".$key."%".$key2." = [".join(',',@{$region[$n]{$key}{$key2}})."]\n";
              }
            }
          } else {
            $sub_string{"allocate_regions"}=$sub_string{"allocate_regions"}.
              "region($m)%".$key."%active = .false.\n".
              "region($m)%".$key."%description = 'inactive'\n";
          }
        }
      }
    }
  }

#-------------
# print debugging info

  print DEBUG "=================================================================\n";
  print DEBUG "SUMMARY OF ALL REGIONS KNOWN TO SETUP_EQUATIONS\n";
  foreach $n ( 0 .. $#region ) {
    print DEBUG "$n $region[$n]{name}: ".Dumper($region[$n])."\n";
  }
  print DEBUG "=================================================================\n";

}

#-------------------------------------------------------------------------------
# check if a region name exists:
# if it doesn't and it is addable, add it to the list, with its centring (if specified)
# if it does, check that the centring is consistent

sub check_region_and_add_if_not_there {

# on input:
  my $region_name=$_[0]; # region name, already passed through examine_name
  my $centring=$_[1]; # if non-empty, centring this region must be
  my $context=$_[2]; # context to be used for output messages
# on output:
# $_[0] region number

  my $nfound = find_region($region_name);
  if ($nfound >= 0) {
    print DEBUG "INFO: a previously defined region from the context of $context has been found: region = $region[$nfound]{name}:".
      " centring = $region[$nfound]{centring}\n";
    if (nonempty($region[$nfound]{"centring"})) {
# if centring is defined but not consistent, then this is an error
      if (nonempty($centring) && $region[$nfound]{"centring"} ne $centring) {
        error_stop("the centring of a region and the context in which is is used don't match: region = $region[$nfound]{name}: region centring = $region[$nfound]{centring}: context = $context");
      }
    } elsif ($region[$nfound]{"type"} ne "internal" && nonempty($centring)) { # don't try to set internal region centring (specifically for <noloop>)
# if centring is not defined then set it based on the variable
      $region[$nfound]{"centring"} = $centring;
      print DEBUG "INFO: setting centring of region $region[$nfound]{name} to $region[$nfound]{centring} based on context $context\n";
    }
  } else {
    $fortran_regions++;
    push(@region,{ name => $region_name, type => "gmsh", user => 0, centring => $centring, fortran => $fortran_regions, newtstepmax => '', newtstepmin => '' });
    $region[$#region]{"location"}{"description"} = "gmsh from context $context";
    print DEBUG "INFO: no previously defined region $region_name was found in the context of $context: pushing new gmsh $centring region\n";
    $nfound = $#region;
  }

  return ($nfound);

}
#-------------------------------------------------------------------------------
# scans a location string and finds certain things
# on input:
# $_[0] = location description string
# $_[1] = action = type|regionnames|floats|integers|variablenames|regioncentrings|variablecentrings|options
# $_[2] = region_number, used for error messages

sub location_description_scan {

  use strict;
  my $location=$_[0];
  my $action=$_[1];
  my $n=$_[2];
  my ($type,$constant);
  my @regionnames=(); # list of region names referred to in the location
  my @regioncentrings=(); # list of region centrings referred to in the location
  my @floats=(); # list of any floats for the location, as double precision
  my @integers=(); # list of any integers for the location
  my @variablenames=(); # list of any variable names used in the location
  my @variablecentrings=(); # list of variable centrings referred to in the location
  my $options=''; # whatever is in the operator's option that isn't s, dumped in a string
  my $line=$location; # line is actually split up during the deconstruction

  print DEBUG "INFO: within location_description_scan with location = $location: action = $action: region number = $n: region name = $region[$n]{name}\n";
  if ($line =~ /^\s*(associated( |)with|boundary( |)of|surrounds|domain( |)of|union|intersection|compound|common|at|within( |)box|normal|gmsh|variable|all|none|expand)(\[(.*?)\]|)(\(|\s|$)/i) {
#                   1          2                3                      4                                                  5                                         6  7        8 
    $type = "\L$1";
    $line = $';
    my $next = $8; # the character following the match
    if (nonempty($7)) { $options = $7; }
    if ($type eq "union") { $type = "compound"; }
    if ($type eq "intersection") { $type = "common"; }
    if ($type =~ / /) { $type =~ s/ //g; print "WARNING: for consistency with variable operators, the use of spaces in region location operator names has been deprecated: run the individual words together instead as in $type (found in $region[$n]{name})\n"; }
    if ($next eq " ") { print "WARNING: for consistency with variable operators, operators within region location descriptions should now have their arguments inclosed in brackets, as in $type(arguments) (found in $region[$n]{name})\n"; }
    if ($next eq "(") { if (!($line =~ /\)\s*$/)) { error_stop("missing closing bracket on $type operator for region $region[$n]{name} location = $location"); } else { $line = $`; } } # remove trailing bracket
    print DEBUG "location type = $type: options = $options\n";
  } else {
    error_stop("location type not recognised from the following location string used with region $region[$n]{name}: location = $location");
  }

# ref: location ref: region location

# move through each type of location type
  if ($action eq "type") { print DEBUG "returning from location_description_scan with type\n"; return ($type); }

# all other actions
  if ($type eq "expand") {
# extract region name from operator contents
    if ($line =~ /^\s*(<(.+?)>)\s*/) {
      push(@regionnames,examine_name($1,"regionname"));
      push(@regioncentrings,"cell"); # must be cell centred for now
      $line = $';
    } else {
      error_stop("cannot find region name in the $type operator for region $region[$n]{name}: location = $location");
    }
# and extract maxseparation from options, assuming a single separation level if one isn't specified
    if ($options =~ /(^|\,)\s*(max|maximum)separation\s*=\s*(\d+?)\s*(\,|$)/i) { push(@integers,$3); } else { push(@integers,1); }
    if ($options =~ /(^|\,)\s*faceseparation\s*(\,|$)/i) { $integers[0]=-$integers[0]; } # use a negative integer here to indicate that face separation should be used in the loop
  } elsif ($type eq "variable") {
# just a single variable name required
    if ($line =~ /^\s*(<(.+?)>)\s*/) {
      push(@variablenames,examine_name($1,"name"));
      push(@variablecentrings,$region[$n]{"centring"}); # centring of variable needs to be consistent with the region centring
      $line = $';
    } else {
      error_stop("variable in the $type operator for region $region[$n]{name} cannot be recognised: location = $location");
    }
  } elsif ($type =~ /^(associatedwith|boundaryof|domainof|surrounds)$/) {
# single region required, no delimiters used
    if ($line =~ /^\s*(<(.+?)>)\s*/) {
      push(@regionnames,examine_name($1,"regionname"));
      push(@regioncentrings,""); # centrings are unknown from these statements
      $line = $';
    } else {
      error_stop("region in the $type operator for region $region[$n]{name} cannot be recognised: location = $location");
    }
  } elsif ($type =~ /^(compound|common)$/) {
# multiple regions required, which may be delimited by +|,|space, and additionally for compound|union, -
    while ($line =~ /^\s*(\+|-|,|)\s*(<(.+?)>)\s*/) {
      push(@regionnames,examine_name($2,"regionname"));
      push(@regioncentrings,$region[$n]{"centring"}); # centrings for these regions must be consistent with that of the calling region
      $line=$';
      if ($1 eq "-") { $constant = -1; } else { $constant = 1; }
      if ($type eq "compound") {
        push(@integers,$constant);
      } elsif ( $constant eq -1 ) {
        error_stop("regions in the $type operator for region $region[$n]{name} should be listed between commas, not minuses: location = $location");
      }
    }
  } elsif ($type =~ /^(at|withinbox|normal)$/) {
# pull location boundaries out from these regions
    while ($line =~ /^\s*(,|)\s*([\+\-\d\.][\+\-\ded\.]*)\s*(,|)\s*/i) { # numbers must start with either +-. or a digit, so options cannot start with any of these
      $line = $'; $constant = "\L$2";
      if ($constant !~ /\d/) { error_stop("constants in the $type location for region $region[$n]{name} must be valid numbers: location = $location: line = $line"); }
      $constant =~ s/e/d/;
      if ($constant !~ /d/) { $constant = $constant."d0"; }
      if ($constant !~ /\./) { $constant =~ s/d/.d/; }
      push(@floats,$constant);
    }
# TODO: check on number of floats?
  } else {
    $line=""; # remove everything that isn't from this type to signal no error
  }

  if ($line) { error_stop("trailing material $line found in the $type location for region $region[$n]{name}: location = $location"); }

  if ($action eq "regionnames") {
    print DEBUG "returning from location_description_scan with regionnames = @regionnames\n";
    return (@regionnames);
  } elsif ($action eq "regioncentrings") {
    print DEBUG "returning from location_description_scan with regioncentrings = @regioncentrings\n";
    return (@regioncentrings);
  } elsif ($action eq "integers") {
    print DEBUG "returning from location_description_scan with integers = @integers\n";
    return (@integers);
  } elsif ($action eq "floats") {
    print DEBUG "returning from location_description_scan with floats = @floats\n";
    return (@floats);
  } elsif ($action eq "variablenames") {
    print DEBUG "returning from location_description_scan with variablenames = @variablenames\n";
    return (@variablenames);
  } elsif ($action eq "variablecentrings") {
    print DEBUG "returning from location_description_scan with variablecentrings = @variablecentrings\n";
    return (@variablecentrings);
  } elsif ($action eq "options") {
    print DEBUG "returning from location_description_scan with options = $options\n";
    return ($options);
  }

  error_stop("unknown $action for region $region[$n]{name}: location = $location");

}
#-------------------------------------------------------------------------------
# sees if a given region name ($_[0]) matches that of another region,
#  if so returns region number
#  if not returns -1

sub find_region {

  use strict;
  my $match=-1;
  my $region_to_find=$_[0];

  foreach my $n ( 0 .. $#region ) {
    if (match_region($n,$region_to_find)) {
      $match = $n;
      last;
    }
  }
  
  return ($match);

}
#-------------------------------------------------------------------------------
# sees if a given region name ($_[1]) matches that of region[$_[0]], taking care of the name being a possible regex

sub match_region {

  use strict;
  
  if ( ( $region[$_[0]]{'type'} eq 'internal' && $_[1] =~ /$region[$_[0]]{name}/ ) ||
       ( $region[$_[0]]{'type'} ne 'internal' && $_[1] eq $region[$_[0]]{"name"}) ) {
    return (1);
  } else {
    return (0);
  }

}
#-------------------------------------------------------------------------------
# this routine now takes the raw information contained in the $asread_variable array and creates the variable{$type}[$mvar] hash/array used in the rest of the code,
#  checking that enough definitions and the correct options have been specified at the same time

sub organise_user_variables {

  use strict;
  my ($type, $name, $centring, $mvar, $option, $option_name, $repeats, $masread, $n, $tmp, $match, $condition);

  print DEBUG "INFO: sub organise_user_variables\n";

# distill asread_variable into variables of different types, while at the same time, checking that each variable has a type, centring and region

  foreach $masread ( 0 .. $#asread_variable ) {

    $name = $asread_variable[$masread]{"name"};
    print DEBUG "INFO: creating variable from asread_variable: name = $name: masread = $masread\n";

# check a type was defined - must be - no defaults
    if (empty($asread_variable[$masread]{"type"})) { error_stop("variable $asread_variable[$masread]{name} has no type.  Every variable must have a type."); };
    $type = $asread_variable[$masread]{"type"};

    $m{$type}++; # these were previously set to 0 to represent empty types
    $m{"user"} = $masread+1; # this stores the total number of user defined variables
    $mvar = $m{$type}; # for convienience
    $asread_variable[$masread]{"mvar"} = $mvar; # and save this back in the original structure
    $variable{$type}[$mvar] = dclone($asread_variable[$masread]); # for here on work with variable{$type} hash rather than asread_variable array
    delete $variable{$type}[$mvar]{"type"}; # delete this duplication
    $variable{$type}[$mvar]{"masread"} = $masread; # save the asread index for this variable, which represents the order that the variables appear in the file
    $variable{$type}[$mvar]{"otype"} = $type; # save original and type and mvar for this expression
    $variable{$type}[$mvar]{"omvar"} = $mvar;
    $variable{$type}[$mvar]{"newtstepmin"} = '';
    $variable{$type}[$mvar]{"newtstepmax"} = '';
# now set magnitude constants
    if ($type eq "unknown" || $type eq "equation") {
      $variable{$type}[$mvar]{"magnitude_constant"} = 0; # specifies that it is not to be set from a none-centred constant value
    }

# check a centring was defined, and if not, try to work out a default
    if (empty($variable{$type}[$mvar]{"centring"})) { $variable{$type}[$mvar]{"centring"} = 'none';
      print "INFO: no centring for $type variable $variable{$type}[$mvar]{name} was defined: defaulting to none\n";
      print DEBUG "INFO: no centring for $type variable $variable{$type}[$mvar]{name} was defined: defaulting to none\n";
    }
    $centring = $variable{$type}[$mvar]{"centring"};

# check whether a region for the variable was defined, and if not, try to guess
    if (empty($variable{$type}[$mvar]{"region"})) { # set default region for variable
      if ($centring ne "none") {
        if ($centring eq "cell") {
          if ($type eq "equation") {
            $variable{$type}[$mvar]{"region"} = "<domain>"; # default cell region for equations
          } else {
            $variable{$type}[$mvar]{"region"} = "<all cells>"; # default cell region
          }
        } elsif ($centring eq "face") {
          if ($type eq "equation") {
            $variable{$type}[$mvar]{"region"} = "<boundaries>"; # default face region for equations
          } else {
            $variable{$type}[$mvar]{"region"} = "<all faces>"; # default face region
          }
        } else { # elsif ($centring eq "node") {
          $variable{$type}[$mvar]{"region"} = "<all nodes>"; # default node region
        }
        print "INFO: region for $type variable $variable{$type}[$mvar]{name} not set:  Defaulting to $variable{$type}[$mvar]{region} based on $centring centring\n";
        print DEBUG "INFO: region for $type variable $variable{$type}[$mvar]{name} not set:  Defaulting to $variable{$type}[$mvar]{region} based on $centring centring\n";
      }
    } elsif ( $centring eq "none" ) {
      error_stop("attempting to set region to $variable{$type}[$mvar]{region} for none centred variable $variable{$type}[$mvar]{name}");
    } else {
      print "INFO: $type $variable{$type}[$mvar]{name} has $variable{$type}[$mvar]{centring} region = $variable{$type}[$mvar]{region}\n"; 
      print DEBUG "INFO: $type $variable{$type}[$mvar]{name} has $variable{$type}[$mvar]{centring} region = $variable{$type}[$mvar]{region}\n"; 
    }

# check whether the appropriate equations or numerical constants have been read in and
# check on equation/initial_equation/constant_list consistency and write out constant statements to the fortran input file
    if (nonempty($variable{$type}[$mvar]{"constant_list"})) { # this indicates that this constant is to be numerically read
      if ($type eq "constant") { 
        if ($variable{$type}[$mvar]{"centring"} eq "none") {
          print FORTRAN_INPUT "CONSTANT $variable{$type}[$mvar]{name} $variable{$type}[$mvar]{constant_list}[0]\n";
          print DEBUG "INFO: for $type $name writing numerical constants to the fortran input file: constant = $variable{$type}[$mvar]{constant_list}[0]\n";
        } else {
          if (nonempty($variable{$type}[$mvar]{"region_list"})) { # this indicates that this is a REGION_CONSTANT
# first preceed region_list and constant_list with zero value for entire region
            unshift(@{$variable{$type}[$mvar]{"constant_list"}},"0.d0");
          }
          unshift(@{$variable{$type}[$mvar]{"region_list"}},$variable{$type}[$mvar]{"region"});
          foreach $n ( 0 .. $#{$variable{$type}[$mvar]{"region_list"}} ) {
            print FORTRAN_INPUT "CONSTANT $variable{$type}[$mvar]{name} $variable{$type}[$mvar]{constant_list}[$n] ON $variable{$type}[$mvar]{region_list}[$n] \n";
          }
          print DEBUG "INFO: for $type $name writing numerical constants to the fortran input file: regions and constants = @{$variable{$type}[$mvar]{region_list}} @{$variable{$type}[$mvar]{constant_list}}\n";
        }
      } else {
# don't think that the code structure above actually allows this
        error_stop("a constant_list (ie, a numerical value specification) has been given for $type variable $name, indicating that a variable ".
          "that was previously a constant has been redefined as a $type, however an appropriate equation has not been specified");
      }
    } elsif (empty($variable{$type}[$mvar]{"equation"})) {
      if ($type eq "unknown") {
        print "NOTE: applying default initial value of 1.d0 to unknown $name\n";
        print DEBUG "NOTE: applying default initial value of 1.d0 to unknown $name\n";
        $variable{$type}[$mvar]{"equation"} = "1.d0";
      } else {
        error_stop("no equation has been given for $type variable $variable{$type}[$mvar]{name}.  Every $type variable needs either an equation or numerical value (for constants only)");
      }
    } else { # a transient or newtient must have an equation specified to be here
# if initial equation isn't specified for a transient/newtient
      if (($type eq "transient" || $type eq "newtient") && empty($variable{$type}[$mvar]{"initial_equation"})) {
        if (($variable{$type}[$mvar]{"rindex"} > 0 && $type eq "transient") || $type eq "newtient" || defined($variable{$type}[$mvar]{"initial_equation"}) ) {
# if transient with r>0, or newtient, or initial_equation was specified as an empty variable (""), set initial_equation=equation
          print "INFO: applying initial value given by the update equation to $type $name\n";
          print DEBUG "INFO: applying initial value given by the update equation to $type $name\n";
          $variable{$type}[$mvar]{"initial_equation"} = $variable{$type}[$mvar]{"equation"};
        } else {
# otherwise apply 0.d0
          print "INFO: applying default initial equation of \"0.d0\" to current relstep (r=0) $type $name\n";
          print DEBUG "INFO: applying default initial equation of \"0.d0\" to current relstep (r=0) $type $name\n";
          $variable{$type}[$mvar]{"initial_equation"} = "0.d0";
        }
      }
      if ($type eq "transient" || $type eq "newtient") {
        print DEBUG "INFO: for $type $name: initial_equation = $variable{$type}[$mvar]{initial_equation}: equation = $variable{$type}[$mvar]{equation}\n";
      } else {
        print DEBUG "INFO: for $type $name: equation = $variable{$type}[$mvar]{equation}\n";
      }
    }

# calculate maximum repeats of the definitions, and centring and type changes
    foreach $repeats ( keys(%statement_repeats) ) {
      $statement_repeats{$repeats} = max($statement_repeats{$repeats},$variable{$type}[$mvar]{$repeats});
    }
      
# print some summary stuff now about the single line read
    print "INFO: formed user variable $type [$mvar]: name = $name: centring = $variable{$type}[$mvar]{centring}: rindex = $variable{$type}[$mvar]{rindex}: region = $variable{$type}[$mvar]{region}: multiplier = $variable{$type}[$mvar]{multiplier}: units = $variable{$type}[$mvar]{units}: definitions = $variable{$type}[$mvar]{definitions}: typechanges = $variable{$type}[$mvar]{typechanges}: centringchanges = $variable{$type}[$mvar]{centringchanges}\n";
    print DEBUG "INFO: formed user variable $type [$mvar]: name = $name: centring = $variable{$type}[$mvar]{centring}: rindex = $variable{$type}[$mvar]{rindex}: region = $variable{$type}[$mvar]{region}: multiplier = $variable{$type}[$mvar]{multiplier}: units = $variable{$type}[$mvar]{units}: definitions = $variable{$type}[$mvar]{definitions}: typechanges = $variable{$type}[$mvar]{typechanges}: centringchanges = $variable{$type}[$mvar]{centringchanges}\n";

# process variable options, removing clearoptions statements and creating individual variable options lists
    if ($asread_variable[$masread]{"options"} =~ /.*(^|\,)\s*clearoptions\s*(\,|$)/i) { # match the last occurrence of this option by putting a greedy match of anything on the left
      $asread_variable[$masread]{"options"} = $'; # the only valid options on this line are those that follow the clearoptions statement
    }
    $variable{$type}[$mvar]{"options"} = $asread_variable[$masread]{"options"}; # save this straight to variable now
# print some summary stuff now about the single line read
    if ($variable{$type}[$mvar]{"options"}) { print "INFO: options read in for $type $name = $variable{$type}[$mvar]{options}\n";} else { print "INFO: no options read in for $type $name\n"; }
    if ($variable{$type}[$mvar]{"options"}) { print DEBUG "INFO: options read in for $type $name = $variable{$type}[$mvar]{options}\n";} else { print DEBUG "INFO: no options read in for $type $name\n"; }

# ref: options ref: componentoptions ref: compoundoptions
# options include (with p=perl and f=fortran indicating which piece of code needs to know the option):
#p  derivative/noderivative - for DERIVED, EQUATION, LOCAL : do or do not calculate Jacobian derivatives for this variable
#p  positive/negative/nocheck - for DERIVED, UNKNOWN, EQUATION, LOCAL : check at each iteration that variable is positive/negative
#f  output/nooutput - for ALL : output compound to msh files (equivalently compoundoutput/nocompoundoutput)
#f  componentoutput/nocomponentoutput - for ALL : output just this component to msh files
#f  stepoutput/stepoutputnoupdate/nostepoutput - for ALL : output compound to step file.  The noupdate one does not update the variable when the step file is written (needed for recording when output occurred for example). (equivalently compoundstepoutput/compoundstepoutputnoupdate/nocompoundstepoutput)
#f  componentstepoutput/componentstepoutputnoupdate/nocomponentstepoutput - for ALL : output just this component to step files
#f  input/noinput - for CONSTANT, TRANSIENT, UNKNOWN : read in compound from msh files - only these 3 variable types can be read in
#f  componentinput/nocomponentinput - for CONSTANT, TRANSIENT, UNKNOWN : read in just this component from msh files - only these 3 variable types can be read in
#f  elementdata,elementnodedata,elementnodelimiteddata - for CELL centred var : data type when writing this compound (unless gmesh overide is specified) (also same for components with prefix component) (equivalently compoundelementdata,compoundelementnodedata,compoundelementnodelimiteddata)
#p  outputcondition,stopcondition,convergencecondition,bellcondition - for CONDITION, type of condition, can have multiple conditions for the one variable
#f  magnitude=[value|<a none centred constant>] - for EQUATION, UNKNOWN specifies the initial variable magnitude to be used (rather than being based on the initial variable values) - a negative number will cause the magnitude to be set based upon the initial values (which is the default)
#f  dynamicmagnitude/staticmagnitude - for EQUATION, UNKNOWN, adjust magnitude of variable dynamically as the simulation progresses, or keep it constant at the initial magnitude (default is dynamic for equations, and static for unknowns)
#f  dynamicmagnitudemultiplier=value - for EQUATION, UNKNOWN, multiplier to use when adjusting magnitude of variable dynamically (=>1.d0, with 1.d0 equivalent to static magnitudes, and large values placing no restriction on the change in magnitude from one newton iteration to the next) (default is 1.1 for equations, 2.0 for unknowns)
#   clearoptions - remove all previously (to the left and above the clearoptions word) user-specified options for this variable

# general rule with options is that they don't include any underscores between words
# general rule with keywords is that they do include underscores between words
# perl options now coded as variables (deriv=0/1,check=type,conditions=list) rather than as list of all options
# in all use of option statements, later (i.e. rightmost) and lower options in the input file take precedence over earlier and higher options
# no compound options are used in the perl program

# set defaults for perl options-related variables
# deriv specifies whether maxima needs to calculate a derivative for the variable
    if ($type eq "derived" || $type eq "equation" || $type eq "local") {
      $variable{$type}[$mvar]{"deriv"} = 1; # all derived, local and equation variables generally require a jacobian calculation
    } else {
      $variable{$type}[$mvar]{"deriv"} = 0;
    }
# hasderiv specifies whether the variable has a valid derivative
    $variable{$type}[$mvar]{"hasderiv"} = $variable{$type}[$mvar]{"deriv"};
# hasderiv and deriv only differ for unknowns, which have a (static) derivative set by the fortran, and so do not need a maxima calculated expression
# hasderiv is used when performing variable centring interpolation
    if ($type eq "unknown") { $variable{$type}[$mvar]{"hasderiv"} = 1; }
    $variable{$type}[$mvar]{'check'} = 'nocheck';
    $variable{$type}[$mvar]{'conditions'} = '';

    if (nonempty($variable{$type}[$mvar]{options})) {
      $tmp = $variable{$type}[$mvar]{"options"};
      $variable{$type}[$mvar]{"options"} = '';
      print DEBUG "INFO: before processing $type $name has loaded (unchecked) options of $tmp\n";
      while ($tmp =~ /(^|\,)\s*([^\,]+?)\s*(\,|$)/i) {
        $option = $2; $tmp = $`.','.$';
        if ($option =~ /^(|no)(|compound)(output|input)$/i) { next ;}
        elsif ($option =~ /^(|no)(|compound)stepoutput(|noupdate)$/i) { next ;}
        elsif ($option =~ /^(|compound)element(|node|nodelimited)data$/i) { next ;}
        elsif ($option =~ /^(|no)component(output|(stepoutput(|noupdate)))$/i) {
          $variable{$type}[$mvar]{"options"} = $variable{$type}[$mvar]{"options"}.",\L$1$2"; }
        elsif ($option =~ /^component(element(|node|nodelimited)data)$/i) {
          if ($variable{$type}[$mvar]{"centring"} eq "cell") {
            $variable{$type}[$mvar]{"options"} = $variable{$type}[$mvar]{"options"}.",\L$1";
          } else { print "WARNING: option $option specified for $variable{$type}[$mvar]{centring} $type $name is only relevant for cell centred variables and is ignored\n"; } }
        elsif ($option =~ /^(|no)component(input)$/i) {
          if ($type eq "unknown" || $type eq "constant" || $type eq "transient" || $type eq "output" || $type eq "derived" || $type eq "equation" ) { # allowing derived and equation now for v0.50
            $variable{$type}[$mvar]{"options"} = $variable{$type}[$mvar]{"options"}.",\L$1$2";
            if ($type eq "equation") { print "WARNING: are you sure that you want option $option specified for variable $name?\n"; }
          } else { print "WARNING: option $option specified for $type $name is not relevant for this type of variable and is ignored\n"; } }
        elsif ($option =~ /^(dynamic|static)magnitude$/i) {
          if ($type eq "unknown" || $type eq "equation") {
            $variable{$type}[$mvar]{"options"} = $variable{$type}[$mvar]{"options"}.",\L$option";
          } else { print "WARNING: option $option specified for $type $name is not relevant for this type of variable and is ignored\n"; } }
        elsif ($option =~ /^(magnitude|dynamicmagnitudemultiplier)\s*=\s*([\+\-\d\.][\+\-\ded\.]*)$/i) {
          $option_name = "\L$1";
          $match = "\L$2"; # match is the magnitude of the variable, converted to double precision
          $match =~ s/e/d/; if ($match !~ /d/) { $match = $match."d0"; } if ($match !~ /\./) { $match =~ s/d/.d/; }
          if ($type eq "unknown" || $type eq "equation") {
            $variable{$type}[$mvar]{"options"} = $variable{$type}[$mvar]{"options"}.",$option_name=$match";
            if ($option_name eq "magnitude") { $variable{$type}[$mvar]{"magnitude_constant"} = 0; } # this also resets the magnitude_constant variable
          } else { print "WARNING: option $option specified for $type $name is not relevant for this type of variable and is ignored\n"; } }
        elsif ($option =~ /^magnitude\s*=\s*(<.+?>)$/i) { # alternatively, the magnitude may be specified as a none centred constant variable, which will be checked later
          if ($type eq "unknown" || $type eq "equation") {
            $variable{$type}[$mvar]{"magnitude_constant"} = examine_name($1,"name");
          } else { print "WARNING: option $option specified for $type $name is not relevant for this type of variable and is ignored\n"; } }
        elsif ($option =~ /^(output|stop|convergence|bell)condition$/i) {
          $condition = "\L$1";
          if ($type eq "condition") {
            if (empty($variable{$type}[$mvar]{'conditions'})) { $variable{$type}[$mvar]{'conditions'} = $condition; }
            else { $variable{$type}[$mvar]{'conditions'} = $variable{$type}[$mvar]{'conditions'}.','.$condition; }
          } else { print "WARNING: option $option specified for $type $name is not relevant for this type of variable and is ignored\n"; } }
        elsif ($option =~ /^(positive|negative|nocheck)$/i) {
          if ($type eq "derived" || $type eq "unknown" || $type eq "equation" || $type eq "local") {
            $variable{$type}[$mvar]{"check"} = "\L$1"; # both the check and deriv variables will be from the last occurrence of these options
            print DEBUG "INFO: setting check option $variable{$type}[$mvar]{check} for $type $name\n";
          } else { print "WARNING: option $option specified for $type $name is not relevant for this type of variable and is ignored\n"; } }
        elsif ($option =~ /^(|no)deriv(|ative)$/i) {
          if ($type eq "derived" || $type eq "equation" || $type eq "local") {
            if ("\L$1" eq 'no') { $variable{$type}[$mvar]{"deriv"} = 0; } else { $variable{$type}[$mvar]{"deriv"} = 1; }
          } else { print "WARNING: option $option specified for $type $name is not relevant for this type of variable and is ignored\n"; } }
# ref: newtstepmax ref: newtstepmin
        elsif ($option =~ /^(newtstep(max|min))(|(\s*=(|\s*([\+\-\d][\+\-\de]*))))$/i) { # integer max/min of newtsteps during which this variable should be updated
          $option_name = "\L$1";
          if (empty($6)) { 
            $match = "-1";
          } else { 
            $match = "\L$6"; # match is the magnitude of the variable, which needs to be an integer
          }
          if ($match < 0) { # a negative values clears this option
            $variable{$type}[$mvar]{$option_name} = '';
          } elsif ($type eq "derived" || $type eq "equation") { # newtstep limiting is only done on equations or deriveds right now
            $variable{$type}[$mvar]{$option_name} = $match;
#         } else { print "WARNING: option $option specified for $type $name is not relevant for this type of variable and is ignored\n"; } }
          } else { error_stop("option $option specified for variable $type $name cannot be used for this type of region"); }
        } else { error_stop("unknown option of $option specified for $type $name"); }
      }
# remove extra leading comma and output to fortran file
      $variable{$type}[$mvar]{"options"} =~ s/^\s*\,//;
      if (nonempty($variable{$type}[$mvar]{options})) {
        print FORTRAN_INPUT "VARIABLE_OPTIONS $name $variable{$type}[$mvar]{options}\n";
        print "INFO: the $type $name has the final component-specific options of: $variable{$type}[$mvar]{options}\n";
        print DEBUG "INFO: the $type $name has the final component-specific options of: $variable{$type}[$mvar]{options}\n";
      }
    }

# create maxima names for the variable
    if ($variable{$type}[$mvar]{"centring"} eq "none") {
      $variable{$type}[$mvar]{"maxima"}=$basename{$type}."[$m{user}]";
    } else {
      $variable{$type}[$mvar]{"maxima"}=$basename{$type}."[".ijkstring($variable{$type}[$mvar]{"centring"}).",$m{user}]";
    }
    $variable{$type}[$mvar]{"someloop"} = 0;

# create fortran names for the variable as used in ze code if not a local
    if ($type ne 'local') {
      if ($variable{$type}[$mvar]{"centring"} ne "none") {
        $variable{$type}[$mvar]{"fortran"}="var($m{user})%funk(nsvar(m=$m{user},ijk=".ijkstring($variable{$type}[$mvar]{"centring"}).",error_string=error_string))%v";
      } else { # none
        $variable{$type}[$mvar]{"fortran"}="var($m{user})%funk(1)%v";
      } 
# create mfortran names for the variable, which are what maxima produces from the f90 package
      $variable{$type}[$mvar]{"mfortran"}=$variable{$type}[$mvar]{"maxima"}; # only differ by braces used on indices
      $variable{$type}[$mvar]{"mfortran"}=~s/\[/(/;
      $variable{$type}[$mvar]{"mfortran"}=~s/\]/)/;
      $variable{$type}[$mvar]{"fortranns"}="var($m{user})%funk(ns)%v"; # fortran name referenced by ns rather than location
    }
    $variable{$type}[$mvar]{"fortran_number"} = $masread+1; # fortran var array element number which starts at 1 (previously we used $m{user} for this)

  }

#-----------------------------------------------------
# find and check none-centred constant magnitude variable

  foreach $type ( "unknown", "equation" ) {
    foreach $mvar ( 1 .. $m{$type} ) {
      if ($variable{$type}[$mvar]{"magnitude_constant"}) {
        $name = $variable{$type}[$mvar]{"magnitude_constant"};
        $variable{$type}[$mvar]{"magnitude_constant"} = 0;
        for my $mvarc ( 1 .. $m{"constant"} ) {
          if ($name eq $variable{"constant"}[$mvarc]{"name"}) {
            $variable{$type}[$mvar]{"magnitude_constant"} = $mvarc;
            last;
          }
        }
        if (!($variable{$type}[$mvar]{"magnitude_constant"})) {
          error_stop("the constant variable $name used to specify the magnitude of $type $variable{$type}[$mvar]{name} is not found\n");
        } elsif ($variable{"constant"}[$variable{$type}[$mvar]{"magnitude_constant"}]{"centring"} ne "none") {
          error_stop("the constant variable $name used to specify the magnitude of $type $variable{$type}[$mvar]{name} needs to be none centred: ".
            "it is $variable{constant}[$variable{$type}[$mvar]{magnitude_constant}]{centring} centred\n");
        }
# if we are here then we have identified a none centred constant variable that should be used to set the magnitude - store the fortran number of this variable
        print DEBUG "INFO: identified the none centred constant $name which will be used as the magnitude of $type $variable{$type}[$mvar]{name}\n";
        $variable{$type}[$mvar]{"magnitude_constant"} = $variable{"constant"}[$variable{$type}[$mvar]{"magnitude_constant"}]{"fortran_number"};
      }
    }
  }

#-----------------------------------------------------
# set transientdelta and newtientdelta system variables, and simulation type based on transient/newtient variables

  if ($variable{"transient"}) { set_transient_simulation(1); print DEBUG "INFO: setting simulation type to transient based on the detection of at least one transient variable\n"; }
  if ($variable{"newtient"}) { $newtient=1; print DEBUG "INFO: setting simulation type to newtient based on detection of at least one newtient variable\n"; }
  foreach $mvar ( 1 .. $m{"system"} ) {
    if ($variable{"system"}[$mvar]{"name"} eq "<transientdelta>") { $variable{"system"}[$mvar]{"maxima"} = $transient; }
    if ($variable{"system"}[$mvar]{"name"} eq "<newtientdelta>") { $variable{"system"}[$mvar]{"maxima"} = $newtient; }
  }

# tell the user if multiple definitions were used anywhere
  foreach $repeats (keys(%statement_repeats)) {
    if ($statement_repeats{$repeats} > 1) {
      print "NOTE: the following variables had multiple $repeats - make sure that this is what you intended:\n";
      print DEBUG "NOTE: the following variables had multiple $repeats - make sure that this is what you intended:\n";
      foreach $type ( @user_types ) {
        foreach $mvar ( 1 .. $m{$type} ) {
          if ($variable{$type}[$mvar]{$repeats} > 1) {
            print "  $type $variable{$type}[$mvar]{name} had $variable{$type}[$mvar]{$repeats} $repeats\n";
            print DEBUG "  $type $variable{$type}[$mvar]{name} had $variable{$type}[$mvar]{$repeats} $repeats\n";
          }
        }
      }
    }
  }


}

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
# here we run through all equation expressions substituting in maxima names and 
#  setting correct variable locations - create mequations

sub create_mequations {

  use strict;
  use Storable qw(dclone);
  my ($type, $mvar, $otype, $omvar, $name);

  print DEBUG "in create_mequations\n";

# make a copy of transients and newtients here called initial_transient/newtient - these are processed separately
  foreach $type ( "transient", "newtient" ) {
    if ($m{$type}) {
      $variable{"initial_$type"} = dclone($variable{$type}); # have to clone data structure 
      $m{"initial_$type"} = $m{$type};
    # and set equation to initial_equation
      foreach $mvar ( 1 .. $m{"initial_$type"} ) {
        if (empty($variable{"initial_$type"}[$mvar]{"initial_equation"})) { error_stop("an initial_equation in $type $variable{$type}[$mvar]{name} is not set"); }
        $variable{"initial_$type"}[$mvar]{"equation"} = $variable{"initial_$type"}[$mvar]{"initial_equation"};
#       $variable{"initial_$type"}[$mvar]{"initial_equation"} = ();
        delete $variable{"initial_$type"}[$mvar]{"initial_equation"};
        $variable{"initial_$type"}[$mvar]{"otype"} = "initial_$type"; # set otype to initial_transient/newtient to distinguish it from the transient/newtient equations later: omvar remains the same
#       $variable{$type}[$mvar]{"initial_equation"} = ();
        delete $variable{$type}[$mvar]{"initial_equation"};
      }
    }
  }

# first perform maxima name substitutions
  print "INFO: doing maxima name substitutions for user-supplied variables to create mequations\n";
  print DEBUG "INFO: doing maxima name substitutions for user-supplied variables to create mequations\n";
  foreach $type ( @user_types,"initial_transient","initial_newtient" ) {
    foreach $mvar ( 1 .. $m{$type} ) {
      print DEBUG "about to do name substitutions for $type [$mvar] $variable{$type}[$mvar]{name}\n";
      $variable{$type}[$mvar]{"mequation"}="";
      if (empty($variable{$type}[$mvar]{"equation"})) { next; }
      print DEBUG "equation = $variable{$type}[$mvar]{equation}\n";
      $variable{$type}[$mvar]{"mequation"}=$variable{$type}[$mvar]{"equation"};
      name_to_maxima($variable{$type}[$mvar]{"mequation"},$type,$mvar);
      print DEBUG "mequation = $variable{$type}[$mvar]{mequation}\n";
#     if ($type eq "local") { $variable{$type}[$mvar]{oequation} = $variable{$type}[$mvar]{mequation}; } # store original mequation for use later for locals
      $variable{$type}[$mvar]{oequation} = $variable{$type}[$mvar]{mequation}; # store original mequation for substitution of dependencies later
    }
  }

# now look for operators, do location interpolation and use maxima to simplify the expressions
  print DEBUG "INFO: commencing mequation interpolation\n";
  foreach $type ( @user_types,"initial_transient","initial_newtient","someloop") {
    $mvar = 1;
    while ($m{$type} && $mvar <= $m{$type}) {
      if (empty($variable{$type}[$mvar]{"mequation"})) { # numerical constants don't have mequations
        print DEBUG "INFO: mequation interpolation skipped for $type $variable{$type}[$mvar]{name}: maxima = $variable{$type}[$mvar]{maxima}\n";
        $mvar++; next;
      } 
      print "INFO: doing mequation interpolation for $type $mvar $variable{$type}[$mvar]{name}\n";
      print DEBUG "INFO: doing mequation interpolation for $type $mvar $variable{$type}[$mvar]{name}\n";
# do location-specific changes
      mequation_interpolation($variable{$type}[$mvar]{"mequation"},$variable{$type}[$mvar]{"centring"},$variable{$type}[$mvar]{"deriv"},$variable{$type}[$mvar]{"otype"},$variable{$type}[$mvar]{"omvar"},$type,$mvar);
# check that mequation doesn't contain any original variable or region names at this stage
# have to change region_delta to not be delimited to do this, have gone back to what was there before
# problem is that there are other parts of a someloop (eg, default) that also need to be considered
# error will be picked up anyway in run_maxima_fortran, maybe just a bit criptically
#     if ($variable{$type}[$mvar]{"mequation"} =~ /(<.*>)/) {
#       error_stop("mequation for $type $variable{$type}[$mvar]{name} contains a reference to an unknown variable $1.  ".
#         "Check the definition of this variable for syntax errors.  The offending mequation is:\n $variable{$type}[$mvar]{mequation}");
#     }
# run through maxima performing any simplifications
      run_maxima_simplify($variable{$type}[$mvar]{"mequation"},$variable{$type}[$mvar]{"otype"},$variable{$type}[$mvar]{"omvar"});
      $mvar++;
    }
  }

# move the local variable mequations to unique someloops, recording the someloop number so that the fortran can evaluate these locals
# practically these particular someloops will only be used for outputting, as other embedded local instances will already have there own omvar/otype specific someloops
  $type = "local";
  foreach $mvar ( 1 .. $m{$type} ) {
    create_someloop($variable{$type}[$mvar]{"mequation"},"sum",$variable{$type}[$mvar]{"centring"},"<noloop>",$variable{$type}[$mvar]{"deriv"},$type,$mvar); # the processed mequation for this variable will be placed in this someloop
    $variable{$type}[$mvar]{"mequation"} = ''; # remove mequation so that no fortran is written for this local
    $variable{$type}[$mvar]{"someloop"} = $m{"someloop"}; # record the someloop number
    print DEBUG "local variable $variable{$type}[$mvar]{name} assigned to someloop number $variable{$type}[$mvar]{someloop}\n";
  }

  print DEBUG "INFO: generated mequations\n";
  foreach $type ( @user_types,"initial_transient","initial_newtient","someloop") {
    foreach $mvar ( 1 .. $m{$type} ) {
      if (empty($variable{$type}[$mvar]{"mequation"})) { next; }
      print DEBUG "\n$variable{$type}[$mvar]{maxima}: $type $mvar $variable{$type}[$mvar]{name} $variable{$type}[$mvar]{centring}\nequation: $variable{$type}[$mvar]{equation}\nmequation: $variable{$type}[$mvar]{mequation}\n";
    }
  }

  print DEBUG "leaving create_mequations\n";
}

#-------------------------------------------------------------------------------
# processes a mequation, given
# -> $_[0] contains mequation to process
# -> $_[1] is contextcentring which is what we want this mequation to have
# -> $_[2] is contextderiv (=0,1), which is whether a derivative is required for this mequation
# -> $_[3] is otype, which is the type for the original expresion that this one derived from
# -> $_[4] is omvar, which is the mvar for the original expresion that this one derived from
# -> $_[5] is ltype, which is the type for the immediate variable that is being interpolated
# -> $_[6] is lmvar, which is the mvar for the immediate variable that is being interpolated
# <- $_[0] is returns the modified mequation

sub mequation_interpolation {

  use strict;
  use List::Util qw( min max );
  use Data::Dumper;
  my ($operator_type, $type, $mvar, $operator, $tmp, $n, $l, $pre, $post, $contextcentring, $centring, $options,
    $contextderiv, $someregion, $flux, $default, $limiter, $deriv, $condition, $true, $false, $expression, $dexpression,
    $bound, $previous, $sum, $product, $otype, $omvar, $to_centring, $to_region, $from_region, $mlink, $next_contents_number,
    $substitute, $lousysubstitute, $tmpderiv, $handle, $distance, $difference, $l2, $dx, $name, $gradient, $unknown,
    $mcheck, $not_found, $reflect_multiplier_string, $reflect_option_string, $top, $bottom, $dtype, $ltype, $lmvar, $someloop_mvar,
    $minseparation, $maxseparation, $external_arguments, $faceseparationflag, $method, $vector, $shape, $external_operator_file,
    $external_operator_type, $from_centring, $interpolate_centring, $nfound, $to_region_number, $from_region_number);
  my $nbits=0;
  my @outbit=();
  my @inbit=();
  my @reflect=();

  $contextcentring = $_[1];
  $contextderiv = $_[2];
  $otype = $_[3];
  $omvar = $_[4];
  $ltype = $_[5];
  $lmvar = $_[6];
  print DEBUG "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n".
              "in mequation_interpolation before: \n".
              "  ovariable = $variable{$otype}[$omvar]{name}: omaxima = $variable{$otype}[$omvar]{maxima}: otype = $otype: omvar = $omvar\n".
              "  lvariable = $variable{$ltype}[$lmvar]{name}: lmaxima = $variable{$ltype}[$lmvar]{maxima}: ltype = $ltype: lmvar = $lmvar\n".
              "  contextcentring = $contextcentring: contextderiv = $contextderiv: _[0] = ",$_[0],"\n";

# print  "++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n".
#             "in mequation_interpolation before: \n".
#             "  ovariable = $variable{$otype}[$omvar]{name}: omaxima = $variable{$otype}[$omvar]{maxima}: otype = $otype: omvar = $omvar\n".
#             "  lvariable = $variable{$ltype}[$lmvar]{name}: lmaxima = $variable{$ltype}[$lmvar]{maxima}: ltype = $ltype: lmvar = $lmvar\n".
#             "  contextcentring = $contextcentring: contextderiv = $contextderiv: _[0] = ",$_[0],"\n";

#-------------------------------------------------
# look for any operators and deal with their contents by creating new someloop variables

# while ($_[0] =~ m/(face|cell|none)(tocell|toface|)(div|grad|divgrad|ave|limiter|if|sum|max|min|product|delta|link|newtonupdate|magnitude|vofd|vofphi|vofphishape)(\[(.*?)\]){0,1}\(/i) { #
# while ($_[0] =~ m/(face|cell|none)(tocell|toface|)(div|grad|divgrad|ave|limiter|if|sum|max|min|product|delta|link|newtonupdate|magnitude|(\w+?))(\[(.*?)\]){0,1}\(/i) { #
  while ($_[0] =~ m/(cell|face|node|none)(((to|from)(cell|face|node|none))|)(div|grad|divgrad|ave|limiter|if|sum|max|min|product|delta|link|newtonupdate|magnitude|boundangle|(\w+?))(\[([^]]*)\]){0,1}\(/i) { # now options cannot contain a closing brace ], and operator must be followed by a (: Note, a pair of () defines an operator
#                   1                    234        5                       6                                                                                      7      8  9    
    $operator = $&; # $& holds complete regex match
    $outbit[$nbits] = $`; # $` holds before match
    $_[0] = $'; # $' holds after match
#   print DEBUG "after function matching: 1 = $1: 2 = $2: 3 = $3: 4 = $4: 5 = $5: 6 = $6: 7 = $7: 8 = $8: 9 = $9\n";
    $centring = "\L$1"; # centring and (operator) type
    $operator_type = "\L$6";
    if ($7) {$external_operator_type = $7;} else {$external_operator_type = "";}
    if ($8) {$options = $9;} else {$options = "";}
# just dump to/from centring in variables, and check that they are correct later
    $to_centring = 0; $from_centring = 0;
    if ($2) { if ($4 eq 'to') { $to_centring = $5; } else { $from_centring = $5; } }
# if this is an external operator need to check that it is defined in a fortran externals file
    if ($external_operator_type) {
      $external_operator_file = -1;
      for my $external_operator ( keys(%external_operators) ) {
        print DEBUG "test external_operator = $external_operator: centring+external_operator_type = $centring$external_operator_type: external_operators (externals number) = $external_operators{$external_operator}\n";
        if ($centring.$external_operator_type eq $external_operator) {
          $external_operator_file = $external_operators{$external_operator};
          $externals[$external_operator_file]{"used"}=1; # flag that this routine is being used
          last;
        }
      }
      if ($external_operator_file < 0) { error_stop("The operator $centring$external_operator_type is not known, either as an internal or external operator.  Most probably there is a spelling mistake in this operator name, or if it is supposed to be an external operator, the fortran externals file has not been read in or does not contain an arb_external_operator definition for this operator."); }
    }
    $operator =~ s/\($//; # remove trailing brace from operator
    print DEBUG "about to do a split_mequation_at_character with: character = ): _[0] = $_[0]\n";
    ($inbit[$nbits],$_[0],$not_found) = split_mequation_at_character( $_[0], ')' );
    print DEBUG "after split_mequation_at_character: inbit[$nbits] = $inbit[$nbits]: _[0] = $_[0]: not_found = $not_found\n";
    if ($not_found) { error_stop("the $operator operator does not have matching parenthesis in $otype $variable{$otype}[$omvar]{name}: string = $_[0]"); }
    print DEBUG "contains a $centring centred $operator_type operator with options $options and contents: $inbit[$nbits]\n";
    print DEBUG "contextcentring = $contextcentring: type = $operator_type\n";

# check consistency of centring

# first check on to/from centrings
# to centring only valid for link regions
    if ($to_centring) {
      if ($operator_type ne "link") { error_stop("to_centring of $to_centring inappropriately defined for $operator_type operator in $otype $variable{$otype}[$omvar]{name}"); }
    }
# from centring is not implemented for everything, only where there is possible confusion
# TODO: clean this up, or atleast go through it more thoroughly
    if ($from_centring) {
      if ( ( $operator_type eq "ave" && $centring eq "cell" && 0 ) || # cellfromfaceave, cellfromnodeave, cellfromcellave (v0.50)
           ( $operator_type eq "ave" && $centring eq "face" && $from_centring ne "cell" ) || # facefromcellave
           ( $operator_type eq "ave" && $centring eq "node" && $from_centring ne "cell" ) || # nodefromcellave
           ( $operator_type eq "grad" && $centring eq "cell" && $from_centring eq "cell" ) || # cellfromfacegrad, cellfromnodegrad
           ( $operator_type eq "grad" && $centring eq "face" && $from_centring ne "cell" ) || # facefromcellgrad
           ( $operator_type eq "grad" && $centring eq "node" && $from_centring ne "cell" ) || # nodefromcellgrad
           ( $operator_type eq "divgrad" && $centring eq "cell" && $from_centring ne "face" ) || # cellfromfacedivgrad
           ( $operator_type eq "div" && $centring eq "cell" && $from_centring ne "face" ) ) { # cellfromfacediv
        error_stop("from_centring of $from_centring is not valid for $operator_type operator in $otype $variable{$otype}[$omvar]{name}");
      }
      if (!( $operator_type eq "ave" || $operator_type eq "grad" || $operator_type eq "divgrad" || $operator_type eq "div" )) {
        error_stop("from_centring of $from_centring should not be defined for $operator_type operator in $otype $variable{$otype}[$omvar]{name}");
      }
    }

# for these operators either cell, face or node centring is required
    if ($operator_type =~ /^(grad|div|divgrad|ave|delta|link|vofd|vofphi|vofphishape)$/ && ($centring eq "none" || $contextcentring eq "none")) {
      error_stop("$contextcentring equation centring and $centring operator centring must be either cell, face or node for $operator_type operator in $otype $variable{$otype}[$omvar]{name}");
    }

# for these operators the centring of the operator should correspond to the centring of the context
    if ($operator_type =~ /^(grad|div|divgrad|ave|limiter|if|delta|link|newtonupdate|vofd|vofphi|vofphishape|boundangle)$/ && $centring ne "none" && $contextcentring ne "none" && $centring ne $contextcentring) {
# if centring is incorrect, average operator to correct centring, reconstruct string and try again
      my $operator_to_centring = $contextcentring;
      my $operator_from_centring = '';
      if ($centring eq 'node') {
        $operator_from_centring = 'fromnode';
        if ($contextcentring eq 'face') { # cannot ave directly from node to face, first must ave to cell and deal with ave to face in the next iteration
          $operator_to_centring = 'cell';
        }
      }
      if ($centring eq 'face' && $contextcentring eq 'node') {
        $operator_to_centring = 'cell'; # cannot ave directly from face to node, first must ave to cell and deal with ave to node in the next iteration
      }
      $_[0] = $outbit[$nbits].$operator_to_centring.$operator_from_centring."ave(".$operator."(".$inbit[$nbits]."))".$_[0];
      $inbit[$nbits] = 0;
      $outbit[$nbits] = 0;
      print DEBUG "$centring centring was inconsistent with $contextcentring contextcentring in $otype $variable{$otype}[$omvar]{name} : averaging and resetting to _[0] = $_[0]\n";
      print "NOTE: averaging $centring$operator_type operator to $contextcentring centring in $otype $variable{$otype}[$omvar]{name} (are you sure you want this?)\n";
      next;
    }
      
# looks for generic option of noderivative in this operator
    $deriv = $contextderiv;
    if ($options && $options =~ /(^|\,)\s*noderivative\s*(\,|$)/) { $deriv = 0; }

# reflect=0|1|2|3 is a common option (actually reflect=0 does nothing and is ignored)
# reflect_multiplier is only used if a reflect (component) option is passed to the operation, meaning that the operation is performed on a component of a vector or tensor whose value needs to be reflected in this direction across a reflection boundary
    $reflect_multiplier_string = ''; @reflect = (); $reflect_option_string = '';
    if ($options && ( $options =~ /(^|\,)\s*reflect\s*=\s*(1|2|3)\s*(\,|$)/ )) {
      $reflect_multiplier_string = '*reflect_multiplier';
      foreach $l2 ( 1 .. 3 ) { if ( $options =~ /(^|\,)\s*reflect\s*=\s*$l2\s*(\,|$)/ ) { push(@reflect,$l2); $reflect_option_string = $reflect_option_string.",reflect=$l2"; } }
      print DEBUG "for operator $operator the following reflect coordinates have been found: @reflect: in $otype $variable{$otype}[$omvar]{name}\n";
    }

# reset and create operator_contents
    $tmp = $inbit[$nbits]; # for now pass a copy of this inbit to create_operator_contents - in the future once inbit is no longer needed pass the inbit itself
    create_operator_contents($tmp,$otype,$omvar);
    if (!(%operator_contents)) {
      print "WARNING: operator $operator has no associated contents in the expression for $otype $variable{$otype}[$omvar]{name}\n";
      print DEBUG "WARNING: operator $operator has no associated contents in the expression for $otype $variable{$otype}[$omvar]{name}\n";
    } else {
      print DEBUG "INFO: after create_operator_contents operator_contents = ".Dumper(\%operator_contents)."\n";
    }
    $next_contents_number = 1; # this is the number of the next numerical operator_contents to get

# process each type of operator

#---------------------
# ref: celldiv
# div is an operator representing the divergence which acts over surrounding faces of a cell (which must be within the domain)
    if ($operator_type eq "div") {
      if ($centring ne "cell") { error_stop("$operator_type operator has incorrect $centring centring in $otype $variable{$otype}[$omvar]{name} (do you mean celldiv?)"); }
      ($expression,$name) = search_operator_contents("expression",$next_contents_number);
      if (empty($expression)) { error_stop("expression part for $operator in $otype $variable{$otype}[$omvar]{name} not found:\n  operator_contents = ".Dumper(\%operator_contents)); }
      $inbit[$nbits] = 'divop[i,j]*('.$expression.')';
      create_someloop($inbit[$nbits],"sum","face","<nobcelljfaces>",$deriv,$otype,$omvar);
      
#---------------------
# ref: grad
# grad operators
    } elsif ($operator_type eq "grad") {

      ($expression,$name) = search_operator_contents("expression",$next_contents_number);
      if (empty($expression)) { error_stop("expression part for $operator in $otype $variable{$otype}[$omvar]{name} not found:\n  operator_contents = ".Dumper(\%operator_contents)); }

#---------
# ref: cellgrad
# cellgrad[l=1|2|3] is an operator representing the gradient in the cell, evaluated using surround cell values
      if ($centring eq "cell") {
        if ($options && $options =~ /(^|\,)\s*l\s*=\s*(\d)(\,|$)/) { $l = $2 } else { $l = 0; }
        if ( $l < 1 || $l > 3) { error_stop("$centring $operator_type operator has an incorrect or unspecified direction l=$l (should be 1|2|3) in $otype $variable{$otype}[$omvar]{name}"); }

        if ($from_centring eq "node") {
          error_stop("cellfromnodegrad differencing has not been implemented in the fortran source (and may never be implemented... is it needed here?): found in $otype $variable{$otype}[$omvar]{name}");
# node from_centring: ref: cellfromnodegrad
# don't think that reflect_multiplier_string is needed here
#         $inbit[$nbits] = "cellkernel[i,".scalar($l+4).",ns]*(".$expression.')'.$reflect_multiplier_string;
          $inbit[$nbits] = "cellkernel[i,".scalar($l+4).",ns]*(".$expression.')';
          $someregion = '<cellkernelregion[l='.scalar($l+4).']>';
          create_someloop($inbit[$nbits],"sum","node",$someregion,$deriv,$otype,$omvar);

        } else {
# cell from_centring
          if ($options && ( $options =~ /(^|\,)\s*highorder\s*(\,|$)/ )) {
# option=highorder uses an average of the surrounding face gradients, which are calculated using the KERNEL_OPTION polynomialorder
            $inbit[$nbits] = "cellkernel[i,0,ns]*(facegrad[l=$l,$reflect_option_string](".$expression.'))';
            create_someloop($inbit[$nbits],"sum","face","<cellkernelregion[l=0]>",$deriv,$otype,$omvar);

          } else {
# the standard kernels are calculated with the KERNEL_OPTION polynomialcellorder which by default is 1, but which will be more compact than the above
            $inbit[$nbits] = "cellkernel[ilast,$l,ns]*(".$expression.')'.$reflect_multiplier_string;
            $someregion = '<cellkernelregion[l='.$l.']>';
            create_someloop($inbit[$nbits],"sum","cell",$someregion,$deriv,$otype,$omvar);
            if (@reflect) { @{$variable{"someloop"}[$m{"someloop"}]{"reflect"}} = @reflect; }
          }
        }

#---------
# ref: facegrad
# facegrad is an operator representing the gradient of an expression taken in coordinate (l=1,2,3) or face orientated (l=4,5,6) directions
      } elsif ($centring eq "face") {
        if ($options && $options =~ /(^|\,)\s*l\s*=\s*(\d)(\,|$)/) { $l = $2 } else { $l = 4; }
        if (!( $l >= 1 && $l <= 6)) { error_stop("$centring $operator_type operator has an incorrect direction l=$l (should be 1|2|3|4|5|6) in $otype $variable{$otype}[$omvar]{name}"); }

        if ($options && ( $options =~ /(^|\,)\s*(adjacentcells|dxunit)\s*(\,|$)/ )) {
# approximate kernels that are based on adjacentcells only
          if ($l ne 4) { error_stop("$2 option for facegrad can only be applied with option l=4 in $otype $variable{$otype}[$omvar]{name}"); }

# form the gradient from two loops, the first of which calculates the difference and knows about the reflection, all done within a second loop that calculates the distance multipliers
          $inbit[$nbits] = "($expression)*(adjacentcellsignns[ns])$reflect_multiplier_string";
          create_someloop($inbit[$nbits],"sum","cell","<adjacentfaceicells>",$deriv,$otype,$omvar);
          if (@reflect) { @{$variable{"someloop"}[$m{"someloop"}]{"reflect"}} = @reflect; }

          if ( $options =~ /(^|\,)\s*dxunit\s*(\,|$)/ ) {
# option=dxunit gradient is based on adjacent cells only and is in the direction of dxunit (ie, the direction between the adjacent cells) but not strictly centred at face centroid
            $inbit[$nbits] = "($inbit[$nbits])/facedx[j]";
          } else {
# option=adjacentcells gradient is based on adjacent cells only, but that attempts to be in the direction of the face normal (an approximation)
            $distance = '';
            foreach $l2 ( 1 .. 3 ) {
              $distance = $distance."+facedxunit[j,$l2]*facenorm[j,$l2]";
            }
            $distance = "($distance)*facedx[j]";
            create_someloop($distance,"max","face","<noloop>",$deriv,$otype,$omvar);
            $variable{"someloop"}[$m{"someloop"}]{"default"} = "tinyish";
            $inbit[$nbits] = "($inbit[$nbits])/$distance"; # distance should be positive
          }

        } else {
# normal kernel based gradients - facegrad
          $inbit[$nbits] = 'facekernel[j,'.$l.',ns]*('.$expression.')'.$reflect_multiplier_string;
          $someregion = '<facekernelregion[l='.$l.']>';
          create_someloop($inbit[$nbits],"sum","cell",$someregion,$deriv,$otype,$omvar);
# if reflect is active then push the list of reflection coordinates to the someloop for both facegrad and cellgrad
# will work with all cell centred kernel regions and <adjacentfaceicells> ONLY (right now)
          if (@reflect) { @{$variable{"someloop"}[$m{"someloop"}]{"reflect"}} = @reflect; }
        }

#---------
# ref: nodegrad
# nodegrad[l=1|2|3] is an operator representing the gradient in the node, evaluated using surround cell values
      } elsif ($centring eq "node") {
        if ($options && $options =~ /(^|\,)\s*l\s*=\s*(\d)(\,|$)/) { $l = $2 } else { $l = 0; }
        if ( $l < 1 || $l > 3) { error_stop("$centring $operator_type operator has an incorrect or unspecified direction l=$l (should be 1|2|3) in $otype $variable{$otype}[$omvar]{name}"); }

# the standard kernels are calculated with the KERNEL_OPTION polynomialcellorder which by default is 1, but which will be more compact than the above
        $inbit[$nbits] = "nodekernel[k,$l,ns]*(".$expression.')'.$reflect_multiplier_string;
        $someregion = '<nodekernelregion[l='.$l.']>';
        create_someloop($inbit[$nbits],"sum","cell",$someregion,$deriv,$otype,$omvar);
        if (@reflect) { @{$variable{"someloop"}[$m{"someloop"}]{"reflect"}} = @reflect; }

      }

#---------------------
# ref: celldivgrad
# celldivgrad[l=1|2|3] is an operator representing the gradient in the cell, evaluated using the divergence of surround face values
    } elsif ($operator_type eq "divgrad") {
      if ($centring ne "cell") { error_stop("a $centring $operator_type operator does not exist in $otype $variable{$otype}[$omvar]{name} (do you mean celldivgrad?)"); }
      if ($options && $options =~ /(^|\,)\s*l\s*=\s*(\d)(\,|$)/) { $l = $2 } else { $l = 0; }
      if (!( $l >= 1 && $l <= 3)) { error_stop("a $centring $operator_type operator has incorrect or unspecified direction l=$l in $otype $variable{$otype}[$omvar]{name}"); }
      ($expression,$name) = search_operator_contents("expression",$next_contents_number);
      if (empty($expression)) { error_stop("expression part for $operator in $otype $variable{$otype}[$omvar]{name} not found:\n  operator_contents = ".Dumper(\%operator_contents)); }
      $inbit[$nbits] = "divop[i,j]*facenorm[j,$l]*(".$expression.')';
      create_someloop($inbit[$nbits],"sum","face","<nobcelljfaces>",$deriv,$otype,$omvar);
 
#---------------------
# ref: ave
# ave
    } elsif ($operator_type eq "ave") {

      ($expression,$name) = search_operator_contents("expression",$next_contents_number);
      if (empty($expression)) { error_stop("expression part for $operator in $otype $variable{$otype}[$omvar]{name} not found:\n  operator_contents = ".Dumper(\%operator_contents)); }

#---------
# ref: cellave
# cellave is an operator that takes face centred quantities and averages to cell centres
      if ($centring eq "cell") {

        if ($from_centring eq "node") {
# node from_centring: ref: cellfromnodeave
          $inbit[$nbits] = "cellkernel[i,4,ns]*(".$expression.')'.$reflect_multiplier_string;
          $someregion = '<cellkernelregion[l=4]>';
          create_someloop($inbit[$nbits],"sum","node",$someregion,$deriv,$otype,$omvar);

        } elsif ($from_centring eq "cell") {
# cell from_centring: ref: cellfromcellave
# for now this is only to find properties at the centre of a separation loop
          if ($options && $options =~ /(^|\,)\s*sep(|aration)cent(|re|er)(\d*)\s*(\,|$)/) {
            $inbit[$nbits] = $expression;
            if ($4) {
              $someregion = "<separationcentre$4>";
            } else {
              $someregion = '<separationcentre1>'; # if no separation loop number is specified, then 1 is assumed, which is the last separation loop initialised within the current thread
            }
            create_someloop($inbit[$nbits],"sum","cell",$someregion,$deriv,$otype,$omvar);
          } else {
            error_stop("$operator in $otype $variable{$otype}[$omvar]{name} only presently supports separationcentre averaging:\n  operator_contents = ".Dumper(\%operator_contents));
          }

        } else {
# face from_centring
# lastface averaging - suppress interpolation to current cell centre using values from the last face instead
# unlike lastfacenoglue, this will move to the particular face that is adjacent to the current cell in the case that the lastface was a glued face
          if ($options && $options =~ /(^|\,)\s*lastface\s*(\,|$)/) {
            $inbit[$nbits] = $expression;
            create_someloop($inbit[$nbits],"sum","face","<lastface>",$deriv,$otype,$omvar);
            $variable{"someloop"}[$m{"someloop"}]{"checkj"} = 1; # in fortran will need to check that the jlast is defined

# lastfacenoglue averaging - suppress interpolation to current cell centre using values from the last face instead
          } elsif ($options && $options =~ /(^|\,)\s*lastfacenoglue\s*(\,|$)/) {
            $inbit[$nbits] = $expression;
            create_someloop($inbit[$nbits],"sum","face","<noloop>",$deriv,$otype,$omvar);
            $variable{"someloop"}[$m{"someloop"}]{"checkj"} = 1; # in fortran will need to check that the jlast is defined

# othercell averaging - use the cell on the otherside of the lastface to the current cell
# as we are looping through a <faceicells> type of region we need to consider reflect
          } elsif ($options && $options =~ /(^|\,)\s*othercell\s*(\,|$)/) {
            $inbit[$nbits] = "($expression)$reflect_multiplier_string";
            create_someloop($inbit[$nbits],"sum","cell","<adjacentfaceothercell>",$deriv,$otype,$omvar);
            if (@reflect) { @{$variable{"someloop"}[$m{"someloop"}]{"reflect"}} = @reflect; }
            $variable{"someloop"}[$m{"someloop"}]{"checkj"} = 1; # in fortran will need to check that the jlast is defined

# normal kernel arithmetic average from face to cell values (so no reflect required)
          } else {
            $inbit[$nbits] = "cellkernel[i,0,ns]*(".$expression.")";
            create_someloop($inbit[$nbits],"sum","face","<cellkernelregion[l=0]>",$deriv,$otype,$omvar);
          }
        }

#---------
# ref: faceave
# faceave is an operator that takes cell centred quanities and averages to face centres
      } elsif ($centring eq "face") {

# advection averaging - takes a cell centred expression, a face centred flux, and optionally a cell centred gradient limiter (0->1)
        if ($options && $options =~ /(^|\,)\s*advection\s*(\,|$)/) {

          ($flux,$name) = search_operator_contents("flux",$next_contents_number);
          if (empty($flux)) { error_stop("flux part for $operator in $otype $variable{$otype}[$omvar]{name} not found:\n  operator_contents = ".Dumper(\%operator_contents)); }
          ($limiter,$name) = search_operator_contents("limiter",$next_contents_number);
          if (empty($limiter)) { $limiter = "1"; print "INFO: using default limiter = 1 for $operator in $otype $variable{$otype}[$omvar]{name}\n"; } # default limiter is 1
          print DEBUG "advection operator components found: expression = $expression: flux = $flux: limiter = $limiter\n";

# pull out optional gradient too
          $inbit[$nbits] = $expression; # assemble expression
#         if ($limiter) {
            foreach $l ( 1 .. 3 ) {
              ($gradient,$name) = search_operator_contents("gradient[l=$l]",$next_contents_number);
#             if ($gradient eq "") { $gradient = "cellsum(cellkernel[ilast,$l,ns]*(".$expression.'),<cellkernelregion[l='.$l.']>)';
              if (empty($gradient)) { $gradient = "cellgrad[l=$l".$reflect_option_string."](".$expression.')'; # passes reflect options through to gradient
              } else { print "INFO: found gradient[l=$l] expression for $operator in $otype $variable{$otype}[$omvar]{name}\n"; }
              $inbit[$nbits] = $inbit[$nbits].
#               "+(".$limiter.")*cellsum(cellkernel[ilast,$l,ns]*(".$expression.'),<cellkernelregion[l='.$l.']>)'.
                "+(".$limiter.")*(".$gradient.")*(cellave[lastface](facex[j,$l])-cellx[i,$l])";
            }
#         }
          create_someloop($inbit[$nbits],"sum","cell","<upwindfaceicells>",$deriv,$otype,$omvar);
          $someloop_mvar = $m{"someloop"}; # save someloop type

          mequation_interpolation($flux,"face",0,$otype,$omvar,"someloop",$someloop_mvar); # convert flux to face centred as will not become part of someloop - deriv not needed
          $variable{"someloop"}[$someloop_mvar]{"flux"} = $flux; # save flux for evaluating upwindfaceicells region
          print DEBUG "setting flux = $flux for someloop $someloop_mvar\n";

        } elsif ($options && ( $options =~ /(^|\,)\s*(nonlimited|limited|)harmonic\s*(\,|$)/ )) {
# harmonic averaging for general diffusivities
# doesn't reflect components (which doesn't make sense anyway)
# for v0.42 default has become limitedharmonic, which assumes that expression is positive
# for v0.42, back to nonlimitedharmonic as the default - limitedharmonic needs work
          $sum = $expression;
          create_someloop($sum,"sum","cell","<adjacentfaceicells>",$deriv,$otype,$omvar);
          $product = $expression;
          create_someloop($product,"product","cell","<adjacentfaceicells>",$deriv,$otype,$omvar);
          if ($options =~ /(^|\,)\s*(nonlimited|)harmonic\s*(\,|$)/) {
            $inbit[$nbits] = "(2.*$product/$sum)";
          } else {
            $inbit[$nbits] = "faceif($sum-1.d-20,2.*$product/$sum,2.*$product/1.d-20)"; # allow for zero values within harmonic averaging but contents must be positive
            create_someloop($inbit[$nbits],"sum","face","<noloop>",$deriv,$otype,$omvar);
          }

# harmonic, limited, but weighted by distances normal to face
# doesn't reflect components (which doesn't make sense anyway for harmonic averaging)
# assumes that expression is positive
        } elsif ($options && ( $options =~ /(^|\,)\s*harmonicweighted\s*(\,|$)/ )) {

          $dx = '';
          foreach $l ( 1 .. 3 ) {
            $dx = $dx."+facetoicellr[j,$l,ns]*cellave[lastface](facenorm[j,$l])";
          }

          $distance = "abs($dx)";
          create_someloop($distance,"sum","cell","<adjacentfaceicells>",0,$otype,$omvar);
          $product = $expression;
          create_someloop($product,"product","cell","<adjacentfaceicells>",$deriv,$otype,$omvar);
          $top = "$distance*$product";

          $bottom = "cellsum(expression=abs($dx)*($expression),region=<adjacentfaceicells>)"; # here dx is the distance normal to the interface on this side of the face
          create_someloop($bottom,"max","cell","<noloop>",$deriv,$otype,$omvar);
          $variable{"someloop"}[$m{"someloop"}]{"default"} = "tinyish";

          $inbit[$nbits] = "(($top)/($bottom))";

# average based on adjacent cells only, weighted by distance
        } elsif ($options && ( $options =~ /(^|\,)\s*adjacentcells(|weighted)\s*(\,|$)/ )) {

          $dx = '';
          foreach $l ( 1 .. 3 ) {
            $dx = $dx."+cellave[othercell](facetoicellr[j,$l,ns])*cellave[lastface](facenorm[j,$l])";
          }
          $top = "abs($dx)*($expression)$reflect_multiplier_string"; # here dx is the distance normal to the interface on the other side of the face
          create_someloop($top,"sum","cell","<adjacentfaceicells>",$deriv,$otype,$omvar);
          if (@reflect) { @{$variable{"someloop"}[$m{"someloop"}]{"reflect"}} = @reflect; }
          $dx = '';
          foreach $l ( 1 .. 3 ) {
            $dx = $dx."+facetoicellr[j,$l,ns]*cellave[lastface](facenorm[j,$l])";
          }
          $bottom = "cellsum(expression=abs($dx),region=<adjacentfaceicells>)"; # here dx is the distance normal to the interface on this side of the face
          create_someloop($bottom,"max","cell","<noloop>",0,$otype,$omvar);
          $variable{"someloop"}[$m{"someloop"}]{"default"} = "tinyish";

          $inbit[$nbits] = "(($top)/($bottom))"; # bugfix v0.5

#         $dx = '';
#         foreach $l ( 1 .. 3 ) {
#           $dx = $dxup."+(faceave[upcell](cellx[i,$l])-faceave[glueface](facex[j,$l]))*facenorm[j,$l]";
#           $dx = $dx."+(-faceave[othercell](cellx[i,$l])+facex[j,$l])*facenorm[j,$l]";
#         }
#         $inbit[$nbits] = "($expression)$reflect_multiplier_string";
#         create_someloop($inbit[$nbits],"sum","cell","<adjacentfaceicells>",$deriv,$otype,$omvar);
#         if (@reflect) { @{$variable{"someloop"}[$m{"someloop"}]{"reflect"}} = @reflect; }
#         $inbit[$nbits] = "(($inbit[$nbits])/2.d0)"

# # TODO REFLECT
#           $dxup = '';
#           $dxdown = '';
# # absolute distance
# #         foreach $l ( 1 .. 3 ) {
# #           $dxup = $dxup."+(faceave[upcell](cellx[i,$l])-facex[j,$l])^2";
# #           $dxdown = $dxdown."+(faceave[downcell](cellx[i,$l])-facex[j,$l])^2";
# #         }
# #         $dxup = "sqrt($dxup)";
# #         $dxdown = "sqrt($dxdown)";
# # distance normal to face
#           foreach $l ( 1 .. 3 ) {
#             $dxup = $dxup."+(faceave[upcell](cellx[i,$l])-faceave[glueface](facex[j,$l]))*facenorm[j,$l]";
#             $dxdown = $dxdown."+(-faceave[downcell](cellx[i,$l])+facex[j,$l])*facenorm[j,$l]";
#           }
#           create_someloop($dxup,"sum","face","<noloop>",$deriv,$otype,$omvar);
#           create_someloop($dxdown,"sum","face","<noloop>",$deriv,$otype,$omvar);
#           $phiup = $expression;
#           create_someloop($phiup,"sum","cell","<adjacentfaceupcell>",$deriv,$otype,$omvar);
#           $phidown = $expression;
#           create_someloop($phidown,"sum","cell","<adjacentfacedowncell>",$deriv,$otype,$omvar);
#           $inbit[$nbits] = "(($phiup)*($dxdown)+($phidown)*($dxup))/($dxup+$dxdown)";

# #         $inbit[$nbits] = "(faceave[upcell]($inbit[$nbits])*$dxdown+faceave[downcell]($inbit[$nbits])*$dxup)/($dxup+$dxdown)";
# #         create_someloop($inbit[$nbits],"sum","face","<noloop>",$deriv,$otype,$omvar);

# average based on adjacent cells only, no weighting
        } elsif ($options && ( $options =~ /(^|\,)\s*adjacentcellsevenweighting\s*(\,|$)/ )) {
          $inbit[$nbits] = "($expression)$reflect_multiplier_string";
          create_someloop($inbit[$nbits],"sum","cell","<adjacentfaceicells>",$deriv,$otype,$omvar);
          if (@reflect) { @{$variable{"someloop"}[$m{"someloop"}]{"reflect"}} = @reflect; }
          $inbit[$nbits] = "(($inbit[$nbits])/2.d0)"

# lastcell averaging - suppress interpolation to current face centre
        } elsif ($options && $options =~ /(^|\,)\s*lastcell\s*(\,|$)/) {
          $inbit[$nbits]=$expression;
          create_someloop($inbit[$nbits],"sum","cell","<noloop>",$deriv,$otype,$omvar);
          $variable{"someloop"}[$m{"someloop"}]{"checki"} = 1; # in fortran will need to check that the ilast is defined

# upcell averaging - only use face(j)%icell(2), that is, cell in direction of face normal
        } elsif ($options && $options =~ /(^|\,)\s*upcell\s*(\,|$)/) {
          $inbit[$nbits]=$expression.$reflect_multiplier_string;
          create_someloop($inbit[$nbits],"sum","cell","<adjacentfaceupcell>",$deriv,$otype,$omvar);
          if (@reflect) { @{$variable{"someloop"}[$m{"someloop"}]{"reflect"}} = @reflect; }

# downcell averaging - only use face(j)%icell(1), that is, cell in opposite direction to the face normal
        } elsif ($options && $options =~ /(^|\,)\s*downcell\s*(\,|$)/) {
          $inbit[$nbits]=$expression.$reflect_multiplier_string;
          create_someloop($inbit[$nbits],"sum","cell","<adjacentfacedowncell>",$deriv,$otype,$omvar);
          if (@reflect) { @{$variable{"someloop"}[$m{"someloop"}]{"reflect"}} = @reflect; }

# othercell averaging - use the cell on the otherside of the lastface to the lastcell
        } elsif ($options && $options =~ /(^|\,)\s*othercell\s*(\,|$)/) {
          $inbit[$nbits]=$expression.$reflect_multiplier_string;
          create_someloop($inbit[$nbits],"sum","cell","<adjacentfaceothercell>",$deriv,$otype,$omvar);
          $variable{"someloop"}[$m{"someloop"}]{"checki"} = 1; # in fortran will need to check that the ilast is defined
          if (@reflect) { @{$variable{"someloop"}[$m{"someloop"}]{"reflect"}} = @reflect; }

# glueface averaging - interpolate from face that face is glued to, or if not glued, the current face
        } elsif ($options && $options =~ /(^|\,)\s*glueface\s*(\,|$)/) {
          $inbit[$nbits]=$expression;
          create_someloop($inbit[$nbits],"sum","face","<glueface>",$deriv,$otype,$omvar);
# not needed as already interpolated to a face
#         $variable{"someloop"}[$m{"someloop"}]{"checkj"} = 1; # in fortran will need to check that the jlast is defined

# normal arithmetic average - faceave
        } else {
          $inbit[$nbits] = "facekernel[j,0,ns]*(".$expression.')'.$reflect_multiplier_string;
          create_someloop($inbit[$nbits],"sum","cell","<facekernelregion[l=0]>",$deriv,$otype,$omvar);
# if reflect is active then push the list of reflection coordinates to the someloop
          if (@reflect) { @{$variable{"someloop"}[$m{"someloop"}]{"reflect"}} = @reflect; }
        }

#---------
# ref: nodeave: ref: nodefromcellave
# nodeave is an operator that takes cell centred quantities and averages to nodes
      } elsif ($centring eq "node") {

        $inbit[$nbits] = "nodekernel[k,0,ns]*(".$expression.")";
        create_someloop($inbit[$nbits],"sum","cell","<nodekernelregion[l=0]>",$deriv,$otype,$omvar);
        if (@reflect) { @{$variable{"someloop"}[$m{"someloop"}]{"reflect"}} = @reflect; }

      }

#---------------------
# ref: celllimiter
# celllimiter
    } elsif ($operator_type eq "limiter") {
      if ($centring ne "cell") { error_stop("$operator_type operator has incorrect $centring centring in $otype $variable{$otype}[$omvar]{name} (do you mean celllimiter?)"); }

      ($expression,$name) = search_operator_contents("expression",$next_contents_number);
      if (empty($expression)) { error_stop("expression part for $operator in $otype $variable{$otype}[$omvar]{name} not found:\n  operator_contents = ".Dumper(\%operator_contents)); }

      if ($options && ( $options =~ /(^|\,)\s*upper\s*(\,|$)/ || $options =~ /(^|\,)\s*lower\s*(\,|$)/ )) {
        if ($options =~ /(^|\,)\s*upper\s*(\,|$)/ && $options =~ /(^|\,)\s*lower\s*(\,|$)/) {
          error_stop("either upper or lower bound can be specified in celllimiter options, not both in $otype $variable{$otype}[$omvar]{name}");
        }
        ($bound,$name) = search_operator_contents("bound",$next_contents_number);
#       if ($bound eq "") { error_stop("bound specified but none given in celllimiter in $otype $variable{$otype}[$omvar]{name}"); }
# now put in default bound
        if (empty($bound)) {
          if ($options =~ /(^|\,)\s*upper\s*(\,|$)/) {
            $bound = "cellmax[$reflect_option_string]((".$expression.")$reflect_multiplier_string,,<adjacentcellicells>)"; # upper bound
          } else {
            $bound = "cellmin[$reflect_option_string]((".$expression.")$reflect_multiplier_string,,<adjacentcellicells>)"; # lower bound
          }
          print DEBUG "setting default bound in $otype $variable{$otype}[$omvar]{name} celllimiter to bound = $bound\n";
        }
        if ($options =~ /(^|\,)\s*upper\s*(\,|$)/) {
          $bound = "-(".$bound.")";
          $expression = "-(".$expression.")";
        }
      } else { $bound = "0.d0"; }

      ($default,$name) = search_operator_contents("default",$next_contents_number);
      if (empty($default)) { $default = "1.d0"; }

# form expression for change in quantity between cell centre and face
      $dexpression = "";
      foreach $l ( 1 .. 3 ) {
        ($gradient,$name) = search_operator_contents("gradient[l=$l]",$next_contents_number);
        if (empty($gradient)) { $gradient = "cellgrad[l=$l".$reflect_option_string."](".$expression.')';
        } else { print "INFO: found gradient[l=$l] expression for $operator in $otype $variable{$otype}[$omvar]{name}\n"; }
        $dexpression = $dexpression.
#         "+faceave[lastcell](cellsum(cellkernel[ilast,$l,ns]*(".$expression.'),<cellkernelregion[l='.$l.']>))'.
          "+faceave[lastcell](".$gradient.")*(facex[j,$l]-faceave[lastcell](cellx[i,$l]))";
      }

      if ($options && $options =~ /(^|\,)\s*linear\s*(\,|$)/) {
# linear function
        $inbit[$nbits] = 'cellmin(cellmin(('.$bound.')-('.$expression.'),0.d0)/'.
          '(facemin('.$dexpression.',,<celljfaces>)-limitertolerance),'.$default.')';
      } else {
# continuous function
        $inbit[$nbits] = 'cellmin(cellmin(('.$bound.')-('.$expression.'),0.d0)/'.
          '(facemin('.$dexpression.',,<celljfaces>)-limitertolerance),limitercontgrad)';
        $inbit[$nbits] = 'cellmin((-cos('.$inbit[$nbits].'*pi/limitercontgrad)+1.d0)/2.d0,'.$default.')';
      }

#     create_someloop($inbit[$nbits],"sum","cell","<noloop>",$deriv,$otype,$omvar);

# instead of creating someloop, reconstruct string using primative functions and try again
      $_[0] = $outbit[$nbits]."(".$inbit[$nbits].")".$_[0];
      $inbit[$nbits] = 0;
      $outbit[$nbits] = 0;
      print DEBUG "reconstructing mequation string after processing limiter operator in $otype $variable{$otype}[$omvar]{name} : resetting to _[0] = $_[0]\n";
      next;

#---------------------
# ref: if, ref: cellif, ref: faceif, ref: noneif
# if statement 
# three arguments: condition, true and false
# if true or false are not defined then they are given a zero value
    } elsif ($operator_type eq "if") {

      ($condition,$name) = search_operator_contents("condition",$next_contents_number);
      if (empty($condition)) { error_stop("condition expression in if operator not found in $otype $variable{$otype}[$omvar]{name}"); }
      ($true,$name) = search_operator_contents("true",$next_contents_number);
      if (empty($true)) { $true = "0"; }
#       error_stop("true expression in if operator not found in $otype $variable{$otype}[$omvar]{name}"); }
      ($false,$name) = search_operator_contents("false",$next_contents_number);
      if (empty($false)) { $false = "0"; }
#       error_stop("false expression in if operator not found in $otype $variable{$otype}[$omvar]{name}"); }

      $inbit[$nbits] = $condition; # main expression is condition (giving the true value if >=0, else the false value)
      create_someloop($inbit[$nbits],$operator_type,$centring,"<noloop>",0,$otype,$omvar);
      $someloop_mvar = $m{"someloop"}; # save someloop type

# save true and false conditions into recently created someloop
      mequation_interpolation($true,$centring,$deriv,$otype,$omvar,"someloop",$someloop_mvar);
      mequation_interpolation($false,$centring,$deriv,$otype,$omvar,"someloop",$someloop_mvar);
      $variable{"someloop"}[$someloop_mvar]{"false"} = $false;
      $variable{"someloop"}[$someloop_mvar]{"true"} = $true;
# save deriv for these values which is different to deriv used for condition (=0)
      $variable{"someloop"}[$someloop_mvar]{"valuederiv"} = $deriv;

#---------------------
# ref: sum, ref: cellsum, ref: facesum, ref: product, ref: cellproduct, ref: faceproduct
# ref: max, ref: cellmax, ref: facemax, ref: min, ref: cellmin, ref: facemin
# sum|max|min|product
# centring for these is where the contents needs to be located, rather than where the operator is located
    } elsif ($operator_type =~ /^(sum|max|min|product)$/) {

# first argument is always the equation
# if a min|max:
#   expression (or 1) if one argument is present then an automatic default value and <noloop> is assumed
#   default (or 2) if two arguments are present, second argument is a specified default value and <noloop> is assumed (ie, max|min of two quantities)
#   region (or 3) if three arguments are present, the second is a specified default value and the third is the region
#     - if the second field is left empty then an automatic default value is used
# if a sum|product
#   expression (or 1) if one argument is present then default regions are applied depending on the operator
#   region (or 2) if a second argument is present then it is the region over which the sum|product is applied

      ($expression,$name) = search_operator_contents("expression",$next_contents_number);
      if (empty($expression)) { error_stop("expression part for $operator in $otype $variable{$otype}[$omvar]{name} not found:\n  operator_contents = ".Dumper(\%operator_contents)); }
      
      $default = 0;
      if ($operator_type eq "min" || $operator_type eq "max") {
        ($default,$name) = search_operator_contents("default",$next_contents_number);
      }

# ref: separation
      $minseparation=-1;
      $maxseparation=-1;
# if separation options are specified then transfer these to someloop too
      if ($options && $options =~ /(^|\,)\s*(|min|minimum|max|maximum|face|node|no)separation\s*(=\s*\d+?|)\s*(\,|$)/i) {
        print DEBUG "INFO: found separation options in $centring $operator in $otype $variable{$otype}[$omvar]{name}\n";
        print "INFO: found separation options in $centring $operator in $otype $variable{$otype}[$omvar]{name}\n";
        if ($options =~ /(^|\,)\s*noseparation\s*(=\s*(\d+?)|)\s*(\,|$)/i) {
          if ($2) { error_stop("noseparation option in $centring $operator in $otype $variable{$otype}[$omvar]{name} has trailing characters"); }
          print DEBUG "INFO: separation options ignored from $options for $centring $operator in $otype $variable{$otype}[$omvar]{name}\n";
        } else { # all other options indicate that we are going to do a separation loop
          $minseparation=0; # without any integer, it imposes no limit on maxseparation but does signal that we are to use the separation loop
          if ($centring ne "cell") { error_stop("separation options have been specified for $centring $operator in $otype $variable{$otype}[$omvar]{name} indicating that a separation loop is required, however separation loops are only supported in cell based loops (ie cellsum, cellproduct, cellmax and cellmin), and this isn't one of those"); }
          if ($contextcentring ne "cell") { error_stop("separation options have been specified for $centring $operator in $otype $variable{$otype}[$omvar]{name} indicating that a separation loop is required, however the context centring for this loop is $contextcentring rather than the required cell centring.  Remember, a separation loop loops outward from a starting cell."); }
          if ($options =~ /(^|\,)\s*(max|maximum)separation\s*(=\s*(\d+?)|)\s*(\,|$)/i) {
            if (nonempty($4)) { $maxseparation=$4;
              if ($maxseparation !~ /^(|\+|-)\d+$/) { error_stop("maxseparation ($maxseparation) for $centring $operator in $otype $variable{$otype}[$omvar]{name} has nonsensical trailing characters - should be an integer"); }
            }
          }
          if ($options =~ /(^|\,)\s*(|min|minimum)separation\s*(=\s*(\d+?)|)\s*(\,|$)/i) {
            if (nonempty($4)) { $minseparation=$4;
              if ($minseparation !~ /^(|\+|-)\d+$/) { error_stop("minseparation ($minseparation) for $centring $operator in $otype $variable{$otype}[$omvar]{name} has nonsensical trailing characters - should be an integer"); }
            }
          }
          print DEBUG "INFO: setting minseparation = $minseparation and maxseparation = $maxseparation for $centring $operator in $otype $variable{$otype}[$omvar]{name}\n";
        }
      }
      
      ($someregion,$name) = search_operator_contents("region",$next_contents_number);
      if ( $someregion =~ /^\<.+?\>$/ ) {
        $someregion = examine_name($someregion,"regionname"); # standardise regionname
        print DEBUG "INFO: location of $someregion has been found to $centring centred $operator_type operator used in $otype variable $variable{$otype}[$omvar]{name}\n";
      } elsif (empty($someregion)) {
        if ($minseparation >= 0) {
          $someregion = "<all cells>"; # default region if separation options are specified is <all cells>
        } elsif (($operator_type eq "sum" || $operator_type eq "product") && $contextcentring eq "cell" && $centring eq "face") {
          $someregion = "<celljfaces>"; # default face loop around a cell
        } elsif (($operator_type eq "sum" || $operator_type eq "product") && $contextcentring eq "cell" && $centring eq "cell") {
          $someregion = "<nocadjacentcellicells>"; # default cell loop around a cell is just adjacent cells
        } elsif (($operator_type eq "sum" || $operator_type eq "product") && $contextcentring eq "face" && $centring eq "cell") {
          $someregion = "<adjacentfaceicells>"; # default cell loop around a face is just the 2 adjacent faces
        } elsif (($operator_type eq "max" || $operator_type eq "min") && $contextcentring ne $centring && $centring ne "none") {
          error_stop("the $operator operator within $otype $variable{$otype}[$omvar]{name} does not have a region listed and a default region cannot be safely ".
                     "guessed from the context.  You need to explicitly specify the region within this operator.  An alternative possibility is that this operator is in the wrong ".
                     "centring context (contextcentring = $contextcentring: centring = $centring) and needs first to be averaged to the correct centring using a surrounding ".
                     $contextcentring."ave operator.  Yet another possibility is that you have not included the default value for this operator when using unname operator contents parts, or left it blank by ".
                     "properly delimiting it via a comma - i.e. like this - $operator(<an expression>,,<region).");
        } else {
          $someregion = "<noloop>"; # default min/max region otherwise is current location
        }
        print "INFO: default location of $someregion has been applied to $operator operator used in $otype variable $variable{$otype}[$omvar]{name}\n";
        print DEBUG "INFO: default location of $someregion has been applied to $operator operator used in $otype variable $variable{$otype}[$omvar]{name}\n";
        print DEBUG "centring = $centring: contextcentring = $contextcentring\n";
      } else {
        error_stop("region for $operator in $variable{$otype}[$omvar]{name} is not a valid region name: $someregion");
      }

      print DEBUG "INFO: the contents of $operator in $variable{$otype}[$omvar]{name} was found to be:\n".
        "  expression = $expression\n  default = $default\n  someregion = $someregion\n  minseparation,maxseparation = $minseparation,$maxseparation\n";

      $inbit[$nbits] = $expression.$reflect_multiplier_string;

      create_someloop($inbit[$nbits],$operator_type,$centring,$someregion,$deriv,$otype,$omvar);
      $someloop_mvar = $m{"someloop"}; # save someloop type
      if ($minseparation >= 0) { # this is the flag that indicates that a separation loop is being conducted
        $variable{"someloop"}[$someloop_mvar]{"minseparation"} = $minseparation; # separation indices (minseparation determines whether separation looping will be used)
        $variable{"someloop"}[$someloop_mvar]{"maxseparation"} = $maxseparation;
        $mseparation_list++;
        $sub_string{"set_mseparation_list"}="mseparation_list = $mseparation_list\n"; # will only be set if >0
        $variable{"someloop"}[$someloop_mvar]{"separation_list_number"} = $mseparation_list;
        if ($options =~ /(^|\,)\s*faceseparation\s*(\,|$)/i) { $variable{"someloop"}[$someloop_mvar]{"faceseparation"} = 1; } # flag to indicate that we're loop through cells that share a face, rather than a node
        $faceseparationflag = 0;
        ($faceseparationflag,$name) = search_operator_contents("faceseparationflag",$next_contents_number);
        if (nonempty($faceseparationflag)) {
          $variable{"someloop"}[$someloop_mvar]{"faceseparation"} = 1; # make sure that we are looping through surrounding cells separated by faces rather than nodes
          mequation_interpolation($faceseparationflag,"face",0,$otype,$omvar,"someloop",$someloop_mvar); # convert faceseparationflag to face centred, with no derivative required
          $variable{"someloop"}[$someloop_mvar]{"faceseparationflag"} = $faceseparationflag; # save for fortran evaluation
          print DEBUG "setting faceseparationflag = $faceseparationflag for someloop $someloop_mvar\n";
        }
      }

# include the effects of any reflect options in the someloop - if they are not relevant to the particular region then they will be ignored when the fortran is created
      if (@reflect) { @{$variable{"someloop"}[$m{"someloop"}]{"reflect"}} = @reflect; }

      if ($operator_type eq "min" || $operator_type eq "max") {
        if (empty($default) && $operator_type eq "min") {
          $default = "+huge";
        } elsif (empty($default) && $operator_type eq "max") {
          $default = "-huge";
        } else {
          mequation_interpolation($default,$contextcentring,$deriv,$otype,$omvar,"someloop",$someloop_mvar); # interpolate default to current location
        }
        print DEBUG "INFO: default value will be: $default\n";
        $variable{"someloop"}[$someloop_mvar]{"default"} = $default; # save default
        print DEBUG "INFO: setting default = $default for someloop $someloop_mvar\n";
      }

#---------------------
# ref: delta, ref: celldelta, ref: facedelta
# delta
# the delta function is one in the specified region, zero otherwise
# single argument is the region
    } elsif ($operator_type eq "delta") {

# TODO: use new region knowledge
      ($someregion,$name) = search_operator_contents("region",$next_contents_number);
      if (empty($someregion)) { error_stop("region not found in $centring $operator in $otype $variable{$otype}[$omvar]{name}"); }
      if ( $someregion !~ /^<.+?>$/ ) { error_stop("format for region incorrect in $centring $operator in $otype $variable{$otype}[$omvar]{name}: region = $someregion"); }
      $someregion = examine_name($someregion,"regionname"); # standardise regionname
#     ($someregion) = $someregion =~ /^<(.*)>$/; # now remove delimiters so that the mequation does not contain any <> strings
#     if ( $centring eq "none" ) { error_stop("problem with $centring centring of $operator in $otype $variable{$otype}[$omvar]{name}"); }
#     $inbit[$nbits] = 'region_delta('.ijkstring($centring).',"'.$centring.'","'.$someregion.'")';
      if ( $centring !~ /cell|face|node/ ) { error_stop("problem with $centring centring of region $someregion in $operator in $otype $variable{$otype}[$omvar]{name}"); }
# find region index to place directly into code
      my $nfound = check_region_and_add_if_not_there($someregion,$centring,"region $someregion used in $centring $operator in $otype $variable{$otype}[$omvar]{name}"); 
      $inbit[$nbits] = 'region_delta(ijk='.ijkstring($centring).',region_number='.$region[$nfound]{"fortran"}.')';

#---------------------
# ref: link, ref: celllink, ref: facelink, ref: facetofacelink, ref: facetocelllink, ref: celltocelllink, ref: celltofacelink
# link
# facelink = facetofacelink, celllink = celltocelllink, facetocelllink, celltofacelink (local-to-remote-link)
# evaluate expression at element in remote region that is closest to element in local region
    } elsif ($operator_type eq "link") {

# three arguments are required:
# this is a link from the local (from) region to the remote (to) region, allowing data from the remote (to) region to be place in the local (from) region
# hence in celltofacelink, facetocelllink etc:
#  first centring is that of the whole operator, and thus also the centring of the local (from) region
#  second centring is that of the remote (to) region
# first argument is the equation to be evaluated in the remote or to region
# second argument is the local (from) region - every element within this region will have a link to an element in the remote (to) region defined
# third argument is the remote (to) region

      if (!($to_centring)) { $to_centring = $centring; } # facelink = facetofacelink etc

      ($expression,$name) = search_operator_contents("expression",$next_contents_number);
      if (empty($expression)) { error_stop("expression part for $operator in $otype $variable{$otype}[$omvar]{name} not found:\n  operator_contents = ".Dumper(\%operator_contents)); }

# find both regions
# TODO: use new region knowledge

      ($from_region,$name) = search_operator_contents("fromregion","localregion",$next_contents_number);
      if ( $from_region =~ /^\<.+?\>$/ ) {
        $from_region=examine_name($from_region,"regionname");
        print DEBUG "INFO: from_region $from_region of centring $centring identified in $otype $variable{$otype}[$omvar]{name}\n";
# find region index to place directly into code
        $nfound = check_region_and_add_if_not_there($from_region,$centring,"from $centring region $from_region used in $centring $operator in $otype $variable{$otype}[$omvar]{name}"); 
        if ($region[$nfound]{"dynamic"}) { error_stop("from $centring region $from_region used in $centring $operator is dynamic: link regions must be static: in $otype $variable{$otype}[$omvar]{name}"); }
        $from_region_number = $region[$nfound]{"fortran"};
      } elsif (empty($from_region)) { error_stop("from_region of centring $centring missing in $otype $variable{$otype}[$omvar]{name}");
      } else { error_stop("from_region $from_region of centring $centring incorrectly specified in $otype $variable{$otype}[$omvar]{name}"); }

      ($to_region,$name) = search_operator_contents("toregion","remoteregion",$next_contents_number);
      if ( $to_region =~ /^\<.+?\>$/ ) {
        $to_region=examine_name($to_region,"regionname");
        print DEBUG "INFO: to_region $to_region of centring $to_centring identified in $otype $variable{$otype}[$omvar]{name}\n";
# find region index to place directly into code
        $nfound = check_region_and_add_if_not_there($to_region,$to_centring,"to $to_centring region $to_region used in $centring $operator in $otype $variable{$otype}[$omvar]{name}"); 
        if ($region[$nfound]{"dynamic"}) { error_stop("to $to_centring region $to_region used in $centring $operator is dynamic: link regions must be static: in $otype $variable{$otype}[$omvar]{name}"); }
        $to_region_number = $region[$nfound]{"fortran"};
      } elsif (empty($to_region)) {
        error_stop("to_region of centring $to_centring missing in $otype $variable{$otype}[$omvar]{name}");
      } else {
        error_stop("to_region $to_region of centring $to_centring incorrectly specified in $otype $variable{$otype}[$omvar]{name}");
      }

      print DEBUG "INFO: the contents of $operator in $otype $variable{$otype}[$omvar]{name} was found to be:\n".
        "  expression = $expression\n  centring fromregion fromregionnumber (localregion) = $centring $from_region $from_region_number\n  tocentring toregion toregionnumber (remoteregion) = $to_centring $to_region $to_region_number\n";

      $inbit[$nbits] = $expression;

      create_someloop($inbit[$nbits],$operator_type,$to_centring,"<noloop>",$deriv,$otype,$omvar);

# find region_link if it already exists and save to someloop
      foreach $mlink ( 0 .. $#region_link ) { # $# signifies the index of the last element in the array
        if ($region_link[$mlink]{"to_region_number"} eq $to_region_number && $region_link[$mlink]{"from_region_number"} eq $from_region_number &&
            $region_link[$mlink]{"to_centring"} eq $to_centring && $region_link[$mlink]{"from_centring"} eq $centring &&
            $region_link[$mlink]{"options"} eq $options) {
          $variable{"someloop"}[$m{"someloop"}]{"region_mlink"} = $mlink; # previous link can be reused
          print DEBUG "INFO: reusing region_link number $region_link[$mlink]{number} in $operator for $otype $variable{$otype}[$omvar]{name}\n";
          last;
        }
      }

#      print "mlink = $mlink\n";
# otherwise create a new one
#      if (!($variable{"someloop"}[$m{"someloop"}]{"region_mlink"})) {
      if (!(defined($variable{"someloop"}[$m{"someloop"}]{"region_mlink"}))) {
        $mlink = $#region_link + 1;
        $region_link[$mlink]{"to_region"} = $to_region;
        $region_link[$mlink]{"from_region"} = $from_region;
        $region_link[$mlink]{"to_region_number"} = $to_region_number;
        $region_link[$mlink]{"from_region_number"} = $from_region_number;
        $region_link[$mlink]{"to_centring"} = $to_centring;
        $region_link[$mlink]{"from_centring"} = $centring;
        $region_link[$mlink]{"options"} = $options;
        $region_link[$mlink]{"number"} = $mlink + 1; # perl and fortran indices are offset by one
        $variable{"someloop"}[$m{"someloop"}]{"region_mlink"} = $mlink; # tell someloop the mlink number
        print DEBUG "INFO: creating region_link number $region_link[$mlink]{number} in $operator for $otype $variable{$otype}[$omvar]{name}\n";
      }

#---------------------
# ref: newtonupdate
# ref: magnitude
# newtonupdate is used for debugging
# it evaluates as the latest (unbackstepped) newton update for the named unknown variable
# magnitude gives the magnitude for an unknown variable
# both functions only accept an unknown name as an argument (ie, not an expression)
    } elsif ($operator_type eq "newtonupdate" || $operator_type eq "magnitude") {

      ($unknown,$name) = search_operator_contents("unknown",$next_contents_number); # the only argument to this function should be a reference to one unknown variable
      if (empty($unknown)) { error_stop("unknown variable name for $operator in $otype $variable{$otype}[$omvar]{name} not found:\n  operator_contents = ".Dumper(\%operator_contents)); }
# now find the unknown number and check on it's centring
      $mvar = 0;
      foreach $mcheck ( 1 .. $m{"unknown"} ) {
        if ($unknown eq $variable{"unknown"}[$mcheck]{"maxima"}) { $mvar = $mcheck; last; }
      }
      if (!($mvar)) { error_stop("variable (maxima) name $unknown found in $operator in $otype $variable{$otype}[$omvar]{name} is not identified as an unknown variable:\n  operator_contents = ".Dumper(\%operator_contents)); }

      if ($operator_type eq "newtonupdate") {
# for the newtonupdate the centrings have to be consistent
        if ($centring ne $variable{"unknown"}[$mvar]{"centring"})  { error_stop("unknown variable $variable{unknown}[$mvar]{name} which is found in $operator in $otype $variable{$otype}[$omvar]{name} does not have the same centring as the newtonupdate operator"); }
        $inbit[$nbits] = "newtonupdate[$variable{unknown}[$mvar]{fortran_number},".ijkstring($centring)."]";
      } else {
# for the magnitude the centring must be none
        if ($centring ne "none") { error_stop("none centring must be used for the $operator operator which is found in the expression for $otype $variable{$otype}[$omvar]{name}"); }
        $inbit[$nbits] = "magnitude[$variable{unknown}[$mvar]{fortran_number}]";
      }

#---------------------
# ref: boundangle
# increment multiples of 2pi until an angle is in the range -pi < angle <= pi
    } elsif ($operator_type eq "boundangle") {

      ($expression,$name) = search_operator_contents("expression",$next_contents_number);
      if (empty($expression)) { error_stop("expression part for $operator in $otype $variable{$otype}[$omvar]{name} not found:\n  operator_contents = ".Dumper(\%operator_contents)); }
      create_someloop($expression,"sum",$centring,"<noloop>",$deriv,$otype,$omvar);
      $inbit[$nbits] = "atan2(sin($expression),cos($expression))"; # just use trig functions as simplest

#---------------------
# ref: cellvofd
# here we use vof techniques to find the position of a volume fraction interface within a cell
    } elsif ($operator_type eq "vofd") {
      if ($centring ne "cell") { error_stop("$operator_type operator has incorrect $centring centring in $otype $variable{$otype}[$omvar]{name} (do you mean cellvofd?)"); }
# look for each of the components and place them in someloops to be picked up by the vof fortran subroutines
      $external_arguments = 'thread,m,i,j,k,error_string,deriv='.$deriv;

# 1) first the volume fraction phi
      ($tmp,$name) = search_operator_contents("phi",$next_contents_number);
      if (empty($tmp)) { error_stop("phi part for $operator in $otype $variable{$otype}[$omvar]{name} not found:\n  operator_contents = ".Dumper(\%operator_contents)); }
      create_someloop($tmp,"sum","cell","<noloop>",$deriv,$otype,$omvar);
# start assembling fortran call, disguised as maxima array
      $external_arguments = $external_arguments.',msomeloop_phi='.$m{someloop};

# 2-4) each of the unit normals
      for $l ( 1 .. 3) {
        ($tmp,$name) = search_operator_contents("normal[l=$l]",$next_contents_number);
        if (empty($tmp)) { 
          $external_arguments = $external_arguments.',msomeloop_normal_l'.$l.'=-1'; # an index of -1 means no normal is specified in this direction
        } else {
          create_someloop($tmp,"sum","cell","<noloop>",$deriv,$otype,$omvar);
          $external_arguments = $external_arguments.',msomeloop_normal_l'.$l.'='.$m{someloop};
        }
      }

# 5) the volume fraction cutoff phitol
      ($tmp,$name) = search_operator_contents("phitol",$next_contents_number);
      if (empty($tmp)) {
        $external_arguments = $external_arguments.',msomeloop_phitol=-1'; # an index of -1 means no phitol value is specified
      } else {
        create_someloop($tmp,"sum","cell","<noloop>",$deriv,$otype,$omvar);
        $external_arguments = $external_arguments.',msomeloop_phitol='.$m{someloop};
      }

# choose method to use based on options
      $method = 1; # default is linearone for now
      if ($options && $options =~ /(^|\,)\s*linear(|one)\s*(\,|$)/) {
        $method = 1; # which is linear fit between d_max and d_min
      } elsif ($options && $options =~ /(^|\,)\s*lineartwo\s*(\,|$)/) {
        $method = 2; # which is linear fit over two regions, between d_max, centroid and d_min
      } elsif ($options && $options =~ /(^|\,)\s*parabolic\s*(\,|$)/) {
        $method = 3; # which is parabolic fit between d_max, centroid and d_min
      } elsif ($options && $options =~ /(^|\,)\s*exactpiecewise\s*(\,|$)/) {
        $method = 4; # which is matches volume precisely
      } elsif ($options && $options =~ /(^|\,)\s*exact\s*(\,|$)/) {
        $method = 5; # which is matches volume precisely
      } elsif ($options && $options =~ /(^|\,)\s*best\s*(\,|$)/) {
        $method = 0; # chooses the most accurate method available based on the cell's dimension
      }
      $external_arguments = $external_arguments.",method=$method";

      $inbit[$nbits] = $centring.$operator_type."[".$external_arguments."]"; # place dummy function in someloop to trigger someloop updates in sub update_someloop_fortran
      create_someloop($inbit[$nbits],"external","cell","<noloop>",$deriv,$otype,$omvar);
      $variable{"someloop"}[$m{"someloop"}]{"external_subroutine"} = $centring.$operator_type.'('.$external_arguments.')'; # give the fortran subroutine to be called to update someloop(m)
      
#---------------------
# ref: facevofphi
# here we use vof techniques to find the advection flux of phi through a face
    } elsif ($operator_type eq "vofphi") {
      if ($centring ne "face") { error_stop("$operator_type operator has incorrect $centring centring in $otype $variable{$otype}[$omvar]{name} (do you mean facevofphi?)"); }
# look for each of the components and place them in someloops to be picked up by the vof fortran subroutines
# first arguments (thread,m,i,j,error_string,deriv) are the same for every external routine, for consistency
      $external_arguments = 'thread,m,i,j,k,error_string,deriv='.$deriv;

# 1) first the volume fraction phi, necessary incase there is no interface in this local
      ($tmp,$name) = search_operator_contents("phi",$next_contents_number);
      if (empty($tmp)) { error_stop("phi part for $operator in $otype $variable{$otype}[$omvar]{name} not found:\n  operator_contents = ".Dumper(\%operator_contents)); }
      create_someloop($tmp,"sum","cell","<noloop>",$deriv,$otype,$omvar);
# start assembling fortran call, disguised as maxima array
      $external_arguments = $external_arguments.',msomeloop_phi='.$m{someloop};

# 2) d, the signed distance from the cell centre to interface line
      ($tmp,$name) = search_operator_contents("d",$next_contents_number);
      if (empty($tmp)) {
        error_stop("d part for $operator in $otype $variable{$otype}[$omvar]{name} not found:\n  operator_contents = ".Dumper(\%operator_contents));
      } else {
        create_someloop($tmp,"sum","cell","<noloop>",$deriv,$otype,$omvar);
        $external_arguments = $external_arguments.',msomeloop_d='.$m{someloop};
      }

# 3) flux, which is not passed to routine but instead is used to determine i from j
      ($flux,$name) = search_operator_contents("flux",$next_contents_number);
      if (empty($flux)) {
        error_stop("flux part for $operator in $otype $variable{$otype}[$omvar]{name} not found:\n  operator_contents = ".Dumper(\%operator_contents));
      }

# 4-6) each of the unit normals
      for $l ( 1 .. 3) {
        ($tmp,$name) = search_operator_contents("normal[l=$l]",$next_contents_number);
        if (empty($tmp)) { 
          $external_arguments = $external_arguments.',msomeloop_normal_l'.$l.'=-1'; # an index of -1 means no normal is specified in this direction
        } else {
          create_someloop($tmp,"sum","cell","<noloop>",$deriv,$otype,$omvar);
          $external_arguments = $external_arguments.',msomeloop_normal_l'.$l.'='.$m{someloop};
        }
      }

# 7) the volume fraction cutoff phitol
      ($tmp,$name) = search_operator_contents("phitol",$next_contents_number);
      if (empty($tmp)) {
        $external_arguments = $external_arguments.',msomeloop_phitol=-1'; # an index of -1 means no phitol value is specified
      } else {
        create_someloop($tmp,"sum","cell","<noloop>",$deriv,$otype,$omvar);
        $external_arguments = $external_arguments.',msomeloop_phitol='.$m{someloop};
      }

# choose method to use based on options
      $method = 1; # default is linearone for now
      if ($options && $options =~ /(^|\,)\s*linearone\s*(\,|$)/) {
        $method = 1; # which is linear fit between d_max and d_min
      } elsif ($options && $options =~ /(^|\,)\s*lineartwo\s*(\,|$)/) {
        $method = 2; # which is linear fit over two regions, between d_max, centroid and d_min
      } elsif ($options && $options =~ /(^|\,)\s*parabolic\s*(\,|$)/) {
        $method = 3; # which is parabolic fit between d_max, centroid and d_min
      } elsif ($options && $options =~ /(^|\,)\s*exact\s*(\,|$)/) {
        $method = 4; # which is matches volume precisely
      }
      $external_arguments = $external_arguments.",method=$method";

      $inbit[$nbits] = $centring.$operator_type."[".$external_arguments."]"; # place dummy function in someloop to trigger someloop updates in sub update_someloop_fortran
#     create_someloop($inbit[$nbits],"external","face","<noloop>",$deriv,$otype,$omvar);
      create_someloop($inbit[$nbits],"external","cell","<upwindfaceicells>",$deriv,$otype,$omvar);
#         create_someloop($inbit[$nbits],"sum","cell","<upwindfaceicells>",$deriv,$otype,$omvar);
      $someloop_mvar = $m{"someloop"}; # save someloop type

# give the fortran subroutine to be called to update someloop(m)
      $variable{"someloop"}[$m{"someloop"}]{"external_subroutine"} = $centring.$operator_type.'('.$external_arguments.')'; # give the fortran subroutine to be called to update someloop(m)

# also deal with flux variable
      mequation_interpolation($flux,"face",0,$otype,$omvar,"someloop",$someloop_mvar); # convert flux to face centred as will not become part of someloop - deriv not needed
      $variable{"someloop"}[$someloop_mvar]{"flux"} = $flux; # save flux for evaluating upwindfaceicells region
      print DEBUG "setting flux = $flux for facevofphi someloop $someloop_mvar\n";
      
#---------------------
# ref: cellvofphiadjust
# here we adjust a phif advection amount to ensure that new phi remains bounded
    } elsif ($operator_type eq "vofphiadjust") {
      if ($centring ne "cell") { error_stop("$operator_type operator has incorrect $centring centring in $otype $variable{$otype}[$omvar]{name} (do you mean cellvofphiadjust?)"); }
# look for each of the components and place them in someloops to be picked up by the vof fortran subroutines
      $external_arguments = 'thread,m,i,j,k,error_string,deriv='.$deriv;

# 1) first the previous relstep volume fraction phi:  phi[r=1]
      ($tmp,$name) = search_operator_contents("phi[r=1]",$next_contents_number);
      if (empty($tmp)) { error_stop("phi[r=1] part for $operator in $otype $variable{$otype}[$omvar]{name} not found:\n  operator_contents = ".Dumper(\%operator_contents)); }
      create_someloop($tmp,"sum","cell","<noloop>",$deriv,$otype,$omvar);
      $external_arguments = $external_arguments.',msomeloop_phi_r1='.$m{someloop};

# 2) the face centred advection phi:  phif
      ($tmp,$name) = search_operator_contents("phif",$next_contents_number);
      if (empty($tmp)) { error_stop("phif part for $operator in $otype $variable{$otype}[$omvar]{name} not found:\n  operator_contents = ".Dumper(\%operator_contents)); }
      create_someloop($tmp,"sum","face","<noloop>",$deriv,$otype,$omvar);
      $external_arguments = $external_arguments.',m_phif='.$m{someloop};

# 3) flux, which is passed to routine here (so uses m_flux rather than msomeloop_flux call)
      ($tmp,$name) = search_operator_contents("flux",$next_contents_number);
      if (empty($tmp)) { error_stop("flux part for $operator in $otype $variable{$otype}[$omvar]{name} not found:\n  operator_contents = ".Dumper(\%operator_contents)); }
      create_someloop($tmp,"sum","face","<noloop>",$deriv,$otype,$omvar);
      $external_arguments = $external_arguments.',m_flux='.$m{someloop};

# 4) dt
      ($tmp,$name) = search_operator_contents("dt",$next_contents_number);
      if (empty($tmp)) { error_stop("dt part for $operator in $otype $variable{$otype}[$omvar]{name} not found:\n  operator_contents = ".Dumper(\%operator_contents)); }
      create_someloop($tmp,"sum","none","<noloop>",$deriv,$otype,$omvar);
      $external_arguments = $external_arguments.',msomeloop_dt='.$m{someloop};

      $inbit[$nbits] = $centring.$operator_type."[".$external_arguments."]"; # place dummy function in someloop to trigger someloop updates in sub update_someloop_fortran
      create_someloop($inbit[$nbits],"external","cell","<noloop>",$deriv,$otype,$omvar);
      $variable{"someloop"}[$m{"someloop"}]{"external_subroutine"} = $centring.$operator_type.'('.$external_arguments.')'; # give the fortran subroutine to be called to update someloop(m)

#---------------------
# ref: cellvofphishape
# the cellvofphishape function takes three vectors, a size, centre and axis, returning a volume fraction for each cell based on the requested shape
# the size of the object is determined by the maximum size that is consistent with all of the object's dimensions (defaults to huge), so for a sphere set one size to the sphere's diameter
# the centre of the object is specified by the centre vector, defaulting to zero if a component is not specified
# the axis vector specifies a line about which the axis of the object is rotated, with a length equal to the rotation about that line required (in degrees) - by default each object has its unrotated centreline along the z axis
# phitol is the accuracy required, which is used to determine the number of sample points
# for 2D shapes choose a 3D shape that gives the correct intersection with the 2D plane (ie, sphere for circle, box for rectangle)
    } elsif ($operator_type eq "vofphishape") {
      if ($centring ne "cell") { error_stop("$operator_type operator has incorrect $centring centring in $otype $variable{$otype}[$omvar]{name} (do you mean cellvofphishape?)"); }
# look for each of the components and place them in someloops to be picked up by the vof fortran subroutines
      $external_arguments = 'thread,m,i,j,k,error_string,deriv='.$deriv;

# 1-3) each of the sizes
# 4-6) each of the centres - note spelling
# 7-10) each of the rotation axiss
      for $l ( 1 .. 3) {
        for $vector ( "size","centre","axis") {
          ($tmp,$name) = search_operator_contents("$vector\[l=$l\]",$next_contents_number);
          if (empty($tmp)) { 
            $external_arguments = $external_arguments.",msomeloop_$vector".$l.'=-1'; # an index of -1 means not specified in this direction
          } else {
            create_someloop($tmp,"sum","cell","<noloop>",$deriv,$otype,$omvar);
            $external_arguments = $external_arguments.",msomeloop_$vector".$l.'='.$m{someloop};
          }
        }
      }

# 11) the volume fraction accuracy required
      ($tmp,$name) = search_operator_contents("phitol",$next_contents_number);
      if (empty($tmp)) {
        $external_arguments = $external_arguments.',msomeloop_phitol=-1'; # an index of -1 means no phitol value is specified
      } else {
        create_someloop($tmp,"sum","cell","<noloop>",$deriv,$otype,$omvar);
        $external_arguments = $external_arguments.',msomeloop_phitol='.$m{someloop};
      }

# choose shape to use based on options
# accept 2D shape names as well, for convienience
      $shape = 1; # default is sphere for now
      if ($options && $options =~ /(^|\,)\s*sphere|circle\s*(\,|$)/) { # as per ellipsoid but all diameters equal to the minimum specified
        $shape = 1;
      } elsif ($options && $options =~ /(^|\,)\s*ellipsoid|ellipse\s*(\,|$)/) {
        $shape = 2;
      } elsif ($options && $options =~ /(^|\,)\s*cube|square\s*(\,|$)/) { # as per box but all side lengths equal to the minimum specified
        $shape = 3;
      } elsif ($options && $options =~ /(^|\,)\s*box|rectangle\s*(\,|$)/) {
        $shape = 4;
      } elsif ($options && $options =~ /(^|\,)\s*cylinder\s*(\,|$)/) { # size[l=1] is diameter, size[l=2] is length, centre is geometrical centre
        $shape = 5;
      }
      $external_arguments = $external_arguments.",shape=$shape";

# sanity check on deriv
      if ($deriv) { print "WARNING: cellvofphishape cannot calculate a derivative, yet deriv is true in $otype $variable{$otype}[$omvar]{name}\n"; }

      $inbit[$nbits] = $centring.$operator_type."[".$external_arguments."]"; # place dummy function in someloop to trigger someloop updates in sub update_someloop_fortran
      create_someloop($inbit[$nbits],"external","cell","<noloop>",$deriv,$otype,$omvar);
      $variable{"someloop"}[$m{"someloop"}]{"external_subroutine"} = $centring.$operator_type.'('.$external_arguments.')'; # give the fortran subroutine to be called to update someloop(m)
      
#---------------------
    } else { error_stop("problem with $operator in mequation_interpolation in $otype $variable{$otype}[$omvar]{name}"); }

    if (%operator_contents) {
      error_stop("the contents of $operator in $variable{$otype}[$omvar]{name} contains some arguments which are either duplicated or not valid: ".
        "the offending arguments are ".Dumper(\%operator_contents));
    }
      
    print DEBUG "current parts in mequation_interpolation: operator = $operator: nbits = $nbits: outbit = $outbit[$nbits]: inbit = $inbit[$nbits]: _[0] = $_[0]\n";
    $nbits++;
  }
  $outbit[$nbits] = $_[0];

# #-------------------------------------------------
# # loop through bits outside of operators checking on validity and doing replacements

#   $n = 0;
#   while ( $n <= $nbits ) {
#     print DEBUG "in mequation_interpolation checking centring of outbit = $outbit[$n]\n";
# # as empties are all zero don't need to interpolate those
#     foreach $type (@user_types,"system") {
#       foreach $mvar ( 1 .. $m{$type} ) {
#         if ($variable{$type}[$mvar]{"centring"} eq "none") { next; } # skip variables that have no centring
#         if ($variable{$type}[$mvar]{"centring"} eq $contextcentring ) { next; } # skip variables that have correct centring
#         if ($outbit[$n] =~ /\Q$variable{$type}[$mvar]{"maxima"}/) { # look for variable in outbit
#           $pre = $`; $post = $';
#           if ($contextcentring eq "none") {
#             print "WARNING: mequation is none centred but variable $variable{$type}[$mvar]{name} in outbit has $variable{$type}[$mvar]{centring} centring\n";
#             next;
#           }
#           print DEBUG "found match with $variable{$type}[$mvar]{centring} centred $variable{$type}[$mvar]{maxima} in outbit = $outbit[$n]\n";
#           print DEBUG "before splicing: nbits = $nbits: outbit = @outbit: inbit = @inbit\n";
# # splice in new in and out bits here
#           $nbits++;
#           splice(@outbit,$n,1,$pre,$post); # remove present outbit and replace with two new elements, pre and post
#           $tmp = $variable{$type}[$mvar]{"maxima"};
#           if ($contextcentring eq "cell" && $variable{$type}[$mvar]{"centring"} eq "face") {
#             $tmp = 'cellkernel[i,0,ns]*('.$tmp.')';
#             $someregion = "<cellkernelregion[l=0]>";
#           } elsif ($contextcentring eq "face" && $variable{$type}[$mvar]{"centring"} eq "cell") {
#             $tmp = 'facekernel[j,0,ns]*('.$tmp.')';
#             $someregion = "<facekernelregion[l=0]>";
#           } else { error_stop("problem with locating $variable{$type}[$mvar]{name} in outbit in $otype $variable{$otype}[$omvar]{name}"); }
# # looks for noderivative for this variable
#           $deriv = $contextderiv;
#           if ($variable{$type}[$mvar]{"options"} && $variable{$type}[$mvar]{"options"} =~ /(^|\,)noderivative(\,|$)/) { $deriv = 0; }
#           create_someloop($tmp,"sum",$variable{$type}[$mvar]{"centring"},$someregion,$deriv,$otype,$omvar);
#           splice(@inbit,$n,0,$tmp); # add a new element at location n-1
#           print DEBUG "after splicing: nbits = $nbits: outbit = @outbit: inbit = @inbit\n";
#         }
#       }
#     }
#     $n++;
#   }

# #-------------------------------------------------
# now assemble equation again
  $_[0] = "";
  foreach $n ( 0 .. $nbits-1 ) {
    $_[0] = $_[0].$outbit[$n].$inbit[$n];
  }
  $_[0] = $_[0].$outbit[$nbits];

  print DEBUG "after operators have been dealt with: from originator $otype $variable{$otype}[$omvar]{name}: mequation _[0] = $_[0]\n";

#-------------------------------------------------
# run through checking on centring of all variables, interpolating any that are inconsistent (and that were in the outbits) to the correct centring
# someloops don't need interpolation, as they should have had the correct contextcentring when they were created

  foreach $type (@user_types,"system") { 
    foreach $mvar ( 1 .. $m{$type} ) {
      if ($variable{$type}[$mvar]{"centring"} eq "none") {next;} # none centred variables can appear anywhere
      if ($variable{$type}[$mvar]{"centring"} eq $contextcentring ) { next; } # skip variables that have correct centring
      if ($_[0] =~ /\Q$variable{$type}[$mvar]{"maxima"}/) { # see if variable is in the mequation
        if ($contextcentring eq "none") { error_stop("mequation has none contextcentring but variable $variable{$type}[$mvar]{name} used in the definition of ".
          "$otype $variable{$otype}[$omvar]{name} has $variable{$type}[$mvar]{centring} centring: mequation = $_[0]\n"); }
        print DEBUG "found $variable{$type}[$mvar]{centring} centred $type $variable{$type}[$mvar]{name} $variable{$type}[$mvar]{maxima} in ".
          "$otype $variable{$otype}[$omvar]{name} mequation that is $contextcentring centred\n";
        print DEBUG "before interpolating $variable{$type}[$mvar]{maxima}: $_[0]\n";
        $tmp = $variable{$type}[$mvar]{"maxima"};
# implicit centring averaging cannot know whether what is being averaged is a component, and hence will not get reflections right for vectors
        $interpolate_centring = $variable{$type}[$mvar]{"centring"};
        if ($contextcentring eq "cell" && $variable{$type}[$mvar]{"centring"} eq "face") {
          $tmp = 'cellkernel[i,0,ns]*('.$tmp.')';
          $someregion = "<cellkernelregion[l=0]>";
        } elsif ($contextcentring eq "cell" && $variable{$type}[$mvar]{"centring"} eq "node") {
          $tmp = 'cellkernel[i,4,ns]*('.$tmp.')';
          $someregion = "<cellkernelregion[l=4]>";
        } elsif ($contextcentring eq "face" && $variable{$type}[$mvar]{"centring"} eq "cell") {
          $tmp = 'facekernel[j,0,ns]*('.$tmp.')';
          $someregion = "<facekernelregion[l=0]>";
        } elsif ($contextcentring eq "face" && $variable{$type}[$mvar]{"centring"} eq "node" ) {
# for the case of a face centred context and node centred variable, in this level average variable from cells to faces (subsequent level inside this will average from nodes to cells)
          $interpolate_centring = "cell";
          $tmp = 'facekernel[j,0,ns]*('.$tmp.')';
          $someregion = "<facekernelregion[l=0]>";
        } elsif ($contextcentring eq "node" && $variable{$type}[$mvar]{"centring"} eq "cell") {
          $tmp = 'nodekernel[k,0,ns]*('.$tmp.')';
          $someregion = "<nodekernelregion[l=0]>";
        } elsif ($contextcentring eq "node" && $variable{$type}[$mvar]{"centring"} eq "face") {
# for the case of a node centred context and face centred variable, in this level average variable from cells to nodes (subsequent level inside this will average from faces to cells)
          $interpolate_centring = "cell";
          $tmp = 'nodekernel[k,0,ns]*('.$tmp.')';
          $someregion = "<nodekernelregion[l=0]>";
        } else { error_stop("problem with locating $variable{$type}[$mvar]{name} in outbit in $otype $variable{$otype}[$omvar]{name}"); }
# looks for noderivative for this variable
#       $deriv = $contextderiv;
#       if ($deriv ne $variable{$type}[$mvar]{"deriv"}) { $deriv = 0; }
# unknowns are the exception here - even though they have deriv = 0 (meaning that a derivative does not need to be calculated), they still have a valid derivative value (hasderiv = 1)
#       print "contextderiv = $contextderiv: hasderiv = $variable{$type}[$mvar]{hasderiv}: name = $variable{$type}[$mvar]{name} \n";
#       print "local variable = $variable{$ltype}[$lmvar]{name}\n"; 
#       print "parent variable = $variable{$otype}[$omvar]{name}\n"; 
        $deriv = min($contextderiv,$variable{$type}[$mvar]{"hasderiv"});
# create replacement someloop variable and substitute it back in the original expression (multiple times if necessary)
#       create_someloop($tmp,"sum",$variable{$type}[$mvar]{"centring"},$someregion,$deriv,$otype,$omvar); # this replaces tmp with someloop maxima name
        create_someloop($tmp,"sum",$interpolate_centring,$someregion,$deriv,$otype,$omvar); # this replaces tmp with someloop maxima name
        $_[0] =~ s/\Q$variable{$type}[$mvar]{"maxima"}/$tmp/g;
        print DEBUG "after interpolating $variable{$type}[$mvar]{maxima}: $_[0]\n";
      }
    }
  }

#-------------------------------------------------
# run through equations again checking on dependencies of variables, and redefining any if possible to someloops
# update schedule in fortran:
#  - read constants
#  - update constants
#  - update unknowns (initial values)
#  - update initial_transients (T)
#  - update initial_newtients (N)
#  - update derived
#  - update equations
#  - update output (T)

#  - start of time loop (T)
#    - update transient (T)
#    - update initial_newtient (T and N)
#    - update derived (T)
#    - update equation (T)
     
#    - start of newt loop
#      - update unknown (updated values)
#      - update newtients (N)
#      - update derived (N)
#      - update equation (N)
#      - update output (possibly)
#    - end of newt loop

#    - update output (possibly)
#  - end of time loop

#  - check (update) conditions can happen anywhere
#  - update output can also happen pretty-much anywhere

# general rules:
# all variables: may not refer to themselves, no substitution possible
# in search terms ($type) ignore transient and newtient references, regarding initial_transient and initial_newtients as generic instead
# transients, newtients, outputs and conditions only depend on previously evaluated variables, by definition
# locals can appear anywhere and are uniquely unwrapped anyway (substitution occurs below)
# any someloops appearing in the expression at this stage were created directly above, so do not need dependency checking as they will be of the same otype/omvar
# constant: may only refer to other constants of lower mvar: substitute until this occurs
#  ie, subs -constant, unknown, initial_transient, initial_newtient, derived, equation, output, condition
# unknown: may only refer to constants and other unknowns of lower mvar: substitute until this occurs
#  ie, subs -unknown, initial_transient, initial_newtient, derived, equation, output, condition
# initial_transient: may only refer to constants, unknowns and other initial_transients of either lower rindex or same rindex and lower mvar: substitute until this occurs
#  ie, subs -initial_transient, initial_newtient, derived, equation, output, condition
# initial_newtient: may refer to constants, unknowns, transients (or initial_transients) and other initial_newtients of either lower rindex or same rindex and lower mvar: substitute until this occurs
#  ie, subs -initial_newtient, derived, equation, output, condition
# derived: as initial_newtient plus can refer to any newtients and lower order derived
#  ie, subs -derived, equation, output, condition
# equations: as derived plus can refer to any derivied and lower order equations
#  ie, subs -equation, output, condition
# output: as equations plus can refer to any equations and lower order output
#  ie, subs -output, condition
# condition: as output plus can refer to any outputs - any other conditions should be unwrapped - ie, anything is OK except for other conditions
#  ie, subs any other condition

# TODO: create chart showing dependencies of each equation
# TODO: warn about substitution of derived and equation variables in derived expressions, and equation variables and equation expressions

# first record dependencies on any someloops before any substitutions take place
  $type = "someloop";
  foreach $mvar ( 1 .. $m{$type} ) {
    if ($_[0] =~ /\Q$variable{$type}[$mvar]{"maxima"}/) { # look for variable in the final equation
      print DEBUG "INFO: found variable $type $variable{$type}[$mvar]{name} in expression for $otype $variable{$otype}[$omvar]{name}: saving variable's dependency\n";
      $tmpderiv = min($variable{$type}[$mvar]{"hasderiv"},$variable{$otype}[$omvar]{"deriv"}); # noderivative if either replacement or originating variables are noderivative
# record the someloop as a primary dependent variable
      push (@{$variable{$ltype}[$lmvar]{"dependency"}}, { "type" => $type, "mvar" => $mvar, "deriv" => $tmpderiv } );
    }
  }

# now search for other variables, do substitutions and also recording primary and substituted dependencies
  foreach $type (@user_types,"initial_transient","initial_newtient") { # include initial_newtients/transients instead of transients/newtients as if substitution is required, it will be for the initial expressions rather than the normal ones
    foreach $mvar ( 1 .. $m{$type} ) {
      $substitute = 0;
      $lousysubstitute = 0;
      if ($_[0] =~ /\Q$variable{$type}[$mvar]{"maxima"}/) { # look for variable in the final equation
        print DEBUG "INFO: found variable $type $variable{$type}[$mvar]{name} in expression for $otype $variable{$otype}[$omvar]{name}: checking on variable's dependencies:\n";

# all other variables: may not refer to themselves, no substitution possible
        if ($otype eq $type && $omvar == $mvar) {
          print DEBUG "INFO: the equation for $otype variable $variable{$otype}[$omvar]{name} contains a circular reference to itself ".
            "which is not allowed: check this variable's equation\n"."  offending variable is $type variable $variable{$type}[$mvar]{name}\n";
          error_stop("the equation for $otype variable $variable{$otype}[$omvar]{name} contains a circular reference to itself ".
            "which is not allowed: check this variable's equation\n"."  offending variable is $type variable $variable{$type}[$mvar]{name}");
        }

# transients and newtients can be anywhere, initial_transients and initial_newtients are matched separately
# locals can also be anywhere, and are dealt with separately in the next loop
# this is different to the next as these are not included (atleast at this stage for locals) in the saved dependency arrays
        if ($type eq "transient" || $type eq "newtient" || $type eq "local" ) {
          print DEBUG "INFO: variable $otype $variable{$otype}[$omvar]{name} depends on $type $variable{$type}[$mvar]{name} so skipping dependency loop\n";
          next;

# numerical constants, that do not have a mequation, are read in first and so can be used anywhere
# we record the dependence on these however
        } elsif ( !($variable{$type}[$mvar]{"mequation"}) ) {
          print DEBUG "INFO: variable $otype $variable{$otype}[$omvar]{name} depends on $type $variable{$type}[$mvar]{name} so skipping dependency checks\n";

# conditions, newtients, transients, locals - only need conditions substituted
        } elsif ($otype eq "condition" || $otype eq "transient" || $otype eq "newtient" || $otype eq "local") {
          if ($type eq "condition") { $substitute = 1; }

# outputs 
        } elsif ($otype eq "output") {
          if ($type eq "condition" || ($type eq "output" && $mvar > $omvar)) { $substitute = 1;
            if ($type eq "output" && $mvar > $omvar) { $lousysubstitute = 1; } # TODO: probably need to think more about what makes a condition variable a lousy substitute
          }

# equations 
        } elsif ($otype eq "equation") {
          if ($type eq "condition" || $type eq "output" || ($type eq "equation" && $mvar > $omvar)) { $substitute = 1;
            if ($type eq "output" || ($type eq "equation" && $mvar > $omvar)) { $lousysubstitute = 1; }
          }

# deriveds 
        } elsif ($otype eq "derived") {
          if ($type eq "condition" || $type eq "output" || $type eq "equation" || ($type eq "derived" && $mvar > $omvar)) { $substitute = 1;
            if ($type eq "output" || $type eq "equation" || ($type eq "derived" && $mvar > $omvar)) { $lousysubstitute = 1; }
          }

# initial_newtients 
        } elsif ($otype eq "initial_newtient") {
          if ($type eq "condition" || $type eq "output" || $type eq "equation" || $type eq "derived" || ($type eq "initial_newtient" && 
            ( $variable{$type}[$mvar]{"rindex"} > $variable{$otype}[$omvar]{"rindex"} || 
            ( $variable{$type}[$mvar]{"rindex"} == $variable{$otype}[$omvar]{"rindex"} && $mvar > $omvar ) ) ) ) { $substitute = 1; }

# initial_transients 
        } elsif ($otype eq "initial_transient") {
          if ($type eq "condition" || $type eq "output" || $type eq "equation" || $type eq "derived" || $type eq "initial_newtient" || 
            ($type eq "initial_transient" && ( $variable{$type}[$mvar]{"rindex"} > $variable{$otype}[$omvar]{"rindex"} || 
            ( $variable{$type}[$mvar]{"rindex"} == $variable{$otype}[$omvar]{"rindex"} && $mvar > $omvar ) ) ) ) { $substitute = 1; }

# unknowns (initial)
        } elsif ($otype eq "unknown") {
          if ($type eq "condition" || $type eq "output" || $type eq "equation" || $type eq "derived" || $type eq "initial_newtient" || 
            $type eq "initial_transient" || ( $type eq "unknown" && $mvar > $omvar ) ) { $substitute = 1; }

# constants
        } elsif ($otype eq "constant") {
          if ($type eq "condition" || $type eq "output" || $type eq "equation" || $type eq "derived" || $type eq "initial_newtient" || 
            $type eq "initial_transient" || $type eq "unknown" || ( $type eq "constant" && $mvar > $omvar ) ) { $substitute = 1; }

        }

# setup some generic dependency info to be stored
        $tmpderiv = min($variable{$type}[$mvar]{"hasderiv"},$variable{$otype}[$omvar]{"deriv"}); # noderivative if either replacement or originating variables are noderivative
        $dtype = $type; # this is the primary dependency type
        $dtype =~ s/^initial_//; # rename initial types as normal variables for dependency purposes, noting that initial and normal variables share common mvars

# now if substitute is specified do the replacement using the original mequation (oequation)
        if ($substitute) {
          foreach $handle (*STDOUT,*DEBUG) { # http://docstore.mik.ua/orelly/perl/cookbook/ch07_17.htm suggests that * is to be used for filehandles
            print $handle "INFO: replacing $type variable $variable{$type}[$mvar]{name} with a someloop in expression in $otype variable $variable{$otype}[$omvar]{name}\n";
            if ($lousysubstitute) { print $handle "WARNING: this lousy substitution will work, but will probably result in very inefficient code being generated.  The defining input statements of these ".
              "two variables need to be reordered to better reflect their dependencies.  Most likely the definition for $variable{$otype}[$omvar]{name} needs to be moved down the input file ".
              "so that it appears after the definition for $variable{$type}[$mvar]{name}.\n";
            }
          }
          if ($lousysubstitute) { $number_of_lousysubstitutes = $number_of_lousysubstitutes + 1; }
          print DEBUG "INFO: substitution specified: replacing $type $mvar $variable{$type}[$mvar]{name} by a someloop\n".
                      "  before change mequation was $_[0]\n".
                      "  variable that is being replaced has mequation = $variable{$type}[$mvar]{mequation}\n";
          $tmp = $variable{$type}[$mvar]{"oequation"}; # this is passed to create_someloop as the equation, and returns as the maxima someloop name
# difference between hasderiv and deriv is irrelevant, as they only differ for unknowns and unknowns should only be substituted for other unknowns and constants which have deriv=0 anyway
          print DEBUG "  tmpderiv = $tmpderiv: type/mvar hasderiv = $variable{$type}[$mvar]{hasderiv}: otype/omvar deriv = $variable{$otype}[$omvar]{deriv}\n";
          create_someloop($tmp,"sum",$variable{$type}[$mvar]{"centring"},"<noloop>",$tmpderiv,$otype,$omvar);
#         replace_substrings($_[0],$variable{$type}[$mvar]{"maxima"},$tmp);
          $_[0] =~ s/\Q$variable{$type}[$mvar]{"maxima"}/$tmp/g;
          print DEBUG "  after change mequation was $_[0]\n";

# record dependency info, including the someloop variable for any substitutions
          push (@{$variable{$ltype}[$lmvar]{"dependency"}}, { "type" => $dtype, "mvar" => $mvar, "substituted_type" => "someloop", "substituted_mvar" => $m{"someloop"}, "deriv" => $tmpderiv } );
        } else {
          print DEBUG "INFO: no substitutions required for $type $mvar $variable{$type}[$mvar]{name}\n";
# record only the dependent variable if not a local, transient or newtient
          push (@{$variable{$ltype}[$lmvar]{"dependency"}}, { "type" => $dtype, "mvar" => $mvar, "deriv" => $tmpderiv } );
        }

      }
    }
  }

#-------------------------------------------------
# now run through replacing any locals with someloops containing original mequations (oequations)

  $type = "local";
  foreach $mvar ( 1 .. $m{$type} ) {
    if ($_[0] =~ /\Q$variable{$type}[$mvar]{"maxima"}/) { # see if variable is in the mequation
      if ($variable{$type}[$mvar]{"centring"} ne $contextcentring) { error_stop("something amiss with local centring in $otype $variable{$otype}[$omvar]{name} expression"); }
      print DEBUG "found $variable{$type}[$mvar]{centring} centred $type $variable{$type}[$mvar]{name} $variable{$type}[$mvar]{maxima} in ".
        "$otype $variable{$otype}[$omvar]{name} mequation that is $contextcentring centred: replacing this local with a someloop\n";
      print DEBUG "before replacing $variable{$type}[$mvar]{maxima}: $_[0]\n";
      $tmp = $variable{$type}[$mvar]{"oequation"};
      $deriv = min($contextderiv,$variable{$type}[$mvar]{"deriv"});
      create_someloop($tmp,"sum",$variable{$type}[$mvar]{"centring"},"<noloop>",$deriv,$otype,$omvar); # this replaces tmp with someloop maxima name
      $_[0] =~ s/\Q$variable{$type}[$mvar]{"maxima"}/$tmp/g;
      print DEBUG "after replacing $variable{$type}[$mvar]{maxima}: $_[0]\n";
# record dependency info, including someloop for substituted variable
      push (@{$variable{$ltype}[$lmvar]{"dependency"}}, { "type" => $type, "mvar" => $mvar, "substituted_type" => "someloop", "substituted_mvar" => $m{"someloop"}, "deriv" => $deriv } );
    }
  }

  print DEBUG "in mequation_interpolation: after _[0] = ",$_[0],"\n".
              "------------------------------------------------------------------------\n";
}

#-------------------------------------------------------------------------------

sub search_operator_contents {
# subroutine to find a value corresponding to a name in the operator_contents hash
# enter with
# - @_ = prioritised list of names to search for, with the last one being numerical
# exit with
#  ( found value (which may be blank), found name (if blank indicates that name was not found))
# if numerical name is found and is the last calling argument then this is incremented by one
# the found_name/found_value pair are removed from operator_contents

  use strict;
  use Data::Dumper;
  my ($name);
  my $found_name = "";
  my $found_value = "";

  print DEBUG "INFO: on entering search_operator_contents: searches = @_: operator_contents = ".Dumper(\%operator_contents)."\n";
  foreach $name ( @_ ) {
    if (exists($operator_contents{"$name"})) {
      $found_name = $name;
      $found_value = $operator_contents{"$name"};
      delete $operator_contents{"$name"};
      if ($name =~ /^\d+$/ && $name eq $_[$#_]) { $_[$#_] = $_[$#_] + 1; } # if last search element is numerical then increment it by one
      last;
    }
  }

  print DEBUG "INFO: on exiting search_operator_contents: operator_contents = ".Dumper(\%operator_contents)."\n";
  print DEBUG "INFO: on exiting search_operator_contents: _ = @_: found_name = $found_name: found_value = $found_value\n";
  return ($found_value,$found_name);
  
}

#-------------------------------------------------------------------------------

sub split_mequation_at_character {
# subroutine to split an mequation at a particular character (actually ), = or ,) that is outside of variable and operator names and options
# enter with
# - $_[0] as the string (without opening ( in ( case)
# - $_[1] as the character to match (eg ), , or =)
# - $_[2] is otype, which is the type from which the expression originated
# - $_[3] is omvar, which is the mvar from which the expression originated
# within the sub we alter the array operator_contents which is defined in the calling sub
# exit with
# - $_[0] as anything left over, which indicates an error (actually, error terminates script)
# return with
# (before character match (or empty if no character found), after character match (or everything if no character found), not_found flag)

  use strict;
  my $string = $_[0]; # save the complete contents
  my $character = $_[1]; # save the complete contents
  my $otype = $_[2];
  my $omvar = $_[3];
  my $before = '';
  my $after = $string;
  my $not_found = 0;
  my ($match,$closing_delimiter);
  my @delimiters=(); # this will become a list of the matched delimiters, with the last one on the list being the last one matched
  push(@delimiters,$character);

  while ($after =~ /((\()|(<)|(\[))|(\Q$delimiters[$#delimiters]\E)|$/) { # \Q starts quoting, \E ends it: also perl matches attempts to match whole regrep along the length of the string
    $before = $before.$`;
    $after = $';
    if ($1) {
# we have matched one of the opening delimiters
      $before = $before.$1; # include the delimiter itself in the save
# check to see whether this is relevant here, and if so, push the opposite delimiter onto the list
      if ($delimiters[$#delimiters] !~ />|\]/) { # if we're looking for either > or ] then we ignore all other characters
        if ($2) { push(@delimiters,')'); }
        elsif ($3) { push(@delimiters,'>'); }
        else { push(@delimiters,']'); }
      }
    } elsif ($5) { # we have matched one of the delimiters
      pop(@delimiters); # remove that delimiter
      if (!(@delimiters)) { last; }# if there are no delimiters left, we have found what we are looking for so exit
      $before = $before.$5; # replace closing delimiter and continue
    } else { # found end of string without finding the requested delimiter, so exit but flag potential problem
      $not_found = 1;
      last;
    }
  }

  $before =~ s/^\s*//; $before =~ s/\s*$//; # remove leading and trailing spaces
  $after =~ s/^\s*//; $after =~ s/\s*$//; # remove leading and trailing spaces

  $_[0] = $after;
  return($before,$after,$not_found);
}

#-------------------------------------------------------------------------------

sub create_operator_contents {
# subroutine to extract multiple parts of the mequation contents of an operator
# enter with
# - $_[0] as the contents of the operator
# - $_[1] is otype, which is the type from which the expression originated
# - $_[2] is omvar, which is the mvar from which the expression originated
# within the sub we alter the array operator_contents which is defined in the calling sub

  use strict;
  use Data::Dumper;
  my ($both,$name,$value,$tmp,$store,$match,$not_found);
  my $contents = $_[0]; # save the complete contents
  my $otype = $_[1];
  my $omvar = $_[2];
  %operator_contents=();
  my $numeric_key = 0;

  my $saved_contents = $contents;
  $contents =~ s/^\s*//; $contents =~ s/\s*$//; # remove leading and trailing spaces
  while (nonempty($contents)) {
    $tmp = $contents; # use dummy variable
    print DEBUG "before split_mequation_at_character ',': tmp = $tmp\n";
    ($both,$contents,$not_found) = split_mequation_at_character($tmp,',',$otype,$omvar); # first split mequation at comma that separates different operator contents
# if $tmp doesn't contain a comma then it is all placed in $both and $contents is emptied
    print DEBUG "after split_mequation_at_character ',' and before '=': both = $both: contents = $contents: not_found = $not_found\n";
    ($name,$value,$not_found) = split_mequation_at_character($both,'=',$otype,$omvar); # and now split at equals sign that separates name and values
    print DEBUG "after split_mequation_at_character '=': name = $name: value = $value: not_found = $not_found\n";
# if $value is empty then no name was specified and so a name needs to be made up (numeric)
    if (empty($value)) { # this means that a variable name wasn't specified
      $numeric_key ++;
      if (empty($name)) {
# further, if $name (and hence actually the value at this stage) is empty then a default empty value is used
        $value = '';
      } else {
        $value = $name;
      }
      $name = $numeric_key;
    }
    $name =~ s/\s//g; # remove any spaces from name - actually, there shouldn't be any anyway
    $operator_contents{$name} = $value;
    print DEBUG "adding: name = $name: value = $value\n";
  }

  if (!(%operator_contents)) {
    print DEBUG "INFO: within create_operator_contents: no operator_contents found\n";
  } else {
    print DEBUG "INFO: within create_operator_contents: operator_contents = ".Dumper(\%operator_contents)."\n";
  }

  return;

}

#-------------------------------------------------------------------------------
# here we create a new someloop
# enter with
# - $_[0] as the mequation, to be replaced by the someloop maxima name
# - $_[1] as the loop type, (sum|max|min|product)
# - $_[2] as the centring of the region over which the loop is performed:  Note, this is not the contextcentring of the variable, so is different to the centring as defined and used with other variables
# - $_[3] as the region name over which the loop is performed
# - $_[4] is 0 or 1 dependending on whether a derivative is required
# - $_[5] is otype, which is the type from which the expression originated
# - $_[6] is omvar, which is the mvar from which the expression originated

sub create_someloop {

  use strict;

  $m{"someloop"} ++;
  $variable{"someloop"}[$m{"someloop"}]{"mequation"} = $_[0];
  $variable{"someloop"}[$m{"someloop"}]{"type"} = $_[1];
  $variable{"someloop"}[$m{"someloop"}]{"centring"} = $_[2];
  $variable{"someloop"}[$m{"someloop"}]{"region"} = $_[3];
# $variable{"someloop"}[$m{"someloop"}]{"name"} = "SYSTEM generated $_[1] someloop over $_[2] region $_[3] which is a component of $_[5] $variable{$_[5]}[$_[6]]{name}";
# $variable{"someloop"}[$m{"someloop"}]{"equation"} = "SYSTEM generated $_[1] someloop over $_[2] region $_[3] which is a component of $_[5] $variable{$_[5]}[$_[6]]{name}";
  $variable{"someloop"}[$m{"someloop"}]{"name"} = "$_[1] someloop over $_[2] region $_[3] which is part of $_[5] variable $variable{$_[5]}[$_[6]]{name}";
  $variable{"someloop"}[$m{"someloop"}]{"equation"} = "MEQUATION: $_[0]";
  $variable{"someloop"}[$m{"someloop"}]{"deriv"} = $_[4];
  $variable{"someloop"}[$m{"someloop"}]{"hasderiv"} = $_[4];
  $variable{"someloop"}[$m{"someloop"}]{"maxima"} = "someloop[$m{someloop}]";
  $variable{"someloop"}[$m{"someloop"}]{"mfortran"} = "someloop($m{someloop})";
  $variable{"someloop"}[$m{"someloop"}]{"fortran"} = "someloop(thread)%funk($m{someloop})%v";
  $variable{"someloop"}[$m{"someloop"}]{"fortranns"} = "someloop(thread)%funk($m{someloop})%v";
  $variable{"someloop"}[$m{"someloop"}]{"fortran_number"} = $m{"someloop"};
  $variable{"someloop"}[$m{"someloop"}]{"otype"} = $_[5];
  $variable{"someloop"}[$m{"someloop"}]{"omvar"} = $_[6];
  $variable{"someloop"}[$m{"someloop"}]{"minseparation"} = -1; # if this one is >= 0 then this signifies that the loop is to be conducted in increasing order of separation
  $variable{"someloop"}[$m{"someloop"}]{"maxseparation"} = -1;
  $variable{"someloop"}[$m{"someloop"}]{"separation_list_number"} = 0; # fortran number of separation_list to be used, if 0 then there is no separation loop, so can be used as indicator of separation loop
  $variable{"someloop"}[$m{"someloop"}]{"faceseparation"} = 0; # indicates, if a separation loop is being conducted, that cells are adjacent only if separated by a face

# $variable{"someloop"}[$m{"someloop"}]{"default"} = $default;

  $_[0] = $variable{"someloop"}[$m{"someloop"}]{"maxima"}; 

# set msomeloop - allocations now done in setup_vars
  $sub_string{"set_msomeloop"}="msomeloop = $m{someloop}\n"; # will only be set if >0

  print DEBUG "+++++++++++++++++++++\n".
    "in create_someloop with: number = $m{someloop}: originated from $variable{someloop}[$m{someloop}]{otype} variable ".
    "$variable{$variable{someloop}[$m{someloop}]{otype}}[$variable{someloop}[$m{someloop}]{omvar}]{name}: maxima = $variable{someloop}[$m{someloop}]{maxima}: ".
    "mequation = $variable{someloop}[$m{someloop}]{mequation}: type = $variable{someloop}[$m{someloop}]{type}: ".
    "centring = $variable{someloop}[$m{someloop}]{centring}: region = $variable{someloop}[$m{someloop}]{region}: deriv = $variable{someloop}[$m{someloop}]{deriv}\n".
    "---------------------\n";
}

#-------------------------------------------------------------------------------
# runs a comma separated list of mequations through maxima for simplification

sub run_maxima_simplify {

  use strict;
  my ($systemcall, $line, $out, $in, $otype, $omvar, $bit, $call_maxima, $n, $tmp, $not_found);
  my @replacements = ();

  if (-e $stopfile) {die "HALT: found $stopfile in run_maxima_simplify\n";}

  print DEBUG "+++++++++++++++++++++\n".
    "in run_maxima_simplify: before _[0] = ",$_[0],"\n";

  $in = $_[0];
  $otype = $_[1];
  $omvar = $_[2];
  $out = "";

  while (nonempty($in)) {

#   $bit = split_off_leading_mequation($in,$otype,$omvar);
    $tmp = $in; # use dummy variable as tmp will be zeroed after call
    ($bit,$in,$not_found) = split_mequation_at_character($tmp,',',$otype,$omvar);

# modify bit, replacing any maxima variables by generic variables, g[1], g[2] etc
    @replacements = construct_generic_mequation($bit,"m"); 

# now check whether equation was previously run through maxima
    $call_maxima = 1;

    if ($reuse_maxima_results) {
      foreach $n ( 0 .. $#maxima_simplify_results ) {
#       print DEBUG "looping through maxima_simplify_results: n = $n: input = $maxima_simplify_results[$n]{input}: bit = $bit\n";
        if ($bit eq $maxima_simplify_results[$n]{"input"}) {
          $bit = $maxima_simplify_results[$n]{"output"};
          $call_maxima = 0;
          print DEBUG "reusing maxima result $n\n";
          print DEBUG " tmp_file_number = $maxima_simplify_results[$n]{tmp_file_number}\n";
          print DEBUG " used = $maxima_simplify_results[$n]{used}\n";
          print DEBUG " input = $maxima_simplify_results[$n]{input}\n";
          print DEBUG " output = $maxima_simplify_results[$n]{output}\n";
          $maxima_simplify_results[$n]{"used"} = 1;
          last;
        }
      }
    }
        
    if ($call_maxima) {

      $tmp_file_number ++;

# initialise a new maxima_simplify_results entry
      push (@maxima_simplify_results, { "input" => $bit, "used" => 1, "tmp_file_number" => $tmp_file_number } );

      open(MAXIMA, ">$tmp_dir/tmp$tmp_file_number.maxima") or die "ERROR: problem opening temporary maxima file $tmp_dir/tmp$tmp_file_number.maxima: something funny is going on: check permissions??\n";
      print MAXIMA "foo:$bit;";
      print MAXIMA "with_stdout (\"$tmp_dir/tmp$tmp_file_number.simp\", grind(foo));";
      close(MAXIMA);
      $systemcall="$maxima_bin -b $tmp_dir/tmp$tmp_file_number.maxima >$tmp_dir/tmp$tmp_file_number.txt";
      (!(system("$systemcall"))) or die "ERROR: could not $systemcall\n";
      open(SIMP, "<$tmp_dir/tmp$tmp_file_number.simp") or do {
        print "ERROR: problem in run_maxima_simplify processing the following mequation (maxima equation):\n'$bit'\n".
          "  This equation originated from $otype variable $variable{$otype}[$omvar]{name} and this\n".
          "  error indicates that maxima had problems processing this equation.  All variables that\n".
          "  that appear in this equation should be referred to by their maxima names:  Hence if any\n".
          "  variables appear in the above as their original names (e.g. <my variable>) then\n".
          "  check their equations as there is some problem with their definition (maybe a typo?).\n".
          "  Also check that any mathematical functions and syntax used in the above are compatible with maxima.\n";
        print "  Maxima input lines are contained in the file $tmp_dir/tmp$tmp_file_number.maxima\n";
        print "  Maxima processing output contained in the file $tmp_dir/tmp$tmp_file_number.txt\n";
        print "  Maxima output lines are contained in the file $tmp_dir/tmp$tmp_file_number.simp\n";
        print DEBUG "ERROR: problem in run_maxima_simplify processing the following mequation:\n'$bit'\n".
        die;
      };
      $bit = "";
      while ($line = <SIMP>) {
        chompm($line); 
        $line =~ s/\s*\$\s*$//;
        $line =~ s/^\s*//;
        $bit=$bit.$line;
      }
      close(SIMP);

# save result for possible future use
      $maxima_simplify_results[$#maxima_simplify_results]{"output"} = $bit;
      print DEBUG "saving maxima result:\n";
      print DEBUG " tmp_file_number = $maxima_simplify_results[$#maxima_simplify_results]{tmp_file_number}\n";
      print DEBUG " input = $maxima_simplify_results[$#maxima_simplify_results]{input}\n";
      print DEBUG " output = $maxima_simplify_results[$#maxima_simplify_results]{output}\n";

    }

# now put equation back in terms of maxima variables
    deconstruct_generic_mequation($bit,"m",@replacements); 

    print DEBUG "midway through run_maxima_simplify: in = $in: out = $out: bit = $bit\n";
    if (nonempty($out)) {$out = $out.",$bit";} else {$out = "$bit";}

  }

  $_[0] = $out;

  print DEBUG "in run_maxima_simplify: after _[0] = ",$_[0],"\n".
    "---------------------\n";
}

#-------------------------------------------------------------------------------
# takes a string which contains user defined <> delimited names and replaces with maxima names
# second and third arguments are calling type and mvar, respectively

sub name_to_maxima {

  use strict;
  use Text::Balanced qw ( extract_bracketed );
  my ($type, $mvar, $otype, $omvar, $withdots, $withoutdots);
  my ($pre, $post, $operator, $midl, $midr, $midrnovar, $n1, $n2, $name, $original, $consistent_name);
  $withdots = $_[0];
  $otype = $_[1];
  $omvar = $_[2];

  print DEBUG "in name_to_maxima: before _[0] = ",$_[0],"\n";

# look for any dot and ddot operators and replace with scalar mathematics
# coding cannot handle nested ddot or dot operators
# move string from $withdots to $withoutdots while doing replacements
  $withoutdots = '';

  while ($withdots =~ /(<.*?>)|ddot\(|dot\(/) {
    $pre = $`;
    $post = $';
    $operator = $&;
# move over any variable and region names
    if ($1) { $withdots = $post; $withoutdots = $withoutdots.$pre.$operator; next; }
    $operator =~ s/\($//;
    print "INFO: found operator $operator in $withdots\n";
    print DEBUG "found operator $operator in $withdots\n";
    print DEBUG "pre = $pre: post = $post\n";

    $midl = "";
    while ( $post =~ /(<.+?>)|(\,)/ ) {
      $post = $';
      if ($1) {
        $midl = $midl.$`.$1;
      } else {
        $midl = $midl.$`;
        last;
      }
    }
    print DEBUG "left side finalised: midl = $midl: post = $post\n";
        
    $midr = "";
    $midrnovar = "";
    while ( $post =~ /(<.+?>)|(\))/ ) {
      $post = $';
      if ($1) {
        $midr = $midr.$`.$1;
        $midrnovar = $midrnovar.$`;
      } else {
        if (extract_bracketed( "($midrnovar)", '()' )) {last;} # if contents of right without the variables has matching brackets, then we're in business
        $midr = $midr.$`.")";
        $midrnovar = $midrnovar.$`.")";
      }
    }
    print DEBUG "right side finalised: midr = $midr: midrnovar = $midrnovar: post = $post\n";

    my @posl = ();
# this match allows r indices before and after a l index, matching the first colon after the l
    print DEBUG "before looking for first colon: midl = $midl\n";
    while ($midl =~ /<.+?\[[r=\d\s,]*?l[=\d\s,]*?(:)[,\d\s:]*?[r=\d\s,]*?\]>/) {
      push (@posl, $+[1]); # $+[1] is the starting position of the first match
      substr($midl, $+[1]-1, 1, " "); #replace : with blank so that it is not matched
      print DEBUG "midl = $midl: posl = @posl\n";
    }
    @posl = sort{ $a <=> $b } @posl; # make sure index positions are in order
    my @posr = ();
    print DEBUG "before looking for first colon: midr = $midr\n";
    while ($midr =~ /<.+?\[[r=\d\s,]*?l[=\d\s,]*?(:)[,\d\s:]*?[r=\d\s,]*?\]>/) {
      push (@posr, $+[1]); # $+[1] is the starting position of the first match
      substr($midr, $+[1]-1, 1, " "); #replace : with blank so that it is not matched
      print DEBUG "midr = $midl: posr = @posl\n";
    }
    @posr = sort{ $a <=> $b } @posr; # make sure index positions are in order
    print DEBUG "after: midl = $midl: posl = @posl\n";
    print DEBUG "after: midr = $midr: posr = @posr\n";

    my $mid = "(";
    if ($operator eq "dot") {
      if (@posl != 1 || @posr != 1) { error_stop("wrong number of colons in dot product in $otype $variable{$otype}[$omvar]{name}: check the variables' [l=:] syntax"); } # scalar(@posl) is number of elements of posl array
      foreach $n1 ( 1 .. 3 ) {
        substr($midl, $posl[0]-1, 1, $n1); #replace blank with correct index
        substr($midr, $posr[0]-1, 1, $n1); #replace blank with correct index
        $mid = $mid." + (".$midl.")*(".$midr.")";
      }
    } else { # double dot product
      if (@posl != 2 || @posr != 2) { error_stop("wrong number of colons in ddot product in $otype $variable{$otype}[$omvar]{name}: check the variables' [l=:,:]/[l=:] syntax"); } # scalar(@posl) is number of elements of posl array
      foreach $n1 ( 1 .. 3 ) {
        substr($midl, $posl[0]-1, 1, $n1); #replace blank with correct index
        substr($midr, $posr[1]-1, 1, $n1); #replace blank with correct index
        foreach $n2 ( 1 .. 3 ) {
          substr($midl, $posl[1]-1, 1, $n2); #replace blank with correct index
          substr($midr, $posr[0]-1, 1, $n2); #replace blank with correct index
          $mid = $mid." + (".$midl.")*(".$midr.")";
        }
      }
    }
    $mid = $mid." )";
        
    print DEBUG "mid = $mid\n";
      
    $withdots = $post;
    $withoutdots = $withoutdots.$pre.$mid;

    print DEBUG "after $operator operator expansion: withoutdots.withdots = ".$withoutdots.$withdots."\n";

  }

# reconstruct string including final unmatched bits
  $_[0] = $withoutdots.$withdots;
      
# loop through and replace variables with standardised names
  $original = $_[0];
  $_[0] = "";
  while ($original =~ /(<(.*?)>)/) {
    $pre = $`;
    $post = $';
    $name = $1;
    if (empty($2)) { error_stop("empty name found in equation for $otype $variable{$otype}[$omvar]{name}:\n$variable{$otype}[$omvar]{equation}"); }
    $consistent_name = examine_name($name,"name");
# now replace this with the maxima name if it is identified as a variable (it could be a region)
    MATCH_LOOP: {
      foreach $type (@user_types,"empty","system") {
        foreach $mvar ( 1 .. $m{$type} ) {
          if ($consistent_name eq $variable{$type}[$mvar]{"name"}) {
            print DEBUG "found a match between a name and maxima name: name = $name: consistent_name = $consistent_name: maxima = $variable{$type}[$mvar]{maxima}\n";
            $name = $variable{$type}[$mvar]{"maxima"};
            last MATCH_LOOP;
          }
        }
      }
    }
    $_[0] = $_[0].$pre.$name;
    $original = $post;
  }
  if (nonempty($original)) { $_[0] = $_[0].$original; }

  print DEBUG "in name_to_maxima: after _[0] = ",$_[0],"\n";

}

#-------------------------------------------------------------------------------
# calling sequence is: string, substring, replacement

sub replace_substrings {
  use strict;
  $_[0] =~ s/\Q$_[1]/$_[2]/g; # \Q escapes any funny characters in $_[1]
}

#-------------------------------------------------------------------------------
# assemble compound arrays for each of the variables

sub create_compounds {

  use strict;
# use Data::Dumper;
  my ($type, $mvar, $name, $indices, $type2, $mvar2, $index, $mcheck, $rank, $component_list,
    $nrank, $mmvar, $l1, $l2, $nvar, $rindex, $tmp, $option, $n);

# loop through all vars
  foreach $type (@user_types) {
    foreach $mvar ( 1 .. $m{$type} ) {

# use examine_name function to get info about the component
      ($name,$rank,$nrank,$index,$rindex) = examine_name($variable{$type}[$mvar]{"name"},"all");
      print DEBUG "Examining compound for: name = $variable{$type}[$mvar]{name}: compoundname = $name: index = $index: rank = $rank: nrank = $nrank: rindex = $rindex\n";

# see if this compound has been defined before
      $mvar2 = 0;
      foreach $mcheck ( 1 .. $m{"compound"} ) {
        if ($variable{"compound"}[$mcheck]{"name"} eq $name) {
          print "INFO: found existing $rank compound with name = $name for component $variable{$type}[$mvar]{name}\n";
          print DEBUG "INFO: found existing $rank compound with name = $name for component $variable{$type}[$mvar]{name}\n";
          if (!($rank eq $variable{"compound"}[$mcheck]{"rank"})) {
            error_stop("rank mismatch found between component ($rank) and compound ($variable{compound}[$mcheck]{rank})");
          } elsif (!($variable{$type}[$mvar]{"rindex"} eq $variable{"compound"}[$mcheck]{"rindex"})) {
            error_stop("rindex mismatch found between component ($variable{$type}[$mvar]{rindex}) and compound ($variable{compound}[$mcheck]{rindex})");
          } elsif (!($variable{$type}[$mvar]{"units"} eq $variable{"compound"}[$mcheck]{"units"})) {
            error_stop("units mismatch found between component ($variable{$type}[$mvar]{units}) and compound ($variable{compound}[$mcheck]{units})");
          } elsif (!($variable{$type}[$mvar]{"centring"} eq $variable{"compound"}[$mcheck]{"centring"})) {
            error_stop("centring mismatch found between component ($variable{$type}[$mvar]{centring}) and compound ($variable{compound}[$mcheck]{centring})");
          } elsif (!($variable{$type}[$mvar]{"region"} eq $variable{"compound"}[$mcheck]{"region"})) {
            error_stop("region mismatch found between component ($variable{$type}[$mvar]{region}) and compound ($variable{compound}[$mcheck]{region})");
          } elsif (!($variable{$type}[$mvar]{"multiplier"} eq $variable{"compound"}[$mcheck]{"multiplier"})) {
            error_stop("multiplier mismatch found between component ($variable{$type}[$mvar]{multiplier}) and compound ($variable{compound}[$mcheck]{multiplier})");
          } elsif (!($type eq $variable{"compound"}[$mcheck]{"type"})) {
            error_stop("type mismatch found between component ($type) and compound ($variable{compound}[$mcheck]{type})");
          }
          $mvar2 = $mcheck;
          last;
        }
      }

# create new compound variable if it doesn't exist
      if (!($mvar2)) {
        $m{"compound"}++;
        $mvar2=$m{"compound"};
        $variable{"compound"}[$mvar2]{"name"} = $name;
        $variable{"compound"}[$mvar2]{"rank"} = $rank;
        $variable{"compound"}[$mvar2]{"nrank"} = $nrank;
        $variable{"compound"}[$mvar2]{"rindex"} = $rindex;
        $variable{"compound"}[$mvar2]{"units"} = $variable{$type}[$mvar]{"units"};
        $variable{"compound"}[$mvar2]{"multiplier"} = $variable{$type}[$mvar]{"multiplier"};
        $variable{"compound"}[$mvar2]{"centring"} = $variable{$type}[$mvar]{"centring"};
        $variable{"compound"}[$mvar2]{"type"} = $type;
        $variable{"compound"}[$mvar2]{"fortran_number"} = $mvar2;
        $variable{"compound"}[$mvar2]{"region"} = $variable{$type}[$mvar]{"region"};
        print "INFO: creating new $rank compound with name = $name and centring = $variable{$type}[$mvar]{centring} for component $variable{$type}[$mvar]{name}\n";
        print DEBUG "INFO: creating new $rank compound with name = $name and centring = $variable{$type}[$mvar]{centring} for component $variable{$type}[$mvar]{name}\n";
        foreach $mmvar ( 1 .. $nrank ) {
          $variable{"compound"}[$mvar2]{"component"}[$mmvar] = 0;
          $variable{"compound"}[$mvar2]{"fortran_component"}[$mmvar] = 0;
        }
      }

# finally allocate component after checking that it is not already allocated
      if ($variable{"compound"}[$mvar2]{"component"}[$index]) { error_stop("component $index of $rank $name has already been allocated"); }
      $variable{"compound"}[$mvar2]{"component"}[$index] = $mvar;
      $variable{"compound"}[$mvar2]{"fortran_component"}[$index] = $variable{$type}[$mvar]{"fortran_number"};

    } # mvar loop: all compound variables for this type have been constructed
  } # type loop: all compound variables have been constructed

#-----------------
# deal with compound related options, writing them to the fortran file
# ref: options, ref: compoundoptions

  open(FORTRAN_INPUT, ">>$fortran_input_file");

# loop through each compound type constructing an options array
  foreach $mvar2 ( 1 .. $m{"compound"} ) {
    $type = $variable{"compound"}[$mvar2]{"type"};
    $name = $variable{"compound"}[$mvar2]{"name"};
# first construct a compound options array by concatenating together the relevant component options array in the order that they were written
    $variable{"compound"}[$mvar2]{"options"} = '';
    foreach my $masread ( 0 .. $#asread_variable ) {
      if (empty($asread_variable[$masread]{"options"})) { next; }
      if ($type ne $asread_variable[$masread]{"type"}) { next; }
# use smartmatch
#     if (!($asread_variable[$masread]{"mvar"} ~~ @{$variable{"compound"}[$mvar2]{"component"}})) { next; } # smartmatch operator looks for left element in the array on the right
# smartmatch is not available pre perl 5.10, so use the following workaround (keep these debugging statements)
#     print "dumper = ".Dumper(\$variable{"compound"}[$mvar2]{"component"})."\n";
#     print '$ = ',$variable{"compound"}[$mvar2]{"component"},"\n";
#     my @temp = @{$variable{"compound"}[$mvar2]{"component"}}[ 1 .. $#{$variable{"compound"}[$mvar2]{"component"}}];
#     print "@ = @temp\n";
# some explanation re this ouchy perl:
# $variable{"compound"}[$mvar2]{"component"} is a reference to an array
# @{$variable{"compound"}[$mvar2]{"component"}} is the array
# $#{$variable{"compound"}[$mvar2]{"component"}} is the index of the last element in the array
# the first element of this array is undef, so we only pass the 2nd to last elements of the array to present_in_array
      if (!(present_in_array($asread_variable[$masread]{"mvar"},@{$variable{"compound"}[$mvar2]{"component"}}[ 1 .. $#{$variable{"compound"}[$mvar2]{"component"}} ]))) { next; } # smartmatch operator looks for left element in the array on the right
      print DEBUG "INFO: the $type compound $name is receiving the following options from the component $asread_variable[$masread]{name}: options = $asread_variable[$masread]{options}\n";
      $variable{"compound"}[$mvar2]{"options"} = $variable{"compound"}[$mvar2]{"options"}.','.$asread_variable[$masread]{"options"};
    }
    $variable{"compound"}[$mvar2]{"options"} =~ s/^\s*\,//; # remove leading comma if present
    print DEBUG "INFO: before extracting just the compound options, the $type compound $name has the combined options = $variable{compound}[$mvar2]{options}\n";
# now process this option list, extracting only the options relevant to compounds
    if (nonempty($variable{"compound"}[$mvar2]{options})) {
      $tmp = $variable{"compound"}[$mvar2]{"options"};
      $variable{"compound"}[$mvar2]{"options"} = '';
      while ($tmp =~ /(^|\,)\s*([^\,]+?)\s*(\,|$)/i) {
        $option = $2; $tmp = $`.','.$';
        if ($option =~ /^(|no)(|compound)(output|(stepoutput(|noupdate)))$/i) {
          $variable{"compound"}[$mvar2]{"options"} = $variable{"compound"}[$mvar2]{"options"}.",\L$1$3"; }
        elsif ($option =~ /^(|no)(|compound)(element(|node|nodelimited)data)$/i) {
          if ($variable{"compound"}[$mvar2]{"centring"} eq "cell") {
            $variable{"compound"}[$mvar2]{"options"} = $variable{"compound"}[$mvar2]{"options"}.",\L$1$3";
          } else { print "WARNING: option $option specified for $variable{compound}[$mvar2]{centring} compound $name is only relevant for cell centred variables and is ignored\n"; } }
        elsif ($option =~ /^(|no)(|compound)(input)$/i) {
          if ($type eq "unknown" || $type eq "constant" || $type eq "transient" || $type eq "output" || $type eq "derived" || $type eq "equation" ) { # allowing derived and equation now for v0.50
            $variable{"compound"}[$mvar2]{"options"} = $variable{"compound"}[$mvar2]{"options"}.",\L$1$3";
            if ($type eq "equation") { print "WARNING: are you sure that you want option $option specified for compound $name?\n"; }
          } else { print "WARNING: option $option specified for compound $name is not relevant for this type of variable and is ignored\n"; }
        }
# do not have to check other component options here as they were checked previously
      }
      $variable{"compound"}[$mvar2]{"options"} =~ s/^\s*\,//; # remove leading comma if present
      if (nonempty($variable{"compound"}[$mvar2]{options})) {
        print FORTRAN_INPUT "COMPOUND_OPTIONS $name $variable{compound}[$mvar2]{options}\n";
        print "INFO: the compound $name has the final compound-specific options of: $variable{compound}[$mvar2]{options}\n";
        print DEBUG "INFO: the compound $name has the final compound-specific options of: $variable{compound}[$mvar2]{options}\n";
      } else {
        print DEBUG "INFO: the compound $name has no compound-specific options\n";
      }
    }
  }

  close(FORTRAN_INPUT);

#-----------------
# run through each compound variable now disseminating the info to the components and
#  creating new empty scalar components that aren't otherwise defined

  foreach $mvar2 ( 1 .. $m{"compound"} ) {
    $type = $variable{"compound"}[$mvar2]{"type"};
    $component_list = "[";
    foreach $nvar ( 1 .. $variable{"compound"}[$mvar2]{"nrank"} ) {
      $component_list = $component_list." $variable{compound}[$mvar2]{fortran_component}[$nvar] ,";
    }
    $component_list =~ s/,$/]/;
    print "INFO: compound $mvar2 $variable{compound}[$mvar2]{name} has components: $component_list\n";
    $variable{"compound"}[$mvar2]{"component_list"} = $component_list; # a string suitable for fortran

    $l1 = 1;
    $l2 = 1;
    foreach $nvar ( 1 .. $variable{"compound"}[$mvar2]{"nrank"} ) {
      $mvar = $variable{"compound"}[$mvar2]{"component"}[$nvar];
      print DEBUG "Examining compound components: l1 = $l1: l2 = $l2: mvar = $mvar\n";
      if (!($mvar)) {
# create new empty/zero component
        $m{"empty"}++ ;
        $name = "<".examine_name($variable{"compound"}[$mvar2]{"name"},"basename"); # use examine_name sub to get basename
        if ($variable{"compound"}[$mvar2]{"nrank"} eq 1) {
          $name = $name."[r=".$variable{"compound"}[$mvar2]{"rindex"}."]>";
        } elsif ($variable{"compound"}[$mvar2]{"nrank"} eq 3) {
          $name = $name."[l=".$l1.",r=".$variable{"compound"}[$mvar2]{"rindex"}."]>";
        } elsif ($variable{"compound"}[$mvar2]{"nrank"} eq 9) {
          $name = $name."[l=".$l2.",".$l1.",r=".$variable{"compound"}[$mvar2]{"rindex"}."]>";
        }
        $variable{"empty"}[$m{"empty"}]{"name"} = examine_name($name,"name"); # standardise name (basically just remove any r=0 references)
        print DEBUG "INFO: creating new empty $type scalar component: $variable{empty}[$m{empty}]{name}\n";
        print "INFO: creating new empty $type scalar component: $variable{empty}[$m{empty}]{name}\n";
#       $variable{"empty"}[$m{"empty"}]{"maxima"} = "0.";
#       $variable{"empty"}[$m{"empty"}]{"fortran"} = "0.d0"; # these should not be needed
#       $variable{"empty"}[$m{"empty"}]{"fortranns"} = "0.d0";
        $variable{"empty"}[$m{"empty"}]{"maxima"} = 0;
        $variable{"empty"}[$m{"empty"}]{"someloop"} = 0;
        $variable{"empty"}[$m{"empty"}]{"rank"} = $variable{"compound"}[$mvar2]{"rank"};
        $variable{"empty"}[$m{"empty"}]{"nrank"} = $variable{"compound"}[$mvar2]{"nrank"};
        $variable{"empty"}[$m{"empty"}]{"rindex"} = $variable{"compound"}[$mvar2]{"rindex"};
        $variable{"empty"}[$m{"empty"}]{"units"} = $variable{"compound"}[$mvar2]{"units"};
        $variable{"empty"}[$m{"empty"}]{"multiplier"} = $variable{"compound"}[$mvar2]{"multiplier"};
        $variable{"empty"}[$m{"empty"}]{"centring"} = $variable{"compound"}[$mvar2]{"centring"};
        $variable{"empty"}[$m{"empty"}]{"compound_name"} = $variable{"compound"}[$mvar2]{"name"};
        $variable{"empty"}[$m{"empty"}]{"compound_number"} = $variable{"compound"}[$mvar2]{"fortran_number"};
        $variable{"empty"}[$m{"empty"}]{"component_list"} = $component_list;
        $variable{"empty"}[$m{"empty"}]{"fortran_number"} = $m{"empty"};
        $variable{"empty"}[$m{"empty"}]{"type"} = $type;
      } else { # copy compound data back to existing component
        $variable{$type}[$mvar]{"rank"} = $variable{"compound"}[$mvar2]{"rank"};
        $variable{$type}[$mvar]{"nrank"} = $variable{"compound"}[$mvar2]{"nrank"};
        $variable{$type}[$mvar]{"compound_name"} = $variable{"compound"}[$mvar2]{"name"};
        $variable{$type}[$mvar]{"compound_number"} = $variable{"compound"}[$mvar2]{"fortran_number"};
        $variable{$type}[$mvar]{"component_list"} = $component_list;
      }
      $l1++;
      if ($l1 > 3) { $l1 = $l1 - 3; $l2++; }
    }
  }
      
}

#-------------------------------------------------------------------------------
# does a string match on the elements in the array seeing if the first is present in the rest
# can just use a smartmatch from perl 5.10 onwards, but this is for compatibility with older perl versions

sub present_in_array {

  my @array = @_;
  my $nmax = $#array;
  my $flag = 0;
  my $n;

# print "entering with nmax = $nmax: array = @array\n";
  foreach $n ( 1 .. $nmax ) {
#   print "array[0] = $array[0]: array[$n] = $array[$n]\n";
    if ($array[0] eq $array[$n]) { #print "match\n";
      $flag = 1; last;
    }
  }

  return $flag;
}

#-------------------------------------------------------------------------------
# assemble metadata allocation statements

sub create_allocations {

  use strict;
  my ($type, $mvar, $comparator, $index1, $index2, $mlink, $o, $mfortran, $fortran, $n);

# allocate var and compound
  $sub_string{"allocate_meta_arrays"} = 
    "\n! allocate var and compound arrays\n".
    "allocate(var($m{user}))\n".
    "allocate(compound($m{compound}))\n";

  foreach $type (@user_types,"compound") {
    print "INFO: creating meta statements for type = ",$type,"\n";

    if (!($m{$type})) { next; }

    foreach $mvar ( 1 .. $m{$type} ) {

# write out initialisation statements for meta data
      if ( $type ne "compound" ) {
        $mfortran = ''; $fortran = '';
        if (nonempty($variable{$type}[$mvar]{mfortran})) { $mfortran = $variable{$type}[$mvar]{mfortran}; }
        if (nonempty($variable{$type}[$mvar]{fortran})) { $fortran = $variable{$type}[$mvar]{fortran}; }
        $sub_string{"allocate_meta_arrays"}=$sub_string{"allocate_meta_arrays"}.
          "\n!$type: $variable{$type}[$mvar]{name} -> $variable{$type}[$mvar]{maxima} ".
          "-> $mfortran -> $fortran\n".
          "$basename{$type}($variable{$type}[$mvar]{fortran_number})%name=\'$variable{$type}[$mvar]{name}\'\n".
          "$basename{$type}($variable{$type}[$mvar]{fortran_number})%multiplier=$variable{$type}[$mvar]{multiplier}\n".
          "$basename{$type}($variable{$type}[$mvar]{fortran_number})%units=\'$variable{$type}[$mvar]{units}\'\n".
          "$basename{$type}($variable{$type}[$mvar]{fortran_number})%centring=\'$variable{$type}[$mvar]{centring}\'\n".
          "$basename{$type}($variable{$type}[$mvar]{fortran_number})%type=\'$type\'\n".
          "$basename{$type}($variable{$type}[$mvar]{fortran_number})%rank=\'$variable{$type}[$mvar]{rank}\'\n".
          "$basename{$type}($variable{$type}[$mvar]{fortran_number})%relstep=$variable{$type}[$mvar]{rindex}\n".
          "$basename{$type}($variable{$type}[$mvar]{fortran_number})%region=\'$variable{$type}[$mvar]{region}\'\n".
          "$basename{$type}($variable{$type}[$mvar]{fortran_number})%update_region=\'$variable{$type}[$mvar]{update_region}\'\n".
          "$basename{$type}($variable{$type}[$mvar]{fortran_number})%region_number=$variable{$type}[$mvar]{region_number}\n".
          "$basename{$type}($variable{$type}[$mvar]{fortran_number})%update_region_number=$variable{$type}[$mvar]{update_region_number}\n".
          "$basename{$type}($variable{$type}[$mvar]{fortran_number})%compound_name=\'$variable{$type}[$mvar]{compound_name}\'\n".
          "$basename{$type}($variable{$type}[$mvar]{fortran_number})%compound_number=$variable{$type}[$mvar]{compound_number}\n".
          "$basename{$type}($variable{$type}[$mvar]{fortran_number})%someloop=$variable{$type}[$mvar]{someloop}\n".
          "allocate($basename{$type}($variable{$type}[$mvar]{fortran_number})%component($variable{$type}[$mvar]{nrank}))\n".
          "$basename{$type}($variable{$type}[$mvar]{fortran_number})%component=$variable{$type}[$mvar]{component_list}\n";
        if ($type eq "unknown" || $type eq "equation") {
          $sub_string{"allocate_meta_arrays"}=$sub_string{"allocate_meta_arrays"}.
            "$basename{$type}($variable{$type}[$mvar]{fortran_number})%magnitude_constant=$variable{$type}[$mvar]{magnitude_constant}\n";
        }
      } else {
        $sub_string{"allocate_meta_arrays"}=$sub_string{"allocate_meta_arrays"}.
          "\n!$type: $variable{$type}[$mvar]{name} of $variable{$type}[$mvar]{rank} rank\n".
          "$basename{$type}($variable{$type}[$mvar]{fortran_number})%name=\'$variable{$type}[$mvar]{name}\'\n".
          "$basename{$type}($variable{$type}[$mvar]{fortran_number})%units=\'$variable{$type}[$mvar]{units}\'\n".
          "$basename{$type}($variable{$type}[$mvar]{fortran_number})%centring=\'$variable{$type}[$mvar]{centring}\'\n".
          "$basename{$type}($variable{$type}[$mvar]{fortran_number})%type=\'$variable{$type}[$mvar]{type}\'\n".
          "$basename{$type}($variable{$type}[$mvar]{fortran_number})%rank=\'$variable{$type}[$mvar]{rank}\'\n".
          "$basename{$type}($variable{$type}[$mvar]{fortran_number})%relstep=$variable{$type}[$mvar]{rindex}\n".
          "$basename{$type}($variable{$type}[$mvar]{fortran_number})%region=\'$variable{$type}[$mvar]{region}\'\n".
          "$basename{$type}($variable{$type}[$mvar]{fortran_number})%region_number=$variable{$type}[$mvar]{region_number}\n".
          "$basename{$type}($variable{$type}[$mvar]{fortran_number})%update_region=\'$variable{$type}[$mvar]{update_region}\'\n".
          "$basename{$type}($variable{$type}[$mvar]{fortran_number})%update_region_number=$variable{$type}[$mvar]{update_region_number}\n".
          "allocate($basename{$type}($variable{$type}[$mvar]{fortran_number})%component($variable{$type}[$mvar]{nrank}))\n".
          "$basename{$type}($variable{$type}[$mvar]{fortran_number})%component=$variable{$type}[$mvar]{component_list}\n";
      }
    }
  }

# allocate any region links
  if (@region_link) {
    $sub_string{"allocate_meta_arrays"}=$sub_string{"allocate_meta_arrays"}.
      "\n! now allocating and defining region links\n".
      "allocate(region_link($region_link[$#region_link]{number}))\n";
    foreach $mlink ( 0 .. $#region_link ) {
      $sub_string{"allocate_meta_arrays"}=$sub_string{"allocate_meta_arrays"}."\n".
        "region_link($region_link[$mlink]{number})%to_region = '$region_link[$mlink]{to_region}'\n".
        "region_link($region_link[$mlink]{number})%from_region = '$region_link[$mlink]{from_region}'\n".
        "region_link($region_link[$mlink]{number})%to_region_number = $region_link[$mlink]{to_region_number}\n".
        "region_link($region_link[$mlink]{number})%from_region_number = $region_link[$mlink]{from_region_number}\n".
        "region_link($region_link[$mlink]{number})%to_centring = '$region_link[$mlink]{to_centring}'\n".
        "region_link($region_link[$mlink]{number})%from_centring = '$region_link[$mlink]{from_centring}'\n";
    }
  }

# run through variables creating any constraint statements
  foreach $type ("unknown","derived","equation","local") {
    $sub_string{$type."_constraints"}="";
    foreach $mvar ( 1 .. $m{$type} ) {
      $comparator = 0;
      if ($variable{$type}[$mvar]{"check"} eq "positive") {
        $comparator = '<';
      } elsif ($variable{$type}[$mvar]{"check"} eq "negative") {
        $comparator = '>';
      }
      if ($comparator) {
        print "INFO: creating check statements for $type $variable{$type}[$mvar]{name} with check option $variable{$type}[$mvar]{check}\n";
        $sub_string{$type."_constraints"}=$sub_string{$type."_constraints"}.
          "\n!$type: $variable{$type}[$mvar]{name} has check option $variable{$type}[$mvar]{check}\n".
          "ierror = 2\n"; # ierror = 2 is the flag for a violated variable constraint
#       "do ns = 1, ubound(var($variable{$type}[$mvar]{fortran_number})%funk,1)\n".
#       "if ($variable{$type}[$mvar]{fortranns} $comparator 0.d0) return\n".
# now handles local variables using var_value function
#       "do ns = 1, ubound(region(var($variable{$type}[$mvar]{fortran_number})%region_number)%ijk,1)\n".
# fix for none-centred variables too
        if ($variable{$type}[$mvar]{"centring"} eq 'none') {
          $sub_string{$type."_constraints"}=$sub_string{$type."_constraints"}.
            "do ns = 1, 1\n";
        } else {
          $sub_string{$type."_constraints"}=$sub_string{$type."_constraints"}.
#           "do ns = 1, ubound(region(var($variable{$type}[$mvar]{fortran_number})%region_number)%ijk,1)\n";
            "do ns = 1, allocatable_integer_size(region(var($variable{$type}[$mvar]{fortran_number})%region_number)%ijk)\n";
        }
        $sub_string{$type."_constraints"}=$sub_string{$type."_constraints"}.
#         "if (var_value($variable{$type}[$mvar]{fortran_number},ns) $comparator 0.d0) return\n".
          "if (var_value($variable{$type}[$mvar]{fortran_number},ns) $comparator 0.d0) then\n".
          "formatline = '(a,'//trim(indexformat)//')'\n".
          "write(*,fmt=formatline) 'BACKSTEPPING: $variable{$type}[$mvar]{check} constraint on $variable{$type}[$mvar]{centring} $type $variable{$type}[$mvar]{name} ".
          "has been violated at '//ijkstring('$variable{$type}[$mvar]{centring}')//' = ',ijkvar($variable{$type}[$mvar]{fortran_number},ns)\n".
          "if (convergence_details_file) then\n".
          "write(fconverge,fmt=formatline) 'BACKSTEPPING: $variable{$type}[$mvar]{check} constraint on $variable{$type}[$mvar]{centring} $type $variable{$type}[$mvar]{name} ".
          "has been violated at '//ijkstring('$variable{$type}[$mvar]{centring}')//' = ',ijkvar($variable{$type}[$mvar]{fortran_number},ns)\n".
          "end if\nreturn\nend if\n".
          "end do\n".
          "ierror = 0\n";
      }
    }
  }

# now setup var_lists, which now include dynamic regions too

# list of strings taken from fortran, there are 9, with the 10th becoming "all": ie, max(ntype) = 10
# these are actually the same as the user_types
#character(len=100), dimension(9), parameter :: var_types = [ "constant   ", "transient  ", "newtient   ", "unknown    ", &
#  "derived    ", "equation   ", "output     ", "condition  ", "local      " ]
# and these are the centring numbers
# if (trim(centring) == "cell") then
#   ncentring = 1
# else if (trim(centring) == "face") then
#   ncentring = 2
# else if (trim(centring) == "node") then
#   ncentring = 3
# else if (trim(centring) == "none") then
#   ncentring = 4
# else if (trim(centring) == "all") then
#   ncentring = 5
#var_list_number = ntype + (ncentring-1)*(ubound(var_types,1)+1)
# with include_regions var_list_number = var_list_number*2
#max(var_list_number) = ntype + (ncentring-1)*(ubound(var_types,1)+1)
#var_list_number = (ntypemax + (ncentring-1)*(ntypemax))*2 = ncentringmax*ntypemax*2 = 10*5*2 = 100

  $sub_string{"allocate_var_lists"}="! allocating var_lists based on variable and region numbers calculated in setup_equations.pl\n".
    "allocate(var_list(100))\n";

  my @centrings = ( "cell", "face", "node", "none", "all" );
  my @types = ( @user_types, "all" );
  foreach my $ncentring ( 0 .. $#centrings ) {
    my $centring = $centrings[$ncentring];
    foreach my $ntype ( 0 .. $#types ) {
      my $type = $types[$ntype];
      foreach my $include_regions ( 0 .. 1 ) {
# calculate var_list_number here, but check that it checks with what the fortran is calculating
        my $var_list_number = ($ntype+1) + ($ncentring)*($#types+1) + $include_regions*50;
        $sub_string{"allocate_var_lists"}=$sub_string{"allocate_var_lists"}.
          "! setting var_list: var_list_number = $var_list_number: centring = $centring: type = $type: include_regions = $include_regions\n".
          "if (var_list_number(centring=\"$centring\",type=\"$type\",include_regions=".fortran_logical_string($include_regions).") /= $var_list_number) call error_stop('internal error: var_list_number is being incorrectly calculated')\n".
          "var_list($var_list_number)%centring = \"$centring\"\n".
          "var_list($var_list_number)%type = \"$type\"\n".
          "var_list($var_list_number)%include_regions = ".fortran_logical_string($include_regions)."\n";
# assemble arrays in perl, then write to fortran
        my @var_list = (); # this will hold the element numbers
        my @region_list = (); # this will say whether the element is a region (1) or variable (0)
# loop through all masread locations, starting from before the first variable
        for my $masread ( -1 .. $#asread_variable ) {
# look for variables in the fortran order
          if ($masread >= 0 && ( $type eq "all" || $type eq $asread_variable[$masread]{"type"} ) && 
                              ( $centring eq "all" || $centring eq $asread_variable[$masread]{"centring"} ) ) {
            push(@var_list,$variable{$asread_variable[$masread]{"type"}}[$asread_variable[$masread]{"mvar"}]{"fortran_number"});
            push(@region_list,".false."); # and this signifies that this is a variable, not a region
          }
# and if relevant, look for regions in the fortran order
          if ($include_regions) {
            for my $nregion ( 0 .. $#region ) {
              if (!($region[$nregion]{"dynamic"})) { next; }
              if ( ( $type eq "all" || $type eq $region[$nregion]{"type"} ) &&
                  ( $centring eq "all" || $centring eq $region[$nregion]{"centring"} ) && $region[$nregion]{"last_variable_masread"} == $masread ) {
                push(@var_list,$region[$nregion]{"fortran"});
                push(@region_list,".true."); # and this signifies that this is a region
              }
            }
          }
        }
# allocate the arrays (allocating zero size is OK now in fortran) and set them
        $sub_string{"allocate_var_lists"}=$sub_string{"allocate_var_lists"}.
          "allocate(var_list($var_list_number)%list(".scalar($#var_list+1)."))\n".
          "allocate(var_list($var_list_number)%region(".scalar($#var_list+1)."))\n";
        if ($#var_list >= 0) {
          $sub_string{"allocate_var_lists"}=$sub_string{"allocate_var_lists"}.
            "var_list($var_list_number)%list = [".join(",",@var_list)."]\n".
            "var_list($var_list_number)%region = [".join(",",@region_list)."]\n";
        }
      }
    }
  }
      
# also create fortran update statements
  foreach my $type ( @user_types, "initial_newtient", "initial_transient" ) {
    if ($type =~ /(initial_|)(constant|transient|newtient|derived|equation|output|unknown)/) {
      $sub_string{$type."_region"}='';
      my $regioninitial;
      if ($1) { $regioninitial=1; } else { $regioninitial=0; }
      my $regiontype = $2;
      my $first = 1;
      for my $nregion ( 0 .. $#region ) {
        if ($region[$nregion]{"dynamic"} && $region[$nregion]{"type"} eq $regiontype) {
          if (!($first)) { 
            $sub_string{$type."_region"}=$sub_string{$type."_region"}."else if ";
          } else {
            $sub_string{$type."_region"}=$sub_string{$type."_region"}."if ";
            $first = 0;
          }
          $sub_string{$type."_region"}=$sub_string{$type."_region"}.
            "(m == $region[$nregion]{fortran}) then\n";

# if this uses newtstepmin/max then have to surround update in conditionals
          if (nonempty($region[$nregion]{newtstepmax}) || nonempty($region[$nregion]{newtstepmin})) {
            $sub_string{$type."_region"}=$sub_string{$type."_region"}.
              "if (.not.(".newtstepcondition($region[$nregion]{newtstepmin},$region[$nregion]{newtstepmax}).")) then\n";
          }
# do the update
          $sub_string{$type."_region"}=$sub_string{$type."_region"}.
            "call update_region(m=$region[$nregion]{fortran},initial=".fortran_logical_string($regioninitial).")\n";
          if (nonempty($region[$nregion]{newtstepmax}) || nonempty($region[$nregion]{newtstepmin})) {
            $sub_string{$type."_region"}=$sub_string{$type."_region"}."end if\n";
          }

        }
      }
      if (!($first)) { $sub_string{$type."_region"}=$sub_string{$type."_region"}."end if\n"; }
    }
  }

}

#-------------------------------------------------------------------------------
# this runs a maxima formated equation through the f90 package of maxima

sub run_maxima_fortran {

  use strict;
  my ($line, $systemcall, $bit, $otype, $omvar, $call_maxima, $n);
  my @replacements = ();

  if (-e $stopfile) {die "HALT: found $stopfile in run_maxima_fortran";}

  print DEBUG "+++++++++++++++++++++\n".
    "in run_maxima_fortran: before _[0] = ",$_[0],"\n";
  $bit = $_[0];
  $otype = $_[1];
  $omvar = $_[2];

# modify bit, replacing any maxima variables by generic variables, g[0], g[1] etc
  @replacements = construct_generic_mequation($bit,"f"); 

# now check whether equation was previously run through maxima
  $call_maxima = 1;
  if ($reuse_maxima_results) {
    foreach $n ( 0 .. $#maxima_fortran_results ) {
  #   print DEBUG "looping through maxima_fortran_results: n = $n: input = $maxima_fortran_results[$n]{input}: bit = $bit\n";
      if ($bit eq $maxima_fortran_results[$n]{"input"}) {
        $bit = $maxima_fortran_results[$n]{"output"};
        $call_maxima = 0;
        print DEBUG "reusing maxima result $n\n";
        print DEBUG " tmp_file_number = $maxima_fortran_results[$n]{tmp_file_number}\n";
        print DEBUG " used = $maxima_fortran_results[$n]{used}\n";
        print DEBUG " input = $maxima_fortran_results[$n]{input}\n";
        print DEBUG " output = $maxima_fortran_results[$n]{output}\n";
        $maxima_fortran_results[$n]{"used"} = 1;
        last;
      }
    }
  }
      
  if ($call_maxima) {

    $tmp_file_number ++;

# initialise a new maxima_fortran_results entry
    push (@maxima_fortran_results, { "input" => $bit, "used" => 1, "tmp_file_number" => $tmp_file_number } );

    open(MAXIMA, ">$tmp_dir/tmp$tmp_file_number.maxima") or die "ERROR: problem opening temporary maxima file $tmp_dir/tmp$tmp_file_number.maxima: something funny is going on: check permissions??\n";
    print MAXIMA "load(f90);";
    print MAXIMA "gradef(heaviside(x),0);"; # define derivative of heaviside function to be zero
    print MAXIMA "gradef(signum(x),0);"; # define derivative of maxima sign function to be zero (sign of x)
    print MAXIMA "gradef(floor(x),0);"; # define derivative of maxima floor function (round down to the nearest integer)
    print MAXIMA "gradef(mod(x,y),1,x*log(y));"; # define derivative of maxima mod function - second element is wrt x, third element is wrt y (remainder when x is divided by y)
    print MAXIMA "gradef(log10(x),1/(x*float(log(10))));"; # define derivative of log10
    print MAXIMA "foo:$bit;";
    print MAXIMA "with_stdout (\"$tmp_dir/tmp$tmp_file_number.fortran\", f90(foo));";
    close(MAXIMA);
    $systemcall="$maxima_bin -b $tmp_dir/tmp$tmp_file_number.maxima >$tmp_dir/tmp$tmp_file_number.txt";
    (!(system("$systemcall"))) or die "ERROR: could not $systemcall\n";
    open(FORTRAN, "<$tmp_dir/tmp$tmp_file_number.fortran") or do {
      print "ERROR: problem in run_maxima_fortran processing the following mequation (maxima equation):\n'$bit'\n".
        "  This equation originated from $otype variable $variable{$otype}[$omvar]{name} and this\n".
        "  error indicates that maxima had problems processing this equation.  All variables that\n".
        "  that appear in this equation should be referred to by their maxima names:  Hence if any\n".
        "  variables appear in the above as their original names (e.g. <my variable>) then\n".
        "  check their equations as there is some problem with their definition (maybe a typo?).\n".
        "  Also check that any mathematical functions and syntax used in the above are compatible with\n".
        "  maxima and the f90 package.  Finally, if a diff (derivative) is requested, check that the\n".
        "  derivative exists and that maxima is actually able to calculate it.\n";
      print "  Maxima input lines are contained in the file $tmp_dir/tmp$tmp_file_number.maxima\n";
      print "  Maxima processing output contained in the file $tmp_dir/tmp$tmp_file_number.txt\n";
      print "  Maxima output lines are contained in the file $tmp_dir/tmp$tmp_file_number.fortran\n";
      print DEBUG "ERROR: problem in run_maxima_fortran processing the following mequation:\n$bit\n".
      die;
    };
    $bit = "";
    while ($line = <FORTRAN>) {
      chompm($line); 
      $line =~ s/\s*&\s*$//; # remove any space up to and including continuation character
      $line =~ s/^\s*&?//; # also any space at start of line and if present, the continuation character (used in more recent maxima f90 versions) - ? is the greedy version of {0,1}, ie, it will match '&' if it is present
      $bit=$bit.$line;
    }

    close(FORTRAN);

# check for functions that maxima couldn't do a derivative of
    if ($bit =~ /^\s*\'diff\(/) {
      print "ERROR: problem in run_maxima_fortran processing the following mequation (maxima equation):\n$bit\n".
        "  This equation originated from $otype variable $variable{$otype}[$omvar]{name} and this\n".
        "  error indicates that maxima could not differentiate what is perceived to be a function that is used in this line.\n".
        "  It could be a typo in your input file.  If not and the function is what you intended, maxima may not be\n".
        "  able to differentiate it for some reason.  Consider rewriting the equation in terms of alternative\n".
        "  functions that maxima can handle.  Also try to do the calculations in maxima separately to see what its capabilities are.\n";
      print "  Maxima input lines are contained in the file $tmp_dir/tmp$tmp_file_number.maxima\n";
      print "  Maxima processing output contained in the file $tmp_dir/tmp$tmp_file_number.txt\n";
      print "  Maxima output lines are contained in the file $tmp_dir/tmp$tmp_file_number.fortran\n";
      print DEBUG "ERROR: problem in run_maxima_fortran processing the following mequation:\n$bit\n";
      die;
    }

# save result for possible future use
    $maxima_fortran_results[$#maxima_fortran_results]{"output"} = $bit;
    print DEBUG "saving maxima result:\n";
    print DEBUG " tmp_file_number = $maxima_fortran_results[$#maxima_fortran_results]{tmp_file_number}\n";
    print DEBUG " input = $maxima_fortran_results[$#maxima_fortran_results]{input}\n";
    print DEBUG " output = $maxima_fortran_results[$#maxima_fortran_results]{output}\n";

  }

# now put equation back in terms of maxima variables
  deconstruct_generic_mequation($bit,"f",@replacements); 

  $_[0] = $bit;

  print DEBUG "in run_maxima_fortran: after _[0] = ",$_[0],"\n".
    "---------------------\n";
}

#-------------------------------------------------------------------------------

sub fortran_logical_string {

  use strict;
  my $string;

  if ($_[0]) {
    $string = ".true.";
  } else {
    $string = ".false.";
  }
  return $string;

}

#-------------------------------------------------------------------------------
# this takes a single maxima line and converts it into a single fortran line with
#  references suitable for the final code

sub maxima_to_fortran {

  use strict;
  my ($type, $mvar, $ii, $ji, $pre, $post, $search, $replace, $n, $tmp1, $tmp2, $nextchar);
  my ($ns, $nsnext, $nlast, $nnext); 
  my @iilist = ();
  my $otype = $_[1];
  my $omvar = $_[2];

  print DEBUG "in maxima_to_fortran: before _[0] = ",$_[0],"\n";

  run_maxima_fortran($_[0],$otype,$omvar);

# create two lists of words which should be swapped in the string
  my @searches = ();
  my @replaces = ();
  my @kernel_type = (); # now also include a kernel type field that specifies if the string is a kernel that needs an availability set

  foreach $type (@user_types,"system","someloop") {

    foreach $mvar ( 1 .. $m{$type}) {

      if (nonempty($variable{$type}[$mvar]{"mfortran"})) { # mfortran is empty in (system) variables that have a maxima name that is an actual value (eq transientdelta)
        push(@searches,$variable{$type}[$mvar]{"mfortran"});
        push(@replaces,$variable{$type}[$mvar]{"fortran"});
				if (nonempty($variable{$type}[$mvar]{"kernel_type"})) {
					push(@kernel_type,$variable{$type}[$mvar]{"kernel_type"});
				} else {
					push(@kernel_type,'');
				}
      }

    }
  }

# print DEBUG "doing search replacements of the following pairs\n";
# foreach $ns ( 0 .. $#searches ) {
#   print DEBUG "ns = $ns: searches[ns] = $searches[$ns]: replaces[ns] = $replaces[$ns]\n";
# }
  
# replace strings by looping along each character from the start, to avoid double matches
  print DEBUG "about to do replacements on string _[0] = $_[0]\n";
  $nlast = 0; # we search for the next string at or beyond this location - NB, index starts at 0 for first character
  $nnext = 0;
  while ($nnext < length($_[0])) {

# find position of next match
    $nnext = length($_[0]); # this sets next match to be beyond the last index in the string, as indices are from 0 but length is number of characters
    foreach $ns ( 0 .. $#searches ) { # $#array is the index of the last element of the array
      $n = index($_[0],$searches[$ns],$nlast); # find occurance of search string which is closest to previous

      if ($n < 0 || $n > $nnext) { next; } # if string is not found or lies beyond previous match, try next search
      if ($n eq $nnext && length($searches[$ns]) < length($searches[$nsnext])) { next; } # if strings are in the same starting location, but the current string is shorter that the last, then it must be a substring of another match so skip
      $nnext = $n, $nsnext = $ns; # if we are here then this search string is currently the one that should be replaced

# coding before we checked for substring match
#     if ($n >= 0 && $n < $nnext) { $nnext = $n, $nsnext = $ns; }

    }

    if ($nnext < length($_[0])) { # if nnext occurs within the string, then a match must have been found
      print DEBUG "found match with searches = $searches[$nsnext]: replaces = $replaces[$nsnext]: at nnext = $nnext\n".
        "on string _[0] = $_[0]\n";
      substr($_[0],$nnext,length($searches[$nsnext]),$replaces[$nsnext]);
# if the string found is a kernel element, then set corresponding kernel availability
			if (nonempty($kernel_type[$nsnext])) { 
				$kernel_availability{$kernel_type[$nsnext]} = 1;
				print DEBUG "INFO: kernel_type $kernel_type[$nsnext] found in equation/derivative for $variable{$otype}[$omvar]{name}\n";
			}
      $nlast = $nnext + length($replaces[$nsnext]);
    }

  }

  print DEBUG "in maxima_to_fortran: after _[0] = ",$_[0],"\n";

}

#-------------------------------------------------------------------------------
# here we setup equations

sub create_fortran_equations {

  use strict;
  my ($type, $mvar, $mequation, $tmp, $m2var, $deriv, $inequality, $first, $fequation, $mlink, $firstcondition,
    $otype, $omvar, $l, $n, $reflect_string_init, $reflect_string_form, $openloop, $maxseparation, $separation_loop, 
    $separation_list, $ii2max);

  foreach $type (@user_types,"initial_transient","initial_newtient","someloop") {

    print DEBUG "FORTRAN: starting type = $type\n";
    
    $sub_string{$type}="";
    $first = 1;

    foreach $mvar ( 1 .. $m{$type} ) {

      print DEBUG "FORTRAN: starting variable: mvar = $mvar: name = $variable{$type}[$mvar]{name}\n";

      if (empty($variable{$type}[$mvar]{"mequation"})) {
        print DEBUG "FORTRAN: skipping generating fortran equations for $type [$mvar]: $variable{$type}[$mvar]{name}\n";
        next;
      } # local variables will be skipped here, as well as numerical constant

      $deriv = $variable{$type}[$mvar]{"deriv"};
      $mequation = $variable{$type}[$mvar]{"mequation"};
      $otype = $variable{$type}[$mvar]{"otype"};
      $omvar = $variable{$type}[$mvar]{"omvar"};
      $deriv = $variable{$type}[$mvar]{"deriv"};
      if ($otype eq "unknown") { $deriv = 0; } # special case this deriv, as unknowns need a derivative, but it is set by the fortran rather than being calculated

      print "INFO: generating fortran equations for $type [$mvar]: $variable{$type}[$mvar]{name}: deriv = $deriv\n";
      print DEBUG "FORTRAN: generating fortran equations for $type [$mvar]: $variable{$type}[$mvar]{name}: deriv = $deriv\n";

# open if statement

      if (!($first)) { $sub_string{$type}=$sub_string{$type}."else "; }
      $sub_string{$type}=$sub_string{$type}."if (m == $variable{$type}[$mvar]{fortran_number}) then\n";
      $first = 0;

      $sub_string{$type}=$sub_string{$type}.
        "! $type $variable{$type}[$mvar]{name}\n".
        "! $variable{$type}[$mvar]{equation}\n".
        "! $variable{$type}[$mvar]{mequation}\n\n";

# check ilast, jlast and klast indices if this operation depends on them (eg, a lastface, lastcell or lastnode operation)
      if ($type eq "someloop") {
        if ($variable{$type}[$mvar]{"checki"}) {
          $sub_string{$type}=$sub_string{$type}.
            "if (ilast == 0) call error_stop('A type of lastcell averaging has resulted in a i = 0 reference when updating ".
            "a someloop variable: '//trim(error_string))\n";
        } 
        if ($variable{$type}[$mvar]{"checkj"}) {
          $sub_string{$type}=$sub_string{$type}. 
            "if (jlast == 0) call error_stop('A type of lastface averaging has resulted in a j = 0 reference when updating ".
            "a someloop variable: '//trim(error_string))\n";
        }
        if ($variable{$type}[$mvar]{"checkk"}) {
          $sub_string{$type}=$sub_string{$type}. 
            "if (klast == 0) call error_stop('A type of lastnode averaging has resulted in a k = 0 reference when updating ".
            "a someloop variable: '//trim(error_string))\n";
        }
      }

# pre loop statements - deal with variables required prior to loop commencing
# set default value for max/min
      if ($type eq "someloop" && nonempty($variable{$type}[$mvar]{"default"}) ) {
        $tmp = $variable{$type}[$mvar]{"default"}; 
        maxima_to_fortran($tmp,$variable{$type}[$mvar]{"otype"},$variable{$type}[$mvar]{"omvar"});
        $sub_string{$type}=$sub_string{$type}.
          "! update default expression outside main loop\n".
          "! ".$variable{$type}[$mvar]{"default"}."\n". 
          update_someloop_fortran($variable{$type}[$mvar]{"default"})."\n".
          $variable{$type}[$mvar]{"fortran"}." = ".$tmp."\n";
        if ($deriv) {
          $tmp = create_fortran_derivative_multiplier($variable{$type}[$mvar]{"fortran"},$variable{$type}[$mvar]{"default"},$variable{$type}[$mvar]{"otype"},$variable{$type}[$mvar]{"omvar"});
          $sub_string{$type}=$sub_string{$type}."$tmp\n";
        }
      }
# set flux value for upwind/downwind cells regions
      if ($type eq "someloop" && nonempty($variable{$type}[$mvar]{"flux"}) ) {
        $tmp = $variable{$type}[$mvar]{"flux"}; 
        maxima_to_fortran($tmp,$variable{$type}[$mvar]{"otype"},$variable{$type}[$mvar]{"omvar"});
        $sub_string{$type}=$sub_string{$type}.
          "! update flux expression outside main loop\n".
          "! ".$variable{$type}[$mvar]{"flux"}."\n". 
          update_someloop_fortran($variable{$type}[$mvar]{"flux"})."\n".
          "flux_direction = nint(sign(1.d0,".$tmp."))\n";
      }
      if ($type eq "someloop" && $variable{$type}[$mvar]{"type"} eq "product" ) {
# set initial value for product loop
        $sub_string{$type}=$sub_string{$type}.
          "! setting product initially to 1.\n".
          "$variable{$type}[$mvar]{fortranns} = 1.d0\n\n";
      }

# for conditions write skip statements
      if ($type eq "condition") {
        if (nonempty($variable{$type}[$mvar]{"conditions"})) {
          $sub_string{$type}=$sub_string{$type}."if (.not.(";
          $firstcondition = 1;
          foreach my $condition_type ("output", "stop", "convergence", "bell") {
            if ($variable{$type}[$mvar]{"conditions"} =~ /(^|\,)\s*($condition_type)\s*(\,|$)/) {
              if (!($firstcondition)) { $sub_string{$type}=$sub_string{$type}.".or."; }
              $sub_string{$type} = $sub_string{$type}."condition_type == \"$condition_type\"";
              $firstcondition = 0;
            }
          }
          $sub_string{$type}=$sub_string{$type}.")) cycle\n\n";
        } else {
          $sub_string{$type}=$sub_string{$type}."cycle\n\n";
          print "WARNING: CONDITION variable $variable{$type}[$mvar]{name} has no condition option listed\n";
        }
      }

# form reflect strings for relevant kernel, cellicells and faceicells based regions
      $reflect_string_init = '';
      $reflect_string_form = '';
      if ($type eq "someloop" && $variable{$type}[$mvar]{"reflect"}) {
        if ($variable{$type}[$mvar]{"region"} =~ /<.*faceicells>/ || $variable{$type}[$mvar]{"region"} =~ /<adjacentface.+cell>/) {
          $reflect_string_form = "if (face(j)%reflect_present) reflect_multiplier = dble(face(j)%reflect_multiplier($variable{$type}[$mvar]{reflect}[0],ns)";
          foreach $n ( 1 .. $#{$variable{$type}[$mvar]{reflect}} ) {
            $reflect_string_form=$reflect_string_form."*face(j)%reflect_multiplier($variable{$type}[$mvar]{reflect}[$n],ns)";
          }
        } elsif ($variable{$type}[$mvar]{"region"} =~ /<.*cellicells>/) {
          $reflect_string_form = "if (cell(ilast)%reflect_present) reflect_multiplier = dble(cell(ilast)%reflect_multiplier($variable{$type}[$mvar]{reflect}[0],ns)";
          foreach $n ( 1 .. $#{$variable{$type}[$mvar]{reflect}} ) {
            $reflect_string_form=$reflect_string_form."*cell(ilast)%reflect_multiplier($variable{$type}[$mvar]{reflect}[$n],ns)";
          }
        } elsif ($variable{$type}[$mvar]{"region"} =~ /<.*nodeicells>/) {
          $reflect_string_form = "if (node(k)%reflect_present) reflect_multiplier = dble(node(k)%reflect_multiplier($variable{$type}[$mvar]{reflect}[0],ns)";
          foreach $n ( 1 .. $#{$variable{$type}[$mvar]{reflect}} ) {
            $reflect_string_form=$reflect_string_form."*node(k)%reflect_multiplier($variable{$type}[$mvar]{reflect}[$n],ns)";
          }
        } elsif ($variable{$type}[$mvar]{"region"} =~ /<cellkernelregion\[l=([123])\]>/) {
          $reflect_string_form = "if (cell(ilast)%kernel($1)%reflect_present) reflect_multiplier = dble(cell(ilast)%kernel($1)%reflect_multiplier($variable{$type}[$mvar]{reflect}[0],ns)";
          foreach $n ( 1 .. $#{$variable{$type}[$mvar]{reflect}} ) {
            $reflect_string_form=$reflect_string_form."*cell(ilast)%kernel($1)%reflect_multiplier($variable{$type}[$mvar]{reflect}[$n],ns)";
          }
        } elsif ($variable{$type}[$mvar]{"region"} =~ /<facekernelregion\[l=([0123456])\]>/) {
          $reflect_string_form = "if (face(j)%kernel($1)%reflect_present) reflect_multiplier = dble(face(j)%kernel($1)%reflect_multiplier($variable{$type}[$mvar]{reflect}[0],ns)";
          foreach $n ( 1 .. $#{$variable{$type}[$mvar]{reflect}} ) {
            $reflect_string_form=$reflect_string_form."*face(j)%kernel($1)%reflect_multiplier($variable{$type}[$mvar]{reflect}[$n],ns)";
          }
        } elsif ($variable{$type}[$mvar]{"region"} =~ /<nodekernelregion\[l=([0123])\]>/) {
          $reflect_string_form = "if (node(k)%kernel($1)%reflect_present) reflect_multiplier = dble(node(k)%kernel($1)%reflect_multiplier($variable{$type}[$mvar]{reflect}[0],ns)";
          foreach $n ( 1 .. $#{$variable{$type}[$mvar]{reflect}} ) {
            $reflect_string_form=$reflect_string_form."*node(k)%kernel($1)%reflect_multiplier($variable{$type}[$mvar]{reflect}[$n],ns)";
          }
        } elsif ($variable{$type}[$mvar]{"region"} && $variable{$type}[$mvar]{"separation_list_number"}) {
          print DEBUG "FORTRAN: forming reflect_multiplier for separation variable: name = $variable{$type}[$mvar]{name}: ".
            "separation_list_number = $variable{$type}[$mvar]{separation_list_number}: reflect = $variable{$type}[$mvar]{reflect}\n";
          $separation_list="someloop(thread)%separation_list(".$variable{$type}[$mvar]{separation_list_number}.")"; # shorthand for specific separation_list
          $reflect_string_form = "reflect_multiplier = dble($separation_list%reflect_multiplier($variable{$type}[$mvar]{reflect}[0],ii)";
          foreach $n ( 1 .. $#{$variable{$type}[$mvar]{reflect}} ) {
            $reflect_string_form=$reflect_string_form."*$separation_list%reflect_multiplier($variable{$type}[$mvar]{reflect}[$n],ii)";
          }
        }
        if (nonempty($reflect_string_form)) { $reflect_string_form=$reflect_string_form.")\n"; $reflect_string_init = "reflect_multiplier = 1.d0\n"; }
      }

# start loop
      $openloop = 1; # records whether a loop was actually used, so that we know how to close it later
      $separation_loop=""; # 
# all var variables
      if ( $type ne "someloop" ) {
# open omp loop, for all except for condition and someloop
        if ( $type ne "condition" && $variable{$type}[$mvar]{"centring"} ne "none") {
          $sub_string{$type}=$sub_string{$type}."!\$omp parallel do private(ns,i,j,k,derivative_multiplier,thread)\n";
          $openloop = 'omp';
        }
# now start actual loop
        if ($variable{$type}[$mvar]{"centring"} ne "none") {
# face, cell or node centred variable
# note, as the code currently stands, every region over which a variable is defined must have atleast one element, which is checked in setup_var, so don't need to use allocatable_integer_size here
          $sub_string{$type}=$sub_string{$type}.
#           "do ns = 1, ubound(region(var(m)%region_number)%ijk,1)\n".
            "do ns = 1, allocatable_integer_size(region($variable{$type}[$mvar]{region_number})%ijk)\n".
            ijkstring($variable{$type}[$mvar]{"centring"})." = region($variable{$type}[$mvar]{region_number})%ijk(ns)\n";
        } else {
# none centred variable
          $sub_string{$type}=$sub_string{$type}.
#           "do ns = 1, 1\n";
            "! no loop being performed for none centred variable\n".
            "ns = 1\n";
          $openloop = 0;
        }
        if ( $openloop eq "omp" ) {
          $sub_string{$type}=$sub_string{$type}.
            "!\$ thread = omp_get_thread_num() + 1\n".
            "if (debug) write(*,'(2(a,i3),a,i12)') 'm = ',m,': thread = ',thread,': ns = ',ns\n";
        }
        if ( $type eq "unknown" ) {
          $sub_string{$type}=$sub_string{$type}.
            "var(m)%funk(ns)%v = 0.d0\n"; # don't want to reset the derivative for this one
        } else {

# if newtstepmax/min are set then insert statements to check on the range, and if not current, clear the derivative while maintaining the
# previously evaluated value rather than recalculating both value (and possibly derivative)
          if (nonempty($variable{$type}[$mvar]{newtstepmax}) || nonempty($variable{$type}[$mvar]{newtstepmin})) {
            $sub_string{$type}=$sub_string{$type}.
              "if (".newtstepcondition($variable{$type}[$mvar]{newtstepmin},$variable{$type}[$mvar]{newtstepmax}).") then\n".
              "call clear_funk_derivatives(var(m)%funk(ns))\n"."else\n";
          }

          $sub_string{$type}=$sub_string{$type}.
            "call reset_funk(var(m)%funk(ns))\n";

          if ($variable{$type}[$mvar]{"update_region_number"} ne $variable{$type}[$mvar]{"region_number"}) { # only true if update region is dynamic, noting that we cycle through all elements in the parent region to zero elements that are outside of the update region (that previously may not have been)
            $sub_string{$type}=$sub_string{$type}.
              "! as variable is defined on dynamic region $variable{$type}[$mvar]{update_region}, checking whether current element is in this region\n".
              "if (region($variable{$type}[$mvar]{update_region_number})%ns(".ijkstring($variable{$type}[$mvar]{"centring"}).") == 0) cycle\n";
          }

        }
      } else {
# someloop
# ns is not used to reference funk in a someloop

# set up someloops that are relative to current location
        if ($variable{$type}[$mvar]{"region"} eq '<celljfaces>') {
          $sub_string{$type}=$sub_string{$type}.
            "! loop is conducted around region $variable{$type}[$mvar]{region}\n".
            "do ns = 1, ubound(cell(i)%jface,1)\n".
            "j = cell(i)%jface(ns)\n";
        } elsif ($variable{$type}[$mvar]{"region"} eq '<nobcelljfaces>') {
          $sub_string{$type}=$sub_string{$type}.
            "! loop is conducted around region $variable{$type}[$mvar]{region}\n".
            "! move loop to domain cell if cell is a boundary cell\n".
            "if (cell(i)%type == 2) i = cell(i)%icell(2)\n". # second cell%icell index should be neighbouring cell
            "do ns = 1, ubound(cell(i)%jface,1)\n".
            "j = cell(i)%jface(ns)\n";
        } elsif ($variable{$type}[$mvar]{"region"} eq '<cellicells>') {
          $sub_string{$type}=$sub_string{$type}.
            "! loop is conducted around region $variable{$type}[$mvar]{region}\n".$reflect_string_init.
            "do ns = 1, ubound(cell(ilast)%icell,1)\n".
            "i = cell(ilast)%icell(ns)\n".$reflect_string_form;
        } elsif ($variable{$type}[$mvar]{"region"} eq '<cellknodes>') {
          $sub_string{$type}=$sub_string{$type}.
            "! loop is conducted around region $variable{$type}[$mvar]{region}\n".
            "do ns = 1, ubound(cell(i)%knode,1)\n".
            "k = cell(i)%knode(ns)\n";
        } elsif ($variable{$type}[$mvar]{"region"} eq '<faceicells>') {
          $sub_string{$type}=$sub_string{$type}.
            "! loop is conducted around region $variable{$type}[$mvar]{region}\n".$reflect_string_init.
            "do ns = 1, ubound(face(j)%icell,1)\n".
            "i = face(j)%icell(ns)\n".$reflect_string_form;
        } elsif ($variable{$type}[$mvar]{"region"} eq '<faceknodes>') {
          $sub_string{$type}=$sub_string{$type}.
            "! loop is conducted around region $variable{$type}[$mvar]{region}\n".
            "do ns = 1, ubound(face(j)%knode,1)\n".
            "k = face(j)%knode(ns)\n";
        } elsif ($variable{$type}[$mvar]{"region"} eq '<nodeicells>') {
          $sub_string{$type}=$sub_string{$type}.
            "! loop is conducted around region $variable{$type}[$mvar]{region}\n".$reflect_string_init.
            "do ns = 1, ubound(node(k)%icell,1)\n".
            "i = node(k)%icell(ns)\n".$reflect_string_form;
        } elsif ($variable{$type}[$mvar]{"region"} eq '<adjacentcellicells>') {
          $sub_string{$type}=$sub_string{$type}.
            "! loop is conducted around region $variable{$type}[$mvar]{region}\n".$reflect_string_init.
            "do ns = 1, ubound(cell(ilast)%jface,1)+1\n".
            "i = cell(ilast)%icell(ns)\n".$reflect_string_form;
        } elsif ($variable{$type}[$mvar]{"region"} eq '<nocadjacentcellicells>') { # this does not include the centre
          $sub_string{$type}=$sub_string{$type}.
            "! loop is conducted around region $variable{$type}[$mvar]{region}\n".$reflect_string_init.
            "do ns = 2, ubound(cell(ilast)%jface,1)+1\n".
            "i = cell(ilast)%icell(ns)\n".$reflect_string_form;
        } elsif ($variable{$type}[$mvar]{"region"} eq '<adjacentfaceicells>') {
          $sub_string{$type}=$sub_string{$type}.
            "! loop is conducted around region $variable{$type}[$mvar]{region}\n".$reflect_string_init.
            "do ns = 1, 2\n".
            "i = face(j)%icell(ns)\n".$reflect_string_form;
#         $sub_string{$type}=$sub_string{$type}.
#           "! loop is conducted around region $variable{$type}[$mvar]{region}\n".
#           "do ns = 1, 2\n".
#           "i = face(j)%icell(ns)\n";
#             if ($variable{$type}[$mvar]{"reflect"}) {
#               $sub_string{$type}=$sub_string{$type}.
#                 "! loop is conducted around region $variable{$type}[$mvar]{region} with possible reflect occurring\n".
#                 "reflect_multiplier = 1.d0\n".
#                 "do ns = 1, 2\n".
#                 "i = face(j)%icell(ns)\n".
#                 "if (allocated(face(j)%reflect_multiplier)) reflect_multiplier = dble(face(j)%reflect_multiplier($variable{$type}[$mvar]{reflect}[0],ns)";
#               foreach $n ( 1 .. $#{$variable{$type}[$mvar]{reflect}} ) {
#                 $sub_string{$type}=$sub_string{$type}."*face(j)%reflect_multiplier($variable{$type}[$mvar]{reflect}[$n],ns)";
#               }
#               $sub_string{$type}=$sub_string{$type}.")\n";
#             } else {
#               $sub_string{$type}=$sub_string{$type}.
#                 "! loop is conducted around region $variable{$type}[$mvar]{region}\n".
#                 "do ns = 1, 2\n".
#                 "i = face(j)%icell(ns)\n";
#             }
        } elsif ($variable{$type}[$mvar]{"region"} eq '<adjacentfaceupcell>') {
          $sub_string{$type}=$sub_string{$type}.
            "! loop is conducted around region $variable{$type}[$mvar]{region}\n".$reflect_string_init.
#           "do ns = 2, 2\n".
            "ns = 2\n".
            "i = face(j)%icell(ns)\n".$reflect_string_form;
            $openloop = 0;
        } elsif ($variable{$type}[$mvar]{"region"} eq '<adjacentfacedowncell>') {
          $sub_string{$type}=$sub_string{$type}.
            "! loop is conducted around region $variable{$type}[$mvar]{region}\n".$reflect_string_init.
#           "do ns = 1, 1\n".
            "ns = 1\n".
            "i = face(j)%icell(ns)\n".$reflect_string_form;
            $openloop = 0;
        } elsif ($variable{$type}[$mvar]{"region"} eq '<adjacentfaceothercell>') {
#         $sub_string{$type}=$sub_string{$type}.
#           "! loop is conducted around region $variable{$type}[$mvar]{region}\n".
#           "do ns = 1, 1\n".
#           "if (ilast == face(j)%icell(1)) then\n".
#           "i = face(j)%icell(2)\n".
#           "else if (ilast == face(j)%icell(2)) then\n".
#           "i = face(j)%icell(1)\n".
#           "else\n".
#           "call error_stop('problem with use of <adjacentfaceothercell> region')\n".
#           "end if\n";
          $sub_string{$type}=$sub_string{$type}.
            "! loop is conducted around region $variable{$type}[$mvar]{region}\n".$reflect_string_init.
            "if (ilast == face(j)%icell(1)) then\n".
#           "static_ns = 2\n".
#           "else if (ilast == face(j)%icell(2)) then\n".
#           "static_ns = 1\n".
#           "else\n".
#           "call error_stop('problem with use of <adjacentfaceothercell> region')\n".
#           "end if\n".
#           "do ns = static_ns, static_ns\n".
            "ns = 2\n".
            "else if (ilast == face(j)%icell(2)) then\n".
            "ns = 1\n".
            "else\n".
            "call error_stop('Problem with use of <adjacentfaceothercell> region: '//trim(error_string))\n".
            "end if\n".
            "i = face(j)%icell(ns)\n".$reflect_string_form;
            $openloop = 0;
        } elsif ($variable{$type}[$mvar]{"region"} eq '<upwindfaceicells>') {
          $sub_string{$type}=$sub_string{$type}.
            "! loop is conducted around region $variable{$type}[$mvar]{region}\n".$reflect_string_init.
#           "do ns = (3-flux_direction)/2, (3-flux_direction)/2\n".
            "ns = (3-flux_direction)/2\n".
            "i = face(j)%icell(ns)\n".$reflect_string_form;
            $openloop = 0;
        } elsif ($variable{$type}[$mvar]{"region"} eq '<downwindfaceicells>') {
          $sub_string{$type}=$sub_string{$type}.
            "! loop is conducted around region $variable{$type}[$mvar]{region}\n".$reflect_string_init.
#           "do ns = (3+flux_direction)/2, (3+flux_direction)/2\n".
            "ns = (3+flux_direction)/2\n".
            "i = face(j)%icell(ns)\n".$reflect_string_form;
            $openloop = 0;
        } elsif ($variable{$type}[$mvar]{"region"} eq '<cellkernelregion[l=0]>') {
          $sub_string{$type}=$sub_string{$type}.
            "! loop is conducted around region $variable{$type}[$mvar]{region}\n".
            "do ns = 1, allocatable_integer_size(cell(i)%kernel(0)%ijk)\n".
            "j = cell(i)%kernel(0)%ijk(ns)\n";
        } elsif ($variable{$type}[$mvar]{"region"} =~ /<cellkernelregion\[l=([123])\]>/) {
          $sub_string{$type}=$sub_string{$type}.
            "! loop is conducted around region $variable{$type}[$mvar]{region}\n".$reflect_string_init.
            "do ns = 1, allocatable_integer_size(cell(ilast)%kernel($1)%ijk)\n".
            "i = cell(ilast)%kernel($1)%ijk(ns)\n".$reflect_string_form;
        } elsif ($variable{$type}[$mvar]{"region"} =~ /<cellkernelregion\[l=([4567])\]>/) {
          $sub_string{$type}=$sub_string{$type}.
            "! loop is conducted around region $variable{$type}[$mvar]{region}\n".
            "do ns = 1, allocatable_integer_size(cell(i)%kernel($1)%ijk)\n".
            "k = cell(i)%kernel($1)%ijk(ns)\n";
        } elsif ($variable{$type}[$mvar]{"region"} =~ /<facekernelregion\[l=([0123456])\]>/) {
          $sub_string{$type}=$sub_string{$type}.$reflect_string_init.
            "! loop is conducted around region $variable{$type}[$mvar]{region}\n".
            "do ns = 1, allocatable_integer_size(face(j)%kernel($1)%ijk)\n".
            "i = face(j)%kernel($1)%ijk(ns)\n".$reflect_string_form;
        } elsif ($variable{$type}[$mvar]{"region"} =~ /<nodekernelregion\[l=([0123])\]>/) {
          $sub_string{$type}=$sub_string{$type}.$reflect_string_init.
            "! loop is conducted around region $variable{$type}[$mvar]{region}\n".
            "do ns = 1, allocatable_integer_size(node(k)%kernel($1)%ijk)\n".
            "i = node(k)%kernel($1)%ijk(ns)\n".$reflect_string_form;
        } elsif ($variable{$type}[$mvar]{"region"} eq '<glueface>') {
          $sub_string{$type}=$sub_string{$type}.
            "! loop is conducted around region $variable{$type}[$mvar]{region}\n".
#           "do ns = 1, 1\n".
#           "ns = 1\n".
            "if (face(jlast)%glue_jface /= 0) j = face(jlast)%glue_jface\n";
            $openloop = 0;
        } elsif ($variable{$type}[$mvar]{"region"} eq '<lastface>') {
          $sub_string{$type}=$sub_string{$type}.
            "! loop is conducted around region $variable{$type}[$mvar]{region}\n".
#           "do ns = 1, 1\n".
#           "ns = 1\n".
#           "if (face(jlast)%icell(2) == i.and.face(jlast)%glue_jface /= 0) j = face(jlast)%glue_jface\n";
            "if (face(jlast)%glue_jface /= 0) then\nif (face(jlast)%icell(2) == i) j = face(jlast)%glue_jface\nend if\n"; # more efficient nested conditionals
            $openloop = 0;
        } elsif ($variable{$type}[$mvar]{"region"} =~ /<separationcentre(\d*)>/) {
          $sub_string{$type}=$sub_string{$type}.
            "! loop is conducted around region $variable{$type}[$mvar]{region}\n".
            "i = separationcentreicell(thread,$1,error_string)\n"; # this function checks for errors in i
            $openloop = 0;
        } elsif ($variable{$type}[$mvar]{"region"} eq '<noloop>' ||
                 $variable{$type}[$mvar]{"centring"} eq 'none') {
#         $sub_string{$type}=$sub_string{$type}.
#           "! region is $variable{$type}[$mvar]{region} with centring $variable{$type}[$mvar]{centring} ".
#           "so dummy loop is performed\n".
#           "do ns = 1, 1\n";
# now not looping at all with <noloop>
          $sub_string{$type}=$sub_string{$type}.
            "! region is $variable{$type}[$mvar]{region} with centring $variable{$type}[$mvar]{centring} ".
#           "so no loop is performed\nns = 1\n";
            "so no loop is performed\n";
          $openloop = 0;
        
# set up someloops that are absolute locations
        } elsif ($variable{$type}[$mvar]{"update_region_number"}) { # 0 if not defined over a region

# now update_region_number is always defined
          $sub_string{$type}=$sub_string{$type}."region_number = $variable{$type}[$mvar]{update_region_number}\n";

# loop through region in increasing order of increasing separation
          if ($variable{$type}[$mvar]{"separation_list_number"}) {

            $separation_loop="separation_loop_".$variable{$type}[$mvar]{separation_list_number}; # name of this loop
            $separation_list="someloop(thread)%separation_list(".$variable{$type}[$mvar]{separation_list_number}.")"; # shorthand for specific separation_list
            $maxseparation=$variable{$type}[$mvar]{"maxseparation"};
#           if ($maxseparation < 0) { $maxseparation = "ubound(region(region_number)%ijk,1)"; } # if maxseparation has no limit, then set it to the number of cells in the region
            if ($maxseparation < 0) { $maxseparation = "itotal"; } # if maxseparation has no limit, then set it to itotal
            $sub_string{$type}=$sub_string{$type}.$reflect_string_init.
              "call shift_integer_array(array=someloop(thread)%current_separation_list,new_element=$variable{$type}[$mvar]{separation_list_number})\n".
              "$separation_list%nseparation = -1\n". # this will make add_to_separation_list set defaults for the first cell
              "call add_to_separation_list($separation_list,iadd=ilast)\n".
              "$separation_loop: do nseparation = 0,$maxseparation\n".
              "$separation_list%nseparation = nseparation\n". # save current separation for 1) use in expressions and 2) use in updating separation_list
              "call add_to_separation_list($separation_list)\n". # this will initialise the next separation list, so that we can reference its index in the separation_index
              "iistart = 1\nif (nseparation > 0) iistart = $separation_list%separation_index(nseparation)+1\n".
              "iiend = $separation_list%separation_index(nseparation+1)\n".
              "if (iistart > iiend) exit $separation_loop\n".
              "do ii = iistart, iiend\n".
              "i = $separation_list%icell(ii)\n".
              "ns = region(region_number)%ns(i)\n".
              "if (nseparation >= ".$variable{$type}[$mvar]{"minseparation"}.") then\n". # only if the separation is greater than minseparation do we actually evaluate the variable
              "$separation_list%iicurrent = ii\n".$reflect_string_form; # set iicurrent for references to r or reflect_multiplier

          } else {
# loop through region normally in order of increasing ns index of region
                
            $sub_string{$type}=$sub_string{$type}.
#             "do ns = 1, ubound(region(region_number)%ijk,1)\n".
              "do ns = 1, allocatable_integer_size(region(region_number)%ijk)\n".
              ijkstring($variable{$type}[$mvar]{"centring"})." = region(region_number)%ijk(ns)\n";

          }

        } else {
          error_stop("someloop originating from $variable{$otype}[$omvar]{name} has no region specified in create_fortran_equations");
        }


      }
  
# do inner loop operation
      if ($type eq "someloop" && ( $variable{$type}[$mvar]{"type"} eq "min" ||
        $variable{$type}[$mvar]{"type"} eq "max" )) {
# min or max

        $tmp = $mequation;
        maxima_to_fortran($tmp,$otype,$omvar);
        if ($variable{$type}[$mvar]{"type"} eq "min") {
          $inequality = "<";
        } else {
          $inequality = ">";
        }
        $sub_string{$type}=$sub_string{$type}.
          update_someloop_fortran($mequation)."\n".
          "if (".$tmp.$inequality.$variable{$type}[$mvar]{"fortran"}.") then\n".
          "call reset_funk(someloop(thread)%funk(m))\n".
          $variable{$type}[$mvar]{"fortran"}." = ".$tmp."\n";
        if ($deriv) {
          $tmp = create_fortran_derivative_multiplier($variable{$type}[$mvar]{"fortran"},$mequation,$otype,$omvar);
          $sub_string{$type}=$sub_string{$type}."$tmp\n";
        }
        $sub_string{$type}=$sub_string{$type}."end if\n";

      } elsif ($type eq "someloop" && $variable{$type}[$mvar]{"type"} eq "if" ) {
# if

        $deriv = $variable{$type}[$mvar]{"valuederiv"}; # deriv for value is separate from condition deriv (=0)
        $tmp = $mequation; # mequation is condition, so no derivative required 
        maxima_to_fortran($tmp,$otype,$omvar);
        $sub_string{$type}=$sub_string{$type}.
          update_someloop_fortran($mequation)."\n".
#         "if (".$tmp." >= 0.d0) then\n".
          "if (".$tmp." > 0.d0) then\n".
          "! value for positive condition (true)\n";
        $tmp = $variable{$type}[$mvar]{"true"}; 
        maxima_to_fortran($tmp,$otype,$omvar);
        $sub_string{$type}=$sub_string{$type}.
          update_someloop_fortran($variable{$type}[$mvar]{"true"})."\n".
          $variable{$type}[$mvar]{"fortran"}." = ".$tmp."\n";
        if ($deriv) {
          $tmp = create_fortran_derivative_multiplier($variable{$type}[$mvar]{"fortran"},$variable{$type}[$mvar]{"true"},$otype,$omvar);
          $sub_string{$type}=$sub_string{$type}."$tmp\n";
        }
        $sub_string{$type}=$sub_string{$type}.
          "else\n".
          "! value for negative condition (false)\n";
        $tmp = $variable{$type}[$mvar]{"false"}; 
        maxima_to_fortran($tmp,$otype,$omvar);
        $sub_string{$type}=$sub_string{$type}.
          update_someloop_fortran($variable{$type}[$mvar]{"false"})."\n".
          $variable{$type}[$mvar]{"fortran"}." = ".$tmp."\n";
        if ($deriv) {
          $tmp = create_fortran_derivative_multiplier($variable{$type}[$mvar]{"fortran"},$variable{$type}[$mvar]{"false"},$otype,$omvar);
          $sub_string{$type}=$sub_string{$type}."$tmp\n";
        }
        $sub_string{$type}=$sub_string{$type}."end if\n";

      } elsif ($type eq "someloop" && $variable{$type}[$mvar]{"type"} eq "product" ) {
# product

        $fequation = "$mequation";
        maxima_to_fortran($fequation,$otype,$omvar);
        $tmp = "$variable{$type}[$mvar]{fortranns} = $variable{$type}[$mvar]{fortranns}*(".$fequation.")";
        $sub_string{$type}=$sub_string{$type}.
          update_someloop_fortran($mequation)."\n".
          "$tmp\n\n".
          "! if any element is small then exit\n".
          "if (abs($fequation)<eps_dv) exit\n\n";

        if ($deriv) {
          $tmp = create_fortran_derivative_multiplier($variable{$type}[$mvar]{"fortranns"},"log($mequation)",$otype,$omvar);
          $sub_string{$type}=$sub_string{$type}.
            "! using the product rule we first calculate the derivative of the log($mequation) for each element".
            "$tmp\n";
        }

      } elsif ($type eq "someloop" && $variable{$type}[$mvar]{"type"} eq "link" ) {
# link
# create commands to move to a new position
        $mlink = $variable{$type}[$mvar]{"region_mlink"}; # this is the perl region_link array reference
        $sub_string{$type}=$sub_string{$type}.
          "from_ns = region(region_link($region_link[$mlink]{number})%from_region_number)%ns(".ijkstring($region_link[$mlink]{"from_centring"})."last)\n";
        $sub_string{$type}=$sub_string{$type}.
					"if (from_ns == 0) then\ncall error_stop('Problem with a region link from_ns reference which indicates that the region context".
					" of a linking command is incorrect - check that the localregion used in the link is consistent with the region over which the".
					" relevant variable is defined: '//trim(error_string))\nend if\n";
        $sub_string{$type}=$sub_string{$type}.
          "to_ns = region_link($region_link[$mlink]{number})%to_ns(from_ns)\n";
#       $sub_string{$type}=$sub_string{$type}.
# 				"if (to_ns == 0) then\ncall error_stop('Problem with a region link to_ns reference: '//trim(error_string))\nend if\n";
        $sub_string{$type}=$sub_string{$type}.
          ijkstring($region_link[$mlink]{"to_centring"})." = region(region_link($region_link[$mlink]{number})%to_region_number)%ijk(to_ns)\n";

        $tmp = "$mequation";
        maxima_to_fortran($tmp,$otype,$omvar);
        $tmp = "$variable{$type}[$mvar]{fortranns} = (".$tmp.")";
        $sub_string{$type}=$sub_string{$type}.
          update_someloop_fortran($mequation)."\n".
          "$tmp\n";

        if ($deriv) {
          $tmp = create_fortran_derivative_multiplier($variable{$type}[$mvar]{"fortranns"},$mequation,$otype,$omvar);
          $sub_string{$type}=$sub_string{$type}."$tmp\n";
        }

      } elsif ($type eq "someloop" && $variable{$type}[$mvar]{"type"} eq "external" ) {
# use an external subroutine to calculated the someloop
        $sub_string{$type}=$sub_string{$type}.
          "call reset_funk(someloop(thread)%funk(m))\n".
          update_someloop_fortran($mequation)."\n".
          "call ".$variable{$type}[$mvar]{"external_subroutine"}."\n";

      } else {
# sum

        $tmp = "$mequation";
        maxima_to_fortran($tmp,$otype,$omvar);
        $tmp = "$variable{$type}[$mvar]{fortranns} = $variable{$type}[$mvar]{fortranns} + (".$tmp.")";
        $sub_string{$type}=$sub_string{$type}.
          update_someloop_fortran($mequation)."\n".
          "$tmp\n";

        if ($deriv) {
          $tmp = create_fortran_derivative_multiplier($variable{$type}[$mvar]{"fortranns"},$mequation,$otype,$omvar);
          $sub_string{$type}=$sub_string{$type}."$tmp\n";
        }

      }

# for a newtstep check close if statement (NB, only for equations and deriveds)
      if (nonempty($variable{$type}[$mvar]{newtstepmax}) || nonempty($variable{$type}[$mvar]{newtstepmin})) {
        $sub_string{$type}=$sub_string{$type}."end if\n";
      }

# for a condition check on the sign and possibly exit
      if ($type eq "condition") {
        $sub_string{$type}=$sub_string{$type}.
          "if ($variable{$type}[$mvar]{fortranns} > 0.d0) then\ncheck_condition = .true.\nreturn\nend if\n\n";
      }

# close loop, possibly dealing with separation stuff
      if ($openloop) {
# close the loop
        if ($variable{$type}[$mvar]{"separation_list_number"}) {
          if ($variable{$type}[$mvar]{faceseparation}) {
            $ii2max = 'ubound(cell(i)%jface,1)+1'; # for faceseparation change this loop to around the cells that share a face with the current cell
          } else {
            $ii2max = 'ubound(cell(i)%icell,1)'; # this is for node based separation
          }
          $sub_string{$type}=$sub_string{$type}.
            "end if\n".
            "if (nseparation < ".$maxseparation.") then\n".
            "do ii2 = 2, $ii2max\n".
            "i2 = cell(i)%icell(ii2)\n".
            "if (region(region_number)%ns(i2) == 0) cycle\n";
# if faceseparationflag is being used, need to evaluate here
          if (nonempty($variable{$type}[$mvar]{faceseparationflag})) {
            $tmp = $variable{$type}[$mvar]{"faceseparationflag"}; 
            maxima_to_fortran($tmp,$variable{$type}[$mvar]{"otype"},$variable{$type}[$mvar]{"omvar"});
            $sub_string{$type}=$sub_string{$type}.
              "j = cell(i)%jface(ii2-1)\n".
              "! update faceseparationflag expression\n".
              "! ".$variable{$type}[$mvar]{"faceseparationflag"}."\n". 
              update_someloop_fortran($variable{$type}[$mvar]{"faceseparationflag"})."\n".
              "if ($tmp <= 0.d0) cycle\n";
          }
          $sub_string{$type}=$sub_string{$type}.
            "call add_to_separation_list($separation_list,iadd=i2,iiadd=ii2,ifrom=i,iifrom=ii)\n".
            "end do\nend if\nend do\nend do $separation_loop\n". # terminating 3 loops: around surrounding cells, through this separation layer cells, and separation loop
            "call unshift_integer_array(array=someloop(thread)%current_separation_list)\n";
        } else {
          $sub_string{$type}=$sub_string{$type}."end do\n";
        }
# tack on any omp stuff, or an extra space
        if ($openloop eq "omp") {
          $sub_string{$type}=$sub_string{$type}.
            "!\$omp end parallel do\n!\$ thread = 1\n\n"; # close omp loop and make sure thread is back to 1
        } elsif ($openloop) {
          $sub_string{$type}=$sub_string{$type}."\n";
        }
      } else {
        $sub_string{$type}=$sub_string{$type}."\n";
      }

#     if ( $type ne "condition" && $type ne "someloop" && $variable{$type}[$mvar]{"centring"} ne "none") {
#       $sub_string{$type}=$sub_string{$type}.
#         "end do\n!\$omp end parallel do\n!\$ thread = 1\n\n"; # close omp loop and make sure thread is back to 1
#     } elsif (($type eq "someloop" && ($variable{$type}[$mvar]{"region"} eq '<noloop>' ||
#              $variable{$type}[$mvar]{"centring"} eq 'none')) || 
#             ($type ne "someloop" && $variable{$type}[$mvar]{"centring"} eq 'none')) {
#       $sub_string{$type}=$sub_string{$type}."\n";
#     } else {
#       $sub_string{$type}=$sub_string{$type}."end do\n\n";
#     }

# post loop operations
# alter derivative for product
      if ($type eq "someloop" && $variable{$type}[$mvar]{"type"} eq "product" && $deriv) {
        $tmp = $variable{$type}[$mvar]{"fortranns"}; # name as will be used in add_to_dv (fortran)
        $tmp =~ s/%v$//;
        $sub_string{$type}=$sub_string{$type}.
          "! and then multiply all derivatives by the resulting product of all elements\n".
          "call multiply_dv($tmp,$variable{$type}[$mvar]{fortranns})\n\n";
      }
        
      $mvar++;

    }

    if (!($first)) { # if an if block has been opened, we need to close it again
      if ($type eq "constant") {
        $sub_string{$type}=$sub_string{$type}."\nend if\n";
      } else {
        $sub_string{$type}=$sub_string{$type}.
          "\nelse\nwrite(*,*) \"ERROR: the index \",m,\" was not found when updating $type\"\nstop\nend if\n";
      }
    }
  }

}

#-------------------------------------------------------------------------------
# little function to create condition string for fortran that determines whether variable or region is updated or not

sub newtstepcondition {
# on input
  my $newtstepmin=$_[0];
  my $newtstepmax=$_[1];
# on output
  my $string='';

  if (nonempty($newtstepmax)) {
    $string="newtstep > $newtstepmax";
    if (nonempty($newtstepmin)) { $string=$string.".or."; }
  }
  if (nonempty($newtstepmin)) {
    $string=$string."newtstep < $newtstepmin";
  }
  return ($string);
}
#-------------------------------------------------------------------------------
# here we scan a string for any someloops, and write corresponding fortran
#  to call update_someloop
# enter with $_[0] as the mequation (unchanged), exit with fortran

sub update_someloop_fortran {

  use strict;
  my ($fortran, $mvar);

  $fortran = "";

  print DEBUG "SEARCH IN: $_[0]\n";

  $mvar = 1;
  while ($m{"someloop"} && $mvar <= $m{"someloop"}) {
#   if ($_[0] =~ /\Q$variable{"someloop"}[$mvar]{"maxima"}/) {
    print DEBUG "SEARCH FOR: m = $mvar: maxima = $variable{someloop}[$mvar]{maxima}\n";
    if ($_[0] =~ /\Q$variable{"someloop"}[$mvar]{"maxima"}/ || $_[0] =~ /(\[|,)\s*msomeloop_\S+\s*=\s*$mvar\s*(,|\])/) { # now also match fortran subroutine references to someloop variables
# NB: an msomeloop_* reference triggers a call to update_someloop, whereas a m_* reference does not and so for this case update_someloop would need to be called within the subroutine itself
#     $fortran = $fortran."call update_someloop(thread,$mvar,i,j,k,error_string)\n";
      $fortran = $fortran."call update_someloop(thread=thread,m=$mvar,ilast=i,jlast=j,klast=k,error_string=error_string)\n";
      print DEBUG "SEARCH FOUND\n";
    }
    $mvar++;
  }

  return $fortran;
}

#-------------------------------------------------------------------------------
# takes an expression and performs all possible derivatives on that expression, 
#  returning fortran code to evaluate those derivatives
# called with arguments $mname and $mequation - returns fortran string 

sub create_fortran_derivative_multiplier {

  use strict;
  my ($typewrt, $mvarwrt, $tmp, $tmpwrt, $dfortran, $ii, $itmp);

  my $fortran = $_[0]; # name as will be used in add_to_dv (fortran)
  my $mequation = $_[1]; # maxima type equation as passed to routine
  my $otype = $_[2];
  my $omvar = $_[3];
  $fortran =~ s/%v$//;
  my $string = "";

  print DEBUG "in create_fortran_derivative_multiplier with fortran = $fortran and mequation = $mequation\n";

# maxima_to_fortran($name); #convert to fortran format
# $name =~ s/%v//; # but lose subelement reference

# cycle through looking for unknowns, deriveds and someloops - anything that the equations ultimately depend on
  foreach $typewrt ("unknown","derived","someloop") {
    foreach $mvarwrt ( 1 .. $m{$typewrt} ) {
      $tmpwrt = $variable{$typewrt}[$mvarwrt]{"maxima"};
      if ($mequation !~ /\Q$tmpwrt/) {
#       print DEBUG "variable not present in expression so skipping\n";
        next;
      }
      print DEBUG "in create_fortran_derivative_multiplier doing relative diff of: mequation = $mequation: wrt: tmpwrt = $tmpwrt\n";
      $tmp = "diff($mequation,$tmpwrt)";
      maxima_to_fortran($tmp,$otype,$omvar);
      if ($tmp) {
        $dfortran = $variable{$typewrt}[$mvarwrt]{"fortran"};
        $dfortran =~ s/%v//;
        $string=$string."\nderivative_multiplier = $tmp\ncall add_to_dv(thread,$fortran,derivative_multiplier,$dfortran)\n";
      }

    }
  }

  print DEBUG "leaving create_fortran_derivative_multiplier with string = $string\n";

  return $string;
}

#-------------------------------------------------------------------------------
# creates details regarding the system (code generated) variables
# also some default regions

sub create_system_variables {

  use strict;
  my ($mvar, $l, $l1, $l2);
  my $type = "system";

# generally index order should be the same between maxima and fortran and be: i,j,ji,k,l,m,n,ns,p
# i = cell (or ilast being the last cell reference)
# j = face (or jlast similarly)
# k = node (or klast similarly)
# l = dimension, m = number (of variable), n = time, ns = index within region

#------------------------------------
# ref: system variables cell
# cell geometry and index functions
  $m{"system"}++;
  $variable{"system"}[$m{"system"}]{"name"} = "<icell>"; # double precision representation of icell index
  $variable{"system"}[$m{"system"}]{"centring"} = "cell";
  $variable{"system"}[$m{"system"}]{"maxima"} = "icell";
  $variable{"system"}[$m{"system"}]{"fortran"} = "dble(i)";
  foreach $l ( 1 .. 3 ) {
    $m{"system"}++;
    $variable{"system"}[$m{"system"}]{"name"} = "<cellx[l=$l]>"; # cell centred location vector
    $variable{"system"}[$m{"system"}]{"centring"} = "cell";
    $variable{"system"}[$m{"system"}]{"maxima"} = "cellx[i,$l]";
    $variable{"system"}[$m{"system"}]{"fortran"} = "cell(i)%x($l)";
    $variable{"system"}[$m{"system"}]{"units"} = $lengthunit;
    $m{"system"}++;
    $variable{"system"}[$m{"system"}]{"name"} = "<celldx[l=$l]>"; # cell centred size vector
    $variable{"system"}[$m{"system"}]{"centring"} = "cell";
    $variable{"system"}[$m{"system"}]{"maxima"} = "celldx[i,$l]";
    $variable{"system"}[$m{"system"}]{"fortran"} = "cell(i)%dx($l)";
    $variable{"system"}[$m{"system"}]{"units"} = $lengthunit;
  }
  $m{"system"}++;
  $variable{"system"}[$m{"system"}]{"name"} = "<celldxkernel>"; # distance (cell radius) that is characteristic of kernels for this cell
  $variable{"system"}[$m{"system"}]{"centring"} = "cell";
  $variable{"system"}[$m{"system"}]{"maxima"} = "celldxkernel[i]";
  $variable{"system"}[$m{"system"}]{"fortran"} = "cell(i)%dx_kernel";
  $variable{"system"}[$m{"system"}]{"units"} = $lengthunit;
  $m{"system"}++;
  $variable{"system"}[$m{"system"}]{"name"} = "<celldxmax>"; # maximum node to node distance for this cell (ie, the maximum possible cell dimension)
  $variable{"system"}[$m{"system"}]{"centring"} = "cell";
  $variable{"system"}[$m{"system"}]{"maxima"} = "celldxmax[i]";
  $variable{"system"}[$m{"system"}]{"fortran"} = "cell(i)%dx_max";
  $variable{"system"}[$m{"system"}]{"units"} = $lengthunit;
  $m{"system"}++;
  $variable{"system"}[$m{"system"}]{"name"} = "<celldxmin>"; # minimum node to node distance for this cell
  $variable{"system"}[$m{"system"}]{"centring"} = "cell";
  $variable{"system"}[$m{"system"}]{"maxima"} = "celldxmin[i]";
  $variable{"system"}[$m{"system"}]{"fortran"} = "cell(i)%dx_min";
  $variable{"system"}[$m{"system"}]{"units"} = $lengthunit;
  $m{"system"}++;
  $variable{"system"}[$m{"system"}]{"name"} = "<cellvol>"; # cell centred cell volume
  $variable{"system"}[$m{"system"}]{"centring"} = "cell";
  $variable{"system"}[$m{"system"}]{"maxima"} = "cellvol[i]";
  $variable{"system"}[$m{"system"}]{"fortran"} = "cell(i)%vol";
  $variable{"system"}[$m{"system"}]{"units"} = "$lengthunit^3";
  foreach $l ( 1 .. 3 ) {
    $m{"system"}++;
    $variable{"system"}[$m{"system"}]{"name"} = "<facereflect[l=$l]>"; # either 1 (no reflect) or -1 (reflect), depending on whether face has a reflection in this direction
    $variable{"system"}[$m{"system"}]{"centring"} = "face";
    $variable{"system"}[$m{"system"}]{"maxima"} = "facereflect[j,$l]";
    $variable{"system"}[$m{"system"}]{"fortran"} = "facereflect(j=j,l=$l,error_string=error_string)";
  }
# TODO: fix implementation of these
  foreach $l ( 1 .. 3 ) {
    $m{"system"}++;
    $variable{"system"}[$m{"system"}]{"name"} = "<facetoicellr[l=$l]>"; # location of cell from lastface
    $variable{"system"}[$m{"system"}]{"centring"} = "cell";
    $variable{"system"}[$m{"system"}]{"maxima"} = "facetoicellr[j,$l,ns]";
# sum thing is CLUNKY, but can't see way around it with fortran
    $variable{"system"}[$m{"system"}]{"fortran"} = "sum(facetoicellr(j=j,l=$l,ns=ns,error_string=error_string),mask=array_mask$l)"; # function which protects against face(j)%r($l,ns) not being allocated
    $variable{"system"}[$m{"system"}]{"units"} = $lengthunit;
  }
  foreach $l ( 1 .. 3 ) {
    $m{"system"}++;
    $variable{"system"}[$m{"system"}]{"name"} = "<celltoicellr[l=$l]>"; # location of cell from lastcell
    $variable{"system"}[$m{"system"}]{"centring"} = "cell";
    $variable{"system"}[$m{"system"}]{"maxima"} = "celltoicellr[ilast,$l,ns]";
    $variable{"system"}[$m{"system"}]{"fortran"} = "sum(celltoicellr(i=ilast,l=$l,ns=ns,error_string=error_string),mask=array_mask$l)"; # function which protects against cell(i)%r($l,ns) not being allocated
    $variable{"system"}[$m{"system"}]{"units"} = $lengthunit;
  }
  foreach $l ( 1 .. 3 ) {
    $m{"system"}++;
    $variable{"system"}[$m{"system"}]{"name"} = "<celltoseparationicellr[l=$l]>"; # location of cell from lastcell when doing a separation loop
    $variable{"system"}[$m{"system"}]{"centring"} = "cell";
    $variable{"system"}[$m{"system"}]{"maxima"} = "celltoseparationicellr[i,$l]";
    $variable{"system"}[$m{"system"}]{"fortran"} = "sum(celltoseparationicellr(thread=thread,icurrent=i,l=$l,error_string=error_string),mask=array_mask$l)"; # actually uses iicurrent defined in someloop%separation_list, but checks that i and iicurrent are consistent
    $variable{"system"}[$m{"system"}]{"units"} = $lengthunit;
  }
  foreach $l ( 1 .. 3 ) {
    $m{"system"}++;
    $variable{"system"}[$m{"system"}]{"name"} = "<celltoseparationicellreflect[l=$l]>"; # reflect vector made up of 1 (not reflected relative to loop centre) and -1 (reflected) components
    $variable{"system"}[$m{"system"}]{"centring"} = "cell";
    $variable{"system"}[$m{"system"}]{"maxima"} = "celltoseparationicellreflect[$l]";
    $variable{"system"}[$m{"system"}]{"fortran"} = "celltoseparationicellreflect_multiplier(thread=thread,n=1,l=$l,error_string=error_string)" # this is based on latest separation loop and position within that loop, regardless of current cell
# TODO: in the future use n to specify the relative number of the separation loop, as per sepcentreN option somehow
  }
  $m{"system"}++;
  $variable{"system"}[$m{"system"}]{"name"} = "<celltoseparationicellrsquared>"; # distance between separation cell centre and current separation icell, squared
  $variable{"system"}[$m{"system"}]{"centring"} = "cell";
  $variable{"system"}[$m{"system"}]{"maxima"} = "celltoseparationicellrsquared";
  $variable{"system"}[$m{"system"}]{"fortran"} = "celltoseparationicellrsquared(thread=thread,n=1,error_string=error_string)";
  $m{"system"}++;
  $variable{"system"}[$m{"system"}]{"name"} = "<adjacentcellsignns>"; # this variable will be 1 or -1 or 0, depending on whether this cell is the upcell of the lastface (1) or downcell of the lastface (-1).  Based on ns, so needs to be used only when cycling through adjacent cells, but works out reflections across glued boundaries.
  $variable{"system"}[$m{"system"}]{"centring"} = "cell";
  $variable{"system"}[$m{"system"}]{"maxima"} = "adjacentcellsignns[ns]";
  $variable{"system"}[$m{"system"}]{"fortran"} = "adjacentcellsignns(ns)";

#------------------------------------
# ref: system variables face
# face geometry and index functions
  $m{"system"}++;
  $variable{"system"}[$m{"system"}]{"name"} = "<jface>"; # double precision representation of jface index
  $variable{"system"}[$m{"system"}]{"centring"} = "face";
  $variable{"system"}[$m{"system"}]{"maxima"} = "jface";
  $variable{"system"}[$m{"system"}]{"fortran"} = "dble(j)";
  foreach $l ( 1 .. 3 ) {
    $m{"system"}++;
    $variable{"system"}[$m{"system"}]{"name"} = "<facex[l=$l]>"; # face centred location vector
    $variable{"system"}[$m{"system"}]{"centring"} = "face";
    $variable{"system"}[$m{"system"}]{"maxima"} = "facex[j,$l]";
    $variable{"system"}[$m{"system"}]{"fortran"} = "face(j)%x($l)";
    $variable{"system"}[$m{"system"}]{"units"} = $lengthunit;
  }
  $m{"system"}++;
  $variable{"system"}[$m{"system"}]{"name"} = "<facedx>"; # face centred distance between cell centres
  $variable{"system"}[$m{"system"}]{"centring"} = "face";
  $variable{"system"}[$m{"system"}]{"maxima"} = "facedx[j]";
  $variable{"system"}[$m{"system"}]{"fortran"} = "face(j)%dx";
  $variable{"system"}[$m{"system"}]{"units"} = $lengthunit;
  foreach $l ( 1 .. 3 ) {
    $m{"system"}++;
    $variable{"system"}[$m{"system"}]{"name"} = "<facedxup[l=$l]>"; # face centred vector that goes from face centre to the upcell centre
    $variable{"system"}[$m{"system"}]{"centring"} = "face";
    $variable{"system"}[$m{"system"}]{"maxima"} = "facedxup[j,$l]";
    $variable{"system"}[$m{"system"}]{"fortran"} = "face(j)%r($l,2)";
    $variable{"system"}[$m{"system"}]{"units"} = $lengthunit;
  }
  foreach $l ( 1 .. 3 ) {
    $m{"system"}++;
    $variable{"system"}[$m{"system"}]{"name"} = "<facedxdown[l=$l]>"; # face centred vector that goes from face centre to the downcell centre
    $variable{"system"}[$m{"system"}]{"centring"} = "face";
    $variable{"system"}[$m{"system"}]{"maxima"} = "facedxdown[j,$l]";
    $variable{"system"}[$m{"system"}]{"fortran"} = "face(j)%r($l,1)";
    $variable{"system"}[$m{"system"}]{"units"} = $lengthunit;
  }
  $m{"system"}++;
  $variable{"system"}[$m{"system"}]{"name"} = "<facedxkernel>"; # distance that is characteristic of kernels for this face
  $variable{"system"}[$m{"system"}]{"centring"} = "face";
  $variable{"system"}[$m{"system"}]{"maxima"} = "facedxkernel[j]";
  $variable{"system"}[$m{"system"}]{"fortran"} = "face(j)%dx_kernel";
  $variable{"system"}[$m{"system"}]{"units"} = $lengthunit;
  $m{"system"}++;
  $variable{"system"}[$m{"system"}]{"name"} = "<facearea>"; # face centred face area
  $variable{"system"}[$m{"system"}]{"centring"} = "face";
  $variable{"system"}[$m{"system"}]{"maxima"} = "facearea[j]";
  $variable{"system"}[$m{"system"}]{"fortran"} = "face(j)%area";
  $variable{"system"}[$m{"system"}]{"units"} = "$lengthunit^2";
  foreach $l ( 1 .. 3 ) {
    $m{"system"}++;
    $variable{"system"}[$m{"system"}]{"name"} = "<facenorm[l=$l]>"; # face centred unit normal vector
    $variable{"system"}[$m{"system"}]{"centring"} = "face";
    $variable{"system"}[$m{"system"}]{"maxima"} = "facenorm[j,$l]";
    $variable{"system"}[$m{"system"}]{"fortran"} = "face(j)%norm($l,1)";
  }
  foreach $l ( 1 .. 3 ) {
    $m{"system"}++;
    $variable{"system"}[$m{"system"}]{"name"} = "<facedxunit[l=$l]>"; # face centred unit vector that goes from cell(idown) to cell(iup) 
    $variable{"system"}[$m{"system"}]{"centring"} = "face";
    $variable{"system"}[$m{"system"}]{"maxima"} = "facedxunit[j,$l]";
    $variable{"system"}[$m{"system"}]{"fortran"} = "face(j)%dx_unit($l)";
  }
  foreach $l ( 1 .. 3 ) {
    $m{"system"}++;
    $variable{"system"}[$m{"system"}]{"name"} = "<facetang1[l=$l]>"; # face centred unit tangent vector
    $variable{"system"}[$m{"system"}]{"centring"} = "face";
    $variable{"system"}[$m{"system"}]{"maxima"} = "facetang1[j,$l]";
    $variable{"system"}[$m{"system"}]{"fortran"} = "face(j)%norm($l,2)";
  }
  foreach $l ( 1 .. 3 ) {
    $m{"system"}++;
    $variable{"system"}[$m{"system"}]{"name"} = "<facetang2[l=$l]>"; # face centred unit tangent vector
    $variable{"system"}[$m{"system"}]{"centring"} = "face";
    $variable{"system"}[$m{"system"}]{"maxima"} = "facetang2[j,$l]";
    $variable{"system"}[$m{"system"}]{"fortran"} = "face(j)%norm($l,3)";
  }
  $m{"system"}++;
  $variable{"system"}[$m{"system"}]{"name"} = "<facefromcelldirection>"; # this is positive if the normal points outwards from the last cell
  $variable{"system"}[$m{"system"}]{"centring"} = "face";
  $variable{"system"}[$m{"system"}]{"maxima"} = "facefromcelldirection[i,j]";
# $variable{"system"}[$m{"system"}]{"fortran"} = "sign(1.d0,divop(i,j))"; # this doesn't work on a boundary cell, but the new function will
  $variable{"system"}[$m{"system"}]{"fortran"} = "facefromcelldirection(i,j)";
  $m{"system"}++;
  $variable{"system"}[$m{"system"}]{"name"} = "<facedivop>"; # this is positive if the normal points outwards from the last cell
  $variable{"system"}[$m{"system"}]{"centring"} = "face";
  $variable{"system"}[$m{"system"}]{"maxima"} = "facedivop[i,j]";
  $variable{"system"}[$m{"system"}]{"fortran"} = "divop(i,j)";
  $variable{"system"}[$m{"system"}]{"units"} = "$lengthunit^(-1)";
# <lastcell> seems to be a duplicate (negative) of <facefromcelldirection>, so remove
# $m{"system"}++;
# $variable{"system"}[$m{"system"}]{"name"} = "<lastcell>"; # this variable will be 1, -1 or 0, depending on whether the lastcell was the upcell, downcell or not a neighbour (respectively)
# $variable{"system"}[$m{"system"}]{"centring"} = "face";
# $variable{"system"}[$m{"system"}]{"maxima"} = "lastcell[i,j]";
# $variable{"system"}[$m{"system"}]{"fortran"} = "lastcell(i,j)";

#------------------------------------
# ref: system variables node
# node geometry and index functions
  $m{"system"}++;
  $variable{"system"}[$m{"system"}]{"name"} = "<knode>"; # double precision representation of knode index
  $variable{"system"}[$m{"system"}]{"centring"} = "node";
  $variable{"system"}[$m{"system"}]{"maxima"} = "knode";
  $variable{"system"}[$m{"system"}]{"fortran"} = "dble(k)";
  foreach $l ( 1 .. 3 ) {
    $m{"system"}++;
    $variable{"system"}[$m{"system"}]{"name"} = "<nodex[l=$l]>"; # node centred location vector
    $variable{"system"}[$m{"system"}]{"centring"} = "node";
    $variable{"system"}[$m{"system"}]{"maxima"} = "nodex[k,$l]";
    $variable{"system"}[$m{"system"}]{"fortran"} = "node(k)%x($l)";
    $variable{"system"}[$m{"system"}]{"units"} = $lengthunit;
  }
  $m{"system"}++;
  $variable{"system"}[$m{"system"}]{"name"} = "<nodedxkernel>"; # distance that is characteristic of kernels for this node
  $variable{"system"}[$m{"system"}]{"centring"} = "node";
  $variable{"system"}[$m{"system"}]{"maxima"} = "nodedxkernel[k]";
  $variable{"system"}[$m{"system"}]{"fortran"} = "node(k)%dx_kernel";
  $variable{"system"}[$m{"system"}]{"units"} = $lengthunit;

#------------------------------------
# ref: system variables kernels

# face centred kernels
  foreach $l ( 0 .. 6 ) {
# kernel 0 associated with face which gives average from surrounding cells
# kernel l=1-3 associated with face which gives derivative in lth coordinate direction from surrounding cells
# kernel 4 associated with face which gives derivative in face normal direction from surrounding cells
# kernel 5 associated with face which gives derivative in face tang1 direction from surrounding cells
# kernel 6 associated with face which gives derivative in face tang2 direction from surrounding cells
    $m{"system"}++;
    $variable{"system"}[$m{"system"}]{"name"} = "<facekernel[l=$l]>";
    $variable{"system"}[$m{"system"}]{"centring"} = "cell";
    $variable{"system"}[$m{"system"}]{"maxima"} = "facekernel[j,$l,ns]";
    $variable{"system"}[$m{"system"}]{"fortran"} = "face(j)%kernel($l)%v(ns)";
		if ($l eq 0) {
      $variable{"system"}[$m{"system"}]{"kernel_type"} = "faceave";
		} else {
      $variable{"system"}[$m{"system"}]{"kernel_type"} = "facegrad";
		}
			
  }

# cell centred kernels
  $m{"system"}++;
  $variable{"system"}[$m{"system"}]{"name"} = "<cellkernel[l=0]>"; # kernel 0 associated with cell which gives average from surrounding faces
  $variable{"system"}[$m{"system"}]{"centring"} = "face";
  $variable{"system"}[$m{"system"}]{"maxima"} = "cellkernel[i,0,ns]";
  $variable{"system"}[$m{"system"}]{"fortran"} = "cell(i)%kernel(0)%v(ns)";
  $variable{"system"}[$m{"system"}]{"kernel_type"} = "cellave";
  foreach $l ( 1 .. 3 ) {
    $m{"system"}++;
    $variable{"system"}[$m{"system"}]{"name"} = "<cellkernel[l=$l]>"; # kernel l=1-3 associated with cell which gives derivative in lth coordinate direction from surrounding cells
    $variable{"system"}[$m{"system"}]{"centring"} = "cell";
    $variable{"system"}[$m{"system"}]{"maxima"} = "cellkernel[ilast,$l,ns]"; # in all instances i will be used for kernel cell, and ilast will refer to central cell
    $variable{"system"}[$m{"system"}]{"fortran"} = "cell(ilast)%kernel($l)%v(ns)";
    $variable{"system"}[$m{"system"}]{"kernel_type"} = "cellgrad";
  }
  $m{"system"}++;
  $variable{"system"}[$m{"system"}]{"name"} = "<cellkernel[l=4]>"; # kernel 4 associated with cell which gives average from surrounding nodes
  $variable{"system"}[$m{"system"}]{"centring"} = "node";
  $variable{"system"}[$m{"system"}]{"maxima"} = "cellkernel[i,4,ns]";
  $variable{"system"}[$m{"system"}]{"fortran"} = "cell(i)%kernel(4)%v(ns)";
  $variable{"system"}[$m{"system"}]{"kernel_type"} = "cellfromnodeave";
  foreach $l ( 5 .. 7 ) {
    $m{"system"}++;
    $variable{"system"}[$m{"system"}]{"name"} = "<cellkernel[l=$l]>"; # kernel l=5-7 associated with cell which gives derivative in l-4th coordinate direction from surrounding nodes
    $variable{"system"}[$m{"system"}]{"centring"} = "node";
    $variable{"system"}[$m{"system"}]{"maxima"} = "cellkernel[i,$l,ns]"; # in all instances i will be used for kernel cell, and ilast will refer to central cell
    $variable{"system"}[$m{"system"}]{"fortran"} = "cell(i)%kernel($l)%v(ns)";
    $variable{"system"}[$m{"system"}]{"kernel_type"} = "cellfromnodegrad";
  }

# node centred kernels
  $m{"system"}++;
  $variable{"system"}[$m{"system"}]{"name"} = "<nodekernel[l=0]>"; # kernel 0 associated with node which gives average from surrounding cells
  $variable{"system"}[$m{"system"}]{"centring"} = "cell"; # this is centreing of kernel element, not of kernel
  $variable{"system"}[$m{"system"}]{"maxima"} = "nodekernel[k,0,ns]";
  $variable{"system"}[$m{"system"}]{"fortran"} = "node(k)%kernel(0)%v(ns)";
  $variable{"system"}[$m{"system"}]{"kernel_type"} = "nodeave";
  foreach $l ( 1 .. 3 ) {
    $m{"system"}++;
    $variable{"system"}[$m{"system"}]{"name"} = "<nodekernel[l=$l]>"; # kernel l=1-3 associated with cell which gives derivative in lth coordinate direction from surrounding cells
    $variable{"system"}[$m{"system"}]{"centring"} = "cell";
    $variable{"system"}[$m{"system"}]{"maxima"} = "nodekernel[k,$l,ns]"; # in all instances i will be used for kernel cell, and ilast will refer to central cell
    $variable{"system"}[$m{"system"}]{"fortran"} = "node(k)%kernel($l)%v(ns)";
    $variable{"system"}[$m{"system"}]{"kernel_type"} = "nodegrad";
  }

# some kernel diagnostics
  $m{"system"}++;
  $variable{"system"}[$m{"system"}]{"name"} = "<crosskernel>"; # used in limiting the gradient when being used in the advection routine
  $variable{"system"}[$m{"system"}]{"centring"} = "cell";
  $variable{"system"}[$m{"system"}]{"maxima"} = "crosskernel";
  $variable{"system"}[$m{"system"}]{"fortran"} = "cell(i)%cross_kernel";
  $m{"system"}++;
  $variable{"system"}[$m{"system"}]{"name"} = "<centralkernel>";
  $variable{"system"}[$m{"system"}]{"centring"} = "cell";
  $variable{"system"}[$m{"system"}]{"maxima"} = "centralkernel";
  $variable{"system"}[$m{"system"}]{"fortran"} = "cell(i)%central_kernel";
  $m{"system"}++;
  $variable{"system"}[$m{"system"}]{"name"} = "<kernelsum>"; # used in limiting the gradient when being used in the advection routine
  $variable{"system"}[$m{"system"}]{"centring"} = "cell";
  $variable{"system"}[$m{"system"}]{"maxima"} = "kernelsum";
  $variable{"system"}[$m{"system"}]{"fortran"} = "cell(i)%kernel_sum";
#------------------------------------
# ref: system variables
# miscelaneous system variables
  foreach $l1 ( 1 .. 3) {
    foreach $l2 ( 1 .. 3 ) {
      $m{"system"}++;
      $variable{"system"}[$m{"system"}]{"name"} = "<delta[l=$l1,$l2]>"; # coordinate direction delta function
      if ($l1 eq $l2) {
        $variable{"system"}[$m{"system"}]{"maxima"} = 1;
      } else {
        $variable{"system"}[$m{"system"}]{"maxima"} = 0;
      }
    }
  }
  $m{"system"}++;
  $variable{"system"}[$m{"system"}]{"name"} = "<limitertolerance>"; # coordinate direction delta function
  $variable{"system"}[$m{"system"}]{"maxima"} = "limitertolerance";
  $variable{"system"}[$m{"system"}]{"fortran"} = "limitertolerance";
  $m{"system"}++;
  $variable{"system"}[$m{"system"}]{"name"} = "<limitercontgrad>"; # coordinate direction delta function
  $variable{"system"}[$m{"system"}]{"maxima"} = "limitercontgrad";
  $variable{"system"}[$m{"system"}]{"fortran"} = "limitercontgrad";
  $m{"system"}++;
  $variable{"system"}[$m{"system"}]{"name"} = "<pi>"; # pi
  $variable{"system"}[$m{"system"}]{"maxima"} = "pi";
  $variable{"system"}[$m{"system"}]{"fortran"} = "pi";
  $m{"system"}++;
  $variable{"system"}[$m{"system"}]{"name"} = "<huge>"; # largest representable number
  $variable{"system"}[$m{"system"}]{"maxima"} = "huge";
  $variable{"system"}[$m{"system"}]{"fortran"} = "huge(0.d0)";
  $m{"system"}++;
  $variable{"system"}[$m{"system"}]{"name"} = "<tiny>"; # smallest representable number
  $variable{"system"}[$m{"system"}]{"maxima"} = "tiny";
  $variable{"system"}[$m{"system"}]{"fortran"} = "tiny(0.d0)";
  $m{"system"}++;
  $variable{"system"}[$m{"system"}]{"name"} = "<tinyish>"; # larger than tiny
  $variable{"system"}[$m{"system"}]{"maxima"} = "tinyish";
# $variable{"system"}[$m{"system"}]{"fortran"} = "(1.d2*sqrt(tiny(0.d0)))";
  $variable{"system"}[$m{"system"}]{"fortran"} = "tinyish"; # use variable for efficiency
  $m{"system"}++;
  $variable{"system"}[$m{"system"}]{"name"} = "<hugeish>"; # smaller than huge
  $variable{"system"}[$m{"system"}]{"maxima"} = "hugeish";
# $variable{"system"}[$m{"system"}]{"fortran"} = "(1.d-2*sqrt(huge(0.d0)))";
  $variable{"system"}[$m{"system"}]{"fortran"} = "hugeish";
  $m{"system"}++;
  $variable{"system"}[$m{"system"}]{"name"} = "<random>"; # a random number within the range 0 <= random < 1
  $variable{"system"}[$m{"system"}]{"maxima"} = "random";
  $variable{"system"}[$m{"system"}]{"fortran"} = "random()";
  $m{"system"}++;
  $variable{"system"}[$m{"system"}]{"name"} = "<newtstep>"; # double precision representation of newtstep
  $variable{"system"}[$m{"system"}]{"maxima"} = "newtstep";
  $variable{"system"}[$m{"system"}]{"fortran"} = "dble(newtstep)";
  $m{"system"}++;
  $variable{"system"}[$m{"system"}]{"name"} = "<timestep>"; # double precision representation of timestep
  $variable{"system"}[$m{"system"}]{"maxima"} = "timestep";
  $variable{"system"}[$m{"system"}]{"fortran"} = "dble(timestep)";
  $m{"system"}++;
  $variable{"system"}[$m{"system"}]{"name"} = "<newtres>"; # latest evalulation of the newton residual
  $variable{"system"}[$m{"system"}]{"maxima"} = "newtres";
  $variable{"system"}[$m{"system"}]{"fortran"} = "newtres";
  $m{"system"}++;
  $variable{"system"}[$m{"system"}]{"name"} = "<separation>"; # double precision representation of current separation
  $variable{"system"}[$m{"system"}]{"maxima"} = "separation";
  $variable{"system"}[$m{"system"}]{"fortran"} = "dble(someloop(thread)%separation_list(someloop(thread)%current_separation_list(1))%nseparation)";
  $m{"system"}++;
  $variable{"system"}[$m{"system"}]{"name"} = "<transientdelta>"; # 0 or 1 depending on whether simulation is transient or not
# actual values for transientdelta and newtientdelta have to be set once the input files have been read
  $variable{"system"}[$m{"system"}]{"maxima"} = 0;
  $m{"system"}++;
  $variable{"system"}[$m{"system"}]{"name"} = "<newtientdelta>"; # 0 or 1 depending on whether simulation is transient or not
  $variable{"system"}[$m{"system"}]{"maxima"} = 0;

#------------------------------------

  foreach $mvar ( 1 .. $m{"system"} ) {
# create mfortran names for the variable, which are what maxima produces from the f90 package
# as these are used to (reverse) search through the maxima output, only create these for variables that have a fortran name
    if (nonempty($variable{"system"}[$mvar]{"fortran"})) {
      $variable{"system"}[$mvar]{"mfortran"}=$variable{"system"}[$mvar]{"maxima"}; # only differ by braces used on indices
      $variable{"system"}[$mvar]{"mfortran"}=~s/\[/(/g;
      $variable{"system"}[$mvar]{"mfortran"}=~s/\]/)/g;
    }
    if (!($variable{"system"}[$mvar]{"units"})) {$variable{"system"}[$mvar]{"units"} = "1";}
    if (empty($variable{"system"}[$mvar]{"deriv"})) {$variable{"system"}[$mvar]{"deriv"} = 0;}
    if (empty($variable{"system"}[$mvar]{"hasderiv"})) {$variable{"system"}[$mvar]{"hasderiv"} = 0;}
    if (!($variable{"system"}[$mvar]{"multiplier"})) {$variable{"system"}[$mvar]{"multiplier"} = "1.d0";}
    if (!($variable{"system"}[$mvar]{"centring"})) {$variable{"system"}[$mvar]{"centring"} = "none";}
    if (!($variable{"system"}[$mvar]{"region"})) {
      if ($variable{"system"}[$mvar]{"centring"} eq "cell") {
        $variable{"system"}[$mvar]{"region"} = "<all cells>";
      } elsif ($variable{"system"}[$mvar]{"centring"} eq "face") {
        $variable{"system"}[$mvar]{"region"} = "<all faces>";
      } elsif ($variable{"system"}[$mvar]{"centring"} eq "node") {
        $variable{"system"}[$mvar]{"region"} = "<all nodes>";
      } else {
        $variable{"system"}[$mvar]{"region"} = "";
      }
    }
  }

}

#-------------------------------------------------------------------------------
# finds mvar number for system variable with name $_[0]
# if not found dies

sub get_system_mvar {

  use strict;
  my ($mvar);

  foreach $mvar ( 1 .. $m{"system"}+1 ) {
    if ($variable{"system"}[$mvar]{"name"} eq $_[0]) {exit;}
  }
  if ($mvar > $m{"system"}) {die "ERROR: system variable $_[0] not found in get_system_mvar\n";}

  return $mvar;

}

#-------------------------------------------------------------------------------
# takes a string which contains maxima names and replaces with user defined names
# this subroutine possibly not used

sub maxima_to_name {

  my $type;
  my $mvar;

  print DEBUG "in maxima_to_name: before _[0] = ",$_[0],"\n";

# substitute user names with maxima names
  foreach $type (@user_types,"system","empty") {
    foreach $mvar ( 1 .. $m{$type} ) {
      replace_substrings($_[0],$variable{$type}[$mvar]{"maxima"},$variable{$type}[$mvar]{"name"});
    }
  }

  print DEBUG "in maxima_to_name: after _[0] = ",$_[0],"\n";
  return $_[0];

}

#-------------------------------------------------------------------------------
# creates latexable summary files

sub write_latex {

  use strict;
  use Text::Balanced qw ( extract_bracketed );
  my ($type, $mvar);
  my ($type2, $mvar2);
  my ($tmp, $name, $sub, $sup, $part, $text, $texcodename, $texmultiplier, $textype);
  my ($pre, $mid, $post, $operator);

  print DEBUG "in write_latex";

  open(LATEX, ">$tmp_dir/variables.tex");

# create latex names for variables and write a tabular list to tmp.tex
  foreach $type (@user_types) {
    print LATEX "%Variable definition for variable type = $type\n";
    foreach $mvar ( 1 .. $m{$type} ) {
      $tmp = $variable{$type}[$mvar]{"name"};
      $tmp =~ s/^<//; $tmp =~ s/>$//;
      $variable{$type}[$mvar]{"latex"} = "";
#     print LATEX "tmp = $tmp\n";
      while ($tmp) {
        $tmp =~ /^(.+?)(\s+|$)(.*)/;
        $part = $1;
        $tmp = $3;
        if ($part =~ /\(.+\)/) {
          $variable{$type}[$mvar]{"latex"} = $variable{$type}[$mvar]{"latex"}.'\text{'.$part.'}';
        } else {
          $part =~ /^(.+?)((_|\^|$).*)/;
          $name = $1;
          $part = $2;
          $sub = "";
          $sup = "";
          while ($part) {
            if ($part =~ /^_(.*?)((_|\^|$).*)/) {
              if ($sub) {$sub = $sub.',';}
              $sub = $sub.$1;
              $part = $2;
            } elsif ($part =~ /^\^(.*?)((_|\^|$).*)/) {
              if ($sup) {$sup = $sup.',';}
              $sup = $sup.$1;
              $part = $2;
            }
          }
# hard code in a few exceptions
          if ($name eq 'tau') {$name = '\tau';}
          if ($name eq 'mu') {$name = '\mu';}
          if ($name eq 'nu') {$name = '\nu';}
          if ($name eq 'alpha') {$name = '\alpha';}
          if ($name eq 'beta') {$name = '\beta';}
          if ($name eq 'gamma') {$name = '\gamma';}
          if ($name eq 'delta') {$name = '\delta';}
          if ($name eq 'epsilon') {$name = '\epsilon';}
          if ($name eq 'phi') {$name = '\phi';}
          if ($name eq 'theta') {$name = '\theta';}
          if ($name eq 'sigma') {$name = '\sigma';}
          if ($name eq 'rho') {$name = '\rho';}
          if ($sub) {
            $variable{$type}[$mvar]{"latex"} = $variable{$type}[$mvar]{"latex"}.'\scal['.$sub.']{'.$name.'}';
          } else {
            $variable{$type}[$mvar]{"latex"} = $variable{$type}[$mvar]{"latex"}.'\scal{'.$name.'}';
          }
          if ($sup) { $variable{$type}[$mvar]{"latex"} = $variable{$type}[$mvar]{"latex"}.'^\text{'.$sup.'}'; }
        }
#       print LATEX "tmp = $tmp: part = $part\n";
      }
      $texcodename = $variable{$type}[$mvar]{"name"};
      $texcodename =~ s/_/\\_/g;
      $texcodename =~ s/\^/\\^/g;
      $texcodename =~ s/\{/\\{/g;
      $texcodename =~ s/\}/\\}/g;
      $texmultiplier = $variable{$type}[$mvar]{"multiplier"};
      $texmultiplier =~ s/(\d+)\.?d((\+|\-|\d)(\d*))/$1\\ee{$2}/g; # replace double precision floats with exponential
      $texmultiplier =~ s/(\d\.?)\\ee\{0\}/$1/g;
      if ($variable{$type}[$mvar]{"units"}) {
        print LATEX '$'.$variable{$type}[$mvar]{"latex"}.'$ & \units{'.$variable{$type}[$mvar]{"units"}.'} & '.
          $texmultiplier.' & \code{'.$texcodename."} & ".$variable{$type}[$mvar]{"comments"}." \\\\\n";
      } else {
        print LATEX '$'.$variable{$type}[$mvar]{"latex"}.'$ & & '.
          $texmultiplier.' &  \code{'.$texcodename."} & ".$variable{$type}[$mvar]{"comments"}." \\\\\n";
      }
    }
  }
  close(LATEX);

# create and write latex equations
  open(LATEX, ">$tmp_dir/equations.tex");

  foreach $type ("equation","derived","output") {
    $textype = $type;
    $textype =~ s/_/ /g;
    $textype =~ s/$/s/g; # plural
    $textype = "\U$textype";
    print LATEX "\n\\equationheading{$textype}\n";
    print LATEX "%Equation type = $type\n";
    foreach $mvar ( 1 .. $m{$type} ) {
      print LATEX "%Equation name = $variable{$type}[$mvar]{name}\n";
      print LATEX "%Equation = $variable{$type}[$mvar]{equation}\n";
      $tmp = $variable{$type}[$mvar]{"equation"};
      if ($type =~ /function$/) { $tmp = "$variable{$type}[$mvar]{name} = $tmp"; }
      else { $tmp = "$tmp = 0"; }
# swap div and grad names
      while ($tmp =~ /div\(|grad\(|\^\(|min\(|max\(|log\(/) {
        $pre = $`;
        $post = '('.$';
        $operator = $&;
        $operator =~ s/\($//;
        ($mid,$post) = extract_bracketed( $post, '()' ); # http://search.cpan.org/~adamk/Text-Balanced-2.02/lib/Text/Balanced.pm
        if ($operator eq '^') {
          $tmp = $pre.'^{'.$mid.'}'.$post;
        } elsif ($operator eq 'div') {
          $mid =~ s/^\(//; $mid =~ s/\)$//;
          $tmp = $pre.'\vect{\nabla} \cdot \left ('.$mid.' \right )'.$post;
#         $tmp = $pre.'\frac{d}{dx}\left ('.$mid.' \right )'.$post;
        } elsif ($operator eq 'grad') {
          $mid =~ s/^\(//; $mid =~ s/\)$//;
          $tmp = $pre.'\vect{\nabla} \left ('.$mid.' \right )'.$post;
#         $tmp = $pre.'\frac{d}{dx}\left ('.$mid.' \right )'.$post;
        } else {
          $mid =~ s/^\(//; $mid =~ s/\)$//;
          $tmp = $pre.'\\'.$operator.'\left ('.$mid.' \right )'.$post;
        }
      }
# remove *
      $tmp =~ s/\*\*/\^/g;
#     $tmp =~ s/>\*</></g; 
#     $tmp =~ s/\*/\\times/g; 
      $tmp =~ s/\*//g; 
      $tmp =~ s/(\d+)\.?d((\+|\-|\d)(\d*))/$1\\ee{$2}/g; # replace double precision floats with exponential
      $tmp =~ s/(\d\.?)\\ee\{0\}/$1/g;

      foreach $type2 ("constant","transient","newtient","unknown","derived") {
        foreach $mvar2 ( 1 .. $m{$type2} ) {
          $tmp =~ s/\Q$variable{$type2}[$mvar2]{"name"}/$variable{$type2}[$mvar2]{"latex"}/g;
        }
      }
      print LATEX "\\begin{equation}\n$tmp\n\\end{equation}\n";
    }
  }

  close(LATEX);

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
# regionname - can have both rindices and lindices (witness the kernel regions) so is exactly the same as name

# the standarised variable name obeys:
# if the variable is a scalar then no l index is given in the consistent name
# if the variable is for the current timestep then no r index is given in the consistent name
# eitherway the l index always preceeds the r index

sub examine_name {

  use strict;
  my ($name,$action,$compoundname,$basename,$nrank,$rank,$indices,$lindex,$rindex,$lindices);

  $action = $_[1];
  ($name) = $_[0] =~ /^<(.*)>$/;
  if (!($name)) { die "ERROR: an empty variable was passed to sub consistent_name: $_[0]\n"; }

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
  elsif ($action eq "regionname") { return ($name); }
}

#-------------------------------------------------------------------------------
# this subroutine replaces maxima variables in a string with ordered generic `g[?]' variables, avoiding shorter matches
# input:
# $_[0] = string to be have words replaced
# $_[1] = flag indicating either maxima output (=m) or fortran output (=f)
# output:
# $_[0] = string with words replaced
# returned value is an array of replacement values, with subscript being the g index-1 (i.e., starting from 0)

sub construct_generic_mequation{

  my $string = $_[0];
  my $output_flag = $_[1];
  my ($mtype, $mvar, $n);
  my @original = ();

  print DEBUG "++++++++++++++++++++++++++++++++++++++++\nconstruct_generic_mequation\n";
  print DEBUG "string = $string\n";

# create hash of words, variable number
  foreach $mtype ( keys(%variable)) {
    foreach $mvar ( 1 .. $#{$variable{$mtype}} ) {
      if (empty($variable{$mtype}[$mvar]{"maxima"})) { next; } # only go for replacement if variable is there
      if ($variable{$mtype}[$mvar]{"maxima"} !~ /^[a-zA-Z]/) { next; } # and not a number
      if (empty($variable{$mtype}[$mvar]{"mfortran"}) && $output_flag eq "f") { next; } # for fortran construction mfortran must also be defined (it won't be for numerical replacements for example)
#     print "mtype = $mtype: mvar = $mvar: maxima = $variable{$mtype}[$mvar]{maxima}\n";
      push(@original, { "word" => $variable{$mtype}[$mvar]{"maxima"},
                        "length" => length($variable{$mtype}[$mvar]{"maxima"}),
                        "mtype" => $mtype,
                        "mvar" => $mvar,
                        "g" => -1 } ); # if g >= 0 then indicates that variable appears in the string, and has name g["g"]
    }
  }

  @original = sort { $b->{"length"} <=> $a->{"length"} } @original;

# foreach $n ( 0 .. $#original ) {
#   print "n = $n: word = $original[$n]{word}: length = $original[$n]{length}\n";
# }

# now loop through string from start looking for replacements
  my $glast = -1;
  my $pre = "";
  my $post = $string;
  STRING_LOOP: while (nonempty($post)) { # note use of nonempty here which will stil be true even if $post holds a zero
    if ($post !~ /^[a-zA-Z]/) { $pre = $pre.substr($post,0,1); $post = substr($post,1); next; }
    foreach $n ( 0 .. $#original ) {
      if (substr($post,0,$original[$n]{"length"}) eq $original[$n]{"word"}) {
#       print "found replacement: word = $original[$n]{word}\n";
        if ($original[$n]{"g"} == -1) { $glast++; $original[$n]{"g"} = $glast; }
#       else { print "reusing this variable: g = $original[$n]{g}\n"; }
        $pre = $pre."g[".$original[$n]{"g"}."]";
        $post = substr($post,$original[$n]{"length"});
        next STRING_LOOP;
      }
    }
    $pre = $pre.substr($post,0,1); $post = substr($post,1); next; # advance character anyway
  }
      
# forming output array of nonzero g replacements to be used in deconstruction
  my @replacements = ();
  foreach $n ( 0 .. $#original ) {
    if ($original[$n]{"g"} != -1 ) {
      if ($output_flag eq "m") { # output maxima name as it will be contained in maxima output from simplify
        $replacements[$original[$n]{"g"}] = $original[$n]{"word"};
      } elsif ($output_flag eq "f") { # output mfortran name as it will be contained in maxima output from f90 output
        $replacements[$original[$n]{"g"}] = $variable{$original[$n]{"mtype"}}[$original[$n]{"mvar"}]{"mfortran"};
      }
    }
  }

# print "string = $string\n";
# print "post = $post\n";
# print "pre = $pre\n";

  $_[0] = $pre; # calling string is replaced by replacement string
  print DEBUG "string = $pre\n";
  if (@replacements) {
    print DEBUG "replacements = @replacements\n";
  } else {
    print DEBUG "no replacements found\n";
  }
  print DEBUG "----------------------------------------\n";

  if (@replacements) {return (@replacements);} # return array of replacements with index being g array index
}
#-------------------------------------------------------------------------------
# this subroutine reconstructs the actual maxima equation again
# input
# $_[0] = string containing g variables
# $_[1] = flag indicating either maxima output (=m) or fortran output (=f) was used
# $_[2..$#_] = array containing replacements
# output
# $_[0] = reconstructed mequation

sub deconstruct_generic_mequation{

  my $string = $_[0];
  my $output_flag = $_[1];
  my @replacements = ();
  my ($gvar, $n);

  print DEBUG "++++++++++++++++++++++++++++++++++++++++\ndeconstruct_generic_mequation\n";
  print DEBUG "before: string = $string\n";

  if ($#_ > 1) {
    foreach $n ( 2 .. $#_ ) {
      if ($output_flag eq "m") { # output maxima name as it will be contained in maxima output from simplify
        $gvar = 'g['.scalar($n-2).']';
      } elsif ($output_flag eq "f") { # output mfortran name as it will be contained in maxima output from f90 output
        $gvar = 'g('.scalar($n-2).')';
      }
      $string =~ s/([^a-zA-Z]|^)\Q$gvar/$1$_[$n]/g; # should match g vars that are in isolation only - ie, not match g[1] with bang[1]
    }
  }
  
  $_[0] = $string;
  print DEBUG "after: string = $string\n";
  print DEBUG "----------------------------------------\n";
}
#-------------------------------------------------------------------------------
# write out maxima results for subsequent runs

sub write_maxima_results_files {

  use strict;
  use Storable qw(store); # routines for storing data
  my ($n, $dump);

  print DEBUG "++++++++++++++++++++++++++++++++++++\nwrite_maxima_results_files\n";

# maxima_simplify_results:
  print DEBUG "maxima_simplify_results:\n";

# remove any elements that were not used this time: first sort, then strip off leading unused entries
  @maxima_simplify_results = sort { $a->{"used"} <=> $b->{"used"} } @maxima_simplify_results;
  while ( $#maxima_simplify_results >= 0 ) {
    if ($maxima_simplify_results[0]{"used"} == 0) {
      print DEBUG "not including: $maxima_simplify_results[0]{input} -> $maxima_simplify_results[0]{output}\n";
      shift(@maxima_simplify_results);
    } else { last; } 
  }
    
# sort by length of string so that shortest strings are matched first
  @maxima_simplify_results = sort { length($a->{"input"}) <=> length($b->{"input"}) } @maxima_simplify_results;

  foreach $n ( 0 .. $#maxima_simplify_results ) {
    print DEBUG "including: $maxima_simplify_results[$n]{input} -> $maxima_simplify_results[$n]{output}\n";
  }

  if (nonempty(eval{store(\@maxima_simplify_results, "$build_dir/last_maxima_simplify_results")})) {
    print "INFO: written maxima_simplify_results to file for possible reuse\n";
  } else {
    print "WARNING: unable to write maxima_simplify_results to file for possible reuse\n";
  }
  
# maxima_fortran_results:
  print DEBUG "maxima_fortran_results:\n";

# remove any elements that were not used this time: first sort, then strip off leading unused entries
  @maxima_fortran_results = sort { $a->{"used"} <=> $b->{"used"} } @maxima_fortran_results;
  while ( $#maxima_fortran_results >= 0 ) {
    if ($maxima_fortran_results[0]{"used"} == 0) {
      print DEBUG "not including: $maxima_fortran_results[0]{input} -> $maxima_fortran_results[0]{output}\n";
      shift(@maxima_fortran_results);
    } else { last; } 
  }
    
# sort by length of string so that shortest strings are matched first
  @maxima_fortran_results = sort { length($a->{"input"}) <=> length($b->{"input"}) } @maxima_fortran_results;

  foreach $n ( 0 .. $#maxima_fortran_results ) {
    print DEBUG "including: $maxima_fortran_results[$n]{input} -> $maxima_fortran_results[$n]{output}\n";
  }

  if (nonempty(eval{store(\@maxima_fortran_results, "$build_dir/last_maxima_fortran_results")})) {
    print "INFO: written maxima_fortran_results to file for possible reuse\n";
  } else {
    print "WARNING: unable to write maxima_fortran_results to file for possible reuse\n";
  }
  
  print DEBUG "------------------------------------\n";

}

#-------------------------------------------------------------------------------
sub read_maxima_results_files {

  use strict;
  use Storable qw(retrieve); # routines for storing data
  my ($dump, $n);

  print DEBUG "++++++++++++++++++++++++++++++++++++\nread_maxima_results_files\n";

  $dump = eval{retrieve("$build_dir/last_maxima_simplify_results")};
  if ( nonempty($dump) ) {
    print "INFO: read maxima_simplify_results from file for possible reuse\n";
    print DEBUG "INFO: read maxima_simplify_results from file for possible reuse\n";
    @maxima_simplify_results = @$dump;
    foreach $n ( 0 .. $#maxima_simplify_results ) {
      print DEBUG "$maxima_simplify_results[$n]{input} -> $maxima_simplify_results[$n]{output}\n";
      $maxima_simplify_results[$n]{"used"} = 0; # mark as not used this time yet
    }
  } else {
    print "INFO: unable to read maxima_simplify_results from file for possible reuse\n";
    print DEBUG "INFO: unable to read maxima_simplify_results from file for possible reuse\n";
  }

  $dump = eval{retrieve("$build_dir/last_maxima_fortran_results")};
  if ( nonempty($dump) ) {
    print "INFO: read maxima_fortran_results from file for possible reuse\n";
    print DEBUG "INFO: read maxima_fortran_results from file for possible reuse\n";
    @maxima_fortran_results = @$dump;
    foreach $n ( 0 .. $#maxima_fortran_results ) {
      print DEBUG "$maxima_fortran_results[$n]{input} -> $maxima_fortran_results[$n]{output}\n";
      $maxima_fortran_results[$n]{"used"} = 0; # mark as not used this time yet
    }
  } else {
    print "INFO: unable to read maxima_fortran_results from file for possible reuse\n";
    print DEBUG "INFO: unable to read maxima_fortran_results from file for possible reuse\n";
  }

  print DEBUG "------------------------------------\n";
}
#-------------------------------------------------------------------------------
# whatever string is passed to this routine is output as an error message to both screen
#  and DEBUG file, and the script then dies

sub error_stop {

  print "ERROR: $_[0]\n";
  print DEBUG "ERROR: $_[0]\n";
  die;

}
#-------------------------------------------------------------------------------
# little subroutine to extract the first string entry from a line of text, possibly removing any delimiters at the same time
# input
# $_[0] = string
# output
# first return = string removed from the front, dedelimited
# $_[0] = remainder of string, with leading spaces removed
# $_[1] = error flag (0 or 1)

sub extract_first {
  use strict;
  my $remainder=$_[0];
  my $input=$_[0];
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
# return the string and remainder
  $_[0]=$remainder;
  $_[1]=$error;
  return $string;
}

#-------------------------------------------------------------------------------
sub extract_replacements {
# extracts a search and replace pair from a
# on input
# $_[0] = line of text
# $_[1] = file
# $_[2] = oline
# exit with two strings and a flag
#  ( search, replace, cancel )
# $_[3] = cancel = 0,1, indicates whether the CANCEL "string" was used
# $_[4] = default = 0,1, indicates whether the REPLACE* or R* or DEFAULT "string" was used, which indicates that string replacement is only set if it isn't set already
# if search is empty then no string was found

  use strict;
  my $line = $_[0];
  my $file = $_[1];
  my $oline = $_[2];
  my $error = 0;
  my $search = '';
  my $replace = '';
  my $cancel = 0;
  my $default = 0;

  if ($line =~ /^((R|REPLACE)|(R\*|REPLACE\*|DEFAULT|D))\s+/i) { # found a replacement
    print DEBUG "found a replace statement specified as $1: $'\n";
    if (nonempty($3)) { $default=1; }
    $line = $';
    $search = extract_first($line,$error);
    if (!($search) || $error) {
      print "WARNING: possible replacement sequence skipped as search string not identified from:\nfile = $file: line = $oline\n";
      $search = '';
    } else {
      $line =~s/^(W|WITH)\s+//i;
      $replace = extract_first($line,$error);
      if ($error) { print "WARNING: possible replacement sequence skipped as replace string not identified from:\nfile = $file: line = $oline\n"; $search = ''; }
    }
    print DEBUG "search = $search: replace = $replace: default = $default\n";
  } elsif ($line =~ /^(C|CANCEL)\s+/i) { # found a string to cancel
    print DEBUG "found a cancel statement: $'\n";
    $line = $';
    $search = extract_first($line,$error);
    if (!($search) || $error) {
      print "WARNING: possible cancel sequence skipped as search string not identified from:\nfile = $file: line = $oline\n";
      $search = '';
    } else {
      $cancel = 1;
    }
  } else {
    error_stop("there is some sort of syntax error in the following replace statements:\nfile = $file: line = $oline");
  }

  $_[0] = $line;
  return ($search,$replace,$cancel,$default);

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
  
  open (EXTERNAL, "<$working_dir/$filename") or error_stop("Could not find external file $filename");
  
  my ($name) = $filename =~ /(.*)\.(f90|f|for)/;
  push(@externals,{name => $name, preamble => '', contents => '', setup => '', used => 0}); # push a new hash onto this array
  print DEBUG "EXTERNAL: found new external file: name = $name: filename = $filename\n";
  
  while($line=<EXTERNAL>) {
  	chompm($line);
    if ($line =~ /^\s*arb_external_(\S+)($|\s)/) {
      if ($1 eq 'preamble' || $1 eq 'setup' || $1 eq 'contents') {
        $current = $1;
      } elsif ($1 eq 'operator') {
        if ($line =~ /^\s*arb_external_operator\s+(\S+)($|\s)/) { # also form a list of the operators that are within this file
          print DEBUG "EXTERNAL: found operator $1\n";
          push(@{$externals[$#externals]{"operators"}},$1);
          $external_operators{$1}=$#externals;
        } else {
          error_stop("missing arb_external_operator name in external file $filename");
        }
      } else {
        error_stop("unknown arb_external_$1 statement in external file $filename");
      }
  	} elsif (nonempty($current)) {
  		$externals[$#externals]{$current} = $externals[$#externals]{$current}."\n".$line;
  	}
  }
  close(EXTERNAL);
  print DEBUG "EXTERNAL: file $filename contains the following operators: @{$externals[$#externals]{operators}}\n";
  print "INFO: external file $filename contains the following operators: @{$externals[$#externals]{operators}}\n";
# print DEBUG "EXTERNAL PREAMBLE:\n".$externals[$#externals]{preamble}."\n";
# print DEBUG "EXTERNAL SETUP:\n".$externals[$#externals]{setup}."\n";
# print DEBUG "EXTERNAL CONTENTS:\n".$externals[$#externals]{contents}."\n";

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
# based on passed variable, set or unset transient simulation status, including comment strings

sub set_transient_simulation {

  $transient = $_[0];
  for my $n ( 0 .. $#general_replacements ) {
    if ($general_replacements[$n]{"search"} eq "<<steadystatecomment>>") {
      if ($transient) { $general_replacements[$n]{"replace"} = "#" } else { $general_replacements[$n]{"replace"} = "" }
    }
    if ($general_replacements[$n]{"search"} eq "<<transientcomment>>") {
      if ($transient) { $general_replacements[$n]{"replace"} = "" } else { $general_replacements[$n]{"replace"} = "#" }
    }
  }

}

#-------------------------------------------------------------------------------
# search through general_replacements for search string

sub search_general_replacements {

  my $search = $_[0]; # on input, search string
  my $nfound = -1; # on output returns -1 if not found, or general_replacments index if found

  for my $n ( 0 .. $#general_replacements ) {
    if ($search eq $general_replacements[$n]{"search"}) { # found existing general replacements
      $nfound = $n;
      last;
    }
  }

  return $nfound;

}
#-------------------------------------------------------------------------------
# perl tips 
#         ($cunits,$line) = $line =~ /^\[(.*?)\]\s*(.*)$/;  #\s is a space, \S is a nonspace, . is a single wildcard, + is 1 or more times, ? is non greedy
#  @something gives the number of elements in an array when used in an scalar context - ie, so (@something) is true if the something array contains atleast one element
#  $#something gives the last array number in an array, so varies from -1 upwards (importantly can't be used as a test of whether the array has any elements)
# @{$variable{"compound"}[$mvar2]{component}}

