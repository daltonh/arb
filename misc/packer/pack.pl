#!/usr/bin/env perl -w
# file misc/packer/pack.pl
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
# perl script to pack up the bits of arb into one tar file ready for transport or achive
# daltonh, 300610

# requires: perl, rsync (>=2.6.7), tar, gzip

use strict;
use Cwd;
#use File::Basename;
use File::Glob ':glob'; # deals with whitespace better
my ($archive_name, $version, $day, $month, $date, $argument, $directory, $archive_root, $systemcall);

print "pack perl script for packing components of arb into self-contained tar archive\n";

# set default options
my $tar = 1; # by default tar up files
my $unpack = 0; # by default do not unpack again
my $example_inputs = 0; # by default input files contained in archive are those already there, rather than the example ones
my $example_directory = "examples/heat_conduction_around_ellipse";
my $cwd = cwd(); # http://perldoc.perl.org/Cwd.html

# create a list of first level directories that must be created regardless of existing contents
my @directories=( 'src','doc','build','output','tmp','examples','misc','templates' );

# do sanity check that the directories exist, indicating that we are in the root directory
foreach $directory (@directories) {
  if (!(-d $directory)) {
    print "ERROR: the required component directory $directory is not found at this location.\n".
          "Are you sure you are calling pack from the simulation root directory (which contains the directory src for example)\n".
          "For more info on usage of pack try \"pack --help\"\n";
  }
}

my $include_file = "misc/packer/pack.include";
if (!(-e $include_file)) { die "ERROR: include file $include_file not found\n"; }

# these are includes which are additional to the pack.include list
my $includes = "";

# by default all arb, geo and msh files in the root archive are also included, although will be scrapped using -d or -ni
$includes = $includes.' --include="*.in" --include="*.arb" --include "*.geo" --include "*.msh"';

# read through all command line arguments
foreach $argument ( @ARGV )  # first loop looks for distributable files and help request
{
  if ( $argument eq '-h' || $argument eq '--help' ) {
    usage();
  } elsif ( $argument eq '-d' || $argument eq '--distribute') {
    print "INFO: no contributed files included (default anyway), example problem setup in root directory and version recorded to licence/version.txt file\n";
    $includes = "";
    $example_inputs = 1;
  } elsif ( $argument eq '-ni' || $argument eq '--no-input') {
    print "INFO: no working directory input files included\n";
    $includes = "";
  }
}
  
# and again
foreach $argument ( @ARGV )  # second loop looks for other options
{
# print "$argument\n";
  if ( $argument eq '-a' || $argument eq '--all') {
    print "INFO: all example, misc and linear solver files will be included in archive\n";
    $includes = $includes.' --include="examples/***" --include="misc/***" --include="src/" --include="src/contributed/***"';
  } elsif ( $argument eq '-b' || $argument eq '--build') {
    print "INFO: all build files will be included in archive (both setup and compile working files)\n";
    $includes = $includes.' --include="build/***"';
  } elsif ( $argument eq '-m' || $argument eq '--misc') {
    print "INFO: all misc files will be included in archive\n";
    $includes = $includes.' --include="misc/***"';
  } elsif ( $argument eq '-e' || $argument eq '--examples') {
    print "INFO: all example input files will be included in archive\n";
    $includes = $includes.' --include="examples/***"';
  } elsif ( $argument eq '-c' || $argument eq '--contributed') {
    print "INFO: all contributed files will be included in archive\n";
    $includes = $includes.' --include="src/" --include="src/contributed/***"';
  } elsif ( $argument eq '-s' || $argument eq '--setup' || $argument eq '--setup_files') {
    print "INFO: all setup related working files will be included in the archive\n";
    $includes = $includes.' --include="build/" --include="build/equation_module.f90" --include="build/last_maxima*" '.
      '--include="build/last_setup*"';
  } elsif ( $argument eq '-o' || $argument eq '--output') {
    print "INFO: all output files will be included in the archive\n";
    $includes = $includes.' --include="output/" --include="output/output*.*"';
  } elsif ( $argument eq '-nt' || $argument eq '--no-tar' ) {
    print "INFO: archive will not be tarred\n";
    $tar = 0;
  } elsif ( $argument eq '-u' || $argument eq '--unpack' ) {
    print "INFO: archive will be unpacked\n";
    $tar = 0;
    $unpack = 1;
  } elsif ( $argument !~ /^\-/ ) {
    print "INFO: setting archive directory to $argument\n";
    $archive_name = $argument;
  }
}

# the version number is stored in licence/version, and is either 'master' or the full version number, or from a git version, may not even exist
my $dated_version = 0;
if (-e "licence/version") {
  open (VERSION_FILE, "<licence/version");
  $dated_version = <VERSION_FILE>;
  chomp($dated_version);
  close (VERSION_FILE);
}
if (!($dated_version) || $dated_version =~ /^master$/) { 
  print "INFO: as this is an un-distributed code version, a new dated version number will be created for the packed version\n";
  open(SRC, "<src/general_module.f90") or die "ERROR: problem opening src/general_module.f90 to find version number\n";
  while (<SRC>) {
    if (/version\s*=\s*(\S+)/) {
      $version = $1;
      last;
    }
  }
  if (!($version)) { die "ERROR: version number not found in src/general_module.f90\n"; }
# year month day hour min sec
  $date=sprintf("%04d%02d%02d%02d%02d%02d",(localtime)[5]+1900,(localtime)[4]+1,(localtime)[3],(localtime)[2],(localtime)[1],(localtime)[0]);
  $dated_version="v".$version."_".$date;
# now also check whether this is a git branch and if so, and not master, append branch name to generated backup name
  $systemcall="git rev-parse --abbrev-ref HEAD";  
  my $git_branch='';
# print "systemcall = ",system("$systemcall >/dev/null 2>/dev/null"),"\n";
  if (!(system("$systemcall >/dev/null 2>/dev/null"))) { print "INFO: finding branch name using git\n"; $git_branch=`$systemcall`; chompm($git_branch); print "INFO: found git_branch = $git_branch\n"; } else { print "INFO: git not found\n"; }
  if ($git_branch && $git_branch ne "master") {$dated_version=$dated_version."_".$git_branch;}
  print "INFO: dated_version = $dated_version\n";
}
    
if (!($archive_name)) { $archive_name = "arb_".$dated_version; }
# remove archive_name that is empty
if (-d $archive_name) {rmdir $archive_name;}
# create new archive_name
if (-e $archive_name) {
  die "ERROR: $archive_name already exists: choose another name or delete this file/directory\n";
} else {
  mkdir $archive_name;
  print "INFO: created directory $archive_name\n";
  $archive_root = $archive_name.'/archive/';
  mkdir $archive_root;
}
  
# create required directories
foreach $directory (@directories) {
  mkdir $archive_root.$directory;
}
  
# now rsync over necessary files and subdirectories into archive_root
print "INFO: creating archive of files\n";
# basic files
$systemcall="rsync -auR --include-from=$include_file * $archive_root";  
(!(system($systemcall))) or die "ERROR: could not perform $systemcall\n";
# any extra files to include
if ($includes) {
  $systemcall="rsync -auR --exclude=\"recent\" --exclude=\"old\"$includes --exclude=\"*\" * $archive_root";  
  (!(system($systemcall))) or die "ERROR: could not perform $systemcall\n";
}

# record pack info to licence/packer_history.txt file within the archive
$systemcall='echo "PACK operation completed on `hostname -f` by `whoami` on `date` to archive name '.$archive_name.
  " with options @ARGV\" >>".$archive_root."licence/packer_history.txt";
(!(system($systemcall))) or die "ERROR: could not perform $systemcall\n";

# if we are creating an archive from the master version, also record version number in the licence directory
$systemcall="echo ".$dated_version." >".$archive_root."licence/version";
(!(system($systemcall))) or die "ERROR: could not perform $systemcall\n";

# if distribute, include the tutorial example files in working directory
if ($example_inputs) {
  print "INFO: placing example input files\n";
  chdir "$archive_root" or die "ERROR: could not move into $archive_root\n";
  $systemcall="cp $example_directory/*.arb .";
  (!(system($systemcall))) or die "ERROR: could not perform $systemcall\n";
  $systemcall="cp $example_directory/*.msh .";
  (!(system($systemcall))) or die "ERROR: could not perform $systemcall\n";
  $systemcall="cp $example_directory/*.geo .";
  (!(system($systemcall))) or die "ERROR: could not perform $systemcall\n";
# chdir "../.." or die "ERROR: could not move into back from $archive_root\n";
  chdir "$cwd" or die "ERROR: could not move into back to $cwd from $archive_root\n";
}

if ($tar) {
  print "INFO: creating tar archive file\n";
  chdir $archive_root;
# $systemcall="tar -c -f ../archive.tar *"; # tar
  $systemcall="tar -cz -f ../archive.tar.gz *"; # tar and compress
  (!(system($systemcall))) or die "ERROR: could not perform $systemcall\n";
  chdir "..";
  $systemcall="rm -r archive";
  (!(system($systemcall))) or die "ERROR: could not perform $systemcall\n";
  chdir "$cwd" or die "ERROR: could not move into back to $cwd from $archive_name\n";
}

$systemcall="cp misc/packer/unpack $archive_name";
(!(system($systemcall))) or die "ERROR: could not perform $systemcall\n";
$systemcall="cp misc/packer/unpack_readme $archive_name/readme";
(!(system($systemcall))) or die "ERROR: could not perform $systemcall\n";
$systemcall="cp licence/arb_licence.txt $archive_name/licence";
(!(system($systemcall))) or die "ERROR: could not perform $systemcall\n";
$systemcall="echo $dated_version >$archive_name/version";
(!(system($systemcall))) or die "ERROR: could not perform $systemcall\n";

if ($unpack) {
  print "INFO: unpacking archive\n";
  chdir "$archive_name" or die "ERROR: could not move into $archive_name\n";
  $systemcall="./unpack";
  (!(system($systemcall))) or die "ERROR: could not perform $systemcall\n";
  chdir "$cwd" or die "ERROR: could not move back to $cwd from $archive_name\n";
}

print "INFO: success\n";

exit;

#*******************************************************************************
# usage function

sub usage {
  print "\nusage:\n\n".
        "pack [options] <archive_name>\n\n".
        "where <archive_name> is the name of the archive directory to be created,".
        "which must not already exist\n\n".
        "options include:\n".
        " -h or --help = produce this message\n".
        " -e or --examples = include all files within the examples directory (in addition to the defaults)\n".
        " -c or --contributed = include all files within the src/contributed directory (in addition to the defaults, that is, includes possibly non-GPLed files)\n".
        " -b or --build = include all files within the build directory\n".
        " -m or --misc = include all files within the misc directory (in addition to the defaults)\n".
        " -a or --all = equivalent to --examples --misc --contributed\n".
        " -s or --setup = setup related working files in the build directory will be included\n".
        " -o or --output = output files in the output directory required for a restart will be included (that is, output*.*)\n".
        " -ni or --no-input = arb, geo and msh input files within working directory will not be copied over (default is to copy these files with the archive)\n".
        " -d or --distribute = implies -ni, plus example input files will be placed in the working directory and version number recorded\n".
        " -nt or --no-tar = don't tar up the archive\n".
        " -u or --unpack = unpack straight away, ready to run (implies -nt)\n\n".
        "pack must be called from the root directory of a simulation\n";
  exit;
}

#*******************************************************************************
# chomp and remove mac linefeads too if present

sub chompm {
  use strict;
  chomp($_[0]);  # remove linefeed from end
  $_[0]=~tr/\r//d; # remove control m from mac files
}

#*******************************************************************************
