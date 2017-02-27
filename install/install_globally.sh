#!/bin/bash
#
# file install_globally.sh
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
#
# this little script does any contributed software downloads and installs arb so that it can be run from outside the arb_dir
#
# see usage function below
#
# exit status:
# 0: successful
# 1: failed
# 
#-------------------------------------------------------------------------------
# usage function
function usage () {
  echo "install_globally prepares arb for global usage";
  echo 
  echo "HELP/USAGE: ./install_globally [options]";
  echo
  echo "Possible options:";
  echo "  --help|-h: display this HELP INFO";
  echo;
  exit 1;
}
#-------------------------------------------------------------------------------
# function to dereference symlinks and find the real path of a file
# call with the (possibly symlinked) file
# result is returned in (global) variable real_path
function resolve_real_path {
  real_path=$1;
  next_path=`readlink $real_path`;
  while [ -n "$next_path" ] ; do
    real_path=$next_path;
    next_path=`readlink $real_path`;
  done
  unset next_path;
}
#-------------------------------------------------------------------------------
# main script:

sub_dir='';

#---------------------------
# loop through options and update any variables
until [ -z "$1" ];
do
  case $1 in
    "--help"|"-h") usage;;
    *) echo "ERROR: unknown command line entry $1"; usage;;
  esac
  shift;
done ;

#---------------------------
# now (re)set variables that depend on the user-specified options

# get the real path to the script with any symlinks resolved/dereferenced
resolve_real_path "${BASH_SOURCE[0]}";
install_script=$real_path; # the result of resolve_real_path is held in this global variable
# from http://stackoverflow.com/questions/4774054/reliable-way-for-a-bash-script-to-get-the-full-path-to-itself
install_dir="$(cd "$(dirname "$install_script")" && pwd -P)"; # directory that holds this arb script, which determines...
arb_dir="${install_dir%"/install"}"; # arb root directory, known as arb_dir, with no trailing slash

# now make any contributed routines
# TODO: generalise this to other contributed packages
(
  cd "$arb_dir/src/contributed/suitesparse";
  echo "INFO: making contributed package in `pwd`";
  make;
)

# now detect shell

exit 0
# done
#-------------------------------------------------------------------------------
