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
  echo "  --path-only|-p: only set the path in the user's shell config file";
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

# set default options
setpath=1; # set path within user's shell config file
makesuitesparse=1; # download suitesparse libraries
#---------------------------
# loop through options
until [ -z "$1" ];
do
  case $1 in
    "--help"|"-h") usage;;
    "--path-only"|"-p") setpath=1; makesuitesparse=0;;
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
# TODO: generalise this to other contributed packages, and add option to not do this make operation if we are linking to a arb_dir owned by another owner (eg root, installed for all users)
if [ $makesuitesparse -eq 1 ] ; then
  (
    cd "$arb_dir/src/contributed/suitesparse";
    echo "INFO: making contributed package in `pwd`";
    make;
  )
fi;

# now detect shell type as either sh (sh, bash) or csh (csh, tcsh)
if [ $setpath -eq 1 ] ; then
  shelltype=`basename ${SHELL}`;
  echo "INFO: shell identified as $shelltype type";
  if [ $shelltype == "tcsh" -o $shelltype == "csh" ] ; then
    echo "INFO: csh type shell: adding config to the end of ~/.${shelltype}rc";
    echo "# arb path set by install_globally.sh script" >> ~/.${shelltype}rc
    echo "setenv ARB_DIR \"${arb_dir}\"" >> ~/.${shelltype}rc
    echo 'setenv PATH "${PATH}:${ARB_DIR}/bin"' >> ~/.${shelltype}rc
  else
    echo "INFO: sh type shell: adding config to the end of ~/.${shelltype}rc";
    echo "# arb path set by install_globally.sh script" >> ~/.${shelltype}rc
    echo "export ARB_DIR=\"${arb_dir}\"" >> ~/.${shelltype}rc
    echo 'export PATH="${PATH}:${ARB_DIR}/bin"' >> ~/.${shelltype}rc
  fi;
fi;

exit 0
# done
#-------------------------------------------------------------------------------
