#!/bin/bash

# script installs just the packages that are required for arb
# this file is specific to redhat, tested on Red Hat Enterprise Linux Server 7.4 (Maipo) as of 13/9/17
# run as sudo

# WARNING: this script not tested extensively as we generally develop on ubuntu

# ref use hostnamectl and grep on red hat to check correct OS

# os_type=`lsb_release -si`;
# if [ $os_type != "Ubuntu" ]; then
#   "ERROR: script must be run on an Ubuntu system"
#   exit 1
# fi
# ubuntu_version=`lsb_release -sr`;
# echo "INFO: ubuntu_version = $ubuntu_version"

# basic arb dependencies
yum install maxima
yum install valgrind
yum install lapack-devel
yum install suitesparse-devel suiteparse-static
yum install perl-Data-Alias

# red hat doesn't have these libraries, so need to copy from ... ??
# cp /<matplotlibsource>/backend_wx*.py /usr/lib64/python2.7/site-packages/matplotlib/backends
# I downloaded the source from https://sourceforge.net/projects/matplotlib/files/. And only copied the backend py files.

# pre/post-processing - gmsh - more recent binaries are available which are probably worth using instead of the ubuntu packaged version
#yum install gmsh
echo "INFO: no longer installing the ubuntu packaged version of gmsh, but you can easily do this via 'sudo yum install gmsh'.  The better alternative is to download a binary directly from the gmsh website."
# pre/post-processing - paraview
yum install paraview

# for Lachlan's track and plot scripts
yum install python-pip python-devel python-matplotlib
yum install wxPython wxPython-devel
pip install --upgrade numpy
pip install --upgrade pandas
pip install --upgrade numexpr
pip install cycler

# for Christian's rxntoarb script
yum install ruby

echo "INFO: you can now add the arb/bin directory to your path and make alternative linear solvers using the include_globally.sh script";
