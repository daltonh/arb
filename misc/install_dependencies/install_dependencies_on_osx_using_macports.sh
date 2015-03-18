#!/bin/bash

# this script just installs the macports packages that are required for arb
# this file is specific to osx
# run as sudo, as in 'sudo ./install_dependencies_on_osx_using_macports.sh'

# as well as running this file you will also have to:
# 1) install Xcode with the command line utilities
# 2) install Xquartz X11 server (I think)
# 3) install gmsh using their binaries

# other things you might like to do:
# get syntax highlighting working in vim - see misc/vim_syntax
# install some linear solvers - see src/contributed/suitesparse, src/contributed/pardiso
# install paraview as an alternative post-processor to gmsh

######################

# basic arb dependencies
port install maxima gnuplot
# optional 
#port install wxmaxima
# install gfortran
port install gcc49 +gfortran
# install SuiteSparse
port install SuiteSparse

# for Lachlan's plot and track scripts
port install python27 py27-matplotlib py27-numpy py27-scipy py27-pandas py27-numexpr py27-wxpython-2.8
#port select --list python # just for information
port select --set python python27

# some notes on macports/python
#https://astrofrog.github.io/macports-python/
#https://guide.macports.org
#http://truongtx.me/2014/02/25/mac-os-install-python-pip-virtualenv-using-macports/

# finally, link the (strange) macports gfortran name to a standard name within an accessible bin directory
echo "INFO: creating a link to gfortran within /usr/local/bin";
echo "UNINSTALL: to get rid of this link do rm /usr/local/bin/gfortran";
echo "WARNING: this link will not work if you have multiple gfortran versions installed via macports - remote the older ones or just create the link manually"
#ln -s /opt/local/bin/gfortran-mp-4.9 /usr/local/bin/gfortran
ln -s /opt/local/bin/gfortran-mp-* /usr/local/bin/gfortran

echo "INFO: you may now want to install the free suitesparse linear solver umfpack fortran 90 wrapper routine.  If so, cd src/contributed/suitesparse and type make";
echo "INFO: if you haven't already done so install Xcode with the command line utilities, and the Xquartz x11 server";
