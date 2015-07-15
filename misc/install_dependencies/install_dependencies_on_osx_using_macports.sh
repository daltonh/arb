#!/bin/bash

# this script just installs the macports packages that are required for arb
# this file is specific to osx
# run as sudo, as in 'sudo ./install_dependencies_on_osx_using_macports.sh' after you have:
# 1) installed Xcode with the command line utilities, including doing `xcode-select --install`
# 2) installed macports following instructions at https://guide.macports.org/#installing

# other things you might like to do:
# install gmsh using their binaries from http://geuz.org/gmsh/#Download
# get syntax highlighting working in vim - see misc/vim_syntax
# install some other linear solvers - see (e.g.) src/contributed/pardiso
# use this with ifort (intel's fortran compiler) - just installing it should allow it as an option
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
if [ ! -d "/usr/local/bin" ] ; then 
echo "INFO: first creating /usr/local/bin directory";
mkdir -p "/usr/local/bin" ;
fi ;
echo "UNINSTALL: to get rid of this link do rm /usr/local/bin/gfortran";
echo "WARNING: this link will not work if you have multiple gfortran versions installed via macports - remote the older ones or just create the link manually"
#ln -s /opt/local/bin/gfortran-mp-4.9 /usr/local/bin/gfortran
ln -s /opt/local/bin/gfortran-mp-* /usr/local/bin/gfortran

# this is now done differently, and based on the mumfpack routine which is automatically downloaded if required
#echo "INFO: you may now want to install the free suitesparse linear solver umfpack fortran 90 wrapper routine.  If so, cd src/contributed/suitesparse and type make";

# think that this should have already been installed prior to macports actually working, so the following message is redundant?
#echo "INFO: if you haven't already done so install Xcode with the command line utilities, and the Xquartz x11 server";
