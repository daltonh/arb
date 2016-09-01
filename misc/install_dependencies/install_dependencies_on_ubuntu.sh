#!/bin/bash

# script installs just the packages that are required for arb
# this file is specific to ubuntu
# run as sudo

# basic arb dependencies
apt-get install maxima maxima-share gfortran liblapack-dev libblas-dev curl gnuplot valgrind libsuitesparse-dev

# not sure if also need libumfpack5.4.0 - test on new machine

# pre/post-processing - gmsh - more recent binaries are available which are probably worth using instead of the ubuntu packaged version
#apt-get install gmsh
echo "INFO: no longer installing the ubuntu packaged version of gmsh, but you can easily do this via 'sudo apt-get install gmsh'.  The better alternative is to download a binary directly from the gmsh website."
# pre/post-processing - paraview
apt-get install paraview

# for Lachlan's track script
apt-get install python-pip python-dev python-wxgtk2.8 python-wxtools wx2.8-doc wx2.8-examples wx2.8-headers wx2.8-i18n python-matplotlib python-pydot
pip install --upgrade numpy
pip install --upgrade pandas
pip install --upgrade numexpr

echo "INFO: you may now want to install the free suitesparse linear solver umfpack fortran 90 wrapper routine.  If so, cd src/contributed/suitesparse and type make";
