#!/bin/bash

# script installs just the packages that are required for arb
# this file is specific to ubuntu
# run as sudo

# basic arb dependencies
apt-get install maxima maxima-share gfortran liblapack-dev libblas-dev curl gnuplot valgrind 

# pre/post-processing - gmsh - more recent binaries are available which are probably worth using instead of the ubuntu packaged version
apt-get install gmsh
# pre/post-processing - paraview
apt-get install paraview

# for Lachlan's track script
apt-get install python-pip python-dev python-wxgtk2.8 python-wxtools wx2.8-doc wx2.8-examples wx2.8-headers wx2.8-i18n python-matplotlib
pip install --upgrade numpy
pip install --upgrade pandas
pip install --upgrade numexpr
