#!/bin/bash

# script installs just the packages that are required for arb
# this file is specific to osx, and based on macports
# run as sudo

# basic arb dependencies
port install maxima gnuplot
# optional 
#port install wxmaxima
# install gfortran
# TODO: link this and test with OMP etc
port install gcc49 +gfortran

# pre/post-processing - gmsh - more recent binaries are available which are probably worth using instead of the ubuntu packaged version
# pre/post-processing - paraview

# for Lachlan's track script
port install python27 py27-matplotlib py27-numpy py27-scipy py27-pandas py27-numexpr py27-wxpython-2.8
#port select --list python # just for information
port select --set python python27

# some notes on macports/python
#https://astrofrog.github.io/macports-python/
#https://guide.macports.org
#http://truongtx.me/2014/02/25/mac-os-install-python-pip-virtualenv-using-macports/
