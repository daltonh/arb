# arb finite volume solver
# Copyright 2009-2017 Dalton Harvie (daltonh@unimelb.edu.au)
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
# The original copyright of arb is held by Dalton Harvie, however the
# project is now under collaborative development.
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
# makefile to fetch sparse solver from sourceforge and make it into a module
# http://sparse.sourceforge.net
# from the website: Contact the author at kundert@users.sourceforge.net.

#------------------------------------------------------------------------------
# variables below this should be pretty-much automagically set
# this is taken from suitesparse directory, may not be needed

libs =

# find operating system type
# now based on uname -s which seems to be consistent between operating systems and doesn't need to be exported
os_type = $(shell uname -s)

# find machine type
# now based on uname -m which seems to be consistent between operating systems and doesn't need to be exported
mach_type = $(shell uname -m)

ifneq "$(strip $(findstring x86_64,$(mach_type)))" ""
  mach_type = intel64
  long_int = integer*8
  wrapper_addon = 64
else ifneq "$(strip $(filter i%86,$(mach_type)))" ""
# should match i386, i486, i586, i686 etc
  mach_type = ia32
  long_int = integer
else ifneq "$(strip $(findstring ia32,$(mach_type)))" ""
  mach_type = ia32
  long_int = integer
else
# assume that machine is 64bit if neither machinetype is identified but print a warning
  mach_type = intel64
  long_int = integer*8
  wrapper_addon = 64
  $(warning "WARNING:  The machine type could not be determined from the mach_type environment variable $(mach_type) which is set as the output from the command `uname -m`:  Assuming intel64 and continuing")
endif

# set default non-intel_mkl blas and lapack libraries
ifeq "$(os_type)" "Darwin"
# on osX use http://developer.apple.com/performance/accelerateframework.html which contains blas and lapack
  libs = -framework Accelerate
  clibs_extra = -fno-common -no-cpp-precomp
else
  libs = -lblas -llapack
  clibs_extra =
endif

#----------------
# all the rules

all: print sparse/lib/libsparse.a

# print out some info
print :
	@echo "Grabbing mgmres subroutine for arb:"
	@echo " os_type = $(os_type)"
	@echo " mach_type is: $(mach_type)"

download: sparse1.4b.tar.gz

sparse1.4b.tar.gz:
	curl -L -O https://downloads.sourceforge.net/project/sparse/sparse/sparse1.4b/sparse1.4b.tar.gz
	touch sparse1.4b.tar.gz

sparse: sparse1.4b.tar.gz
	rm -rf sparse
	tar -xzf sparse1.4b.tar.gz

# modify sparse config files and compile the library
sparse/lib/libsparse.a: sparse
	perl -p -i -e 's/(#define(\s+)FORTRAN)(\s+).*/\1 YES/' sparse/src/spConfig.h
	perl -p -i -e 's/(#define(\s+)ARRAY_OFFSET)(\s+).*/\1 NO/' sparse/src/spConfig.h
	perl -p -i -e 's/(#define(\s+)DEBUG)(\s+).*/\1 NO/' sparse/src/spConfig.h
	perl -p -i -e 's/(#define(\s+)EXPANDABLE)(\s+).*/\1 NO/' sparse/src/spConfig.h
	perl -p -i -e 's/(#define(\s+)TRANSLATE)(\s+).*/\1 NO/' sparse/src/spConfig.h
	perl -p -i -e 's/(#define(\s+)INITIALIZE)(\s+).*/\1 NO/' sparse/src/spConfig.h
	perl -p -i -e 's/(#define(\s+)DIAGONAL_PIVOTING)(\s+).*/\1 NO/' sparse/src/spConfig.h
	perl -p -i -e 's/(#define(\s+)spCOMPLEX)(\s+).*/\1 0/' sparse/src/spConfig.h
	perl -p -i -e 's/(#define(\s+)DEFAULT_PARTITION)(\s+).*/\1 spINDIRECT_PARTITION/' sparse/src/spConfig.h
	perl -p -i -e 's/(#define(\s+)DEFAULT_THRESHOLD)(\s+).*/\1 1.e-2/' sparse/src/spConfig.h
	perl -p -i -e 's/(#define(\s+)DIAG_PIVOTING_AS_DEFAULT)(\s+).*/\1 NO/' sparse/src/spConfig.h
	perl -p -i -e 's/sparse.a/libsparse.a/' sparse/src/Makefile
	touch sparse/src/spConfig.h
	make -C sparse/src
	
# use this for adding openmp option, although it doesn't seem to work
# perl -p -i -e 's/^(CFLAGS(\s*)=(.*))/\1 -fopenmp/' sparse/src/Makefile

#cat sparse_config_defaults >> sparse/src/spConfig.h
# dynamic variables:
# $< is first prerequisit
# $^ is all
# $@ is target

#fcomp = gfortran
# test2
#test2.o: test2.f90
#	$(fcomp) -c $< -o $@

#test2: test2.o sparse/lib/sparse.a
#	$(fcomp) $^ -o $@

clean_all: clean clean_download

clean:
	rm -rf sparse

clean_download:
	rm -rf sparse1.4b.tar.gz
