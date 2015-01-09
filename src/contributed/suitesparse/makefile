# arb finite volume solver
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
# makefile to fetch and compile UMFPACK specifically for arb
# daltonh, introduced for v0.24, 051210

# lock suitesparse and metis versions so that updates don't break compatibility
suitesparse_version = 3.6.0
metis_version = 4.0.3

#------------------------------------------------------------------------------
# variables below this should be pretty-much automagically set

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

all: print umf4_f77wrapper.o libumfpack.a libamd.a libcholmod.a libcolamd.a libcamd.a libmetis.a libccolamd.a

# print out some info
print :
	@echo "Compiling umfpack libraries for arb:"
	@echo " os_type = $(os_type)"
	@echo " mach_type is: $(mach_type)"

download: SuiteSparse.tar.gz metis-$(metis_version).tar.gz

SuiteSparse.tar.gz:
	curl -O http://www.cise.ufl.edu/research/sparse/SuiteSparse/SuiteSparse-$(suitesparse_version).tar.gz
	mv SuiteSparse-$(suitesparse_version).tar.gz SuiteSparse.tar.gz
	touch SuiteSparse.tar.gz
#wget http://www.cise.ufl.edu/research/sparse/SuiteSparse/SuiteSparse-$(suitesparse_version).tar.gz
# wget http://www.cise.ufl.edu/research/sparse/SuiteSparse/current/SuiteSparse.tar.gz

metis-$(metis_version).tar.gz:
	curl -O http://glaros.dtc.umn.edu/gkhome/fetch/sw/metis/metis-$(metis_version).tar.gz
	touch metis-$(metis_version).tar.gz
# http://glaros.dtc.umn.edu/gkhome/fetch/sw/metis/metis-4.0.3.tar.gz
#wget http://glaros.dtc.umn.edu/gkhome/fetch/sw/metis/metis-4.0.tar.gz

SuiteSparse: SuiteSparse.tar.gz metis-$(metis_version).tar.gz
	rm -rf SuiteSparse
	tar -xzf SuiteSparse.tar.gz
	tar -xzf metis-$(metis_version).tar.gz
	mv metis-$(metis_version) SuiteSparse/metis-4.0
	touch SuiteSparse
	chmod -R u+w SuiteSparse
	@echo "F77 = gfortran" >>SuiteSparse/UFconfig/UFconfig.mk  # actually fortran compiler is not used for libraries and wrapper script
	@echo "F77FLAGS = -O" >>SuiteSparse/UFconfig/UFconfig.mk
	@echo "F77LIB =" >>SuiteSparse/UFconfig/UFconfig.mk
	@echo "CC = gcc" >>SuiteSparse/UFconfig/UFconfig.mk
	@echo "CFLAGS = -O3 $(clibs_extra) -fexceptions" >>SuiteSparse/UFconfig/UFconfig.mk
	@echo "LIB = -lstdc++" >>SuiteSparse/UFconfig/UFconfig.mk
	@echo "BLAS = $(libs)" >>SuiteSparse/UFconfig/UFconfig.mk
	@echo "LAPACK = $(libs)" >>SuiteSparse/UFconfig/UFconfig.mk
	perl -p -i -e 's/__log2/METIS__log2/' SuiteSparse/metis-4.0/Lib/rename.h # bugfix as specified by http://www.cise.ufl.

# the semi colon in these commands is used so that the second command is performed in the directory specified by the cd

umf4_f77wrapper.o: SuiteSparse
	cd SuiteSparse/UMFPACK/Demo; make umf4_f77wrapper$(wrapper_addon).o # wrapper_addon is either 64 or nothing (for ia32)
	cp SuiteSparse/UMFPACK/Demo/umf4_f77wrapper$(wrapper_addon).o umf4_f77wrapper.o

UMFPACK: SuiteSparse
	cd SuiteSparse/UMFPACK/Demo; make libs

libumfpack.a: UMFPACK
	cp SuiteSparse/UMFPACK/Lib/libumfpack.a .

libamd.a: UMFPACK
	cp SuiteSparse/AMD/Lib/libamd.a .

libcholmod.a: UMFPACK
	cp SuiteSparse/CHOLMOD/Lib/libcholmod.a .

libcolamd.a: UMFPACK
	cp SuiteSparse/COLAMD/Lib/libcolamd.a .
	ranlib libcolamd.a

libcamd.a: UMFPACK
	cp SuiteSparse/CAMD/Lib/libcamd.a .

libmetis.a: UMFPACK
	cp SuiteSparse/metis-4.0/libmetis.a .

libccolamd.a: UMFPACK
	cp SuiteSparse/CCOLAMD/Lib/libccolamd.a .
	ranlib libccolamd.a



#libumfpack.a
#libamd.a
#libcholmod.a
#libcolamd.a
#libcamd.a
#libmetis.a
#libccolamd.a


#../Lib/libumfpack.a
#../../AMD/Lib/libamd.a
#../../CHOLMOD/Lib/libcholmod.a
#../../COLAMD/Lib/libcolamd.a
#../../CAMD/Lib/libcamd.a
#../../metis-4.0/libmetis.a
#../../CCOLAMD/Lib/libccolamd.a

clean_all: clean clean_download

clean:
	rm -rf SuiteSparse metis-$(metis_version) *.a *.o

clean_download:
	rm -rf SuiteSparse.tar.gz
	rm -rf metis-*.tar.gz

