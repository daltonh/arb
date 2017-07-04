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

fcomp = gfortran

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

#all: print mgmres_solver.f90
all: print

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
	tar -xzf sparse1.4b.tar.gz
	perl -p -i -e 's/#define(\s+)FORTRAN(\s+).*/#define FORTRAN YES/' sparse/src/spConfig.h
	perl -p -i -e 's/#define(\s+)ARRAY_OFFSET(\s+).*/#define ARRAY_OFFSET NO/' sparse/src/spConfig.h

#rm -rf mUMFPACK
#mkdir mUMFPACK
#tar -xzf mUMFPACK.tgz -C mUMFPACK
#touch mUMFPACK

# modify sparse add as a dependence to this
sparse/lib/sparse.a: sparse
	make -C sparse/src
	
# dynamic variables:
# $< is first prerequisit
# $^ is all
# $@ is target

# test2
test2.o: test2.f90
	$(fcomp) -c $< -o $@

test2: test2.o sparse/lib/sparse.a
	$(fcomp) $^ -o $@

# for curl, -L follows links
#curl -O https://sourceforge.net/projects/sparse/files/sparse/sparse1.4b/sparse1.4b.tar.gz
#mgmres_solver.f90: mgmres.f90 mgmres_additions_pre.f90 mgmres_additions_post.f90
#	cat mgmres_additions_pre.f90 > mgmres_solver.f90
#	cat mgmres.f90 >> mgmres_solver.f90
#	cat mgmres_additions_post.f90 >> mgmres_solver.f90
#	perl -p -i -e 's/end$$/end subroutine/g' mgmres_solver.f90 # need to replace end statements with end subroutine, to convert to a module
#	perl -p -i -e 's/logical, parameter :: verbose = .true./logical :: verbose/' mgmres_solver.f90
#	perl -p -i -e 's/subroutine mgmres_st \( n/subroutine mgmres_st ( verbose, itr_used, rho, n/' mgmres_solver.f90
#	perl -p -i -e 's/subroutine pmgmres_ilu_cr \( n/subroutine pmgmres_ilu_cr ( verbose, n/' mgmres_solver.f90
#	touch mgmres_solver.f90

clean_all: clean clean_download

clean:
	rm -rf sparse

clean_download:
	rm -rf sparse1.4b.tar.gz
