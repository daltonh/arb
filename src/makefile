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
# makefile for arb
# daltonh, v0.24

# history: continuously modified for various applications since 2003 by daltonh
# automated for different compilers and stuff
# incorporates some notes from http://make.mad-scientist.us/multi-arch.html
# to be run from build directory

#---------------------------------------------------------
# variables that are specified by the arb script when make is called
#  (the default values listed here will most probably be overwritten - if you want to change something, look to the arb script options)

# compiler type
# gfortran compiler
#compiler = gnu
# intel compiler
compiler = intel

# debugging?
#debug = on
debug = off

# default optimisation level is 1, although arb script will always pass in a value for this variable
opt = 1

# compile using openmp support
omp = off

# set to intel compiler main version number
intel_version = 13

# long integer used in intel compilation (ie, ilp64 instead of lp64)
ilp = off

# native pardiso solver
# use dummy variable to pass in from arb script, as variables passed in from command line don't seem to be able to be changed within the makefile
pardiso_from_arb = present

#------------------------------------------------------------------------------
# variables below this should be automagically set within this makefile

# default variables, most are checked later but if any are set to not present then that feature is not used
lapack = present
numerical_recipes = present
hsl_ma28d = present
suitesparse = present
donest = present
intel_pardiso = present
pardiso := $(pardiso_from_arb)

# additional libraries to link in
libs =		

# find operating system type
# now based on uname -s which seems to be consistent between operating systems and doesn't need to be exported
os_type = $(shell uname -s)

# find machine type
# now based on uname -m which seems to be consistent between operating systems and doesn't need to be exported
mach_type = $(shell uname -m)

ifneq "$(strip $(findstring x86_64,$(mach_type)))" ""
  mach_type = intel64
else ifneq "$(strip $(filter i%86,$(mach_type)))" ""
# should match i386, i486, i586, i686 etc
  mach_type = ia32
else ifneq "$(strip $(findstring ia32,$(mach_type)))" ""
  mach_type = ia32
else
# assume that machine is 64bit if neither machinetype is identified but print a warning
  mach_type = intel64
  $(warning "WARNING:  The machine type could not be determined from the mach_type environment variable $(mach_type) which is set as the output from the command `uname -m`:  Assuming intel64 and continuing")
endif

# try to get a best guess of where the intel maths kernel libraries are if they are installed and the intel compiler is in use
# this seems to be a moving target with each new ifort version!
ifeq "$(strip $(compiler))" "intel"
  mklroot_local = $(MKLROOT)
  ifeq "$(wildcard $(mklroot_local))" ""
    mklroot_local = $(MKL_HOME)
  endif
  ifeq "$(wildcard $(mklroot_local))" ""
    mklroot_local = $(FPATH)/..
  endif
  ifeq "$(wildcard $(mklroot_local))" ""
    mklroot_local = $(CPATH)/..
  endif
  ifeq "$(wildcard $(mklroot_local))" ""
    mklroot_local = $(LIBRARY_PATH)/..
  endif
  ifeq "$(strip $(mach_type))" "intel64"
    mklpath = $(mklroot_local)/lib/intel64
    ifeq "$(wildcard $(mklpath))" ""
      mklpath = $(mklroot_local)/lib/em64t
    endif
  else
    mklpath = $(mklroot_local)/lib/ia32
    ifeq "$(wildcard $(mklpath))" ""
      mklpath = $(mklroot_local)/lib/32
    endif
  endif
  ifeq "$(wildcard $(mklpath))" ""
    mklpath = $(mklroot_local)/lib
  endif
  intel_mkl = present
  ifeq "$(wildcard $(mklpath)/libmkl_core.a)" ""
    intel_mkl = not found despite the intel compiler being in use
#   $(error ERROR: the intel compiler is specified but the intel maths kernel libraries are not found: try setting the environment variable MKLROOT manually if there really are present)
  endif
else
  intel_mkl = not available as the intel compiler is not in use
  mklpath = not relevant as the intel compiler is not in use
endif

# setup up the libaries for the intel case
ifeq "$(strip $(intel_mkl))" "present"
# intel_link_version variable recognises that link line advisor for ifort haven't changed since version 11
# futureproofing based on the gamble that future versions will have the same link lines too, that of version 12
  intel_link_version = 12
  ifeq "$(intel_version)" "11"
    intel_link_version = $(intel_version)
  else ifeq "$(intel_version)" "10"
# untested
    intel_link_version = $(intel_version)
  else ifeq "$(intel_version)" "9"
# untested
    intel_link_version = $(intel_version)
  endif
# see http://software.intel.com/en-us/articles/intel-mkl-link-line-advisor/
  ifeq "$(os_type)" "Darwin"
    ifeq "$(intel_link_version)" "12"
      ifeq "$(omp)" "on"
# the following have been tested with versions 12.1.2, 13.0.1, 14.0.0
# osX, static linking, 32bit integers (lp64), omp + blas95 and lapack95 interfaces
        libs := $(MKLROOT)/lib/libmkl_blas95_lp64.a  $(MKLROOT)/lib/libmkl_lapack95_lp64.a $(MKLROOT)/lib/libmkl_intel_lp64.a $(MKLROOT)/lib/libmkl_intel_thread.a $(MKLROOT)/lib/libmkl_core.a -openmp -lpthread -lm
      else
# osX, static linking, 32bit integers (lp64), sequential + blas95 and lapack95 interfaces
        libs := $(MKLROOT)/lib/libmkl_blas95_lp64.a  $(MKLROOT)/lib/libmkl_lapack95_lp64.a $(MKLROOT)/lib/libmkl_intel_lp64.a $(MKLROOT)/lib/libmkl_sequential.a $(MKLROOT)/lib/libmkl_core.a -lpthread -lm
      endif
      cflags += -I$(MKLROOT)/include/intel64/lp64 -I$(MKLROOT)/include 
      cdebug += -I$(MKLROOT)/include/intel64/lp64 -I$(MKLROOT)/include 
      c77flags += -I$(MKLROOT)/include/intel64/lp64 -I$(MKLROOT)/include
    else
# osX, static linking, 32bit integers (lp64), sequential, working with 11.1.084
      libs := $(mklpath)/libmkl_solver_lp64_sequential.a $(mklpath)/libmkl_intel_lp64.a $(mklpath)/libmkl_sequential.a $(mklpath)/libmkl_core.a -lpthread
# dynamic
#     libs = -L$(mklpath) $(mklpath)/libmkl_solver_lp64_sequential.a -lmkl_intel_lp64 -lmkl_sequential -lmkl_core -lpthread
    endif
  else
    ifeq "$(intel_link_version)" "12"
# working with 13.1.0 (same as v12)
      ifeq "$(mach_type)" "intel64"
        ifeq "$(omp)" "on"
# linux intel64, static linking, 32bit integers (lp64), omp, working with 12.1.2
          libs := $(MKLROOT)/lib/intel64/libmkl_blas95_lp64.a  $(MKLROOT)/lib/intel64/libmkl_lapack95_lp64.a  -Wl,--start-group  $(MKLROOT)/lib/intel64/libmkl_intel_lp64.a $(MKLROOT)/lib/intel64/libmkl_intel_thread.a $(MKLROOT)/lib/intel64/libmkl_core.a -Wl,--end-group -openmp -lpthread -lm
        else
# linux intel64, static linking, 32bit integers (lp64), sequential, working with 12.1.2
          libs := $(MKLROOT)/lib/intel64/libmkl_blas95_lp64.a  $(MKLROOT)/lib/intel64/libmkl_lapack95_lp64.a  -Wl,--start-group  $(MKLROOT)/lib/intel64/libmkl_intel_lp64.a $(MKLROOT)/lib/intel64/libmkl_core.a $(MKLROOT)/lib/intel64/libmkl_sequential.a -Wl,--end-group -lpthread -lm
        endif
        cflags += -I$(MKLROOT)/include/intel64/lp64 -I$(MKLROOT)/include 
        cdebug += -I$(MKLROOT)/include/intel64/lp64 -I$(MKLROOT)/include 
        c77flags += -I$(MKLROOT)/include/intel64/lp64 -I$(MKLROOT)/include
      else
        ifeq "$(omp)" "on"
# linux ia32, static linking, 32bit integers (lp64), omp, working with 12.1.2
          libs := $(MKLROOT)/lib/ia32/libmkl_blas95.a  $(MKLROOT)/lib/ia32/libmkl_lapack95.a  -Wl,--start-group  $(MKLROOT)/lib/ia32/libmkl_intel.a $(MKLROOT)/lib/ia32/libmkl_intel_thread.a $(MKLROOT)/lib/ia32/libmkl_core.a -Wl,--end-group -openmp -lpthread -lm 
        else
# linux ia32, static linking, 32bit integers (lp64), sequential, working with 12.1.2
          libs := $(MKLROOT)/lib/ia32/libmkl_blas95.a  $(MKLROOT)/lib/ia32/libmkl_lapack95.a  -Wl,--start-group  $(MKLROOT)/lib/ia32/libmkl_intel.a $(MKLROOT)/lib/ia32/libmkl_sequential.a $(MKLROOT)/lib/ia32/libmkl_core.a -Wl,--end-group -lpthread -lm
        endif
        cflags += -I$(MKLROOT)/include/ia32 -I$(MKLROOT)/include
        cdebug += -I$(MKLROOT)/include/ia32 -I$(MKLROOT)/include
        c77flags += -I$(MKLROOT)/include/ia32 -I$(MKLROOT)/include
      endif
    else
# this line for ubuntu, intel 11.0.074
      ifeq "$(mach_type)" "intel64"
# linux intel64, static linking, 32bit integers (lp64), sequential, working with 11.1.069
        libs := $(mklpath)/libmkl_solver_lp64_sequential.a -Wl,--start-group $(mklpath)/libmkl_intel_lp64.a $(mklpath)/libmkl_sequential.a $(mklpath)/libmkl_core.a -Wl,--end-group -lpthread
      else
# linux ia32, static linking, 32bit integers (lp64), sequential, unchecked for 11.1.069
        libs := $(mklpath)/libmkl_solver_sequential.a -Wl,--start-group $(mklpath)/libmkl_intel.a $(mklpath)/libmkl_sequential.a $(mklpath)/libmkl_core.a -Wl,--end-group -lpthread
      endif
    endif
  endif
  ifeq "$(ilp)" "on"
# to go to long integers, need to compile in i8 option and replace lp64 with ilp64 in libraries
    libs := $(subst lp64,ilp64,$(libs))
    cflags := $(subst lp64,ilp64,$(cflags)) -i8
    cdebug := $(subst lp64,ilp64,$(cdebug))
    c77flags := $(subst lp64,ilp64,$(c77flags))
  endif
else
# set default blas and lapack libraries for nonintel_mkl
  ifeq "$(strip $(lapack))" "present"
    ifeq "$(os_type)" "Darwin"
# on osX use http://developer.apple.com/performance/accelerateframework.html which contains blas and lapack
      libs := -framework Accelerate
    else
      libs := -lblas -llapack
    endif
  endif
  ifeq "$(omp)" "on"
    omp = "off as ifort is being used but intel mkl is not"
  endif
endif

# compiler flags for compiling (cflag) and linking (lflag) depend on the compiler in use
# cdebug and ldebug are flags for compiling and linking when debugging the code
ifeq "$(compiler)" "gnu"
  fcomp = gfortran
  cflags += -c -ffree-line-length-none -O$(opt)
# cdebug += -c -g -Wall -ffree-line-length-none -fcheck=all
# cdebug += -c -g -Wall -ffree-line-length-none -fbounds-check
  cdebug += -c -g -Wall -ffree-line-length-none -fbounds-check -Wunused
# cdebug += -c -g -Wall -ffree-line-length-none
  lflags += -O$(opt)
# ldebug += -g -Wall -fcheck=all
# ldebug += -g -Wall -fbounds-check
  ifeq "$(os_type)" "Darwin"
    ldebug = -g -Wall -fbounds-check -Wl,-no_pie
  else
    ldebug = -g -Wall -fbounds-check
  endif
# ldebug = -g -Wall
  c77flags += -c -O$(opt)
# c77debug += -c -g -Wall -fcheck=all
  c77debug += -c -g -Wall -fbounds-check
# c77debug += -c -g -Wall
  ifeq "$(omp)" "on"
    cflags += -fopenmp
    cdebug += -fopenmp
    lflags += -fopenmp
    ldebug += -fopenmp
  endif
	cflagsnoomp := $(cflags)
	cdebugnoomp := $(cdebug)
	intel_version = "not relevant as gnu compiler in use"
else
# intel fortran compiler
#   -g includes debugging info
#   -fltconsistency required for direct solver
  fcomp = ifort
  cflags += -c -O$(opt)
  lflags = -O$(opt)
# cdebug += -c -g -traceback -debug all
  cdebug += -c -g -traceback -debug all -check all -check arg_temp_created
# ldebug = 
  ifeq "$(os_type)" "Darwin"
    ldebug = -check all -Wl,-no_pie
  else
    ldebug = -check all
  endif
  c77flags += -c -O$(opt)
# c77debug += -c -g -traceback -debug all
  c77debug += -c -g -traceback -debug all -check all -check arg_temp_created
# some code (kernel_module.f90) chokes with v13 intel compiler and omp on - remove option as this routine isn't omp anyway
# the := syntax means that the variable is evaluated at the time that it is defined, rather than when it is used
	cflagsnoomp := $(cflags)
	cdebugnoomp := $(cdebug)
  ifeq "$(omp)" "on"
    cflags += -openmp
    cdebug += -openmp
    lflags += -openmp
    ldebug += -openmp
  endif
# the noomp flags are only used when intel version 13 compiler is being used - it strangely throws up an internal error when compiling kernel_module.f90 under both linux and osx
# seems to be OK in version 14, as tested on osx
  ifneq "$(intel_version)" "13"
		cflagsnoomp := $(cflags)
		cdebugnoomp := $(cdebug)
  endif
endif

#----------------

# debugging
ifeq "$(debug)" "on"
  cflags = $(cdebug)
  cflagsnoomp = $(cdebugnoomp)
  lflags = $(ldebug)
  c77flags = $(c77debug)
  logme = 2>> make.log
else
  logme =
endif

srcdir = ../src

vpath	%.f90	$(srcdir)
vpath	%.f	$(srcdir)

# object files are derived here based on source files
#objects := $(patsubst %.f,%.o,$(patsubst %.f90,%.o,$(notdir $(wildcard $(srcdir)/*.f*))))
#objects := $(patsubst %.f90,%.o,$(notdir $(wildcard $(srcdir)/*.f90)))
objects = equations_module.o general_module.o main.o setup_module.o solver_module.o gmesh_module.o output_module.o kernel_module.o

#----------------
# check for existence of pardiso (native, not intel mkl version) libraries
# these pardiso libraries all seem to run in omp, so link in fopenmp with all of them
pardisodir := $(srcdir)/contributed/pardiso
oldlibs := $(libs)
ifeq "$(strip $(pardiso))" "present"
  ifeq "$(os_type)" "Darwin"
# on osx only for gfortran
    ifeq "$(compiler)" "intel"
      pardiso = not present
    else
      ifeq "$(and $(wildcard $(pardisodir)/pardiso.lic), $(wildcard $(pardisodir)/libpardiso500-MACOS-X86-64.dylib) )" ""
        pardiso = not present
      else
# iterative solver doesn't seem to like framework accelerate, not sure why, so completely overwrite libs here
# NB, otool -L build/arb on the mac lists the dynamic libraries that the executable needs
# this works
        libs := -L$(pardisodir) -lpardiso500-MACOS-X86-64 -lblas -llapack -fopenmp
      endif
    endif
  else
# on linux both intel and gfortran
    ifeq "$(compiler)" "intel"
      ifeq "$(and $(wildcard $(pardisodir)/pardiso.lic), $(wildcard $(pardisodir)/libpardiso500-INTEL1301-X86-64.so) )" ""
        pardiso = not present
      else
#       libs += -L$(pardisodir) -lpardiso500-INTEL1301-X86-64 -L/opt/intel/mkl/lib/intel64 -lgfortran -fopenmp
        libs := -L$(pardisodir) -lpardiso500-INTEL1301-X86-64 -L/opt/intel/mkl/lib/intel64 $(oldlibs) -fopenmp
      endif
    else
      ifeq "$(and $(wildcard $(pardisodir)/pardiso.lic), $(wildcard $(pardisodir)/libpardiso500-GNU461-X86-64.so) )" ""
        pardiso = not present
      else
# can't get this to work with the iterative solver
#       libs += -L$(pardisodir) -lpardiso500-GNU461-X86-64 -fopenmp -lpthread -lm
#       libs += -L$(pardisodir) -lpardiso500-GNU461-X86-64 -lblas -llapack -fopenmp
        libs := -L$(pardisodir) -lpardiso500-GNU461-X86-64 $(oldlibs) -fopenmp -lpthread -lm
#       libs := -L$(pardisodir) -lpardiso500-GNU472-X86-64 -lblas -llapack -fopenmp -lpthread -lm
#       libs += -L$(pardisodir) -lpardiso500-GNU461-X86-64 -fopenmp
      endif
    endif
  endif
endif
# setup pardiso interface module
ifeq "$(strip $(pardiso))" "present"
  objects += pardiso_module.o
  linear_solver_modules += pardiso_module.o
  files_to_delete += pardiso_dummy_module.o
else
  objects += pardiso_dummy_module.o
  linear_solver_modules += pardiso_dummy_module.o
  files_to_delete += pardiso_module.o
endif

#----------------
# check for existence of intel pardiso libraries, used if intel_mkl is available but native pardiso is not
ifeq "$(strip $(pardiso))" "present"
  intel_pardiso = not present
endif
ifneq "$(strip $(intel_mkl))" "present"
  intel_pardiso = not present
endif
# setup intel_pardiso interface module
ifeq "$(strip $(intel_pardiso))" "present"
  objects += intel_pardiso_module.o
  linear_solver_modules += intel_pardiso_module.o
  files_to_delete += intel_pardiso_dummy_module.o
else
  objects += intel_pardiso_dummy_module.o
  linear_solver_modules += intel_pardiso_dummy_module.o
  files_to_delete += intel_pardiso_module.o
endif

#----------------
# setup lapack interface module
ifeq "$(strip $(lapack))" "present"
  objects += lapack_module.o
  lapack_module += lapack_module.o
  files_to_delete += lapack_dummy_module.o
else
  objects += lapack_dummy_module.o
  lapack_module += lapack_dummy_module.o
  files_to_delete += lapack_module.o
endif

#----------------
# setup numerical recipes interface module
numrecdir = $(srcdir)/contributed/numerical_recipes
ifeq "$(strip $(numerical_recipes))" "present"
  ifeq "$(and $(wildcard $(numrecdir)/dludcmp.f), $(wildcard $(numrecdir)/dlubksb.f), $(wildcard $(numrecdir)/dsvdcmp.f), $(wildcard $(numrecdir)/dpythag.f) )" ""
    numerical_recipes = not present
  endif
endif
ifeq "$(strip $(numerical_recipes))" "present"
  objects += numerical_recipes_module.o dludcmp.o dlubksb.o dsvdcmp.o dpythag.o dsimp1.o dsimp2.o dsimp3.o dsimplx.o
  numerical_recipes_module += numerical_recipes_module.o
  files_to_delete += numerical_recipes_dummy_module.o
else
  objects += numerical_recipes_dummy_module.o
  numerical_recipes_module += numerical_recipes_dummy_module.o
  files_to_delete += numerical_recipes_module.o dludcmp.o dlubksb.o dsvdcmp.o dpythag.o dsimp1.o dsimp2.o dsimp3.o dsimplx.o
endif

#----------------
# check for existence of hsl_ma28d routines and setup variables
ifeq "$(strip $(hsl_ma28d))" "present"
  ifeq "$(and $(wildcard $(srcdir)/contributed/hsl_ma28d/ma28d.f), $(wildcard $(srcdir)/contributed/hsl_ma28d/ddeps.f) )" ""
    hsl_ma28d = not present
  endif
endif
ifeq "$(strip $(hsl_ma28d))" "present"
  objects += hsl_ma28d_ddeps.o hsl_ma28d.o hsl_ma28d_module.o
  linear_solver_modules += hsl_ma28d_module.o
  files_to_delete += hsl_ma28d_dummy_module.o
else
  objects += hsl_ma28d_dummy_module.o
  linear_solver_modules += hsl_ma28d_dummy_module.o
  files_to_delete += hsl_ma28d_ddeps.o hsl_ma28d.o hsl_ma28d_module.o
endif

#----------------
# check for existence of suitesparse libraries
# compilation of these is handled locally in the suitesparse directory right now
suitesparsedir = $(srcdir)/contributed/suitesparse
ifeq "$(strip $(suitesparse))" "present"
  ifeq "$(wildcard $(suitesparsedir)/umfpack.f90)" ""
    suitesparse = not present
  endif
endif
ifeq "$(strip $(suitesparse))" "present"
  objects += suitesparse_module.o umfpack.o
  oldlibs := $(libs)
  libs := $(libs) -lumfpack
  linear_solver_modules += suitesparse_module.o umfpack.o
  files_to_delete += suitesparse_dummy_module.o
else
  objects += suitesparse_dummy_module.o
  linear_solver_modules += suitesparse_dummy_module.o
  files_to_delete += suitesparse_module.o suitesparse_module_arch.f90 umfpack.o
endif

#----------------
# check for existence of higham88_donest routines and setup variables
donestdir = $(srcdir)/contributed/higham88_donest
ifeq "$(strip $(donest))" "present"
  ifeq "$(wildcard $(donestdir)/donest.f)" ""
    donest = not present
  endif
endif
ifeq "$(strip $(donest))" "present"
  objects += donest.o donest_module.o
  linear_solver_modules += donest_module.o
  files_to_delete += donest_dummy_module.o
else
  objects += donest_dummy_module.o
  linear_solver_modules += donest_dummy_module.o
  files_to_delete += donest.o donest_module.o
endif

#---------------------------------------------
# these targets do not result in files being produced
.PHONY: all print delete_files check_equations clean clean_setup clean_compile compile_only setup_only

# default goal
all : print delete_files check_equations arb

# no checking of equations, so will reuse old equations_module.f90 regardless of changes to the *.in files,
#  unless equations_module_template.f90 or setup_equations.pl have changed
compile_only : print delete_files arb

# no making, just create equations_module.f90
setup_only : print delete_files check_equations equations_module.f90

# print out some info
print :
	@echo "Compiling arb with options:"
	@echo " os_type is: $(os_type)"
	@echo " compiler is: $(compiler)"
	@echo " mach_type is: $(mach_type)"
	@echo " intel mkl is: $(intel_mkl)"
	@echo " lapack is: $(lapack)"
	@echo " numerical recipes routines are: $(numerical_recipes)"
	@echo " intel pardiso linear solver is: $(intel_pardiso)"
	@echo " pardiso (native) linear solver is: $(pardiso)"
	@echo " hsl_ma28d linear solver is: $(hsl_ma28d)"
	@echo " suitesparse linear solver is: $(suitesparse)"
	@echo " higham88 one norm estimator (donest) is: $(donest)"
	@echo " mklpath is: $(mklpath)"
	@echo " openmp is: $(omp)"
	@echo " optimisation level is: $(opt)"
	@echo " intel version is: $(intel_version)"
	@echo " intel link line version is: $(intel_link_version)"
	@echo " intel long integer (ilp64) support is: $(ilp)"
#@echo object files are: $(objects)

# delete any files from previous configurations
delete_files :
	rm -f make.log $(files_to_delete)
  
# check whether equations_module.f90 has to be remade because equations.in or constants.in has changed, and do so if necessary
check_equations :
	../src/setup_equations.pl
  
# dynamic variables:
# $^ is first prerequisit
# $< is all
# $@ is target

arb : $(objects)
	$(fcomp) -o arb $(lflags) $^ $(libs) $(logme)

main.o:	main.f90 general_module.o equations_module.o setup_module.o solver_module.o output_module.o gmesh_module.o
	$(fcomp) $(cflags) $< $(logme) -o $@

general_module.o: general_module.f90
	$(fcomp) $(cflags) $< $(logme) -o $@

# this will remake equations_module.f90 if there are changes to the template or setup_equations.pl only
# changes in the *.in files are picked up via the check_equations entry above
equations_module.f90: ../src/equations_module_template.f90 ../src/setup_equations.pl
	rm -f last_setup_equation_data
	../src/setup_equations.pl

gmesh_module.o: gmesh_module.f90 general_module.o
	$(fcomp) $(cflags) $< $(logme) -o $@

equations_module.o: equations_module.f90 general_module.o gmesh_module.o
	$(fcomp) $(cflags) $< $(logme) -o $@

output_module.o: output_module.f90 general_module.o equations_module.o gmesh_module.o solver_module.o
	$(fcomp) $(cflags) $< $(logme) -o $@

setup_module.o: setup_module.f90 general_module.o equations_module.o gmesh_module.o kernel_module.o output_module.o solver_module.o $(lapack_module)
	$(fcomp) $(cflags) $< $(logme) -o $@

kernel_module.o: kernel_module.f90 general_module.o $(lapack_module) $(numerical_recipes_module)
	$(fcomp) $(cflagsnoomp) $< $(logme) -o $@

solver_module.o: solver_module.f90 general_module.o equations_module.o $(linear_solver_modules)
	$(fcomp) $(cflags) $< $(logme) -o $@

intel_mkl_solver.o:	$(intel_mkl_subroutine)
	$(fcomp) $(cflags) $< $(logme) -o $@

ifeq "$(strip $(hsl_ma28d))" "present"
hsl_ma28d_module.o : $(srcdir)/contributed/hsl_ma28d/hsl_ma28d_module.f90
	$(fcomp) $(cflags) $< $(logme) -o $@
hsl_ma28d.o : $(srcdir)/contributed/hsl_ma28d/ma28d.f 
	$(fcomp) $(c77flags) $< $(logme) -o $@
hsl_ma28d_ddeps.o : $(srcdir)/contributed/hsl_ma28d/ddeps.f 
	$(fcomp) $(c77flags) $< $(logme) -o $@
else
hsl_ma28d_dummy_module.o : $(srcdir)/contributed/hsl_ma28d/hsl_ma28d_dummy_module.f90
	$(fcomp) $(cflags) $< $(logme) -o $@
endif

ifeq "$(strip $(suitesparse))" "present"
umfpack.o : $(suitesparsedir)/umfpack.f90
	$(fcomp) $(cflags) $< $(logme) -o $@
ifeq "$(strip $(mach_type))" "intel64"
suitesparse_module.o : $(suitesparsedir)/suitesparse_module.f90 umfpack.o
	cp $< suitesparse_module_arch.f90
	$(fcomp) $(cflags) suitesparse_module_arch.f90 $(logme) -o $@
else
suitesparse_module.o : $(suitesparsedir)/suitesparse_module.f90 umfpack.o
	cp $< suitesparse_module_arch.f90
	perl -p -i -e 's/integer\*8/integer*4/g' suitesparse_module_arch.f90 # if this is on a ia32 processor need to use shorter integers
	$(fcomp) $(cflags) suitesparse_module_arch.f90 $(logme) -o $@
endif
else
suitesparse_dummy_module.o : $(suitesparsedir)/suitesparse_dummy_module.f90
	$(fcomp) $(cflags) $< $(logme) -o $@
endif

ifeq "$(strip $(intel_pardiso))" "present"
intel_pardiso_module.o : $(srcdir)/contributed/intel_pardiso/intel_pardiso_module.f90
	$(fcomp) $(cflags) $< $(logme) -o $@
else
intel_pardiso_dummy_module.o : $(srcdir)/contributed/intel_pardiso/intel_pardiso_dummy_module.f90
	$(fcomp) $(cflags) $< $(logme) -o $@
endif

ifeq "$(strip $(pardiso))" "present"
pardiso_module.o : $(srcdir)/contributed/pardiso/pardiso_module.f90
	$(fcomp) $(cflags) $< $(logme) -o $@
else
pardiso_dummy_module.o : $(srcdir)/contributed/pardiso/pardiso_dummy_module.f90
	$(fcomp) $(cflags) $< $(logme) -o $@
endif

ifeq "$(strip $(lapack))" "present"
lapack_module.o : $(srcdir)/contributed/lapack/lapack_module.f90
	$(fcomp) $(cflags) $< $(logme) -o $@
else
lapack_dummy_module.o : $(srcdir)/contributed/lapack/lapack_dummy_module.f90
	$(fcomp) $(cflags) $< $(logme) -o $@
endif

ifeq "$(strip $(numerical_recipes))" "present"
numerical_recipes_module.o : $(numrecdir)/numerical_recipes_module.f90
	$(fcomp) $(cflags) $< $(logme) -o $@
dludcmp.o : $(numrecdir)/dludcmp.f 
	$(fcomp) $(c77flags) $< $(logme) -o $@
dlubksb.o : $(numrecdir)/dlubksb.f 
	$(fcomp) $(c77flags) $< $(logme) -o $@
dsvdcmp.o : $(numrecdir)/dsvdcmp.f 
	$(fcomp) $(c77flags) $< $(logme) -o $@
dpythag.o : $(numrecdir)/dpythag.f 
	$(fcomp) $(c77flags) $< $(logme) -o $@
dsimp1.o : $(numrecdir)/dsimp1.f 
	$(fcomp) $(c77flags) $< $(logme) -o $@
dsimp2.o : $(numrecdir)/dsimp2.f 
	$(fcomp) $(c77flags) $< $(logme) -o $@
dsimp3.o : $(numrecdir)/dsimp3.f 
	$(fcomp) $(c77flags) $< $(logme) -o $@
dsimplx.o : $(numrecdir)/dsimplx.f 
	$(fcomp) $(c77flags) $< $(logme) -o $@
else
numerical_recipes_dummy_module.o : $(numrecdir)/numerical_recipes_dummy_module.f90
	$(fcomp) $(cflags) $< $(logme) -o $@
endif

ifeq "$(strip $(donest))" "present"
donest_module.o : $(donestdir)/donest_module.f90
	$(fcomp) $(cflags) $< $(logme) -o $@
donest.o : $(donestdir)/donest.f
	$(fcomp) $(c77flags) $< $(logme) -o $@
else
donest_dummy_module.o : $(donestdir)/donest_dummy_module.f90
	$(fcomp) $(cflags) $< $(logme) -o $@
endif

# clean all files
clean : clean_setup clean_compile
# clean all files created during setup process
clean_setup :
	rm -f equations_module.f90 last_setup_equation_data last_maxima_fortran_results last_maxima_simplify_results
# clean all files created during compile process
clean_compile :
	rm -f arb *.o *.mod make.log last_compile_had_* last_compiler_was_*
