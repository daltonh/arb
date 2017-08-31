! file src/contributed/pardiso/pardiso_dummy_module.f90
!
! Copyright 2009-2017 Dalton Harvie (daltonh@unimelb.edu.au)
! 
! This file is part of arb finite volume solver, referred to as `arb'.
! 
! arb is a software package designed to solve arbitrary partial
! differential equations on unstructured meshes using the finite volume
! method.  Primarily it consists of fortran source code, perl source
! code and shell scripts.  arb replies on certain third party software
! to run, most notably the computer algebra system maxima
! <http://maxima.sourceforge.net/> which is released under the GNU GPL.
! 
! The original copyright of arb is held by Dalton Harvie, however the
! project is now under collaborative development.
! 
! arb is released under the GNU GPL.  arb is free software: you can
! redistribute it and/or modify it under the terms of the GNU General
! Public License (version 3) as published by the Free Software Foundation.
! You should have received a copy of the GNU General Public Licence
! along with arb (see file licence/gpl.txt after unpacking).  If not,
! see <http://www.gnu.org/licences/>.
! 
! arb is distributed in the hope that it will be useful, but WITHOUT
! ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
! FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public Licence
! for more details.
! 
! For full details of arb's licence see the licence directory.
! 
! The current homepage for the arb finite volume solver project is
! <http://people.eng.unimelb.edu.au/daltonh/downloads/arb>.
!
!-------------------------------------------------------------------------
module pardiso_module

implicit none

!-----------------------------------------------------------------
contains

!-----------------------------------------------------------------

subroutine pardiso_linear_solver(a,ia,ja,x,ierror,nthreads,iterative)

double precision, dimension(:), allocatable :: a, x ! already allocated
integer, dimension(:), allocatable :: ia, ja ! already allocated
integer :: ierror, nthreads
logical, optional :: iterative

write(*,*)
write(*,*) 'ERROR: the pardiso (native, not intel MKL) routine has not been compiled into the arb executable'
write(*,*) 'A number of stars have to align to be able to use this solver:'
write(*,*) '* On osx, it is only available using the gnu (gfortran) compiler'
write(*,*) '* On linux, it is available for both gfortran and ifort (intel) compilers (although the iterative '
write(*,*) '  solver doesn''t seem to work under gfortran on linux)'
write(*,*) '* You need to download the relevant library for your compiler and architecture from '
write(*,*) '  http://www.pardiso-project.org and place it in src/contributed/pardiso'
write(*,*) '* You also need to download a license file from http://www.pardiso-project.org '
write(*,*) '  which will be specific to your username (filename = pardiso.lic).  Place this file in your run directory'
write(*,*) '  or see src/contributed/pardiso/readme for other possible locations'
write(*,*) 'Have a look at src/contributed/pardiso/readme for more details'
write(*,*) 'Alternatively, try another matrix inversion technique'
stop

end subroutine pardiso_linear_solver

!-----------------------------------------------------------------

function pardiso_linear_solver_check()

! this function indicates whether the solver is present or not

logical :: pardiso_linear_solver_check

pardiso_linear_solver_check = .false.

end function pardiso_linear_solver_check

!-----------------------------------------------------------------

end module pardiso_module
!-----------------------------------------------------------------
