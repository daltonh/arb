! file src/contributed/intel_pardiso/intel_pardiso_dummy_module.f90
!
! Copyright 2009-2018 Dalton Harvie (daltonh@unimelb.edu.au)
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
module intel_pardiso_module

implicit none

!-----------------------------------------------------------------
contains

!-----------------------------------------------------------------

subroutine intel_pardiso_linear_solver(a,ia,ja,x,ierror,nthreads,ooc,safer)

double precision, dimension(:), allocatable :: a, x ! already allocated
integer, dimension(:), allocatable :: ia, ja ! already allocated
integer :: ierror, nthreads
logical, optional :: ooc, safer

write(*,*)
write(*,*) 'ERROR: the intel mkl pardiso routine has not been compiled into the arb executable'
write(*,*) 'Check that you are using the intel ifort compiler, and that the intel mkl libraries are present'
write(*,*) 'One problem may be that the pardiso libraries are included.  See the arb option --no-pardiso to remove'
write(*,*) 'these and allow the intel pardiso libraries to be used'
write(*,*) 'Alternatively, try another matrix inversion technique'
stop

end subroutine intel_pardiso_linear_solver

!-----------------------------------------------------------------

function intel_pardiso_linear_solver_check()

! this function indicates whether the solver is present or not

logical :: intel_pardiso_linear_solver_check

intel_pardiso_linear_solver_check = .false.

end function intel_pardiso_linear_solver_check

!-----------------------------------------------------------------

end module intel_pardiso_module
!-----------------------------------------------------------------
