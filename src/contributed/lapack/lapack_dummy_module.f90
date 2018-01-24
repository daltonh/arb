! file src/contributed/lapack/lapack_dummy_module.f90
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
module lapack_module
! provides a dummy routine for missing lapack calls

implicit none

!-----------------------------------------------------------------
contains

!-----------------------------------------------------------------

subroutine lapack_linear_solver(a,b,error,overwritea)

! this routine is a dummy lapack routine

double precision, dimension(:,:) :: a
double precision, dimension(:,:) :: b
logical, optional :: overwritea
logical :: error

call lapack_problem

end subroutine lapack_linear_solver

!-----------------------------------------------------------------

subroutine lapack_singular_value_decomposition(a,u,w,v,error)

! this routine is a dummy lapack routine

double precision, dimension(:,:) :: a, u, v
double precision, dimension(:) :: w
logical :: error

call lapack_problem

end subroutine lapack_singular_value_decomposition

!-----------------------------------------------------------------

subroutine lapack_problem

write(*,*)
write(*,*) 'ERROR: the lapack routines have not been compiled into or alongside the arb executable.'
write(*,*) 'If you are using the intel compiler, then they should be available as part of the intel MKL, and '// &
  'this should have been picked up during the compile process.'
write(*,*) 'If you are using the gfortran compiler, they should be available as dynamic libraries on your system.'
write(*,*) 'Eitherway, you may try doing `make clean'' from within the obj directory, and then remaking arb again.'
write(*,*) 'Look to see whether during the make process the lapack routines are marked as present.'
write(*,*) 'Alternatively, you can install the numerical recipes routines as an alternative, '// &
  'but their license is more restrictive (commercial code)'
stop

end subroutine lapack_problem

!-----------------------------------------------------------------

end module lapack_module

!-----------------------------------------------------------------
