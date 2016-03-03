! file src/contributed/mgmres/mgmres_dummy_module.f90
!
! Copyright 2009-2014 Dalton Harvie (daltonh@unimelb.edu.au)
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
! The copyright of arb is held by Dalton Harvie.
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
module mgmres_module

implicit none

!-----------------------------------------------------------------
contains

!-----------------------------------------------------------------

subroutine mgmres_linear_solver(aa,iaa_csr1,jaa,xx,ierror)

double precision, dimension(:), allocatable :: xx ! already allocated
double precision, dimension(:), allocatable :: aa ! already allocated
integer, dimension(:), allocatable :: iaa_csr1, jaa ! already allocated
integer :: ierror

write(*,*)
write(*,*) 'ERROR: the mgmres routines have not been linked into the arb executable.'
write(*,*) 'The default behaviour is now that this routine is not downloaded,'
write(*,*) 'so to include it, pass arb the --mgmres option'
stop

end subroutine mgmres_linear_solver

!-----------------------------------------------------------------

function mgmres_linear_solver_check()

! this function indicates whether the solver is present or not

logical :: mgmres_linear_solver_check

mgmres_linear_solver_check = .false.

end function mgmres_linear_solver_check

!-----------------------------------------------------------------

end module mgmres_module
!-----------------------------------------------------------------
