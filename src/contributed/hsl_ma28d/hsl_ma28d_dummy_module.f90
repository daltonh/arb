! file src/contributed/hsl_ma28d/hsl_ma28d_dummy_module.f90
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
module hsl_ma28d_module

!-----------------------------------------------------------------
contains

!-----------------------------------------------------------------

subroutine hsl_ma28_linear_solver(aa,iaa,jaa,xx,iflag)

implicit none
double precision, dimension(:), allocatable :: aa, xx ! already allocated
integer, dimension(:), allocatable :: iaa, jaa ! already allocated
integer :: iflag

write(*,*)
write(*,*) 'ERROR: the hsl ma28 routines have not been compiled into the arb executable'
write(*,*) 'Check that both ma28d.f and ddeps.f are present in src/linear_solvers/hsl_ma28d, make clean in obj and rerun'
write(*,*) 'Alternatively, try another matrix inversion technique'
stop

end subroutine hsl_ma28_linear_solver

!-----------------------------------------------------------------

function hsl_ma28_linear_solver_check()

! this function indicates whether the solver is present or not

logical :: hsl_ma28_linear_solver_check

hsl_ma28_linear_solver_check = .false.

end function hsl_ma28_linear_solver_check

!-----------------------------------------------------------------

end module hsl_ma28d_module
!-----------------------------------------------------------------
