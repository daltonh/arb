! file src/contributed/suitesparse/suitesparse_dummy_module.f90
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
module suitesparse_module

implicit none

!-----------------------------------------------------------------
contains

!-----------------------------------------------------------------

subroutine suitesparse_linear_solver(aa,iaa_csr1,jaa_csr1,xx,ierror,trans)

double precision, dimension(:), allocatable :: xx ! already allocated
double precision, dimension(:), allocatable :: aa ! already allocated
integer, dimension(:), allocatable :: iaa_csr1, jaa_csr1 ! already allocated
integer :: ierror
logical, optional :: trans

write(*,*)
write(*,*) 'ERROR: the suitesparse UMFPACK routines have not been linked into the arb executable'
write(*,*) 'This most probably means that system-wide umfpack libraries have not been installed'
write(*,*) 'Have a look in misc/install_dependencies for more information on this'
write(*,*) 'Another possibility is that you do not have the fortran 90 wrapper module and that'
write(*,*) 'you are not connected to the internet to allow the arb script to download this'
stop

end subroutine suitesparse_linear_solver

!-----------------------------------------------------------------

function suitesparse_linear_solver_check()

! this function indicates whether the solver is present or not

logical :: suitesparse_linear_solver_check

suitesparse_linear_solver_check = .false.

end function suitesparse_linear_solver_check

!-----------------------------------------------------------------

end module suitesparse_module
!-----------------------------------------------------------------
