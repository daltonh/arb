! file src/contributed/higham88_donest/donest_module.f90
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
! interface to the donest (double precision sonest) subroutine referred to in higham88.pdf (see resources)

module donest_module

implicit none

!-----------------------------------------------------------------
contains

!-----------------------------------------------------------------

subroutine donest_interface(kase,x,est)

integer, intent(in) :: kase ! reverse communication index
double precision, dimension(:), intent(inout) :: x ! vector
double precision, intent(out) :: est ! estimate of 1-norm
integer :: n ! dimension of vector (order of matrix)
integer, dimension(:), allocatable, save :: isgn ! workspace for donest
double precision, dimension(:), allocatable, save :: v ! another array for donest

! provide explicit interface for this f77 routine
interface
  subroutine donest (n, v, x, isgn, est, kase)
  integer n, isgn(n), kase
  double precision v(n), x(n), est
  end subroutine donest
end interface

n = ubound(x,1)
if (.not.allocated(isgn)) allocate(isgn(n))
if (.not.allocated(v)) allocate(v(n))
call donest(n,v,x,isgn,est,kase)

if (kase == 0) deallocate(isgn,v) ! we're finished

end subroutine donest_interface

!-----------------------------------------------------------------

function donest_check()

! this function indicates whether the donest is present or not

logical :: donest_check

donest_check = .true.

end function donest_check

!-----------------------------------------------------------------

end module donest_module
!-----------------------------------------------------------------
