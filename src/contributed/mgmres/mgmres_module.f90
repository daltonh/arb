! file src/contributed/mgmres/mgmres_module.f90
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

! this subroutine is an interface to the mgmres solver

use mgmres_solver

implicit none
double precision, dimension(:), allocatable :: aa, xx ! already allocated
integer, dimension(:), allocatable :: iaa_csr1 ! already allocated
integer, dimension(:), allocatable :: iaa ! to be allocated (coo1 format)
integer, dimension(:), allocatable :: jaa ! already allocated
double precision, dimension(:), allocatable :: rhs ! to be allocated (on entry housed in xx)
integer :: n, nz, nr, nn, ierror, itr_max, mr
double precision :: tol_abs, tol_rel
real ( kind = 8 ) rho
integer ( kind = 4 ) itr_used
character(len=1000) :: formatline
logical, parameter :: debug = .false.
logical :: debug_sparse = .true.
logical :: mgmres_debug = .true.

if (debug) debug_sparse = .true.
if (debug_sparse) write(*,'(80(1h+)/a)') 'subroutine mgmres_linear_solver'

ierror = 1

! arrays produced by the mainsolver subroutine are in compressed sparse row (csr) format with 1 indexing
! find array bounds and check that they are consistent
n = ubound(iaa_csr1,1) - 1
nz = iaa_csr1(n+1) - 1
if (debug) write(*,*) 'n = ',n,': nz = ',nz
if (ubound(aa,1) /= nz.or.ubound(jaa,1) /= nz.or.ubound(xx,1) /= n) stop &
  'ERROR: some of the csr matrices that are entering mgmres_linear_solver are incorrectly sized'

! need to convert array to coordinate format (coo) format with 1 indexing for mgmres routine (as per HSL_ma28)
allocate(iaa(nz))
do nr = 1, n ! loop through rows
  if (iaa_csr1(nr) < iaa_csr1(nr+1)) iaa(iaa_csr1(nr):iaa_csr1(nr+1)-1) = nr
end do
deallocate(iaa_csr1)

! make a copy of the rhs to send to the routine, required for iterative method
allocate(rhs(n))
rhs=xx
! and zero initial guess
xx=0

! print out matrix
if (debug) then
  write(91,*) 'coo 1 basis matrix:'
  formatline = '(a,i3,a,i3)'
  write(91,fmt=formatline) 'n = ',n,': nz = ',nz
  formatline = '(a,i3,a,g10.4,a,i3,a,i3,a,i3,a,i3)'
  do nn = 1, nz
    write(91,fmt=formatline) 'aa(',nn,') = ',aa(nn),': iaa(',nn,') = ',iaa(nn),': jaa(',nn,') = ',jaa(nn)
  end do
end if

! set some parameters
itr_max = 1000
mr = n
!mr = 10
tol_abs = 1.d-10
tol_rel = 1.d-10
itr_used = 0
rho = 0.d0

!subroutine mgmres_st ( verbose, itr_used, rho, n, nz_num, ia, ja, a, x, rhs, itr_max, mr, tol_abs, tol_rel )
call mgmres_st ( mgmres_debug, itr_used, rho, n, nz, iaa, jaa, aa, xx, rhs, itr_max, mr, tol_abs, tol_rel )

formatline = '(a,i10,a,g12.5)'
write(*,fmt=formatline) 'INFO: mgmres solver used ',itr_used,' iterations and achieved a residual of ',rho
deallocate(iaa,jaa,rhs)

ierror = 0

if (debug_sparse) write(*,'(a/80(1h-))') 'subroutine mgmres_linear_solver'

end subroutine mgmres_linear_solver

!-----------------------------------------------------------------

function mgmres_linear_solver_check()

! this function indicates whether the solver is present or not

logical :: mgmres_linear_solver_check

mgmres_linear_solver_check = .true.

end function mgmres_linear_solver_check

!-----------------------------------------------------------------

end module mgmres_module
!-----------------------------------------------------------------
