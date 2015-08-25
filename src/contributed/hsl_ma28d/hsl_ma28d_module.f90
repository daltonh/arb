! file src/contributed/hsl_ma28d/hsl_ma28d_module.f90
!
! Copyright 2009-2015 Dalton Harvie (daltonh@unimelb.edu.au)
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
module hsl_ma28d_module

!-----------------------------------------------------------------
contains

!-----------------------------------------------------------------

subroutine hsl_ma28_linear_solver(aa,iaa_csr1,jaa,xx,ierror)

! this subroutine is an interface to the hsl ma28 linear solver routine

implicit none
double precision, dimension(:), allocatable :: aa, xx ! already allocated
integer, dimension(:), allocatable :: iaa_csr1 ! already allocated
integer, dimension(:), allocatable :: iaa ! to be allocated (coo1 format)
integer, dimension(:), allocatable :: jaa ! already allocated
double precision, dimension(:), allocatable :: a, w, rhs, r
integer, dimension(:), allocatable :: irn, icn
integer, dimension(:,:), allocatable :: ikeep, iw
integer :: n, nz, ierror, lirn, licn, nr, nn
character(len=1000) :: formatline
logical, parameter :: iterative = .false. ! use iterative method
logical, parameter :: debug = .false.

! provide explicit interface for these f77 routines
! N    order of matrix  not altered by subroutine.
! NZ   number of non-zeros in input matrix  not altered by subroutine.
! A    is a real array  length licn.
! LICN integer  length of arrays a and icn.  not altered by subroutine.
! IRN  integer array of length lirn. (=iaa)
! LIRN integer  length of array irn. not altered by the subroutine.
! ICN  integer array of length licn.  holds column indices on entry (=jaa)
interface
  subroutine MA28AD(N, NZ, A, LICN, IRN, LIRN, ICN, U, IKEEP, IW, W, IFLAG)
    INTEGER N, NZ, LICN, LIRN, IFLAG
    INTEGER IRN(LIRN), ICN(LICN), IKEEP(N,5), IW(N,8)
    DOUBLE PRECISION A(LICN), U, W(N)
  end subroutine MA28AD
  subroutine MA28CD(N, A, LICN, ICN, IKEEP, RHS, W, MTYPE)
    INTEGER N, LICN, MTYPE
    INTEGER ICN(LICN), IKEEP(N,5)
    DOUBLE PRECISION A(LICN), RHS(N), W(N)
  end subroutine MA28CD
  subroutine MA28ID(N,NZ,AORG,IRNORG,ICNORG,LICN,A,ICN,IKEEP,RHS,X,R,W,MTYPE,PREC,IFLAG)
    DOUBLE PRECISION PREC
    INTEGER IFLAG,LICN,MTYPE,N,NZ
    DOUBLE PRECISION A(LICN),AORG(NZ),R(N),RHS(N),W(N),X(N)
    INTEGER ICN(LICN),ICNORG(NZ),IKEEP(N,5),IRNORG(NZ)
  end subroutine MA28ID
end interface

if (debug) write(*,'(80(1h+)/a)') 'subroutine hsl_ma28_linear_solver'

ierror = 0

! arrays produced by the mainsolver subroutine are in compressed sparse row (csr) format with 1 indexing
! find array bounds and check that they are consistent
n = ubound(iaa_csr1,1) - 1
nz = iaa_csr1(n+1) - 1
if (debug) write(*,*) 'n = ',n,': nz = ',nz
if (ubound(aa,1) /= nz.or.ubound(jaa,1) /= nz.or.ubound(xx,1) /= n) stop &
  'ERROR: some of the csr matrices that are entering hsl_ma28_linear_solver are incorrectly sized'

! need to convert array to coordinate format (coo) format with 1 indexing for HSL_ma28
allocate(iaa(nz))
do nr = 1, n ! loop through rows
  if (iaa_csr1(nr) < iaa_csr1(nr+1)) iaa(iaa_csr1(nr):iaa_csr1(nr+1)-1) = nr
end do
deallocate(iaa_csr1)

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

licn = 40*nz ! needs to be large enough to store decomposition
lirn = 12*nz ! doesn't need to be as large

! allocate temporary arrays and copy data into them
allocate (a(licn),irn(lirn),icn(licn),w(n),ikeep(n,5),iw(n,8))
a(1:nz)=aa
icn(1:nz)=jaa
irn(1:nz)=iaa

if (debug) write(*,*) 'in linsp_solver: n = ',n,': asize = ',nz,': licn = ',licn,': lirn = ',lirn

if (debug) write(*,*) 'calling MA28AD'
!call MA28AD(n,nz,a,licn,irn,lirn,icn,0.d0,ikeep,iw,w,ierror) ! pivoting for sparsity
call MA28AD(n,nz,a,licn,irn,lirn,icn,1.d0,ikeep,iw,w,ierror) ! pivoting for stability
!     SUBROUTINE MA28AD(N, NZ, A, LICN, IRN, LIRN, ICN, U, IKEEP, IW, W,
!    * IFLAG)

if (ierror < 0) then
  write(*,*) 'WARNING: error after first ma28 routine call: ierror = ',ierror
else if (iterative) then
  if (debug) write(*,*) 'calling MA28ID'
  allocate(rhs(n),r(n))
  rhs=xx
  call MA28ID(n,nz,aa,iaa,jaa,licn,a,icn,ikeep,rhs,xx,r,w,1,1.d-12,ierror)
  deallocate(rhs,r)
! subroutine MA28ID(N,NZ,AORG,IRNORG,ICNORG,LICN,A,ICN,IKEEP,RHS,X,R,W,MTYPE,PREC,IFLAG)
else
  if (debug) write(*,*) 'calling MA28CD'
  call MA28CD(n,a,licn,icn,ikeep,xx,w,1)
! SUBROUTINE MA28CD(N, A, LICN, ICN, IKEEP, RHS, W, MTYPE)
end if

deallocate (a,irn,icn,w,ikeep,iw)

if (debug) write(*,'(a/80(1h-))') 'subroutine hsl_ma28_linear_solver'

end subroutine hsl_ma28_linear_solver

!-----------------------------------------------------------------

function hsl_ma28_linear_solver_check()

! this function indicates whether the solver is present or not

logical :: hsl_ma28_linear_solver_check

hsl_ma28_linear_solver_check = .true.

end function hsl_ma28_linear_solver_check

!-----------------------------------------------------------------

end module hsl_ma28d_module
!-----------------------------------------------------------------
