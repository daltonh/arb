! file src/contributed/lapack/lapack_module.f90
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
module lapack_module
! provides simple fortran90 interface to certain lapack routines

implicit none

!-----------------------------------------------------------------
contains

!-----------------------------------------------------------------

subroutine lapack_linear_solver(a,b,error,overwritea)

! this routine calls the lapack routine dgesv to solve a set of 
!  linear equations
! a is overwritten (by default) by LU decomposition
! b is overwritten by answer to system of equations

double precision, dimension(:,:), intent(inout) :: a
double precision, dimension(:,:), intent(inout) :: b
logical, optional, intent(in) :: overwritea
integer :: n, nrhs, ierror
integer, dimension(:), allocatable :: ipiv
double precision, dimension(:,:), allocatable :: asaved
logical, intent(out) :: error
logical, parameter :: debug = .false.

INTERFACE ! taken from mkl_lapack.fi
SUBROUTINE DGESV(N,NRHS,A,LDA,IPIV,B,LDB,INFO)
INTEGER            INFO,LDA,LDB,N,NRHS
INTEGER            IPIV(*)
DOUBLE PRECISION   A(LDA,*),B(LDB,*)
END
END INTERFACE

if (debug) write(*,'(80(1h+)/a)') 'subroutine lapack_linear_solver'

n = ubound(a,1)
nrhs = ubound(b,2)
if (debug) write(*,*) 'n = ',n,': nrhs = ',nrhs
if (n /= ubound(a,2).or.n /= ubound(b,1)) stop 'ERROR: array sizes don''t match in lapack_linear_solver'

allocate(ipiv(n))

! call getrf(a=mm_inv,ipiv=ipiv,info=ierror)
! if (ierror /= 0) stop 'ERROR: getrf returned an error in subroutine mls_kernel'
! call getri(a=mm_inv,ipiv=ipiv,info=ierror)
! if (ierror /= 0) stop 'ERROR: getri returned an error in subroutine mls_kernel'

if (present(overwritea)) then
  if (.not.overwritea) then
    allocate(asaved(n,n))
    asaved = a
  end if
end if

call dgesv(n,nrhs,a,n,ipiv,b,n,ierror) ! see man gsev

if (present(overwritea)) then
  if (.not.overwritea) then
    a = asaved
    deallocate(asaved)
  end if
end if

if (ierror /= 0) then
  if (debug) write(*,*) 'ERROR: dgsev returned an error in subroutine lapack_linear_solver: ierror = ',ierror
  error = .true.
end if

deallocate(ipiv)

if (debug) write(*,'(a/80(1h-))') 'subroutine lapack_linear_solver'

end subroutine lapack_linear_solver

!-----------------------------------------------------------------

subroutine lapack_singular_value_decomposition(a,u,w,v,error)

! this routine calls the lapack routine dgesvd to perform a singular
!   value decomposition of a(m,n)
! decomposition is returned in u(m,m), w(n) and v(n,n)
! only first min(m,n) elements of w are relevant

double precision, dimension(:,:), intent(inout) :: a
double precision, dimension(:,:), intent(out) :: u
double precision, dimension(:,:), intent(out) :: v
double precision, dimension(:), intent(out) :: w
double precision, dimension(:), allocatable :: work, s
integer :: n, m, lwork, info
logical, intent(out) :: error
logical, parameter :: debug = .false.

INTERFACE ! taken from mkl_lapack.fi
SUBROUTINE DGESVD(JOBU,JOBVT,M,N,A,LDA,S,U,LDU,VT,LDVT,WORK,LWORK,INFO)
CHARACTER          JOBU,JOBVT
INTEGER            INFO,LDA,LDU,LDVT,LWORK,M,N
DOUBLE PRECISION   A(LDA,*),S(*),U(LDU,*),VT(LDVT,*),WORK(*)
END
END INTERFACE

! taken from http://coding.derkeiler.com/Archive/Fortran/comp.lang.fortran/2009-10/msg00687.html
! INTERFACE
! SUBROUTINE DGESVD(JOBU, JOBVT, M, N, A, LDA, S, U, LDU, VT, LDVT, WORK, LWORK, INFO )
!     INTEGER, PARAMETER :: WP = KIND(1.0D0)
!     CHARACTER(LEN=1), INTENT(IN) :: JOBU, JOBVT
!     INTEGER, INTENT(IN) :: M, N, LDA, LDU, LDVT, LWORK
!     INTEGER, INTENT(OUT) :: INFO
!     REAL(WP), INTENT(OUT) :: S(*)
!     REAL(WP), INTENT(INOUT) :: A(LDA,*)
!     REAL(WP), INTENT(OUT) :: U(LDU,*), VT(LDVT,*), WORK(*)
! END SUBROUTINE DGESVD
! END INTERFACE

if (debug) write(*,'(80(1h+)/a)') 'subroutine lapack_singular_value_decomposition'

error = .false.
m = ubound(a,1) ! number of rows in a
n = ubound(a,2) ! number of columns in a
if (ubound(u,1) /= m.or.ubound(u,2) /= m.or.ubound(w,1) /= n.or.ubound(v,1) /= n.or.ubound(v,2) /= n) then
  write(*,*) 'ERROR: u, w and v indicies do not match in lapack_singular_value_decomposition'
  error = .true.
else 

! allocate workspace, as multiple of manual recommendation
  lwork = 100*(max(3*min(m,n)+max(m,n),5*min(m,n)))
! lwork = (max(3*min(m,n)+max(m,n),5*min(m,n)))
  if (debug) write(*,*) 'employed work size = ',lwork
  allocate(work(lwork),s(min(m,n)))
!      DGESVD(JOBU,JOBVT,M,N,A,LDA,S,U,LDU,VT,LDVT,WORK,LWORK,INFO)
  call dgesvd('A', 'A',  m,n,a,m,  s,u,m,  v, n,   work,lwork,info)
  if (debug) write(*,*) 'recommend work size = ',work(1)
  if (lwork < int(work(1))) &
    write(*,*) 'WARNING: increase lwork by a factor of about ',work(1)/dfloat(lwork), &
      ' in lapack_singular_value_decomposition'
  if (info < 0) then
    write(*,*) 'ERROR: the ',abs(info),'th parameter given to dgesvd was incorrect'
    stop
  else if (info /= 0) then
    write(*,*) 'ERROR: the svd in lapack_singular_value_decompostion did not work'
    error = .true.
  end if
! the w vector from this routine only contains min(m,n) valid entries - zero others
  w = 0.d0
  w(1:min(m,n)) = s
  deallocate(work,s)
end if

if (debug) write(*,'(a/80(1h-))') 'subroutine lapack_singular_value_decomposition'

end subroutine lapack_singular_value_decomposition

!-----------------------------------------------------------------

end module lapack_module
!-----------------------------------------------------------------
