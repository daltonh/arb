! file src/contributed/suitesparse/suitesparse_module.f90
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
module suitesparse_module

implicit none

character(len=200), dimension(-19:4), parameter :: suitesparse_error = [ & ! taken from umfpack.f90
  "UMFPACK_ERROR_unknown                               ", & !-19 
  "UMFPACK_ERROR_ordering_failed                       ", & !-18
  "UMFPACK_ERROR_file_IO                               ", & !-17 
  "UMFPACK_ERROR_unknown                               ", & !-16 
  "UMFPACK_ERROR_invalid_permutation                   ", & !-15 
  "UMFPACK_ERROR_unknown                               ", & !-14 
  "UMFPACK_ERROR_invalid_system                        ", & !-13 
  "UMFPACK_ERROR_unknown                               ", & !-12 
  "UMFPACK_ERROR_different_pattern                     ", & !-11 
  "UMFPACK_ERROR_unknown                               ", & !-10 
  "UMFPACK_ERROR_unknown                               ", & !-9
  "UMFPACK_ERROR_invalid_matrix                        ", & !-8
  "UMFPACK_ERROR_unknown                               ", & !-7
  "UMFPACK_ERROR_n_nonpositive                         ", & !-6
  "UMFPACK_ERROR_argument_missing                      ", & !-5
  "UMFPACK_ERROR_invalid_Symbolic_object               ", & !-4
  "UMFPACK_ERROR_invalid_Numeric_object                ", & !-3
  "UMFPACK_ERROR_unknown                               ", & !-2
  "UMFPACK_ERROR_out_of_memory                         ", & !-1
  "UMFPACK_WARNING_OK                                  ", & !0
  "UMFPACK_WARNING_singular_matrix                     ", & !1
  "UMFPACK_WARNING_determinant_underflow               ", & !2
  "UMFPACK_WARNING_determinant_overflow                ", & !3
  "UMFPACK_ERROR_unknown                               " ]  !4

!-----------------------------------------------------------------
contains

!-----------------------------------------------------------------

subroutine suitesparse_linear_solver(aa,iaa_csr1,jaa_csr1,xx,ierror,trans)

! this subroutine is an interface to the suitesparse umfpack solver

use mUMFPACK ! we now use the fortran 90 wrpaaer to this solver kindly provided by 
! http://geo.mff.cuni.cz/~lh/Fortran/UMFPACK/#changelog
! the wrapper routine that contains this module will be automatically downloaded during the arb compilation step

double precision, dimension(:), allocatable :: xx ! already allocated
double precision, dimension(:), allocatable :: aa ! already allocated
integer, dimension(:), allocatable :: iaa_csr1, jaa_csr1 ! already allocated
!integer*8, dimension(:), allocatable :: iaa, jaa ! to be allocated
integer, dimension(:), allocatable :: iaa, jaa ! to be allocated
double precision, dimension(:), allocatable :: rhs ! to be allocated
integer :: ierror, nn, nz
character(len=1000) :: formatline
!double precision, dimension(20) :: control
!double precision, dimension(90) :: info
!integer*8 :: n, symbolic, numeric, sys
!integer*8 :: n, sys
integer :: n
logical, parameter :: iterative = .true. ! use iterative refinement when solving
logical :: debug = .false., debug_sparse = .false.
logical, optional :: trans ! if true then solve using the tranpose of A
logical :: trans_l ! local version of trans 

! UMFPACK constant
integer :: sys=UMFPACK_A
! ---------- SELECT ONE CHOICE: ----------
! C pointers
  type(c_ptr) :: symbolic,numeric
! integer pointers
! integer(c_intptr_t) :: symbolic,numeric
! ----------------------------------------
! zero-based arrays
real(8) :: control(0:UMFPACK_CONTROL-1),info(0:UMFPACK_INFO-1)

if (debug) debug_sparse = .true.
if (debug_sparse) write(*,'(80(1h+)/a)') 'subroutine suitesparse_linear_solver'

trans_l = .false.
if (present(trans)) trans_l = trans

ierror = 0

! arrays produced by the mainsolver subroutine are in compressed sparse row (csr) format with 1 indexing
! find array bounds and check that they are consistent
n = ubound(iaa_csr1,1) - 1
nz = iaa_csr1(n+1) - 1
if (debug) write(91,*) 'n = ',n,': nz = ',nz
if (ubound(aa,1) /= nz.or.ubound(jaa_csr1,1) /= nz.or.ubound(xx,1) /= n) stop &
  'ERROR: some of the csr matrices that are entering suitesparse_linear_solver are incorrectly sized'

! UMFPACK uses compressed sparse column (csc) format with 0 indexing
! However the transpose of the csr matrix is the csc matrix with iaa and jaa swapped
! so copy these index arrays directly and get umfpack to solve the equation aa^T \cdot xx = rhs
! allocate new 0 indexed csc arrays
!allocate(iaa(nz),jaa(n+1))
allocate(iaa(1:nz),jaa(0:n)) ! made this change to jaa indexing to be consistent with mumfpack example, but made no difference to results (so previously indexed arrays were probably OK)
! at the same time change to 0 indexing and change index arrays for integer*8 for intel64
!iaa = jaa_csr1 - 1
!jaa = iaa_csr1 - 1
iaa(1:nz) = jaa_csr1(1:nz) - 1 ! apparently don't have to change indexing of iaa and aa arrays? (as opposed to the indexes they store, that do change)
jaa(0:n) = iaa_csr1(1:n+1) - 1

! print out matrix
if (debug) then
  write(91,*) 'csc 0 basis matrix that is the transpose of csr:'
  formatline = '(a,i10,a,i10)'
  write(91,fmt=formatline) 'n = ',n,': nz = ',nz
  formatline = '(a,i10,a,g10.4,a,i10,a,i10)'
  do nn = 1, nz
    write(91,fmt=formatline) 'aa(',nn,') = ',aa(nn),': iaa(',nn,') = ',iaa(nn)
  end do
  formatline = '(a,i10,a,i10)'
  do nn = 0, n
    write(91,fmt=formatline) 'jaa(',nn,') = ',jaa(nn)
  end do
end if

!----------------------------
! now call the umf routines
! the layout here is based on the umf4hb.f which is included as a Demo in SuiteSparse
! Ap = jaa (n+1)
! Ai = iaa (nz)
! Ax = aa (nz)

! set default parameters
call umf4def (control)

! print control parameters.  set control (UMFPACK_PRL) to 1 to print
! error messages only
if (debug_sparse) then
  control (UMFPACK_PRL) = 2
else
  control (UMFPACK_PRL) = 1
end if
!control(UMFPACK_STRATEGY) = UMFPACK_STRATEGY_UNSYMMETRIC
!control(UMFPACK_SCALE) = 0 ! no scaling
!control(UMFPACK_SCALE) = 1 ! sum scaling (default)
control(UMFPACK_SCALE) = 2 ! max scaling
control(UMFPACK_PIVOT_TOLERANCE) = 0.3d0 ! (default 0.1d0, but with sum scaling this default has given problems on fairly innocuous problems, so increased it as of v0.52)
if (debug_sparse) call umf4pcon (control)

! pre-order and symbolic analysis
!call umf4sym (n, n, Ap, Ai, Ax, symbolic, control, info)
call umf4sym (n, n, jaa, iaa, aa, symbolic, control, info)
!call umf4sym (n, n, Ap, Ai, Ax, symbolic, control, info)
 
if (debug_sparse) write(*,*) 'symbolic analysis complete'

! print statistics computed so far
if (debug) call umf4pinf (control, info)

! check umf4sym error condition
ierror = int(info(UMFPACK_STATUS))
if (ierror /= 0) then
  call deal_with_suitesparse_error(ierror,"umf4sym",iaa,jaa)
  return
end if

! numeric factorization
!call umf4num (Ap, Ai, Ax, symbolic, numeric, control, info)
call umf4num (jaa, iaa, aa, symbolic, numeric, control, info)

if (debug_sparse) write(*,*) 'numerical factorisation complete'

! print statistics for the numeric factorization
if (debug) call umf4pinf (control, info)

ierror = int(info(UMFPACK_STATUS))
if (ierror /= 0) then
  call deal_with_suitesparse_error(ierror,"umf4num",iaa,jaa)
  return
end if

! free the symbolic analysis
call umf4fsym (symbolic)

! solve Ax=b, with iterative refinement
!sys = 0
!sys = 2 !solve A^Tx=b - this is the default as A is actually given in csr format
if (trans_l) then
! sys = 0 ! this is the transpose of A given in csr format
  sys = UMFPACK_A ! this is the transpose of A given in csr format
else
! sys = 2 ! this is the normal A given in csr format
  sys = UMFPACK_Aat ! this is the normal A given in csr format
end if
allocate(rhs(n))
rhs = xx

if (iterative) then
  !call umf4solr (sys, Ap, Ai, Ax, x, b, numeric, control, info)
  call umf4solr (sys, jaa, iaa, aa, xx, rhs, numeric, control, info)
  if (debug_sparse) write(*,*) 'system solved with iterative refinement'
  ierror = int(info(UMFPACK_STATUS))
  if (ierror /= 0) then
    call deal_with_suitesparse_error(ierror,"umf4solr",iaa,jaa,rhs)
    return
  end if
else
  ! call umf4sol (sys, x, b, numeric, control, info)
  call umf4sol (sys, xx, rhs, numeric, control, info)
  if (debug_sparse) write(*,*) 'system solved without iterative refinement'
  ierror = int(info(UMFPACK_STATUS))
  if (ierror /= 0) then
    call deal_with_suitesparse_error(ierror,"umf4sol",iaa,jaa,rhs)
    return
  end if
end if

! free the numeric factorization
call umf4fnum (numeric)

! print final statistics
if (debug) call umf4pinf (control, info)

!----------------------------

deallocate(iaa,jaa,rhs)

if (debug_sparse) write(*,'(a/80(1h-))') 'subroutine suitesparse_linear_solver'

end subroutine suitesparse_linear_solver

!-----------------------------------------------------------------

function suitesparse_linear_solver_check()

! this function indicates whether the solver is present or not

logical :: suitesparse_linear_solver_check

suitesparse_linear_solver_check = .true.

end function suitesparse_linear_solver_check

!-----------------------------------------------------------------

subroutine deal_with_suitesparse_error(ierror,routine,iaa,jaa,rhs)

integer, dimension(:), allocatable :: iaa, jaa
double precision, dimension(:), allocatable, optional :: rhs
character(len=*) :: routine ! name of routine that produced the error
integer :: ierror ! error code

! deallocate any arrays that are allocated
if (allocated(iaa)) deallocate(iaa)
if (allocated(jaa)) deallocate(jaa)
if (present(rhs)) then
  if (allocated(rhs)) deallocate(rhs)
end if

write(*,*) 'ERROR: problem in suitesparse '//trim(routine)//': ierror = ',ierror, &
  ': description = '//trim(suitesparse_error(max(min(ubound(suitesparse_error,1),ierror), &
  lbound(suitesparse_error,1))))

end subroutine deal_with_suitesparse_error

!-----------------------------------------------------------------

end module suitesparse_module
!-----------------------------------------------------------------
