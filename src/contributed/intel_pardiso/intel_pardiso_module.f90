! file src/contributed/intel_pardiso/intel_pardiso_module.f90
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
module intel_pardiso_module

character(len=200), dimension(11), parameter :: pardiso_error = [ & ! taken directly from intel manual
  "input inconsistent                                                        ", & !-1
  "not enough memory                                                         ", & !-2
  "reordering problem                                                        ", & !-3
  "zero pivot, numerical factorization or iterative refinement problem       ", & !-4
  "unclassified (internal) error                                             ", & !-5
  "reordering failed (matrix types 11 and 13 only)                           ", & !-6
  "diagonal matrix is singular                                               ", & !-7
  "32-bit integer overflow problem                                           ", & !-8
  "not enough memory for OOC                                                 ", & !-9
  "problems with opening OOC temporary files                                 ", & !-10
  "read/write problems with the OOC data file                                "] !-11

!-----------------------------------------------------------------
contains

!-----------------------------------------------------------------

subroutine intel_pardiso_linear_solver(a,ia,ja,x,ierror,nthreads,ooc,safer)

! this routine based on pardiso_unsym.f routine from pardiso documentation
!  to call pardiso solver packaged with the intel mkl libraries

implicit none
double precision, dimension(:), allocatable :: a, x
integer, dimension(:), allocatable :: ia, ja
integer :: n, nz, ierror, i, nthreads
! variables required by pardiso
integer :: maxfct = 1 ! number of factoring matrices kept in memory
integer :: mnum = 1 ! particular matrix to factorise from those in memory
integer :: mtype = 11 ! type of matrix (unsymmetric)
integer :: msglvl = 0 ! how much information is printed (quiet=0)
integer :: nrhs = 1 ! number of b matrices
integer :: phase
integer*8, dimension(64) :: pt
integer, dimension(64) :: iparm
integer, dimension(:), allocatable :: perm
double precision, dimension(:), allocatable :: x_dummy
logical, optional :: ooc ! whether out of core solver is required
logical, optional :: safer ! whether to turn on safer (more stable) algorithms
logical :: first = .true. ! this flag records the first use of the routine, when the iparm variables are set
logical :: debug = .false., debug_sparse = .false.

external pardisoinit
external pardiso

if (debug) debug_sparse = .true.
if (debug_sparse) write(*,'(80(1h+)/a)') 'subroutine intel_pardiso_linear_solver'

n = ubound(ia,1) - 1
nz = ia(n+1) - 1
if (debug) write(*,*) 'n = ',n,': nz = ',nz
if (ubound(a,1) /= nz.or.ubound(ja,1) /= nz.or.ubound(x,1) /= n) stop &
  'ERROR: some of the csr matrices that are entering intel_pardiso_linear_solver are incorrectly sized'

allocate(perm(n),x_dummy(n))

! intitalise pointers (pt) - all values must be zero initially
pt = 0

if (first) then

  if (debug) msglvl = 1

  if (debug_sparse) write(*,*) 'calling pardisoinit'

  iparm = 0
  call pardisoinit(pt, mtype, iparm)

! additional non-default options
! iparm(1) = 1 ! allows non-default options - already set from pardisoinit
  if (debug) iparm(27) = 1 ! checks data structure
  iparm(6) = 1 ! stores solution in x rather than x_dummy
  if (present(ooc)) then
    if (ooc) iparm(60) = 2 ! use OOC pardiso version
  end if
! set some options that should improve parallel speed (details in intel manual)
! NOTE: the default for iparm(2) = 2, which is the serial version of the nested dissection algorithm from METIS.  Without additional iterative refinement steps, this algorithm has caused some problem for me with big matrices
! Setting iparm(2) = 0 uses the minimum degree algorithm, which testing shows is slightly slower than the nested dissection algorithm, however when the nested dissection algorithm gives a rubbish result (without iterative refinement) this algorithm works
  iparm(2) = 2
  if (nthreads > 1) iparm(2) = 3 ! the parallel (OpenMP) version of the nested dissection algorithm is used. It can decrease the time of computations on multi-core computers, especially when the time of the PARDISO Phase 1 is significant for your task.
! When using iparm(2) = 2 or 3 also use some iterative refinement!
  iparm(8) = 15 ! the number of steps used is <= this and is reported in iparm(7): Testing shows that the time for these steps is minimal, and is required for iparm(2) = 2 or 3
  if (safer) then
    if (.false.) write(*,*) 'WARNING in intel_pardiso_module.f90 - iterative refinement turned off and safer, slower algorithm'
    iparm(8) = 0 ! the number of steps used
    iparm(2) = 0 ! safe option
  end if
  if (nthreads >= 8) iparm(24) = 1 ! PARDISO uses new two-level factorization algorithm. This algorithm generally improves scalability in case of parallel factorization on many threads (>= 8).
  if (debug_sparse) write(*,'(3(a,i2))') ' pardiso options on entry: iparm(2) = ',iparm(2),': iparm(8) = ',iparm(8),': iparm(24) = ',iparm(24)

  if (debug_sparse) write(*,*) 'done pardisoinit'
  if (debug) then
    write(93,*) 'done pardisoinit'
    do i = 1, 64
      write(93,*) 'iparm(',i,') = ',iparm(i)
    end do
  end if

end if
    
! Reordering and Symbolic Factorization, This step also allocates
!  all memory that is necessary for the factorization 
phase = 11
call pardiso (pt, maxfct, mnum, mtype, phase, n, a, ia, ja, perm, nrhs, iparm, msglvl, x, x_dummy, ierror)
     
if (ierror /= 0) then
  write(*,*) 'ERROR: error in reordering and symbolic factorization of intel pardiso: error = ',ierror
  if (ierror < 0.and.ierror >= -ubound(pardiso_error,1)) write(*,*) '  pardiso error means (from manual): '//trim(pardiso_error(-ierror))
  deallocate(perm,x)
  return
else
  if (debug_sparse) write(*,*) 'Reordering completed ... '
  if (debug_sparse) then
    write(*,*) 'Number of nonzeros in factors   = ',iparm(18)
    write(*,*) 'Number of factorization MFLOPS  = ',iparm(19)
  end if
  if (debug) then
    write(93,*) 'Reordering completed ... '
    do i = 1, 64
      write(93,*) 'iparm(',i,') = ',iparm(i)
    end do
  end if
end if

! Factorization
phase = 22
call pardiso (pt, maxfct, mnum, mtype, phase, n, a, ia, ja, perm, nrhs, iparm, msglvl, x, x_dummy, ierror)

if (ierror /= 0) then
  write(*,*) 'ERROR: error in factorization of intel pardiso: error = ',ierror
  if (ierror < 0.and.ierror >= -ubound(pardiso_error,1)) write(*,*) '  pardiso error means (from manual): '//trim(pardiso_error(-ierror))
  deallocate(perm,x_dummy)
  return
else
  if (debug_sparse) write(*,*) 'Factorization completed ...  '
  if (debug) then
    write(93,*) 'Factorization completed ...  '
    do i = 1, 64
      write(93,*) 'iparm(',i,') = ',iparm(i)
    end do
  end if
end if

! Back substitution and iterative refinement
phase = 33
! iparm(8)  = 1   ! max numbers of iterative refinement steps
! temp &&&
!iparm(8) = 10 ! number of iterative steps to perform
!iparm(32) = 1 ! use recursive iterative solver
!dparm(2) = 1.d-12 ! some tolerance


call pardiso (pt, maxfct, mnum, mtype, phase, n, a, ia, ja, perm, nrhs, iparm, msglvl, x, x_dummy, ierror)
if (ierror /= 0) then
  write(*,*) 'ERROR: error in solve of intel pardiso: error = ',ierror
  if (ierror < 0.and.ierror >= -ubound(pardiso_error,1)) write(*,*) '  pardiso error means (from manual): '//trim(pardiso_error(-ierror))
  deallocate(perm,x_dummy)
  return
else
  if (debug_sparse) write(*,*) 'Solve completed ... '
  if (debug_sparse) write(*,'(a,i3)') ' number of iterative refinement steps completed: iparm(7) = ',iparm(7)
  if (debug) then
    write(93,*) 'Solve completed ... '
    do i = 1, 64
      write(93,*) 'iparm(',i,') = ',iparm(i)
    end do
  end if
end if

! Termination and release of memory
phase = -1
call pardiso (pt, maxfct, mnum, mtype, phase, n, a, ia, ja, perm, nrhs, iparm, msglvl, x, x_dummy, ierror)

if (ierror /= 0) then
  write(*,*) 'ERROR: error in termination of intel pardiso: error = ',ierror
  if (ierror < 0.and.ierror >= -ubound(pardiso_error,1)) write(*,*) '  pardiso error means (from manual): '//trim(pardiso_error(-ierror))
else
  if (debug_sparse) write(*,*) 'Termination completed ... '
  if (debug) then
    write(93,*) 'Termination completed ... '
    do i = 1, 64
      write(93,*) 'iparm(',i,') = ',iparm(i)
    end do
  end if
end if

deallocate(perm,x_dummy)

if (debug_sparse) write(*,'(a/80(1h-))') 'subroutine intel_pardiso_linear_solver'

end subroutine intel_pardiso_linear_solver

!-----------------------------------------------------------------

function intel_pardiso_linear_solver_check()

! this function indicates whether the solver is present or not

logical :: intel_pardiso_linear_solver_check

intel_pardiso_linear_solver_check = .true.

end function intel_pardiso_linear_solver_check

!-----------------------------------------------------------------

end module intel_pardiso_module
!-----------------------------------------------------------------
