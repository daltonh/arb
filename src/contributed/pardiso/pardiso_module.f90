! file src/contributed/pardiso/pardiso_module.f90
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
module pardiso_module

character(len=200), dimension(12), parameter :: pardiso_error = [ & ! taken from pardiso manual
  "input inconsistent                                                        ", & !-1
  "not enough memory                                                         ", & !-2
  "reordering problem                                                        ", & !-3
  "zero pivot, numerical factorization or iterative refinement problem       ", & !-4
  "unclassified (internal) error                                             ", & !-5
  "reordering failed (matrix types 11 and 13 only)                           ", & !-6
  "diagonal matrix problem                                                   ", & !-7
  "32-bit integer overflow problem                                           ", & !-8
  "                                                                          ", & !-9
  "no license file pardiso.lic found                                         ", & !-10
  "license is expired                                                        ", & !-11
  "wrong username or hostname                                                "] !-12

character(len=200), dimension(12), parameter :: pardisoinit_error = [ & ! taken from pardiso manual
  "                                                                          ", & !-1
  "                                                                          ", & !-2
  "                                                                          ", & !-3
  "                                                                          ", & !-4
  "                                                                          ", & !-5
  "                                                                          ", & !-6
  "                                                                          ", & !-7
  "                                                                          ", & !-8
  "                                                                          ", & !-9
  "no license file pardiso.lic found                                         ", & !-10
  "license is expired                                                        ", & !-11
  "wrong username or hostname                                                "] !-12

!-----------------------------------------------------------------
contains

!-----------------------------------------------------------------

subroutine pardiso_linear_solver(a,ia,ja,x,ierror,nthreads,iterative)

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
integer :: solver = 0 ! sparse direct solver = 0 (default), multi-recursive iterative solver = 1
integer*8, dimension(64) :: pt
integer, dimension(64) :: iparm
double precision, dimension(64) :: dparm
integer, dimension(:), allocatable :: perm
double precision, dimension(:), allocatable :: x_dummy
logical, optional :: iterative ! whether iterative solver is used
logical :: first = .true. ! this flag records the first use of the routine, when the iparm variables are set
logical :: debug = .false., debug_sparse = .false.

external pardisoinit
external pardiso

if (debug) debug_sparse = .true.
if (debug_sparse) write(*,'(80(1h+)/a)') 'subroutine pardiso_linear_solver'

n = ubound(ia,1) - 1
nz = ia(n+1) - 1
if (debug) write(*,*) 'n = ',n,': nz = ',nz
if (ubound(a,1) /= nz.or.ubound(ja,1) /= nz.or.ubound(x,1) /= n) stop &
  'ERROR: some of the csr matrices that are entering pardiso_linear_solver are incorrectly sized'

allocate(perm(n),x_dummy(n))

! intitalise pointers (pt) - all values must be zero initially
pt = 0

if (first) then

  if (present(iterative)) then
    if (iterative) solver = 1 ! use OOC pardiso version
  end if

  if (debug) msglvl = 1

  if (debug_sparse) write(*,*) 'calling pardisoinit'

  iparm = 0
  call pardisoinit(pt, mtype, solver, iparm, dparm, ierror)

  if (ierror /= 0) then
    write(*,*) 'ERROR: error in initialisation of pardiso solver: error = ',ierror
    if (ierror < 0.and.ierror >= -ubound(pardisoinit_error,1)) write(*,*) '  pardisoinit error means (from manual): '// &
      trim(pardisoinit_error(-ierror))
    deallocate(perm,x)
    return
  end if

! additional non-default options
  iparm(6) = 1 ! stores solution in x rather than x_dummy

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
call pardiso (pt, maxfct, mnum, mtype, phase, n, a, ia, ja, perm, nrhs, iparm, msglvl, x, x_dummy, ierror, dparm)
     
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
call pardiso (pt, maxfct, mnum, mtype, phase, n, a, ia, ja, perm, nrhs, iparm, msglvl, x, x_dummy, ierror, dparm)

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


call pardiso (pt, maxfct, mnum, mtype, phase, n, a, ia, ja, perm, nrhs, iparm, msglvl, x, x_dummy, ierror, dparm)
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
call pardiso (pt, maxfct, mnum, mtype, phase, n, a, ia, ja, perm, nrhs, iparm, msglvl, x, x_dummy, ierror, dparm)

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

if (debug_sparse) write(*,'(a/80(1h-))') 'subroutine pardiso_linear_solver'

end subroutine pardiso_linear_solver

!-----------------------------------------------------------------

function pardiso_linear_solver_check()

! this function indicates whether the solver is present or not

logical :: pardiso_linear_solver_check

pardiso_linear_solver_check = .true.

end function pardiso_linear_solver_check

!-----------------------------------------------------------------

end module pardiso_module
!-----------------------------------------------------------------
