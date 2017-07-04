! file src/contributed/sparse/sparse_module.f90
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
module sparse_module

implicit none

!-----------------------------------------------------------------
contains

!-----------------------------------------------------------------

subroutine sparse_linear_solver(aa,iaa,jaa,xx,ierror)

! this subroutine is an interface to the sparse solver

use iso_c_binding

implicit none
double precision, dimension(:) :: xx ! already allocated
double precision, dimension(:), allocatable :: aa ! already allocated
integer, dimension(:), allocatable :: iaa ! already allocated
integer, dimension(:), allocatable :: jaa ! already allocated
!double precision, dimension(:), allocatable :: rhs ! to be allocated (on entry housed in xx)
double precision, dimension(size(aa)) :: rhs ! to be allocated (on entry housed in xx)

! sparse library variables
integer(c_int) :: error
type(c_ptr) :: matrix ! pointers are basically long integers, atleast in the notation of spFortran.c
!type(c_ptr), allocatable, dimension(:) :: element ! array of pointers to each element
type(c_ptr), dimension(size(aa)) :: element ! array of pointers to each element

integer :: n, nz, nr, nn, ierror
character(len=1000) :: formatline
logical, parameter :: debug = .true.
logical :: debug_sparse = .true.

!-------------------------------------
! setup interfaces to c routines
interface
! sfCreate( int *Size, int *Complex, int *Error )
!function sfCreate(a,b,error) bind(c,name='sfcreate')
function sfCreate(size,complex,error) bind(c) ! by default this will become the c function name sfcreate
use, intrinsic :: iso_c_binding
integer(c_int) :: size,complex,error
type(c_ptr) :: sfCreate
end function sfCreate
end interface

interface
! sfGetElement( long *Matrix, int *Row, int *Col )
function sfGetElement(matrix,row,col) bind(c)
use, intrinsic :: iso_c_binding
integer(c_int) :: row,col
type(c_ptr) :: sfGetElement,matrix
end function sfGetElement
end interface

interface
! sfClear( long *Matrix )
subroutine sfClear(matrix) bind(c)
use, intrinsic :: iso_c_binding
type(c_ptr) :: matrix
end subroutine sfClear
end interface

interface
! sfDestroy( long *Matrix )
subroutine sfDestroy(matrix) bind(c)
use, intrinsic :: iso_c_binding
type(c_ptr) :: matrix
end subroutine sfDestroy
end interface

interface
! sfAdd1Real( long *Element, spREAL *Real )
subroutine sfAdd1Real(element,real) bind(c)
use, intrinsic :: iso_c_binding
type(c_ptr) :: element
double precision :: real
end subroutine sfAdd1Real
end interface

interface
! sfPrint( long *Matrix, long *PrintReordered, long *Data, long *Header )
subroutine sfPrint(matrix,printreordered,data,header) bind(c)
use, intrinsic :: iso_c_binding
type(c_ptr) :: matrix
logical :: printreordered,data,header
end subroutine sfPrint
end interface

interface
! sfFactor( long *Matrix )
function sfFactor(matrix) bind(c)
use, intrinsic :: iso_c_binding
integer(c_int) :: sfFactor
type(c_ptr) :: matrix
end function sfFactor
end interface

interface
! void sfSolve( long *Matrix, spREAL RHS[], spREAL Solution[] )
subroutine sfSolve(matrix,rhs,solution) bind(c)
use, intrinsic :: iso_c_binding
type(c_ptr) :: matrix
double precision, dimension(*) :: rhs,solution ! note that rhs and solution can be the same array
end subroutine sfSolve
end interface
!-------------------------------------

if (debug) debug_sparse = .true.
if (debug_sparse) write(*,'(80(1h+)/a)') 'subroutine sparse_linear_solver'

ierror = 1

! arrays produced by the mainsolver subroutine are in compressed sparse row (csr) format with 1 indexing
! find array bounds and check that they are consistent
n = ubound(iaa,1) - 1
nz = iaa(n+1) - 1
if (debug) write(*,*) 'n = ',n,': nz = ',nz
if (ubound(aa,1) /= nz.or.ubound(jaa,1) /= nz.or.ubound(xx,1) /= n) stop &
  'ERROR: some of the csr matrices that are entering sparse_linear_solver are incorrectly sized'

! initialise matrix and add the elements into it
matrix = sfCreate(n,0,error)
if (debug) write(*,*) 'allocated matrix: error = ',error,': matrix = ',matrix

do nr = 1, n ! loop through rows, grabbing pointers to each element
  if (iaa(nr) < iaa(nr+1)) then
    do nn = iaa(nr), iaa(nr+1)-1
write(95,*) 'nn,n,nr,jaa(nn)'
write(95,*) nn,n,nr,jaa(nn)
      element(nn) = sfGetElement(matrix,nr,jaa(nn))
!     aa_full(nr,jaa(nn)) = aa(nn)
    end do
  end if
end do
if (debug) write(*,*) 'found pointers to matrix elements'

call sfClear(matrix)
if (debug) write(*,*) 'matrix cleared'

do nn = 1, nz
  call sfAdd1Real(element(nn), aa(nn))
end do
if (debug) write(*,*) 'added matrix elements'

! make a copy of the rhs to send to the routine
rhs=xx
! and zero initial guess
xx=0

error = sfFactor(matrix)
if (debug) write(*,*) 'factorised matrix, error = ',error

call sfSolve(matrix, rhs, xx)
if (debug) write(*,*) 'solved matrix'

call sfDestroy(matrix)
if (debug) write(*,*) 'destroyed matrix'

ierror = 0

if (debug_sparse) write(*,'(a/80(1h-))') 'subroutine sparse_linear_solver'

end subroutine sparse_linear_solver

!-----------------------------------------------------------------

function sparse_linear_solver_check()

! this function indicates whether the solver is present or not

logical :: sparse_linear_solver_check

sparse_linear_solver_check = .true.

end function sparse_linear_solver_check

!-----------------------------------------------------------------

end module sparse_module
!-----------------------------------------------------------------
