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
double precision, dimension(:), intent(inout) :: xx ! here an assumed shape array, passed in from an allocated array
double precision, dimension(:), allocatable, intent(in) :: aa ! already allocated
integer, dimension(:), allocatable, intent(in) :: iaa ! already allocated
integer, dimension(:), allocatable, intent(in) :: jaa ! already allocated

! sparse library variables
integer(c_int) :: error
type(c_ptr) :: matrix ! pointers are basically long integers, atleast in the notation of spFortran.c
type(c_ptr) :: element ! array of pointers to each element

integer :: n, nz, nr, nn, ierror
logical, parameter :: debug = .true.
logical :: debug_sparse = .true.

!-------------------------------------
! setup interfaces to c routines
! notes on c/fortran interoperability:
! http://computer-programming-forum.com/49-fortran/a52bee90ca5e571c.htm
! https://software.intel.com/en-us/blogs/2009/03/31/doctor-fortran-in-ive-come-here-for-an-argument
! https://stackoverflow.com/questions/9374679/c-loc-with-dynamic-arrays
! http://www.nacad.ufrj.br/online/intel/Documentation/en_US/compiler_f/main_for/bldaps_for/common/bldaps_interopc.htm
! https://stackoverflow.com/questions/16330892/passing-an-array-from-fortran-to-a-c-function

interface
! sfCreate( int *Size, int *Complex, int *Error )
!function sfCreate(a,b,error) bind(c,name='sfcreate')
function sfCreate(size,complex,error) bind(c) ! by default this will become the c function name sfcreate
use, intrinsic :: iso_c_binding
integer(c_int) :: size,complex,error
type(c_ptr) :: sfCreate
end function sfCreate

! sfGetElement( long *Matrix, int *Row, int *Col )
function sfGetElement(matrix,row,col) bind(c)
use, intrinsic :: iso_c_binding
integer(c_int) :: row,col
type(c_ptr) :: sfGetElement,matrix
end function sfGetElement

! sfClear( long *Matrix )
subroutine sfClear(matrix) bind(c)
use, intrinsic :: iso_c_binding
type(c_ptr) :: matrix
end subroutine sfClear

! sfDestroy( long *Matrix )
subroutine sfDestroy(matrix) bind(c)
use, intrinsic :: iso_c_binding
type(c_ptr) :: matrix
end subroutine sfDestroy

! sfAdd1Real( long *Element, spREAL *Real )
subroutine sfAdd1Real(element,real) bind(c)
use, intrinsic :: iso_c_binding
type(c_ptr) :: element
double precision :: real
end subroutine sfAdd1Real

! sfPrint( long *Matrix, long *PrintReordered, long *Data, long *Header )
subroutine sfPrint(matrix,printreordered,data,header) bind(c)
use, intrinsic :: iso_c_binding
type(c_ptr) :: matrix
logical :: printreordered,data,header
end subroutine sfPrint

! sfFactor( long *Matrix )
function sfFactor(matrix) bind(c)
use, intrinsic :: iso_c_binding
integer(c_int) :: sfFactor
type(c_ptr) :: matrix
end function sfFactor

! void sfSolve( long *Matrix, spREAL RHS[], spREAL Solution[] )
subroutine sfSolve(matrix,rhs,solution) bind(c)
use, intrinsic :: iso_c_binding
type(c_ptr) :: matrix
double precision, dimension(*) :: rhs,solution ! note that rhs and solution can be the same array, and that c/fortran interoperability requires an assumed size array (I think)
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
if (error /= 0) then
  write(*,*) 'ERROR: sparse linear solver could not create the matrix'
  return
end if
if (debug) write(*,*) 'allocated matrix: error = ',error,': matrix = ',matrix

do nr = 1, n ! loop through rows, grabbing pointers to each element
  if (iaa(nr) < iaa(nr+1)) then
    do nn = iaa(nr), iaa(nr+1)-1
      element = sfGetElement(matrix,nr,jaa(nn))
      call sfAdd1Real(element, aa(nn))
    end do
  end if
end do
if (debug) write(*,*) 'found pointers to matrix elements and added element values'

error = sfFactor(matrix)
if (debug) write(*,*) 'factorised matrix, error = ',error
if (error /= 0) then
  write(*,*) 'ERROR: sparse linear solver could not factorise the matrix'
  return
end if

call sfSolve(matrix, xx, xx)
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
