!
!  Example of a FORTRAN Program Calling Sparse
!
program test

use iso_c_binding

integer matrix, error
!, sfCreate, sfGetElement, spFactor
integer element(10)
double precision rhs(4), solution(4)

interface
! sfCreate( int *Size, int *Complex, int *Error )
!function sfCreate(a,b,error) bind(c,name='sfcreate')
function sfCreate(size_var,complex_var,error) bind(c) ! by default this will become the c function name sfcreate
!use, intrinsic :: iso_c_binding
!integer(c_int) :: size_var,complex_var,error
integer :: size_var,complex_var,error
end function sfCreate
end interface

interface
! sfGetElement( long *Matrix, int *Row, int *Col )
function sfGetElement(matrix_var,row,col) bind(c)
!use, intrinsic :: iso_c_binding
!integer(c_int) :: matrix_var,row,col
integer :: matrix_var,row,col
end function sfGetElement
end interface

matrix = sfCreate(4,0,error)
matrix = sfCreate(4,0,error)
write(*,*) 'done 0'
write(*,*) 'matrix = ',matrix
element(1) = sfGetElement(matrix,1,1)
write(*,*) 'done 1'
element(2) = sfGetElement(matrix,1,2)
element(3) = sfGetElement(matrix,2,1)
element(4) = sfGetElement(matrix,2,2)
element(5) = sfGetElement(matrix,2,3)
element(6) = sfGetElement(matrix,3,2)
element(7) = sfGetElement(matrix,3,3)
element(8) = sfGetElement(matrix,3,4)
element(9) = sfGetElement(matrix,4,3)
element(10) = sfGetElement(matrix,4,4)
! call sfClear(matrix)
! call sfAdd1Real(element(1), 2d0)
! call sfAdd1Real(element(2), -1d0)
! call sfAdd1Real(element(3), -1d0)
! call sfAdd1Real(element(4), 3d0)
! call sfAdd1Real(element(5), -1d0)
! call sfAdd1Real(element(6), -1d0)
! call sfAdd1Real(element(7), 3d0)
! call sfAdd1Real(element(8), -1d0)
! call sfAdd1Real(element(9), -1d0)
! call sfAdd1Real(element(10), 3d0)
! call sfprint(matrix, .false., .false.)
! rhs(1) = 34d0
! rhs(2) = 0d0
! rhs(3) = 0d0
! rhs(4) = 0d0
! error = sfFactor(matrix)
! call sfSolve(matrix, rhs, solution)
! write (6, *) rhs(1), rhs(2), rhs(3), rhs(4)
!   10 format (f 10.2)
write(*,*) 'done'
end

