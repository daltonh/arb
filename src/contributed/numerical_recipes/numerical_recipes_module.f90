! file src/contributed/numerical_recipes/numerical_recipes_module.f90
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
module numerical_recipes_module
! provides simple fortran90 interface to certain numerical_recipes routines

implicit none

!-----------------------------------------------------------------
contains

!-----------------------------------------------------------------

subroutine numerical_recipes_linear_solver(a,b,error,overwritea)

! this routine calls the numerical_recipes routines ludcmp and lubksb to
!   solve a set of linear equations
! a is overwritten (by default) by LU decomposition
! b is overwritten by answer to system of equations
! requires dludcmp.f and dlubksb.f to be in src/contributed/numerical_recipes
!  directory

double precision, dimension(:,:), intent(inout) :: a
double precision, dimension(:,:), intent(inout) :: b
logical, optional, intent(in) :: overwritea
integer :: n, nrhs, nn
integer, dimension(:), allocatable :: indx
double precision, dimension(:,:), allocatable :: asaved
double precision :: d
logical, intent(out) :: error
logical, parameter :: debug = .false.

interface
  subroutine ludcmp(a,n,np,indx,d)
    INTEGER n,np,indx(n)
    DOUBLE PRECISION d,a(np,np)
  end subroutine ludcmp
end interface

interface
  subroutine lubksb(a,n,np,indx,b)
    INTEGER n,np,indx(n)
    DOUBLE PRECISION a(np,np),b(n)
  end subroutine lubksb
end interface

error = .false.

if (debug) write(*,'(80(1h+)/a)') 'subroutine numerical_recipes_linear_solver'

n = ubound(a,1)
nrhs = ubound(b,2)
if (debug) write(*,*) 'n = ',n,': nrhs = ',nrhs
if (n /= ubound(a,2).or.n /= ubound(b,1)) stop 'ERROR: array sizes don''t match in numerical_recipes_linear_solver'

allocate(indx(n))

if (present(overwritea)) then
  if (.not.overwritea) then
    allocate(asaved(n,n))
    asaved = a
  end if
end if

call ludcmp(a,n,n,indx,d)
do nn = 1, nrhs
  call lubksb(a,n,n,indx,b(:,nn))
end do

if (present(overwritea)) then
  if (.not.overwritea) then
    a = asaved
    deallocate(asaved)
  end if
end if

deallocate(indx)

if (debug) write(*,'(a/80(1h-))') 'subroutine numerical_recipes_linear_solver'

end subroutine numerical_recipes_linear_solver

!-----------------------------------------------------------------

subroutine numerical_recipes_singular_value_decomposition(a,u,w,v,error)

! this routine calls the numerical_recipes routine svdcmp to perform a singular
!   value decomposition of u
! input matix is a(m,n)
! decomposition is returned in u(m,m), w(n) and v(n,n)
! requires dsvdcmp.f and dpythag.f to be in src/contributed/numerical_recipes
!  directory

double precision, dimension(:,:), intent(inout) :: a
double precision, dimension(:,:), intent(out) :: u
double precision, dimension(:,:), intent(out) :: v
double precision, dimension(:), intent(out) :: w
integer :: n, m, i, j
logical, intent(out) :: error
double precision, parameter :: small_element = 1.d-10
logical, parameter :: debug = .false.

! provide explicit interface for these f77 routines
interface
  subroutine svdcmp(a,m,n,mp,np,w,v)
    INTEGER m,mp,n,np
    DOUBLE PRECISION a(mp,np),v(np,np),w(np)
  end subroutine svdcmp
end interface

if (debug) write(*,'(80(1h+)/a)') 'subroutine numerical_recipes_singular_value_decomposition'

error = .false.
m = ubound(a,1)
n = ubound(a,2)
if (ubound(u,1) /= m.or.ubound(u,2) /= m.or.ubound(w,1) /= n.or.ubound(v,1) /= n.or.ubound(v,2) /= n) then
  write(*,*) 'ERROR: u, w and v indicies do not match in numerical_recipes_singular_value_decomposition'
  error = .true.
else
  if (debug) then
    write(*,*) 'about to perform svd decomposition'
    write(*,*) 'j:a'
    do j = 1, m
      write(*,'(i2,100(a,g9.2))') j,':',(a(j,i),' ',i=1,n)
    end do
  end if
  call svdcmp(a,m,n,m,n,w,v)
  if (debug) then
    write(*,*) 'done svd decomposition'
    write(*,*) 'j:a'
    do j = 1, m
      write(*,'(i2,100(a,g9.2))') j,':',(a(j,i),' ',i=1,n)
    end do
    write(*,*) 'j:w:v'
    do j = 1, n
      write(*,'(i2,100(a,g9.2))') j,':',w(j),':',(v(j,i),' ',i=1,n)
    end do
  end if
! svdcmp is different to the lapack routine in that the returned u (a) is mxn and contains
!  the orthogonal basis vectors spread throughout the n columns
! so sort this a and place a result back in u
  u = 0.d0
  i = 0
  do j = 1, n
    if (abs(w(j)) > small_element.or.sqrt(dot_product(a(:,j),a(:,j))) > small_element) then
      i = i + 1
      if (i > m) then
        write(*,*) 'ERROR: too many basis vectors found in numerical_recipes_singular_value_decomposition'
        error = .true.
        exit
      end if
      w(i) = w(j)
      u(:,i) = a(:,j)
    end if
  end do
  if (i < n) w(i+1:n) = 0.d0
  if (debug) then
    write(*,*) 'sorting and copying result back into u and w'
    write(*,*) 'j:u'
    do j = 1, m
      write(*,'(i2,100(a,g9.2))') j,':',(u(j,i),' ',i=1,m)
    end do
    write(*,*) 'j:w'
    do j = 1, n
      write(*,'(i2,100(a,g9.2))') j,':',w(j)
    end do
  end if
end if

if (debug) write(*,'(a/80(1h-))') 'subroutine numerical_recipes_singular_value_decomposition'

end subroutine numerical_recipes_singular_value_decomposition

!-----------------------------------------------------------------

subroutine numerical_recipes_simplex(maxfunction,constraints,solution)

! this subroutine is an interface between f90 and the f77 numrec routines
!  that implement the simplex linear programming method

! maxfunction contains the constant and then multiplier list for the function to be maximised
! constraints is in the same format, with a row for each constraints (row, col)
! solution contains the optimal solution

! input/output variables
double precision, dimension(:) :: maxfunction
double precision, dimension(:,:) :: constraints
double precision, dimension(:) :: solution
! simplx variables
double precision, dimension(:,:), allocatable :: a
integer, dimension(:), allocatable :: izrov, iposv
integer :: m,n,mp,np,m1,m2,m3,icase
! local variables
integer :: nn, mm
logical :: debug = .false.

! provide explicit interface for these f77 routines
interface
  subroutine simplx(a,m,n,mp,np,m1,m2,m3,icase,izrov,iposv)
    INTEGER icase,m,m1,m2,m3,mp,n,np,iposv(m),izrov(n)
    DOUBLE PRECISION a(mp,np)
  end subroutine simplx
end interface

if (debug) write(*,'(80(1h+)/a)') 'subroutine numerical_recipes_simplex'

! setup variables
m = ubound(constraints,1)
mp = m + 2
np = ubound(maxfunction,1)
if (ubound(constraints,2) /= np) then
  write(*,*) 'ERROR: problem with constraint/maxfunction matrix in numerical_recipes_simplex'
  stop
end if
n = np - 1
if (ubound(solution,1) /= n) then
  write(*,*) 'ERROR: problem with solution matrix in numerical_recipes_simplex'
  stop
end if
m1 = 0
m2 = 0
m3 = m
allocate(a(mp,np))
a(1,:) = maxfunction
a(2:m+1,:) = constraints(:,:)
allocate(iposv(m),izrov(n))

if (debug) then
  write(*,*) 'm,n,mp,np,m1,m2,m3'
  write(*,*) m,n,mp,np,m1,m2,m3
  write(*,*) "a matrix on input:"
  do nn = 1, ubound(a,1)
    write(*,*) (a(nn,mm),mm=1,ubound(a,2))
  end do
end if

call simplx(a,m,n,mp,np,m1,m2,m3,icase,izrov,iposv)
!      SUBROUTINE simplx(a,m,n,mp,np,m1,m2,m3,icase,izrov,iposv)

if (icase /= 0) then
  write(*,*) 'ERROR: simplex algorithm not able to find a solution in numerical_recipes_simplex'
  stop
end if

if (debug) then
  write(*,*) "a matrix on output:"
  do nn = 1, ubound(a,1)
    write(*,*) (a(nn,mm),mm=1,ubound(a,2))
  end do
end if

! extract solution

solution = 0.d0
do mm = 1, m
  if (debug) write(*,*) 'mm = ',mm,': iposv(mm) = ',iposv(mm),': a(mm+1,1) = ',a(mm+1,1)
  if (iposv(mm) <= n) then
    solution(iposv(mm)) = a(mm+1,1)
  end if
end do

if (debug) then
  write(*,*) "solution:"
  do nn = 1, ubound(solution,1)
    write(*,*) solution(nn)
  end do
end if

deallocate (a,iposv,izrov)

if (debug) write(*,'(a/80(1h-))') 'subroutine numerical_recipes_simplex'

end subroutine numerical_recipes_simplex

!-----------------------------------------------------------------

end module numerical_recipes_module
!-----------------------------------------------------------------
