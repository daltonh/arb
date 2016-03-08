! file src/linear_module.f90
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
! this module contains homegrown linear solvers
module linear_module

implicit none
private
public iterative_mainsolver, multigrid_mainsolver

! this is an object that stores the equation indices that each unknown references
type equation_from_unknown_type
  integer, dimension(:), allocatable :: equation_index ! an equation that uses this unknown
  integer, dimension(:), allocatable :: unknown_within_equation_index ! having a one-to-one correspondance with equation_index, gives the element index within the equation array that corresponds to this unknown
  integer :: nequation_index ! the number of valid elements within equation_index (equation_index expands, but does not decrease in size during a computation, to reduce memory allocation/array copying events)
end type equation_from_unknown_type

type (equation_from_unknown_type), dimension(:), allocatable, save :: equation_from_unknown ! array specifying an equation from unknown lookup structure

!-----------------------------------------------------------------
contains

!-----------------------------------------------------------------

subroutine iterative_mainsolver(ierror)

! here we use a homegrown iterative technique to solve the linear system, using equation funk data directly

use general_module
use equation_module

integer :: nn, m, ns, i, j, iterstep, ierror, ii
double precision :: alpha_ff, beta_ff, iterres, iterres_old, lambda_ff, lambda_used
character(len=1000) :: formatline
double precision, allocatable, dimension(:) :: deldelphi, ff, alpha_delphi, beta_delphi, ff_m, delff
logical :: singular
logical, parameter :: matrix_test = .false. ! do a test matrix inversion using a made-up test matrix instead of solving PDE system
logical, parameter :: dump_matrix = .false. ! dump contents and solution to matrix in fort.91
integer, parameter :: dump_matrix_max = 1000 ! maximum number of elements to include when dumping matrix
logical, parameter :: debug = .false.
logical :: debug_sparse = .true.

if (debug) debug_sparse = .true.
if (debug_sparse) write(*,'(80(1h+)/a)') 'subroutine iterative_mainsolver'

ierror = 1 ! this signals an error

! check on allocations
if (.not.allocated(deldelphi)) then
  allocate(deldelphi(ptotal),ff(ptotal),delff(ptotal),alpha_delphi(ptotal),beta_delphi(ptotal),ff_m(ptotal))
end if

! set initial ff array (linear equation error array) from newton's equations
! also set residual multiplier, ff_m
j = 0
do nn = 1, allocatable_size(var_list(var_list_number_equation)%list)
  m = var_list(var_list_number_equation)%list(nn)
  do ns = 1, ubound(var(m)%funk,1)
    j = j + 1
    ff(j) = var(m)%funk(ns)%v
    ff_m(j) = 1.d0/(var(m)%magnitude**2)
  end do
end do

! calculate initial residual
iterres_old = ff_residual(ff_m,ff)
iterres = iterres_old

! calculate alpha_delphi
alpha_delphi = 0
j = 0
do nn = 1, allocatable_size(var_list(var_list_number_equation)%list)
  m = var_list(var_list_number_equation)%list(nn)
  do ns = 1, ubound(var(m)%funk,1)
    j = j + 1
    do ii = 1, var(m)%funk(ns)%ndv
      i = var(m)%funk(ns)%pp(ii)
      alpha_delphi(i) = alpha_delphi(i) + var(m)%funk(ns)%dv(ii)**2*ff_m(j)
    end do
  end do
end do

if (debug_sparse) then
  write(*,'(a,i12,1(a,g14.7))') "ITERATIONS:        start iterstep = ",0,": iterres = ",iterres
  if (convergence_details_file) &
    write(fconverge,'(a,i12,1(a,g14.7))') "ITERATIONS:        start iterstep = ",0,": iterres = ",iterres
end if

! start iteration loop
iteration_loop: do iterstep = 1, iterstepmax

  if (.true.) then
  ! calculate beta_delphi
    beta_delphi = 0
    j = 0
    do nn = 1, allocatable_size(var_list(var_list_number_equation)%list)
      m = var_list(var_list_number_equation)%list(nn)
      do ns = 1, ubound(var(m)%funk,1)
        j = j + 1
        do ii = 1, var(m)%funk(ns)%ndv
          i = var(m)%funk(ns)%pp(ii)
          beta_delphi(i) = beta_delphi(i) + var(m)%funk(ns)%dv(ii)*ff(j)*ff_m(j)
        end do
      end do
    end do
    
  ! calculate change to delphi, deldelphi
    do i = 1, ptotal
      if (abs(alpha_delphi(i)) < 1.d-60) call error_stop("problem alpha_delphi(i)")
      deldelphi(i) = -beta_delphi(i)/alpha_delphi(i)
    end do
  else
! left-field random update for deldelphi
    do i = 1, ptotal
      deldelphi(i) = random()
    end do
  end if

! calculate corresponding change to ff
! and at the same time, calculate the alpha_ff and beta_ff factors required to calculate lambda_ff
  delff = 0
  alpha_ff = 0
  beta_ff = 0
  j = 0
  do nn = 1, allocatable_size(var_list(var_list_number_equation)%list)
    m = var_list(var_list_number_equation)%list(nn)
    do ns = 1, ubound(var(m)%funk,1)
      j = j + 1
      do ii = 1, var(m)%funk(ns)%ndv
        i = var(m)%funk(ns)%pp(ii)
        delff(j) = delff(j) + var(m)%funk(ns)%dv(ii)*deldelphi(i)
      end do
      alpha_ff = alpha_ff + delff(j)**2*ff_m(j)
      beta_ff = beta_ff + delff(j)*ff(j)*ff_m(j)
    end do
  end do
  if (abs(alpha_ff) < 1.d-60) call error_stop("problem alpha_ff")
  lambda_ff = -beta_ff/alpha_ff

! update ff, delpha and residual f
  do j = 1, ptotal
    ff(j) = ff(j) + lambda_ff*delff(j)
    delphi(j) = delphi(j) + lambda_ff*deldelphi(j)
  end do

  iterres = ff_residual(ff_m,ff)

  if (debug_sparse) then
    if (mod(iterstep,iterstepcheck) == 0) then
      write(*,'(a,i12,3(a,g14.7))') "ITERATIONS: intermediate iterstep = ",iterstep,": iterres = ",iterres,": 1-iterres/iterres_old = ", &
        1.d0-iterres/iterres_old,": lambda_ff = ",lambda_ff
      if (convergence_details_file) &
        write(fconverge,'(a,i12,3(a,g14.7))') &
          "ITERATIONS: intermediate iterstep = ",iterstep,": iterres = ",iterres,": 1-iterres/iterres_old = ", &
          1.d0-iterres/iterres_old,": lambda_ff = ",lambda_ff
    end if
  end if

  if (iterres/iterres_old < iterrestol) then
    ierror = 0
    exit iteration_loop
  end if

  if (mod(iterstep,iterstepcheck) == 0) then
    if (check_stopfile("stopback")) then
      write(*,'(a)') 'INFO: user requested simulation stop via "kill" file'
      ierror = 2
      exit iteration_loop
    end if
  end if

end do iteration_loop
  
if (ierror == 0) then
  if (debug_sparse) then
    write(*,'(a,i12,3(a,g14.7))') "ITERATIONS: convered iterstep = ",iterstep,": iterres = ",iterres,": iterres/iterres_old = ", &
      iterres/iterres_old,": lambda_ff = ",lambda_ff
    if (convergence_details_file) &
      write(fconverge,'(a,i12,3(a,g14.7))') &
        "ITERATIONS: converged iterstep = ",iterstep,": iterres = ",iterres,": iterres/iterres_old = ", &
        iterres/iterres_old,": lambda_ff = ",lambda_ff
  end if
else
  write(*,'(a,i12,3(a,g14.7))') "ITERATIONS WARNING: failed iterstep = ",iterstep,": iterres = ",iterres,": iterres/iterres_old = ", &
    iterres/iterres_old,": lambda_ff = ",lambda_ff
  if (convergence_details_file) &
    write(fconverge,'(a,i12,3(a,g14.7))') &
      "ITERATIONS WARNING: failed iterstep = ",iterstep,": iterres = ",iterres,": iterres/iterres_old = ", &
      iterres/iterres_old,": lambda_ff = ",lambda_ff
end if

if (debug_sparse) write(*,'(a/80(1h-))') 'subroutine iterative_mainsolver'

end subroutine iterative_mainsolver

!-----------------------------------------------------------------

double precision function ff_residual(ff_m,ff)

use general_module
double precision, allocatable, dimension(:) :: ff ! the array of linear equation errors
double precision, allocatable, dimension(:) :: ff_m ! a multiplier for each equation
integer :: j

ff_residual = 0.d0
do j = 1, ubound(ff,1)
  ff_residual = ff_residual + ff(j)**2*ff_m(j)
end do
ff_residual = sqrt(max(ff_residual,0.d0)/dble(max(ptotal,1)))

end function ff_residual

!-----------------------------------------------------------------

subroutine multigrid_mainsolver(ierror)

! here we use a homegrown multigrid technique to solve the linear system, using equation funk data directly

use general_module
use equation_module

integer :: nn, m, ns, i, j, iterstep, ierror, ii
double precision :: alpha_ff, beta_ff, iterres, iterres_old, lambda_ff, lambda_used
character(len=1000) :: formatline
double precision, allocatable, dimension(:) :: deldelphi, ff, alpha_delphi, beta_delphi, ff_m, delff
logical :: singular
logical, parameter :: matrix_test = .false. ! do a test matrix inversion using a made-up test matrix instead of solving PDE system
logical, parameter :: dump_matrix = .false. ! dump contents and solution to matrix in fort.91
integer, parameter :: dump_matrix_max = 1000 ! maximum number of elements to include when dumping matrix
logical, parameter :: debug = .false.
logical :: debug_sparse = .true.

if (debug) debug_sparse = .true.
if (debug_sparse) write(*,'(80(1h+)/a)') 'subroutine multigrid_mainsolver'

ierror = 1 ! this signals an error

! first create reverse lookup table of equations from unknowns
call calc_equation_from_unknown


!---------------------
!---------------------


! check on allocations
if (.not.allocated(deldelphi)) then
  allocate(deldelphi(ptotal),ff(ptotal),delff(ptotal),alpha_delphi(ptotal),beta_delphi(ptotal),ff_m(ptotal))
end if

! set initial ff array (linear equation error array) from newton's equations
! also set residual multiplier, ff_m
j = 0
do nn = 1, allocatable_size(var_list(var_list_number_equation)%list)
  m = var_list(var_list_number_equation)%list(nn)
  do ns = 1, ubound(var(m)%funk,1)
    j = j + 1
    ff(j) = var(m)%funk(ns)%v
    ff_m(j) = 1.d0/(var(m)%magnitude**2)
  end do
end do

! calculate initial residual
iterres_old = ff_residual(ff_m,ff)
iterres = iterres_old

! calculate alpha_delphi
alpha_delphi = 0
j = 0
do nn = 1, allocatable_size(var_list(var_list_number_equation)%list)
  m = var_list(var_list_number_equation)%list(nn)
  do ns = 1, ubound(var(m)%funk,1)
    j = j + 1
    do ii = 1, var(m)%funk(ns)%ndv
      i = var(m)%funk(ns)%pp(ii)
      alpha_delphi(i) = alpha_delphi(i) + var(m)%funk(ns)%dv(ii)**2*ff_m(j)
    end do
  end do
end do

if (debug_sparse) then
  write(*,'(a,i12,1(a,g14.7))') "ITERATIONS:        start iterstep = ",0,": iterres = ",iterres
  if (convergence_details_file) &
    write(fconverge,'(a,i12,1(a,g14.7))') "ITERATIONS:        start iterstep = ",0,": iterres = ",iterres
end if

! start iteration loop
iteration_loop: do iterstep = 1, iterstepmax

  if (.true.) then
  ! calculate beta_delphi
    beta_delphi = 0
    j = 0
    do nn = 1, allocatable_size(var_list(var_list_number_equation)%list)
      m = var_list(var_list_number_equation)%list(nn)
      do ns = 1, ubound(var(m)%funk,1)
        j = j + 1
        do ii = 1, var(m)%funk(ns)%ndv
          i = var(m)%funk(ns)%pp(ii)
          beta_delphi(i) = beta_delphi(i) + var(m)%funk(ns)%dv(ii)*ff(j)*ff_m(j)
        end do
      end do
    end do
    
  ! calculate change to delphi, deldelphi
    do i = 1, ptotal
      if (abs(alpha_delphi(i)) < 1.d-60) call error_stop("problem alpha_delphi(i)")
      deldelphi(i) = -beta_delphi(i)/alpha_delphi(i)
    end do
  else
! left-field random update for deldelphi
    do i = 1, ptotal
      deldelphi(i) = random()
    end do
  end if

! calculate corresponding change to ff
! and at the same time, calculate the alpha_ff and beta_ff factors required to calculate lambda_ff
  delff = 0
  alpha_ff = 0
  beta_ff = 0
  j = 0
  do nn = 1, allocatable_size(var_list(var_list_number_equation)%list)
    m = var_list(var_list_number_equation)%list(nn)
    do ns = 1, ubound(var(m)%funk,1)
      j = j + 1
      do ii = 1, var(m)%funk(ns)%ndv
        i = var(m)%funk(ns)%pp(ii)
        delff(j) = delff(j) + var(m)%funk(ns)%dv(ii)*deldelphi(i)
      end do
      alpha_ff = alpha_ff + delff(j)**2*ff_m(j)
      beta_ff = beta_ff + delff(j)*ff(j)*ff_m(j)
    end do
  end do
  if (abs(alpha_ff) < 1.d-60) call error_stop("problem alpha_ff")
  lambda_ff = -beta_ff/alpha_ff

! update ff, delpha and residual f
  do j = 1, ptotal
    ff(j) = ff(j) + lambda_ff*delff(j)
    delphi(j) = delphi(j) + lambda_ff*deldelphi(j)
  end do

  iterres = ff_residual(ff_m,ff)

  if (debug_sparse) then
    if (mod(iterstep,iterstepcheck) == 0) then
      write(*,'(a,i12,3(a,g14.7))') "ITERATIONS: intermediate iterstep = ",iterstep,": iterres = ",iterres,": 1-iterres/iterres_old = ", &
        1.d0-iterres/iterres_old,": lambda_ff = ",lambda_ff
      if (convergence_details_file) &
        write(fconverge,'(a,i12,3(a,g14.7))') &
          "ITERATIONS: intermediate iterstep = ",iterstep,": iterres = ",iterres,": 1-iterres/iterres_old = ", &
          1.d0-iterres/iterres_old,": lambda_ff = ",lambda_ff
    end if
  end if

  if (iterres/iterres_old < iterrestol) then
    ierror = 0
    exit iteration_loop
  end if

  if (mod(iterstep,iterstepcheck) == 0) then
    if (check_stopfile("stopback")) then
      write(*,'(a)') 'INFO: user requested simulation stop via "kill" file'
      ierror = 2
      exit iteration_loop
    end if
  end if

end do iteration_loop
  
if (ierror == 0) then
  if (debug_sparse) then
    write(*,'(a,i12,3(a,g14.7))') "ITERATIONS: convered iterstep = ",iterstep,": iterres = ",iterres,": iterres/iterres_old = ", &
      iterres/iterres_old,": lambda_ff = ",lambda_ff
    if (convergence_details_file) &
      write(fconverge,'(a,i12,3(a,g14.7))') &
        "ITERATIONS: converged iterstep = ",iterstep,": iterres = ",iterres,": iterres/iterres_old = ", &
        iterres/iterres_old,": lambda_ff = ",lambda_ff
  end if
else
  write(*,'(a,i12,3(a,g14.7))') "ITERATIONS WARNING: failed iterstep = ",iterstep,": iterres = ",iterres,": iterres/iterres_old = ", &
    iterres/iterres_old,": lambda_ff = ",lambda_ff
  if (convergence_details_file) &
    write(fconverge,'(a,i12,3(a,g14.7))') &
      "ITERATIONS WARNING: failed iterstep = ",iterstep,": iterres = ",iterres,": iterres/iterres_old = ", &
      iterres/iterres_old,": lambda_ff = ",lambda_ff
end if

if (debug_sparse) write(*,'(a/80(1h-))') 'subroutine multigrid_mainsolver'

end subroutine multigrid_mainsolver

!-----------------------------------------------------------------

subroutine calc_equation_from_unknown

! here we create/update the equation from unknown lookup structure

use general_module

integer :: n, nn, m, ns, ppp, pp
logical :: debug_sparse = .true.

if (debug_sparse) write(*,'(80(1h+)/a)') 'subroutine calc_equation_from_unknown'

! first create structure
if (.not.allocated(equation_from_unknown)) allocate(equation_from_unknown(ptotal))

! always zero valid number of elements (noting that equation_index array sizes only expand, and are never deallocated)
do n = 1, ptotal
  equation_from_unknown(n)%nequation_index = 0
end do

! loop through each equation, building reverse lookup lists
n = 0
do nn = 1, allocatable_size(var_list(var_list_number_equation)%list)
  m = var_list(var_list_number_equation)%list(nn)
  do ns = 1, ubound(var(m)%funk,1)
    n = n + 1 ! this is the equation number
    do ppp = 1, ubound(var(m)%funk(ns)%pp,1) ! here we cycle through all the unknowns that are referenced within this equation
      pp = var(m)%funk(ns)%pp(ppp) ! this is the unknown number
      equation_from_unknown(pp)%nequation_index = equation_from_unknown(pp)%nequation_index + 1 ! increase the valid number of elements in the lookup array
      if (allocatable_integer_size(equation_from_unknown(pp)%equation_index) < equation_from_unknown(pp)%nequation_index) then
        call push_integer_array(array=equation_from_unknown(pp)%equation_index,new_element=n)
        call push_integer_array(array=equation_from_unknown(pp)%unknown_within_equation_index,new_element=ppp)
      else
        equation_from_unknown(pp)%equation_index(equation_from_unknown(pp)%nequation_index)=n
        equation_from_unknown(pp)%unknown_within_equation_index(equation_from_unknown(pp)%nequation_index)=ppp
      end if
    end do
  end do
end do

if (debug_sparse) write(*,'(a/80(1h-))') 'subroutine calc_equation_from_unknown'

end subroutine calc_equation_from_unknown

!-----------------------------------------------------------------

end module linear_module
!-----------------------------------------------------------------
