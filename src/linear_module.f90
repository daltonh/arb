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

! this is an object that stores details of the equations that each unknown references
type equation_from_unknown_type
  integer, dimension(:), allocatable :: equation_pp ! raw equation number that this unknown refers to
  integer, dimension(:), allocatable :: equation_m ! m (var number) for this equation
  integer, dimension(:), allocatable :: equation_ns ! element number within the region corresponding to this equation
  integer, dimension(:), allocatable :: within_equation_index ! element of dv that this unknown refers to, within var(m)%funk(ns)%dv
  integer :: nequation ! the number of valid elements within all of these arrays, which all have the same size and corresponding elements (all include but do not decrease in size during a computation, to reduce memory allocation/array copying events)
end type equation_from_unknown_type

type (equation_from_unknown_type), dimension(:), allocatable, save :: equation_from_unknown ! array specifying an equation from unknown lookup structure

! this is an object which stores information about an individual grid (of unknowns), which becomes part of the multigrid array
type grid_type
  integer, dimension(:), allocatable :: unknown_elements ! stores a list of the (pp) unknown indices contained with this particular grid
  integer :: nunknown_elements ! the number of current elements in unknown_elements, noting that we only increase the size of unknown_elements presently to save computational time
  double precision, dimension(:), allocatable :: delff ! values for the vector ff array, stored in a compact format
  integer, dimension(:), allocatable :: delff_elements ! in a one-to-one correspondance with delff, specifies what elements each value of delff refers to
  integer :: ndelff ! current number of valid elements in delff
end type grid_type

! inidividual grids of unknowns corresponding to a particular level are grouped together within one multigrid
type multigrid_type
  type(grid_type), dimension(:), allocatable :: grid ! an array of grids contained within the particular multigrid level
end type multigrid_type
  
type (multigrid_type), dimension(:), allocatable :: multigrid ! an array of of the multigrid levels
  
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

! now create (multi)grids, stored in multigrid array
call calc_multigrid

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

integer :: ppe, mm, m, ns, pppu, ppu
logical :: debug_sparse = .true.

if (debug_sparse) write(*,'(80(1h+)/a)') 'subroutine calc_equation_from_unknown'

! first create structure
if (.not.allocated(equation_from_unknown)) allocate(equation_from_unknown(ptotal))

! always zero valid number of elements (noting that equation_index array sizes only expand, and are never deallocated)
do ppu = 1, ptotal
  equation_from_unknown(ppu)%nequation = 0
end do

! loop through each equation, building reverse lookup lists
ppe = 0
do mm = 1, allocatable_size(var_list(var_list_number_equation)%list)
  m = var_list(var_list_number_equation)%list(mm)
  do ns = 1, ubound(var(m)%funk,1)
    ppe = ppe + 1 ! this is the equation pp number
    do pppu = 1, var(m)%funk(ns)%ndv ! here we cycle through all the unknowns that are referenced within this equation
      ppu = var(m)%funk(ns)%pp(pppu) ! this is the unknown pp number
      equation_from_unknown(ppu)%nequation = equation_from_unknown(ppu)%nequation + 1 ! increase the valid number of elements in the lookup array
      if (allocatable_integer_size(equation_from_unknown(ppu)%equation_pp) < equation_from_unknown(ppu)%nequation) then
        call push_integer_array(array=equation_from_unknown(ppu)%equation_m,new_element=m)
        call push_integer_array(array=equation_from_unknown(ppu)%equation_ns,new_element=ns)
        call push_integer_array(array=equation_from_unknown(ppu)%within_equation_index,new_element=pppu)
        call push_integer_array(array=equation_from_unknown(ppu)%equation_pp,new_element=pppe)
      else
        equation_from_unknown(ppu)%equation_m(equation_from_unknown(ppu)%nequation_index)=m
        equation_from_unknown(ppu)%equation_ns(equation_from_unknown(ppu)%nequation_index)=ns
        equation_from_unknown(ppu)%within_equation_index(equation_from_unknown(ppu)%nequation_index)=pppu
        equation_from_unknown(ppu)%equation_pp(equation_from_unknown(ppu)%nequation_index)=pppe
      end if
    end do
  end do
end do

if (debug_sparse) write(*,'(a/80(1h-))') 'subroutine calc_equation_from_unknown'

end subroutine calc_equation_from_unknown

!-----------------------------------------------------------------

! this is an object which stores information about an individual grid (of unknowns), which becomes part of the multigrid array
type grid_type
  integer, dimension(:), allocatable :: unknown_elements ! stores a list of the (pp) unknown indices contained with this particular grid
  integer :: nunknown_elements ! the number of current elements in unknown_elements, noting that we only increase the size of unknown_elements presently to save computational time
  double precision, dimension(:), allocatable :: delff ! values for the vector ff array, stored in a compact format
  integer, dimension(:), allocatable :: delff_elements ! in a one-to-one correspondance with delff, specifies what elements each value of delff refers to
  integer :: ndelff ! current number of valid elements in delff
end type grid_type

! inidividual grids of unknowns corresponding to a particular level are grouped together within one multigrid
type multigrid_type
  type(grid_type), dimension(:), allocatable :: grid ! an array of grids contained within the particular multigrid level
end type multigrid_type
  
type (multigrid_type), dimension(:), allocatable :: multigrid ! an array of of the multigrid levels




subroutine calc_multigrid

! here we create/update the multigrid structure

use general_module

integer :: nmultigrid, ncells
logical :: debug_sparse = .true.

if (debug_sparse) write(*,'(80(1h+)/a)') 'subroutine calc_multigrid'

! first create structure, calculating the number of levels
if (.not.allocated(multigrid)) then
! nmultigrid = 1 + ceiling(log(dble(ptotal))/log(dble(2.d0)))
! to avoid roundoff errors etc, calculate the number of levels by calculating the series explicitly
  ncells = 1
  nmultigrid = 1
  do while (ncells <= ptotal)
    nmultigrid = nmultigrid + 1
    ncells = ncells*2
  end do
  allocate(multigrid(nmultigrid))
! multigrid(1) contains all of the unknowns, as one single grid, and initialise
  allocate(multigrid(1)%grid(1))
  allocate(multigrid(1)%grid(1)%unknown_elements(ptotal))
  multigrid(1)%grid(1)%unknown_elements = 0
  multigrid(1)%grid(1)%nunknown_elements = 0
end do

! create multigrid(1)%grid(1) grid first, which contains all unknown elements, in an order in which neighboring elements are strongly related
multigrid(1)%grid(1)%unknown_elements(1) = 1
multigrid(1)%grid(1)%nunknown_elements = 1
allocate(unknowns_marker(ptotal)) ! create a temporary array which we use as a marker to say whether elements have been included or not, and the index of their inclusion
unknowns_marker = 0
unknowns_marker(1) = 1 ! unknown (pp) 1 is our starting point
last_unallocated_unknown = 2 ! so the first unallocated unknown is the next one, ie, 2
do while (multigrid(1)%grid(1)%nunknown_elements < ptotal)
  ppu_next = 0
  next_coefficient_strength = -1.d0
  ppu_last = multigrid(1)%grid(1)%unknown_elements(multigrid(1)%grid(1)%nunknown_elements)
  do pppe = 1, equation_from_unknown(ppu_last)%nequation ! loop through all equations that this last unknown references
    ppe = equation_from_unknown(ppu_last)%equation_pp(pppe) ! equation pp number
    m = equation_from_unknown(ppu_last)%equation_m(ppe)
    ns = equation_from_unknown(ppu_last)%equation_ns(ppe)
    do pppu = 1, var(m)%funk(ns)%ndv
      ppu = var(m)%funk(ns)%pp(pppu) ! unknown pp number
      if (unknowns_marker(ppu) /= 0) next
      coefficient_strength = abs(var(m)%funk(ns)%dv(equation_from_unknown(ppu_last)%within_equation_index(pppe))* &
        var(m)%funk(ns)%dv(pppu))
      if (coefficient_strength > next_coefficient_strength) then
        next_coefficient_strength = coefficient_strength
        ppu_next = ppu
      end if
    end do
  end do

! at this point there are two possibilities:
! 1) next_coefficient_strength > 0 and ppu_next is defined
! 2) ppu_next is not defined, meaning that all unknowns that are referenced by equations that our last unknown references have already been added to the list
! in this case loop through unknowns marker looking for unallocated elements

end do

deallocate(unknowns_marker)
      

end do


if (debug_sparse) write(*,'(a/80(1h-))') 'subroutine calc_multigrid'

end subroutine calc_multigrid

!-----------------------------------------------------------------

end module linear_module
!-----------------------------------------------------------------
