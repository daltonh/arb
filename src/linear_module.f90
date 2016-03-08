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

stop
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
        call push_integer_array(array=equation_from_unknown(ppu)%equation_pp,new_element=ppe)
      else
        equation_from_unknown(ppu)%equation_m(equation_from_unknown(ppu)%nequation)=m
        equation_from_unknown(ppu)%equation_ns(equation_from_unknown(ppu)%nequation)=ns
        equation_from_unknown(ppu)%within_equation_index(equation_from_unknown(ppu)%nequation)=pppu
        equation_from_unknown(ppu)%equation_pp(equation_from_unknown(ppu)%nequation)=ppe
      end if
    end do
  end do
end do

if (debug_sparse) write(*,'(a/80(1h-))') 'subroutine calc_equation_from_unknown'

end subroutine calc_equation_from_unknown

!-----------------------------------------------------------------

subroutine calc_multigrid

! here we create/update the multigrid structure

use general_module

integer :: nmultigrid, ncells, ppu_next, ppu_unallocated, ppu_last, pppe, ppe, m, ns, pppu, ppu, nm, ng, pp
integer, dimension(:), allocatable :: unknowns_marker
double precision :: coefficient_strength, next_coefficient_strength
logical :: debug_sparse = .true.
logical, parameter :: debug = .true.

if (debug) debug_sparse = .true.
if (debug_sparse) write(*,'(80(1h+)/a)') 'subroutine calc_multigrid'

! first create structure of multigrid array, calculating the number of levels
if (.not.allocated(multigrid)) then
! nmultigrid = 1 + ceiling(log(dble(ptotal))/log(dble(2.d0)))
! to avoid roundoff errors etc, calculate the number of levels by calculating the series explicitly
  ncells = 1
  nmultigrid = 1
  do while (ncells <= ptotal)
    nmultigrid = nmultigrid + 1
    ncells = ncells*2
  end do
  if (debug_sparse) write(*,'(a,i2,2(a,i10))') 'INFO: calculated nmultigrid = ',nmultigrid,' based on ptotal = ', &
    ptotal,' and ncells = ',ncells
  allocate(multigrid(nmultigrid))
! multigrid(1) contains all of the unknowns, as one single grid, and initialise
  allocate(multigrid(1)%grid(1))
  allocate(multigrid(1)%grid(1)%unknown_elements(ptotal))
end if

! next create multigrid(1)%grid(1) grid, which contains all unknown elements, in an order in which neighboring elements are strongly related
! zero previous results
multigrid(1)%grid(1)%unknown_elements = 0
multigrid(1)%grid(1)%nunknown_elements = 0
! set starting point
ppu_next = 1 ! this is the first unknown to be allocated
ppu_unallocated = 2 ! so the first unallocated unknown is the next one, ie, 2
! set first elements of arrays using these
multigrid(1)%grid(1)%unknown_elements(1) = ppu_next
multigrid(1)%grid(1)%nunknown_elements = 1
! create a temporary array which we use as a marker to say whether elements have been included or not
allocate(unknowns_marker(ptotal))
unknowns_marker = 0
unknowns_marker(ppu_next) = 1 ! unknown ppu_next is our starting point
if (debug_sparse) write(*,'(a)') &
  'INFO: setting up multigrid(1) which contains all unknowns in a single neighbourly-related list'
! loop through the number of unknowns
do while (multigrid(1)%grid(1)%nunknown_elements < ptotal)
! object here is to find ppu_next, so that it can be added to multigrid(1)%grid(1)%unknown_elements
  ppu_last = ppu_next ! save last added ppu
  if (debug) then
    write(92,'(a)') repeat('_',80)
    write(92,'(a,i10)') 'multigrid(1)%grid(1)%nunknown_elements = ',multigrid(1)%grid(1)%nunknown_elements
    write(92,'(a,i10)') 'ppu_last = ',ppu_last
    write(92,'(a,i10)') 'ppu_unallocated = ',ppu_unallocated
  end if
  ppu_next = 0 ! zero this to indicate that it isn't allocated
  next_coefficient_strength = -1.d0
! loop through all equations that this last unknown ppu_last references
  do pppe = 1, equation_from_unknown(ppu_last)%nequation
    ppe = equation_from_unknown(ppu_last)%equation_pp(pppe) ! equation pp number
    m = equation_from_unknown(ppu_last)%equation_m(pppe) ! equation var m
    ns = equation_from_unknown(ppu_last)%equation_ns(pppe) ! and corresponding equation region index ns
    if (debug) write(92,'(a,i10,a,i4,a,i10)') &
      'found equation referenced by ppu_last: ppe = ',ppe,': m = ',m,': ns = ',ns
! now loop through all unknowns that this equation references, finding unknown that has the strongest relationship with ppu_last, indicated by the product of their jacobian entries
    do pppu = 1, var(m)%funk(ns)%ndv
      ppu = var(m)%funk(ns)%pp(pppu) ! unknown pp number to be checked for coefficient strength
      if (debug) write(92,'(2(a,i10),a,i1)') 'ppu = ',ppu,': ppu_next = ',ppu_next,': unknowns_marker(ppu) = ', &
        unknowns_marker(ppu) 
      if (unknowns_marker(ppu) /= 0) cycle ! if this unknown is already in the list, then move on
      coefficient_strength = abs(var(m)%funk(ns)%dv(equation_from_unknown(ppu_last)%within_equation_index(pppe))* &
        var(m)%funk(ns)%dv(pppu))
      if (debug) write(92,'(2(a,g14.7))') 'coefficient_strength = ',coefficient_strength, &
        ': next_coefficient_strength = ',next_coefficient_strength
      if (coefficient_strength > next_coefficient_strength) then
        next_coefficient_strength = coefficient_strength
        ppu_next = ppu
      end if
      if (debug) write(92,'(a,i10)') 'after comparison ppu_next = ',ppu_next
    end do
  end do

! at this point there are two possibilities:
! 1) next_coefficient_strength > 0 and ppu_next is defined
! 2) ppu_next is not defined, meaning that all unknowns that are referenced by equations that our last unknown references have already been added to the list, so set ppu_next = ppu_unallocated

  if (debug) write(92,'(a,i10)') 'after searching for ppu_next: ppu_next = ',ppu_next
  if (ppu_next == 0) ppu_next = ppu_unallocated
    
! now add ppu_next to the list of allocated elements
  multigrid(1)%grid(1)%nunknown_elements = multigrid(1)%grid(1)%nunknown_elements + 1
  multigrid(1)%grid(1)%unknown_elements(multigrid(1)%grid(1)%nunknown_elements) = ppu_next
  unknowns_marker(ppu_next) = 1 ! mark this unknown as allocated
    
! now check that ppu_unallocated is infact, still unallocated
  if (ppu_next == ppu_unallocated) then
    ppu_unallocated = 0
    do pp = ppu_next+1, ptotal
      if (unknowns_marker(pp) == 0) then
        ppu_unallocated = pp
        exit
      end if
    end do
  end if

  if (debug) write(92,'(a,i10)') 'at end of loop: ppu_unallocated = ',ppu_unallocated

end do

if (debug) write(92,'(a,i1)') 'check - minimum of unknowns_marker should be 1 = ',minval(unknowns_marker)
deallocate(unknowns_marker)
      
! TODO : write out equation and unknown names and locations using ijkindicies
if (debug) then
  write(92,'(/a/a)') repeat('-',80),'PRINTING out multigrid details'
  do nm = 1, ubound(multigrid,1)
    if (.not.allocated(multigrid(nm)%grid)) then
      write(92,'(a,i4)') 'multigrid = ',nm,': has no grids allocated'
    else
      do ng = 1, ubound(multigrid(nm)%grid,1)
        if (.not.allocated(multigrid(nm)%grid(ng)%unknown_elements)) then
          write(92,'(a,i4,a,i10)') 'multigrid = ',nm,': grid = ',ng,': has no elements allocated'
        else
          do pppu = 1, multigrid(nm)%grid(ng)%nunknown_elements
            ppu = multigrid(nm)%grid(ng)%unknown_elements(pppu)
            write(92,'(a,i4,2(a,i10))') 'multigrid = ',nm,': grid = ',ng,': ppu = ',ppu
          end do
        end if
      end do
    end if
  end do
end if

if (debug_sparse) write(*,'(a/80(1h-))') 'subroutine calc_multigrid'

end subroutine calc_multigrid

!-----------------------------------------------------------------

end module linear_module
!-----------------------------------------------------------------
