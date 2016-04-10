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
public iterative_mainsolver, multigrid_mainsolver, bicg_mainsolver, quasinewton_mainsolver

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
  integer, dimension(:), allocatable :: delff_elements ! in a one-to-one correspondance with delff, specifies what elements (ppe) each value of delff refers to
  integer :: ndelff ! current number of valid elements in delff
  double precision :: minus_reciprocal_delff_dot_delff ! what it says, the reciprocal of the mag^2 of delff, x -1
end type grid_type

! inidividual grids of unknowns corresponding to a particular level are grouped together within one multigrid
type multigrid_type
  type(grid_type), dimension(:), allocatable :: grid ! an array of grids contained within the particular multigrid level
  integer :: ngrid ! number of grids in this level = ubound(grid,1)
end type multigrid_type
  
type (multigrid_type), dimension(:), allocatable :: multigrid ! an array of of the multigrid levels

double precision, dimension(:), allocatable, save :: e_scale ! scaling factor when normalising the equations for some iterative methods

integer, save :: nmultigrid ! number of levels within the multigrid structure, calculated when the multigrid structure is calculated
  
!-----------------------------------------------------------------
contains

!-----------------------------------------------------------------

subroutine iterative_mainsolver(ierror)

! here we use a homegrown iterative technique to solve the linear system, using equation funk data directly

use general_module
use equation_module

integer :: nn, m, ns, i, j, iterstep, ierror, ii
double precision :: alpha_ff, beta_ff, iterres, iterres_old, lambda_ff, lambda_used
double precision, allocatable, dimension(:) :: deldelphi, ff, alpha_delphi, beta_delphi, ff_m, delff
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
  write(*,'(a,i8,1(a,g14.7))') "ITERATIONS:        start iterstep = ",0,": iterres = ",iterres
  if (convergence_details_file) &
    write(fconverge,'(a,i8,1(a,g14.7))') "ITERATIONS:        start iterstep = ",0,": iterres = ",iterres
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
      write(*,'(a,i8,3(a,g14.7))') "ITERATIONS: intermediate iterstep = ",iterstep,": iterres = ",iterres,": 1-iterres/iterres_old = ", &
        1.d0-iterres/iterres_old,": lambda_ff = ",lambda_ff
      if (convergence_details_file) &
        write(fconverge,'(a,i8,3(a,g14.7))') &
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
    write(*,'(a,i8,3(a,g14.7))') "ITERATIONS: convered iterstep = ",iterstep,": iterres = ",iterres,": iterres/iterres_old = ", &
      iterres/iterres_old,": lambda_ff = ",lambda_ff
    if (convergence_details_file) &
      write(fconverge,'(a,i8,3(a,g14.7))') &
        "ITERATIONS: converged iterstep = ",iterstep,": iterres = ",iterres,": iterres/iterres_old = ", &
        iterres/iterres_old,": lambda_ff = ",lambda_ff
  end if
else
  write(*,'(a,i8,3(a,g14.7))') "ITERATIONS WARNING: failed iterstep = ",iterstep,": iterres = ",iterres,": iterres/iterres_old = ", &
    iterres/iterres_old,": lambda_ff = ",lambda_ff
  if (convergence_details_file) &
    write(fconverge,'(a,i8,3(a,g14.7))') &
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

subroutine multigrid_mainsolver(ierror,singlegrid)

! here we use a homegrown multigrid technique to solve the linear system, using equation funk data directly

use general_module
use equation_module

integer :: ierror, mm, m, ns, j, nl, ng, pppe, ppe, pppu, ppu, iterstep, nl_lower, pp
character(len=1000) :: formatline
double precision, dimension(:), allocatable :: deldelphi, ff, delff
double precision :: deldelphi_grid, iterres, iterres_old, delff2, ffdelff, lambda
logical :: singlegrid ! if true then only the low level grid is used
logical, parameter :: normalise = .true. ! to normalise the equations based upon maximum per row jacobian value
logical, parameter :: debug = .false.
logical :: debug_sparse = .true.

if (debug) debug_sparse = .true.
if (debug_sparse) write(*,'(80(1h+)/a)') 'subroutine multigrid_mainsolver'

ierror = 1 ! this signals an error

! create reverse lookup table of equations from unknowns
call calc_equation_from_unknown

! nondimensionalise equations and their derivatives (forward = .true. implies that we are nondimensionalising)
call nondimensionalise_equations(normalise,forward=.true.)

! now create (multi)grids, stored in multigrid array, with corresponding contained delff
call calc_multigrid

!---------------------
! initialise and allocate loop variables

! delphi is the global solution to the linear equations vector
! deldelphi is the update for delphi, nondimensional
! ff is the current (ie, final from previous loop) equation error array, nondimensional

if (.not.allocated(deldelphi)) allocate(deldelphi(ptotal),ff(ptotal),delff(ptotal))

delphi = 0.d0 ! for now this is nondimensionalised, but will dimensionalise before leaving the routine

! set initial ff array (linear equation error array) from newton's equations
j = 0
do mm = 1, allocatable_size(var_list(var_list_number_equation)%list)
  m = var_list(var_list_number_equation)%list(mm)
  do ns = 1, ubound(var(m)%funk,1)
    j = j + 1
    ff(j) = var(m)%funk(ns)%v
  end do
end do

!iterres = sqrt(dot_product(ff,ff)/dble(ptotal)) ! initialise the residual
iterres = iterres_calc(ff,normalise)

if (debug_sparse) then
  write(*,'(a,i8,1(a,g14.7))') "ITERATIONS:        start iterstep = ",0,": iterres = ",iterres
  if (convergence_details_file) &
    write(fconverge,'(a,i8,1(a,g14.7))') "ITERATIONS:        start iterstep = ",0,": iterres = ",iterres
end if

if (singlegrid) then
  nl_lower = nmultigrid
else
  nl_lower = 1
end if

! start iteration loop
iteration_loop: do iterstep = 1, iterstepmax

! loop through all grids, depending on whether called with multigrid or not
  do nl = nl_lower, nmultigrid

    deldelphi = 0.d0

! loop through all levels within this grid
    do ng = 1, multigrid(nl)%ngrid

! first calculate update to deldelphi for this grid
      deldelphi_grid = 0.d0
      do pppe = 1, multigrid(nl)%grid(ng)%ndelff
        ppe = multigrid(nl)%grid(ng)%delff_elements(pppe)
        deldelphi_grid = deldelphi_grid - multigrid(nl)%grid(ng)%delff(pppe)*ff(ppe)
      end do

! now apply this update to all delphis in this grid
      do pppu = 1, multigrid(nl)%grid(ng)%nunknown_elements
        ppu = multigrid(nl)%grid(ng)%unknown_elements(pppu)
        deldelphi(ppu) = deldelphi(ppu) + deldelphi_grid
      end do

    end do

! calculate the direction of change for ff based on this deldelphi
    delff = 0.d0 ! vector
    delff2 = 0.d0 ! scalar
    ffdelff = 0.d0 ! scalar
    ppe = 0
    do mm = 1, allocatable_size(var_list(var_list_number_equation)%list)
      m = var_list(var_list_number_equation)%list(mm)
      do ns = 1, ubound(var(m)%funk,1)
        ppe = ppe + 1
        do pppu = 1, var(m)%funk(ns)%ndv
          ppu = var(m)%funk(ns)%pp(pppu)
          delff(ppe) = delff(ppe) + var(m)%funk(ns)%dv(pppu)*deldelphi(ppu)
        end do
        delff2 = delff2 + delff(ppe)**2
        ffdelff = ffdelff + ff(ppe)*delff(ppe)
      end do
    end do

! now look at delff2 and ffdelff and then calculate lambda
!   if (delff2 < tiny(1.d0)) then
!     write(*,'(2(a,g14.7))') 'delff2 = ',delff2,': ffdelff = ',ffdelff
!     call error_stop('delff2 is too small in subroutine multigrid mainsolver')
!   end if

    if (delff2 < 1.d-60) then
      write(*,'(2(a,g14.7))') 'WARNING: delff2 is small, so setting lambda = 0.d0: delff2 = ',delff2,': ffdelff = ',ffdelff
      lambda = 0.d0
    else
      lambda = -ffdelff/delff2
      if (lambda < tiny(1.d0)) write(*,'(3(a,g14.7))') 'WARNING: lambda is small: delff2 = ',delff2, &
        ': ffdelff = ',ffdelff,': lambda = ',lambda
    end if

!   if (lambda < tiny(1.d0)) then
!     write(*,'(3(a,g14.7))') 'delff2 = ',delff2,': ffdelff = ',ffdelff,': lambda = ',lambda
!     call error_stop('lambda is very small or negative in subroutine multigrid mainsolver')
!   end if

! update delphi
    delphi = delphi + lambda*deldelphi

! calculate new ff and residual
    iterres_old = iterres
    if (.true.) then
! noroundoff brute-force method for calculating new ff and iterres
      j = 0
      do mm = 1, allocatable_size(var_list(var_list_number_equation)%list)
        m = var_list(var_list_number_equation)%list(mm)
        do ns = 1, ubound(var(m)%funk,1)
          j = j + 1
          ff(j) = var(m)%funk(ns)%v
          do pppu = 1, var(m)%funk(ns)%ndv
            ff(j) = ff(j) + var(m)%funk(ns)%dv(pppu)*delphi(var(m)%funk(ns)%pp(pppu))
          end do
        end do
      end do
    else
      ff = ff + lambda*delff
    end if
!   iterres = sqrt(dot_product(ff,ff)/dble(ptotal)) ! initialise the residual
    iterres = iterres_calc(ff,normalise)

!     write(*,'(2(a,i6),a,g14.7)') 'nl = ',nl,': ng = ',ng,': iterres = ',iterres
    
    if (debug) then
      write(93,*) 'MMMMMMMMMMMMMMMMMMMMM'
      write(93,'(2(a,i8))') "iterstep = ",iterstep,": level = ",nl
      write(93,'(3(a,g14.7))') "iterres = ",iterres,": iterres_old = ",iterres_old,": lambda = ",lambda
      write(93,'(3(a,g14.7))') 'delff2 = ',delff2,': ffdelff = ',ffdelff
      write(93,'(a)') 'pp,delphi(old),deldelphi,delphi,ff(old),delff,ff'
      do pp = 1, ptotal
        write(93,'(i10,6(g14.7))') pp,delphi(pp)-lambda*deldelphi(pp),deldelphi(pp),delphi(pp),ff(pp)-lambda*delff(pp), &
          delff(pp),ff(pp)
      end do
    end if

    if (debug_sparse) then
      if (mod(iterstep,iterstepcheck) == 0) then
        write(*,'(2(a,i8),3(a,g14.7))') "ITERATIONS: intermediate iterstep = ",iterstep,": level = ",nl, &
          ": iterres = ",iterres,": del iterres = ", &
          iterres_old-iterres,": lambda = ",lambda
        if (convergence_details_file) &
          write(fconverge,'(2(a,i8),3(a,g14.7))') &
            "ITERATIONS: intermediate iterstep = ",iterstep,": iterres = ",iterres,": level = ",nl,": del iterres = ", &
            iterres_old-iterres,": lambda = ",lambda
      end if
    end if

    if (iterres < iterrestol) then
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

  end do

end do iteration_loop
  
if (ierror == 0) then
  if (debug_sparse) then
    write(*,'(a,i8,2(a,g14.7))') "ITERATIONS: convered iterstep = ",iterstep,": iterres = ",iterres
    if (convergence_details_file) &
      write(fconverge,'(a,i8,2(a,g14.7))') &
        "ITERATIONS: converged iterstep = ",iterstep,": iterres = ",iterres
  end if
else
  write(*,'(a,i8,2(a,g14.7))') "ITERATIONS WARNING: failed iterstep = ",iterstep,": iterres = ",iterres
  if (convergence_details_file) &
    write(fconverge,'(a,i8,2(a,g14.7))') &
      "ITERATIONS WARNING: failed iterstep = ",iterstep,": iterres = ",iterres
end if

!---------------------
! redimensionalise results

! not sure if this is actually needed, but for safety redimensionalise the equations
call nondimensionalise_equations(normalise,forward=.false.)

! and dimensionalise delphi
do ppu = 1, ptotal
  m = unknown_var_from_pp(ppu)
  delphi(ppu) = delphi(ppu)*var(m)%magnitude
end do

!---------------------

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

! here we create/update the multigrid structure and the contained delff arrays

use general_module

integer :: ncells, ppu_next, ppu_unallocated, ppu_last, pppe, ppe, m, ns, pppu, ppu, nm, ng, pp, mu, nsu, &
  nu, default_grid_size, delnu, ngsub, n, ndelff_element_list
integer, dimension(:), allocatable :: unknowns_marker
double precision :: coefficient_strength, next_coefficient_strength, delff_dot_delff
! the following are used as temporary arrays when finding delff contained within multigrid
double precision, dimension(:), allocatable :: delff_linear ! the delff values
logical, dimension(:), allocatable :: delff_marker ! a marker array showing what elements are used
integer, dimension(:), allocatable :: delff_element_list ! and list of the elements
logical :: debug_sparse = .true.
logical, parameter :: debug = .false.

if (debug) debug_sparse = .true.
if (debug_sparse) write(*,'(80(1h+)/a)') 'subroutine calc_multigrid'

! first create structure of multigrid array, calculating the number of levels
if (.not.allocated(multigrid)) then
! nmultigrid = 1 + ceiling(log(dble(ptotal))/log(dble(2.d0)))
! to avoid roundoff errors etc, calculate the number of levels by calculating the series explicitly
  ncells = 1
  nmultigrid = 1
  do while (ncells < ptotal)
    nmultigrid = nmultigrid + 1
    ncells = ncells*2
  end do
  if (debug_sparse) write(*,'(a,i2,2(a,i8))') 'INFO: calculated nmultigrid = ',nmultigrid,' based on ptotal = ', &
    ptotal,' and ncells = ',ncells
  if (debug) write(92,'(a,i2,2(a,i8))') 'INFO: calculated nmultigrid = ',nmultigrid,' based on ptotal = ', &
    ptotal,' and ncells = ',ncells
  allocate(multigrid(nmultigrid))
! multigrid(1) contains all of the unknowns, as one single grid, and initialise
  allocate(multigrid(1)%grid(1))
  allocate(multigrid(1)%grid(1)%unknown_elements(ptotal))
  multigrid(1)%ngrid = 1
! allocate all of the other multigrids too
  if (debug) write(92,*) 'allocating subsequent multigrids:'
  do nm = 2, nmultigrid
    default_grid_size = 2**(nmultigrid-nm) ! this is the maximum number of elements in each grid on this level
    multigrid(nm)%ngrid = ceiling(dble(ptotal)/dble(default_grid_size)-tiny(1.d0))
    if (debug) write(92,'(3(a,i8))') 'nm = ',nm,': default_grid_size = ',default_grid_size,': ngrid = ',multigrid(nm)%ngrid
    allocate(multigrid(nm)%grid(multigrid(nm)%ngrid))
    nu = 0
    do ng = 1, multigrid(nm)%ngrid
      delnu = min(default_grid_size,ptotal-nu)
      allocate(multigrid(nm)%grid(ng)%unknown_elements(delnu))
      multigrid(nm)%grid(ng)%unknown_elements = 0 ! zero this for tidyness
      multigrid(nm)%grid(ng)%nunknown_elements = delnu ! set this now as it doesn't change
      nu = nu + delnu
    end do
  end do
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
    write(92,'(a,i8)') 'multigrid(1)%grid(1)%nunknown_elements = ',multigrid(1)%grid(1)%nunknown_elements
    write(92,'(a,i8)') 'ppu_last = ',ppu_last
    write(92,'(a,i8)') 'ppu_unallocated = ',ppu_unallocated
  end if
  ppu_next = 0 ! zero this to indicate that it isn't allocated
  next_coefficient_strength = -1.d0
! loop through all equations that this last unknown ppu_last references
  do pppe = 1, equation_from_unknown(ppu_last)%nequation
    ppe = equation_from_unknown(ppu_last)%equation_pp(pppe) ! equation pp number
    m = equation_from_unknown(ppu_last)%equation_m(pppe) ! equation var m
    ns = equation_from_unknown(ppu_last)%equation_ns(pppe) ! and corresponding equation region index ns
    if (debug) then
      write(92,'(a,i8,a,i4,a,i8,a,i8)') &
        'EQUATION: found equation referenced by ppu_last: ppe = ',ppe,': m = ',m,': ns = ',ns,': equation = '//trim(var(m)%name)// &
        ': '//trim(ijkstring(var(m)%centring))//' = ',ijkvar(m,ns)
    end if
! now loop through all unknowns that this equation references, finding unknown that has the strongest relationship with ppu_last, indicated by the product of their jacobian entries
    do pppu = 1, var(m)%funk(ns)%ndv
      ppu = var(m)%funk(ns)%pp(pppu) ! unknown pp number to be checked for coefficient strength
      if (debug) then
        mu = unknown_var_from_pp(ppu)
        nsu = ppu - var(mu)%funk(1)%pp(1) + 1
        write(92,'(2(a,i8),a,i1,a,i8,a,g14.7)') 'ppu = ',ppu,': ppu_next = ',ppu_next,': unknowns_marker(ppu) = ', &
          unknowns_marker(ppu),': unknown = '//trim(var(mu)%name)//': '//trim(ijkstring(var(mu)%centring))//' = ', &
          ijkvar(mu,nsu),': coefficient = ',var(m)%funk(ns)%dv(pppu)
      end if
      if (unknowns_marker(ppu) /= 0) cycle ! if this unknown is already in the list, then move on
      coefficient_strength = abs(var(m)%funk(ns)%dv(equation_from_unknown(ppu_last)%within_equation_index(pppe))* &
        var(m)%funk(ns)%dv(pppu))
      if (debug) write(92,'(2(a,g14.7))') 'coefficient_strength = ',coefficient_strength, &
        ': next_coefficient_strength = ',next_coefficient_strength
      if (coefficient_strength > next_coefficient_strength) then
        next_coefficient_strength = coefficient_strength
        ppu_next = ppu
      end if
      if (debug) write(92,'(a,i8)') 'after comparison ppu_next = ',ppu_next
    end do
  end do

! at this point there are two possibilities:
! 1) next_coefficient_strength > 0 and ppu_next is defined
! 2) ppu_next is not defined, meaning that all unknowns that are referenced by equations that our last unknown references have already been added to the list, so set ppu_next = ppu_unallocated

  if (debug) write(92,'(a,i8)') 'after searching for ppu_next: ppu_next = ',ppu_next
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

  if (debug) write(92,'(a,i8)') 'at end of loop: ppu_unallocated = ',ppu_unallocated

end do

if (debug) write(92,'(a,i1)') 'check - minimum of unknowns_marker should be 1 = ',minval(unknowns_marker)
deallocate(unknowns_marker)
      
! now create subsequent multigrids by sequentially pairing off unknowns
do nm = 2, nmultigrid
  ng = 0
  do ngsub = 1, multigrid(nm-1)%ngrid
    nu = 0
    do while (nu < multigrid(nm-1)%grid(ngsub)%nunknown_elements)
      ng = ng + 1
      delnu = multigrid(nm)%grid(ng)%nunknown_elements
      multigrid(nm)%grid(ng)%unknown_elements(1:delnu) = multigrid(nm-1)%grid(ngsub)%unknown_elements(nu+1:nu+delnu)
      nu = nu + delnu
    end do
  end do
end do

if (debug) then
  write(92,'(/a/a)') repeat('-',80),'PRINTING out multigrid details'
  do nm = 1, ubound(multigrid,1)
    write(92,'(a)') 'MMMMMMMMMMMMMMMMMMMMMMMM'
    if (.not.allocated(multigrid(nm)%grid)) then
      write(92,'(a,i4)') 'multigrid = ',nm,': has no grids allocated'
    else
      do ng = 1, ubound(multigrid(nm)%grid,1)
        write(92,'(a)') 'GGGGGGGGGGGGGG'
        if (.not.allocated(multigrid(nm)%grid(ng)%unknown_elements)) then
          write(92,'(a,i4,a,i8)') 'multigrid = ',nm,': grid = ',ng,': has no elements allocated'
        else
          do pppu = 1, multigrid(nm)%grid(ng)%nunknown_elements
            ppu = multigrid(nm)%grid(ng)%unknown_elements(pppu)
            if (ppu == 0) then
              write(92,'(a,i8)') 'unknown_element not set: pppu = ',pppu
            else
              m = unknown_var_from_pp(ppu)
              ns = ppu - var(m)%funk(1)%pp(1) + 1
              write(92,'(a,i4,2(a,i8),a,i4,2(a,i8))') 'multigrid = ',nm,': grid = ',ng,': ppu = ',ppu,': unknown = '// &
                trim(var(m)%name)//': m = ',m,': ns = ',ns,': '//trim(ijkstring(var(m)%centring))//' = ',ijkvar(m,ns)
            end if
          end do
        end if
      end do
    end if
  end do
end if

!---------------------
! now move through each grid within each multigrid calculating delff
allocate(delff_linear(ptotal),delff_marker(ptotal),delff_element_list(ptotal))
delff_linear = 0.d0
delff_marker = .false.
delff_element_list = 0
ndelff_element_list = 0
do nm = 1, ubound(multigrid,1)
  do ng = 1, multigrid(nm)%ngrid

! cycle through all unknowns in this grid, and all corresponding equations
    do nu = 1, multigrid(nm)%grid(ng)%nunknown_elements
      ppu = multigrid(nm)%grid(ng)%unknown_elements(nu)
      do pppe = 1, equation_from_unknown(ppu)%nequation
        ppe = equation_from_unknown(ppu)%equation_pp(pppe)
        m = equation_from_unknown(ppu)%equation_m(pppe)
        ns = equation_from_unknown(ppu)%equation_ns(pppe)
        n = equation_from_unknown(ppu)%within_equation_index(pppe)
        delff_linear(ppe) = delff_linear(ppe) + var(m)%funk(ns)%dv(n)
        if (.not.delff_marker(ppe)) then
          ndelff_element_list = ndelff_element_list + 1
          delff_element_list(ndelff_element_list) = ppe
          delff_marker(ppe) = .true.
        end if
      end do
    end do

! now transfer back into multigrid structure
    multigrid(nm)%grid(ng)%ndelff = ndelff_element_list
    if (allocatable_integer_size(multigrid(nm)%grid(ng)%delff_elements) < ndelff_element_list) then
      call resize_integer_array(array=multigrid(nm)%grid(ng)%delff_elements,new_size=ndelff_element_list, &
        keep_data=.false.)
      call resize_double_precision_array(array=multigrid(nm)%grid(ng)%delff,new_size=ndelff_element_list, &
        keep_data=.false.)
    end if
    multigrid(nm)%grid(ng)%delff_elements(1:ndelff_element_list) = delff_element_list
! now assemble delff from delff_linear, while also resetting the temporary delff arrays
! and also calculating minus_reciprocal_delff_dot_delff
    delff_dot_delff = 0.d0
    do n = 1, ndelff_element_list
      multigrid(nm)%grid(ng)%delff(n) = delff_linear(delff_element_list(n))
      delff_marker(delff_element_list(n)) = .false.
      delff_linear(delff_element_list(n)) = 0.d0
      delff_element_list(n) = 0
      delff_dot_delff = delff_dot_delff + multigrid(nm)%grid(ng)%delff(n)**2
    end do
    if (delff_dot_delff < tiny(1.d0)) call error_stop("delff_dot_delff is <= tiny in subroutine calc multigrid")
    multigrid(nm)%grid(ng)%minus_reciprocal_delff_dot_delff = -1.d0/delff_dot_delff
    ndelff_element_list = 0
    
  end do
end do
deallocate(delff_linear,delff_marker,delff_element_list)

if (debug_sparse) write(*,'(a/80(1h-))') 'subroutine calc_multigrid'

end subroutine calc_multigrid

!-----------------------------------------------------------------

subroutine nondimensionalise_equations(normalise,forward)

! here we nondimensionalise the equations and their derivatives, so that we are working in nondimensional space when solving the linear system

use general_module

integer :: mm, m, ns, pppu, ppu, mu, ppe
logical :: forward ! if true, then we are nondimensionalising the equations, otherwise, if false, we are dimensionalising them again
logical :: normalise ! do additional normalisation, so that each equation/derivative is scaled so that the largest magnitude jacobian element per row becomes (positive) 1.d0, with a corresponding change to the rhs (ie, equation value)

if (.not.allocated(e_scale)) then
  allocate(e_scale(ptotal))
  e_scale = 1.d0 ! if normalise isn't used then e_scale is set to 1
end if
if (normalise) then
  if (forward) e_scale = 0.d0
end if

ppe = 0
do mm = 1, allocatable_size(var_list(var_list_number_equation)%list)
  m = var_list(var_list_number_equation)%list(mm) ! equation var number
  do ns = 1, ubound(var(m)%funk,1)
    ppe = ppe + 1
    if (forward) then
      var(m)%funk(ns)%v = var(m)%funk(ns)%v/var(m)%magnitude
    else
      var(m)%funk(ns)%v = var(m)%funk(ns)%v*var(m)%magnitude
    end if
    do pppu = 1, var(m)%funk(ns)%ndv ! here we cycle through all the unknowns that are referenced within this equation
      ppu = var(m)%funk(ns)%pp(pppu) ! this is the unknown pp number
      mu = unknown_var_from_pp(ppu) ! unknown var number
      if (forward) then
        var(m)%funk(ns)%dv(pppu) = var(m)%funk(ns)%dv(pppu)*var(mu)%magnitude/var(m)%magnitude
        if (normalise) then
          if (abs(var(m)%funk(ns)%dv(pppu)) > abs(e_scale(ppe))) e_scale(ppe) = var(m)%funk(ns)%dv(pppu)
        end if
      else
        var(m)%funk(ns)%dv(pppu) = var(m)%funk(ns)%dv(pppu)*var(m)%magnitude/var(mu)%magnitude
      end if
    end do
  end do
end do

if (normalise) then
  ppe = 0
  do mm = 1, allocatable_size(var_list(var_list_number_equation)%list)
    m = var_list(var_list_number_equation)%list(mm) ! equation var number
    do ns = 1, ubound(var(m)%funk,1)
      ppe = ppe + 1
      if (forward) then
        var(m)%funk(ns)%v = var(m)%funk(ns)%v/e_scale(ppe)
      else
        var(m)%funk(ns)%v = var(m)%funk(ns)%v*e_scale(ppe)
      end if
      do pppu = 1, var(m)%funk(ns)%ndv ! here we cycle through all the unknowns that are referenced within this equation
        if (forward) then
          var(m)%funk(ns)%dv(pppu) = var(m)%funk(ns)%dv(pppu)/e_scale(ppe)
        else
          var(m)%funk(ns)%dv(pppu) = var(m)%funk(ns)%dv(pppu)*e_scale(ppe)
        end if
      end do
    end do
  end do
end if

end subroutine nondimensionalise_equations

!-----------------------------------------------------------------

subroutine bicg_mainsolver(ierror,stabilised)

! here we use a unpreconditioned bicg technique to solve the linear system, using equation funk data directly
! routine is based on pseudo-code from H. A. van der Vorst, SIAM J. Sci. and Stat. Comput., 13(2), 631–644., DOI:10.1137/0913035
! the logical stabilised determines whether bicg or bicgstab is used

use general_module
use equation_module

logical :: stabilised
integer :: ierror, mm, m, ns, j, iterstep, ppu
character(len=1000) :: formatline
double precision, dimension(:), allocatable, save :: r1, r2, p1, p2, v, t, s ! allocate these once as their size doesn't change
double precision :: alpha, beta, iterres, iterres_old, alpha_demoninator, rho_o, rho, omega, omega_demoninator
double precision, parameter :: alpha_max = 1.d6, beta_max = 1.d6, omega_max = 1.d6, &
  alpha_demoninator_min = tiny(1.d0), rho_o_min = tiny(1.d0), increment_multiplier = 1.d-3, omega_min = tiny(1.d0), &
  omega_demoninator_min = tiny(1.d0)
logical :: restart ! this is a flag to indicate that solution needs to be restarted
logical, parameter :: normalise = .true. ! to normalise the equations based upon maximum per row jacobian value
logical, parameter :: debug = .false.
logical :: debug_sparse = .true.

if (debug) debug_sparse = .true.
if (debug_sparse) write(*,'(80(1h+)/a)') 'subroutine bicg_mainsolver'

ierror = 1 ! this signals an error

! nondimensionalise equations and their derivatives (forward = .true. implies that we are nondimensionalising)
call nondimensionalise_equations(normalise,forward=.true.)

!---------------------
! initialise and allocate loop variables
if (.not.allocated(r1)) then
  if (stabilised) then
    allocate(r1(ptotal),r2(ptotal),p1(ptotal),v(ptotal),t(ptotal),s(ptotal))
  else
    allocate(r1(ptotal),r2(ptotal),p1(ptotal),p2(ptotal),v(ptotal))
  end if
end if

restart = .false.

! initial guess for delphi is the zero vector
delphi = 0.d0

! setup the vectors
call initialise_bicg(stabilised,r1,r2,p1,p2,v,rho,alpha,omega,iterres,debug,normalise)

if (debug) then
  call print_jacobian_matrix
  write(93,'(a,i8,1(a,g14.7))') "ITERATIONS:        start iterstep = ",0,": iterres = ",iterres
  write(93,'(a)') repeat('-',80)
end if

! start iteration loop
iterstep = 0
iteration_loop: do

  if (debug) write(93,'(a,i10)') 'At start of iteration_loop, iterstep = ',iterstep

! if restart has been requested, do so now
  if (restart) then
    iterstep = iterstep + 1
    if (debug) write(93,'(a)') 'RRRRR RESTART REQUESTED'
    do j = 1, ptotal
!     delphi(j) = delphi(j) + increment_multiplier*random() ! add a random increment onto delphi, noting that delphi should be normalised
      delphi(j) = random() ! overwrite delphi with a random increment, noting that delphi should be normalised
    end do
    call initialise_bicg(stabilised,r1,r2,p1,p2,v,rho,alpha,omega,iterres,debug,normalise)
    restart = .false.
  end if

! check on convergence, noting that r1 is the residual vector
  iterres_old = iterres
  iterres = iterres_calc(r1,normalise)
  
  if (debug) then
    write(93,'(1(a,i8))') "iterstep = ",iterstep
    write(93,'(2(a,g14.7))') "iterres = ",iterres,": iterres_old = ",iterres_old
  end if

  if (debug_sparse) then
    if (mod(iterstep,iterstepcheck) == 0) then
      write(*,'(1(a,i8),2(a,g14.7))') "ITERATIONS: iterstep = ",iterstep, &
        ": iterres = ",iterres,": del iterres = ",iterres_old-iterres
      if (convergence_details_file) &
        write(fconverge,'(1(a,i8),2(a,g14.7))') "ITERATIONS: iterstep = ",iterstep, &
          ": iterres = ",iterres,": del iterres = ",iterres_old-iterres
    end if
  end if

  if (iterres < iterrestol) then ! iterations have converged
    ierror = 0
    exit iteration_loop
  end if

  if (mod(iterstep,iterstepcheck) == 0) then ! user has requested stop, flag with ierror=2
    if (check_stopfile("stopback")) then
      write(*,'(a)') 'INFO: user requested simulation stop via "kill" file'
      ierror = 2
      exit iteration_loop
    end if
  end if

  if (iterstep == iterstepmax) then ! maximum iterations have been performed without convergence
    exit iteration_loop
  end if

! save rho from the previous iteration as rho_o
  rho_o = rho
! and calculate the new rho
  rho = dot_product(r1,r2)
  if (debug) then
    write(93,'(a,g11.3)') 'rho_o = ',rho_o
    write(93,'(a,g11.3)') 'rho = ',rho
  end if

! calculate beta
  if (abs(rho_o) < rho_o_min) then
    write(*,'(a)') "WARNING: rho_o small in bicg, restarting iterations"
    restart = .true.
    cycle iteration_loop
  end if
  beta = rho/rho_o
  if (stabilised) then
    if (abs(omega) < omega_min) then
      write(*,'(a)') "WARNING: omega small in bicg, restarting iterations"
      restart = .true.
      cycle iteration_loop
    end if
    beta = beta*(alpha/omega)
  end if
  if (debug) then
    write(93,'(a,g11.3)') 'rho = ',rho
    write(93,'(a,g11.3)') 'rho_o = ',rho_o
    write(93,'(a,g11.3)') 'beta = ',beta
  end if
  if (abs(beta) > beta_max) then
    write(*,'(a)') "WARNING: beta large in bicg, restarting iterations"
    restart = .true.
    cycle iteration_loop
  end if

! update p's
  if (stabilised) then
    p1 = r1 + beta*(p1-omega*v)
    if (debug) then
      write(93,'(a)') 'updating p1'
      call print_debug_vector(p1,"p1")
    end if
  else
    p1 = r1 + beta*p1
    p2 = r2 + beta*p2
    if (debug) then
      write(93,'(a)') 'updating ps'
      call print_debug_vector(p1,"p1")
      call print_debug_vector(p2,"p2")
    end if
  end if

! calculate alpha = (r2.r1)/(p2.A.p1)
  if (debug) write(93,'(a)') 'calculating alpha'
  call aa_dot_vector(p1,v) ! v = A.p1
  if (debug) call print_debug_vector(v,"A.p1")
  if (stabilised) then
    alpha_demoninator = dot_product(r2,v)
  else
    alpha_demoninator = dot_product(p2,v)
  end if
  if (debug) write(93,'(a,g11.3)') 'alpha_demoninator = ',alpha_demoninator
  if (abs(alpha_demoninator) < alpha_demoninator_min) then
    write(*,'(a)') "WARNING: alpha_demoninator small in bicg, restarting iterations"
    restart = .true.
    cycle iteration_loop
  end if
  alpha = rho/alpha_demoninator
  if (abs(alpha) > alpha_max) then
    write(*,'(a)') "WARNING: alpha large in bicg, restarting iterations"
    restart = .true.
    cycle iteration_loop
  end if
  if (debug) then
    write(93,'(a,g11.3)') 'rho = ',rho
    write(93,'(a,g11.3)') 'alpha = ',alpha
  end if

  if (stabilised) then
! bicgstab update

! calculate s, t, and omega
    s = r1 - alpha*v
    if (debug) call print_debug_vector(s,"s updated")
    call aa_dot_vector(s,t)
    if (debug) call print_debug_vector(t,"t updated")
    omega_demoninator = dot_product(t,t)
    if (debug) write(93,'(a,g11.3)') 'omega_demoninator = ',omega_demoninator
    if (abs(omega_demoninator) < omega_demoninator_min) then
      write(*,'(a)') "WARNING: omega_demoninator small in bicg, restarting iterations"
      restart = .true.
      cycle iteration_loop
    end if
    omega = dot_product(t,s)/omega_demoninator
    if (debug) write(93,'(a,g11.3)') 'omega = ',omega
    if (abs(omega) > omega_max) then
      write(*,'(a)') "WARNING: omega large in bicg, restarting iterations"
      restart = .true.
      cycle iteration_loop
    end if

! update delphi
    delphi = delphi + alpha*p1 + omega*s
    if (debug) call print_debug_vector(delphi,"delphi updated")

! update r1
    r1 = s - omega*t
    if (debug) call print_debug_vector(r1,"r1 updated")

  else
! bicg update

! update delphi
    delphi = delphi + alpha*p1
    if (debug) call print_debug_vector(delphi,"delphi updated")

! update r's
! r1 = r1 - alpha*A.p1
! v already stores A.p1
    if (debug) write(93,'(a)') 'updating rs'
    r1 = r1 - alpha*v
! r2 = r2 - alpha*A^T.p2
    call aa_transpose_dot_vector(p2,v) ! temporarily use v to store A^T.p2
    r2 = r2 - alpha*v
    if (debug) then
      call print_debug_vector(v,"A^T.p2")
      call print_debug_vector(r1,"r1")
      call print_debug_vector(r2,"r2")
    end if

  end if

  iterstep = iterstep + 1 ! update the number of iterations counter

end do iteration_loop
  
if (ierror == 0) then
  if (debug_sparse) then
    write(*,'(a,i8,2(a,g14.7))') "ITERATIONS: convered iterstep = ",iterstep,": iterres = ",iterres
    if (convergence_details_file) &
      write(fconverge,'(a,i8,2(a,g14.7))') &
        "ITERATIONS: converged iterstep = ",iterstep,": iterres = ",iterres
  end if
else
  write(*,'(a,i8,2(a,g14.7))') "ITERATIONS WARNING: failed iterstep = ",iterstep,": iterres = ",iterres
  if (convergence_details_file) &
    write(fconverge,'(a,i8,2(a,g14.7))') &
      "ITERATIONS WARNING: failed iterstep = ",iterstep,": iterres = ",iterres
end if

!---------------------
! redimensionalise results

! not sure if this is actually needed, but for safety redimensionalise the equations
call nondimensionalise_equations(normalise,forward=.false.)

! and dimensionalise delphi
do ppu = 1, ptotal
  m = unknown_var_from_pp(ppu)
  delphi(ppu) = delphi(ppu)*var(m)%magnitude
end do

!---------------------

if (debug_sparse) write(*,'(a/80(1h-))') 'subroutine bicg_mainsolver'

end subroutine bicg_mainsolver

!-----------------------------------------------------------------

subroutine initialise_bicg(stabilised,r1,r2,p1,p2,v,rho,alpha,omega,iterres,debug,normalise)

! setup start/restart of bicg given set/reset delphi

use general_module
use equation_module

logical :: stabilised, debug, normalise
integer :: mm, m, ns, j
double precision, dimension(:), allocatable :: r1, r2, p1, p2, v
double precision :: alpha, iterres, rho, omega

! calculate initial residual r1
call aa_dot_vector(delphi,r1) ! calculate the product of the equation matrix (A) with the initial solution vector (delphi), and store in r1
! form the initial residual as b - A.x, but with b = -equation (=-var(m)%funk(ns)%v) and A.x stored as r1 from above
! set initial ff array (linear equation error array) from newton's equations
j = 0
do mm = 1, allocatable_size(var_list(var_list_number_equation)%list)
  m = var_list(var_list_number_equation)%list(mm)
  do ns = 1, ubound(var(m)%funk,1)
    j = j + 1
    r1(j) = -var(m)%funk(ns)%v - r1(j)
  end do
end do
r2 = r1
!rho = dot_product(r1,r2)
rho = 1.d0
p1 = 0.d0 
if (stabilised) then
  alpha = 1.d0
  omega = 1.d0
  v = 0.d0 
else
  p2 = 0.d0 
end if

if (debug) then
  call print_debug_vector(r1,"r1 in initialise_bicg")
  call print_debug_vector(r2,"r2 in initialise_bicg")
  call print_debug_vector(p1,"p1 in initialise_bicg")
  if (stabilised) then
    call print_debug_vector(v,"v in initialise_bicg")
  else
    call print_debug_vector(p2,"p2 in initialise_bicg")
  end if
  call print_debug_vector(delphi,"delphi in initialise_bicg")
end if

iterres = iterres_calc(r1,normalise)

end subroutine initialise_bicg

!-----------------------------------------------------------------

double precision function iterres_calc(r,normalise)

use general_module
double precision, dimension(:), allocatable :: r
logical :: normalise

! calculate the residual, here noting that r*e_scale in fortran does element by element multiplication
if (normalise) then
  iterres_calc = sqrt(dot_product(r*abs(e_scale),r*abs(e_scale))/dble(ptotal))
else
  iterres_calc = sqrt(dot_product(r,r)/dble(ptotal))
end if

end function iterres_calc

!-----------------------------------------------------------------

subroutine aa_dot_vector(vector_to_multiply,vector_product)

! here we multiply a vector (vector_to_multiply) with the jacobian matrix stored as equation data,
!  forming the product vector (vector_product)

use general_module
double precision, dimension(:), allocatable, intent(in) :: vector_to_multiply
double precision, dimension(:), allocatable :: vector_product
integer :: ppe, pppu, ppu, mm, m, ns

!---------------------

ppe = 0
vector_product = 0.d0
do mm = 1, allocatable_size(var_list(var_list_number_equation)%list)
  m = var_list(var_list_number_equation)%list(mm)
  do ns = 1, ubound(var(m)%funk,1)
    ppe = ppe + 1
    do pppu = 1, var(m)%funk(ns)%ndv
      ppu = var(m)%funk(ns)%pp(pppu)
      vector_product(ppe) = vector_product(ppe) + var(m)%funk(ns)%dv(pppu)*vector_to_multiply(ppu)
    end do
  end do
end do

end subroutine aa_dot_vector

!-----------------------------------------------------------------

subroutine aa_transpose_dot_vector(vector_to_multiply,vector_product)

! here we multiply a vector (vector_to_multiply) with the jacobian matrix stored as equation data,
!  forming the product vector (vector_product)
! here we use the transpose of the jacobian, as a separate routine copied in the interests of speed

use general_module
double precision, dimension(:), allocatable, intent(in) :: vector_to_multiply
double precision, dimension(:), allocatable :: vector_product
integer :: ppe, pppu, ppu, mm, m, ns

!---------------------

ppe = 0
vector_product = 0.d0
do mm = 1, allocatable_size(var_list(var_list_number_equation)%list)
  m = var_list(var_list_number_equation)%list(mm)
  do ns = 1, ubound(var(m)%funk,1)
    ppe = ppe + 1
    do pppu = 1, var(m)%funk(ns)%ndv
      ppu = var(m)%funk(ns)%pp(pppu)
      vector_product(ppu) = vector_product(ppu) + var(m)%funk(ns)%dv(pppu)*vector_to_multiply(ppe)
    end do
  end do
end do

end subroutine aa_transpose_dot_vector

!-----------------------------------------------------------------

subroutine print_debug_vector(vector,description)

use general_module
double precision, dimension(:), allocatable :: vector
character(len=*) :: description
integer :: n, nmax
character(len=1000) :: formatline

write(93,'(a/a)') repeat('+',10),description
nmax = ubound(vector,1)
formatline = '('//trim(dindexformat(nmax))//',a,g14.6)'
do n = 1, nmax
  write(93,fmt=formatline) n," = ",vector(n)
end do
write(93,'(a)') repeat('-',10)

end subroutine print_debug_vector

!-----------------------------------------------------------------

subroutine print_jacobian_matrix

use general_module
integer :: ppe, mm, m, ns, pppu
character(len=1000) :: formatline
character(len=20) :: ptotalformat

ptotalformat=trim(dindexformat(ptotal))
write(93,'(a/a)') repeat('+',10),"jacobian matrix"
ppe = 0
do mm = 1, allocatable_size(var_list(var_list_number_equation)%list)
  m = var_list(var_list_number_equation)%list(mm)
  do ns = 1, ubound(var(m)%funk,1)
    ppe = ppe + 1
    write(formatline,*) '('//trim(ptotalformat)//',',var(m)%funk(ns)%ndv,'(g11.3,a1,'//trim(ptotalformat)//',a1))'
    write(93,fmt=formatline) ppe,(var(m)%funk(ns)%dv(pppu),'(',var(m)%funk(ns)%pp(pppu),')',pppu=1,var(m)%funk(ns)%ndv)
  end do
end do
write(93,'(a)') repeat('-',10)

end subroutine print_jacobian_matrix

!-----------------------------------------------------------------

subroutine quasinewton_mainsolver(ierror)

! here we use an unpreconditioned quasi-newton technique to solve the linear system, using equation funk data directly

use general_module
use equation_module

integer :: ierror, mm, m, ns, j, iterstep, ppu
character(len=1000) :: formatline
double precision, dimension(:), allocatable, save :: e, v_o, p, x, p_o, x_o, delv, v, w  ! allocate these once as their size doesn't change
double precision :: rrr, rrr_tol, rrr_o, lambda, alpha, beta, gamma, delrrr, iterres, delrrr_o
double precision, parameter :: beta_min = 1.d-10, lambda_min = 1.d-2
logical, parameter :: debug = .true.
logical :: debug_sparse = .true.

if (debug) debug_sparse = .true.
if (debug_sparse) write(*,'(80(1h+)/a)') 'subroutine quasinewton_mainsolver'

ierror = 1 ! this signals an error

! nondimensionalise equations and their derivatives (forward = .true. implies that we are nondimensionalising)
call nondimensionalise_equations(normalise=.false.,forward=.true.)

!---------------------
! initialise and allocate loop variables
if (.not.allocated(e)) allocate(e(ptotal),v_o(ptotal),p(ptotal),x(ptotal),p_o(ptotal),x_o(ptotal), &
  delv(ptotal),v(ptotal),w(ptotal))

! initial guess for delphi is the zero vector
delphi = 0.d0
! form the initial e = E (ie, the equation values)
j = 0
do mm = 1, allocatable_size(var_list(var_list_number_equation)%list)
  m = var_list(var_list_number_equation)%list(mm)
  do ns = 1, ubound(var(m)%funk,1)
    j = j + 1
    e(j) = var(m)%funk(ns)%v
  end do
end do
rrr = dot_product(e,e)
rrr_tol = dble(ptotal)*(iterrestol**2) ! form equivalent rrr tolerance

if (debug) then
  call print_debug_vector(e,"e")
  call print_jacobian_matrix
  write(93,'(a,g14.6)') 'rrr_tol = ',rrr_tol
  write(93,'(a,g14.6)') 'rrr = ',rrr
  write(93,'(a,i8,1(a,g14.7))') "ITERATIONS:        start iterstep = ",0,": rrr = ",rrr
  write(93,'(a)') repeat('-',80)
end if

! start iteration loop
iterstep = 0
outer_loop: do

  if (debug) write(93,'(a,i10)') 'At start of outer iteration_loop, iterstep = ',iterstep

! check on convergence, noting that r1 is the residual vector
  
  if (debug) then
    iterres = sqrt(rrr/dble(ptotal))
    write(93,'(2(a,g14.7))') "iterres = ",iterres,": rrr = ",rrr
  end if

  if (debug_sparse) then
    if (mod(iterstep,iterstepcheck) == 0) then
      iterres = sqrt(rrr/dble(ptotal))
      write(*,'(1(a,i8),1(a,g14.7))') "OUTER ITERATIONS: iterstep = ",iterstep, &
        ": iterres = ",iterres
      if (convergence_details_file) &
        write(fconverge,'(1(a,i8),1(a,g14.7))') "OUTER ITERATIONS: iterstep = ",iterstep, &
          ": iterres = ",iterres
    end if
  end if

  if (rrr < rrr_tol) then ! iterations have converged
    ierror = 0
    exit outer_loop
  end if

! if (mod(iterstep,iterstepcheck) == 0) then ! user has requested stop, flag with ierror=2
!   if (check_stopfile("stopback")) then
!     write(*,'(a)') 'INFO: user requested simulation stop via "kill" file'
!     ierror = 2
!     exit iteration_loop
!   end if
! end if

  if (iterstep == iterstepmax) then ! maximum iterations have been performed without convergence
    exit outer_loop
  end if

  rrr_o = rrr ! save previous rrr
! calculate v_o = 2*J^T.e
  call aa_transpose_dot_vector(e,v_o) ! v_o = A.p1
  v_o = 2.d0*v_o
  p = 0.d0
  x = 0.d0
  delrrr_o = 0.d0
  if (debug) then
    write(93,'(a,g14.6)') 'rrr_o = ',rrr_o
    call print_debug_vector(v_o,"v_o")
    call print_debug_vector(p,"p")
    call print_debug_vector(x,"x")
    write(93,'(a,g14.6)') 'delrrr_o = ',delrrr_o
  end if

  inner_loop: do

    iterstep = iterstep + 1 ! increment iteration number
    lambda = 1.d0 ! reset backstepping parameter
    p_o = p ! save previous projection direction
    x_o = x ! save previous solution

    if (debug) then
      write(93,'(a)') 'start of inner loop:'
      write(93,'(a,g14.6)') 'lambda = ',lambda
      call print_debug_vector(p_o,"p_o")
      call print_debug_vector(x_o,"x_o")
    end if

    lambda_loop: do
  
      p = lambda*x_o + (1.d0-lambda)*p_o ! new trial projection direction
! temp &&&
!     p = [1.d0, 2.d0]
!     write(*,*) 'overwriting p'
! calculate delv = J^T.(J.p)
      call aa_dot_vector(p,v) ! use v here as temporary storage
      call aa_transpose_dot_vector(v,delv)
!     v = v_o + delv
      v = v_o + 2.d0*delv
      alpha = dot_product(v_o,v)
      call aa_dot_vector(v,w)
      beta = dot_product(w,w)
      
      if (debug) then
        write(93,'(a)') 'start of lambda loop:'
        write(93,'(a,g14.6)') 'lambda = ',lambda
        call print_debug_vector(p,"p")
        call print_debug_vector(delv,"delv")
        call print_debug_vector(v,"v")
        write(93,'(a,g14.6)') 'alpha = ',alpha
        call print_debug_vector(w,"w")
        write(93,'(a,g14.6)') 'beta = ',beta
      end if

      if (beta < beta_min) call error_stop('beta small in quasinewton_mainsolver')

      gamma = alpha/(2.d0*beta)
      delrrr = -alpha*gamma + beta*(gamma**2)

      if (debug) then
        write(93,'(a)') 'within lambda loop:'
        write(93,'(a,g14.6)') 'gamma = ',gamma
        write(93,'(a,g14.6)') 'delrrr = ',delrrr
        write(93,'(a,g14.6)') 'delrrr_o = ',delrrr_o
        write(93,'(a,g14.6)') 'delrrr-delrrr_o = ',delrrr-delrrr_o
      end if

! check whether residual has decreased
      if (delrrr <= delrrr_o) exit lambda_loop 

! otherwise decrease lambda and try again
      lambda = lambda/2.d0
      if (debug) then
        write(93,'(a)') 'lambda has been decreased at end of lambda loop:'
        write(93,'(a,g14.6)') 'lambda = ',lambda
      end if

      if (lambda < lambda_min) exit lambda_loop 

    end do lambda_loop

    if (debug) then
      write(93,'(a)') 'exited lambda loop with:'
      write(93,'(a,g14.6)') 'lambda = ',lambda
    end if

! update x
    if (lambda < lambda_min) then
      x = x_o ! if lambda became too small then lambda loop was not successful, so instead use previous inner loop iteration
      delrrr = delrrr_o
      lambda = 0.d0
    else
      x = -gamma*v
    end if
    rrr = rrr_o + delrrr
    delrrr_o = delrrr
    if (debug) then
      call print_debug_vector(x,"x")
      write(93,'(a,g14.6)') 'rrr = ',rrr
    end if

    if (debug_sparse) then
      if (mod(iterstep,iterstepcheck) == 0) then
        iterres = sqrt(rrr/dble(ptotal))
        write(*,'(1(a,i8),2(a,g14.7))') "INNER ITERATIONS: iterstep = ",iterstep, &
          ": iterres = ",iterres,": lambda = ",lambda
        if (convergence_details_file) &
          write(fconverge,'(1(a,i8),2(a,g14.7))') "INNER ITERATIONS: iterstep = ",iterstep, &
            ": iterres = ",iterres,": lambda = ",lambda
      end if
    end if

! if lambda is too small, cycle outer loop
    if (lambda < lambda_min) exit inner_loop

! if tolerance has been reached, exit inner_loop
    if (rrr < rrr_tol) exit inner_loop

! if maximum number of iterations have passed, also exit inner_loop
    if (iterstep == iterstepmax) exit inner_loop

  end do inner_loop

  if (debug) write(93,'(a)') 'exited inner_loop'

! update solution
  delphi = delphi + x
! e = e - gamma*w ! w may not reflect last iteration
! use v as temporary storage
  call aa_dot_vector(x,v)
  e = e + v
! recalculate rrr to guard against roundoff errors
  rrr = dot_product(e,e)

  if (debug) then
    call print_debug_vector(delphi,"delphi")
    call print_debug_vector(e,"e")
  end if

end do outer_loop
  
iterres = sqrt(rrr/dble(ptotal))

if (ierror == 0) then
  if (debug_sparse) then
    write(*,'(a,i8,2(a,g14.7))') "ITERATIONS: convered iterstep = ",iterstep,": iterres = ",iterres
    if (convergence_details_file) &
      write(fconverge,'(a,i8,2(a,g14.7))') &
        "ITERATIONS: converged iterstep = ",iterstep,": iterres = ",iterres
  end if
else
  write(*,'(a,i8,2(a,g14.7))') "ITERATIONS WARNING: failed iterstep = ",iterstep,": iterres = ",iterres
  if (convergence_details_file) &
    write(fconverge,'(a,i8,2(a,g14.7))') &
      "ITERATIONS WARNING: failed iterstep = ",iterstep,": iterres = ",iterres
end if

!---------------------
! redimensionalise results

! not sure if this is actually needed, but for safety redimensionalise the equations
call nondimensionalise_equations(normalise=.false.,forward=.false.)

! and dimensionalise delphi
do ppu = 1, ptotal
  m = unknown_var_from_pp(ppu)
  delphi(ppu) = delphi(ppu)*var(m)%magnitude
end do

!---------------------

if (debug_sparse) write(*,'(a/80(1h-))') 'subroutine quasinewton_mainsolver'

end subroutine quasinewton_mainsolver

!-----------------------------------------------------------------

end module linear_module
!-----------------------------------------------------------------
