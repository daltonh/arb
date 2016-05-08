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
public multigrid_mainsolver, bicg_mainsolver, descent_mainsolver, flexible_mainsolver

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
  double precision, dimension(:), allocatable :: delr ! values for the vector ff array, stored in a compact format
  integer, dimension(:), allocatable :: delr_elements ! in a one-to-one correspondance with delr, specifies what elements (ppe) each value of delr refers to
  integer :: ndelr ! current number of valid elements in delr
  double precision :: minus_reciprocal_delr_dot_delr ! what it says, the reciprocal of the mag^2 of delr, x -1
end type grid_type

! inidividual grids of unknowns corresponding to a particular level are grouped together within one multigrid
type multigrid_type
  type(grid_type), dimension(:), allocatable :: grid ! an array of grids contained within the particular multigrid level
  integer :: ngrid ! number of grids in this level = ubound(grid,1)
end type multigrid_type
  
type (multigrid_type), dimension(:), allocatable :: multigrid ! an array of of the multigrid levels

! this is a sparse matrix structure (csr1) for storing the jacobian and jacobian_transpose
type jacobian_type
  double precision, dimension(:), allocatable :: v
  integer, dimension(:), allocatable :: i
  integer, dimension(:), allocatable :: ja
  integer :: n ! number of active elements (matricies only increase in size)
end type jacobian_type

integer, save :: nmultigrid ! number of levels within the multigrid structure, calculated when the multigrid structure is calculated
  
!-----------------------------------------------------------------
contains

!-----------------------------------------------------------------

subroutine multigrid_mainsolver(ierror,singlegrid)

! here we use a homegrown multigrid technique to solve the linear system, using equation funk data directly

use general_module
use equation_module

integer :: ierror, m, nl, ng, pppe, ppe, pppu, ppu, iterstep, nl_lower, iterstepchecknext, itersteproundoffnext, &
  nelements
double precision, dimension(:), allocatable :: ee, r, ee_scale, deldelphi, delr
double precision :: ee_scale_max, rrr, rrr_newt_tol, rrr_tol, rrr_o, deldelphi_grid, rrr_newt, iterres, lambda
logical :: singlegrid ! if true then only the low level grid is used
integer, parameter :: itersteproundoff = 50 ! this is a second criterion which triggers recalculation of the coefficients
double precision, parameter :: d_min = 1.d-60, roundoff_trigger = 1.d+4
logical, parameter :: single_update = .false. ! instead of updating delphi as each deldelphi_grid is calculated, assemble a deldelphi and do a line search using this - is more expensive, and seems to perform worse
logical, parameter :: debug = .false.
logical :: debug_sparse = .true.

if (debug) debug_sparse = .true.
if (debug_sparse) write(*,'(80(1h+)/a)') 'subroutine multigrid_mainsolver'

ierror = 4 ! this signals an error

!---------------------
! initialise and allocate loop variables

! initialise and allocate loop variables
if (.not.allocated(ee)) then
  allocate(ee(ptotal),r(ptotal),ee_scale(ptotal))
  delphi = 0.d0 ! zero on the first iteration only
  if (single_update) allocate(deldelphi(ptotal),delr(ptotal))
end if

! create reverse lookup table of equations from unknowns
call calc_equation_from_unknown(ppeonly=.false.)

! and calculate the normalisation (iterative preconditioning) vector
call calc_ee_normalise(ee_scale,nelements)

! now create (multi)grids, stored in multigrid array, with corresponding contained delr
call calc_multigrid(ee_scale)

! convert ee_scale into a preconditioned -> nondimensionalised scale factor, forming ee and finding ee_scale_max at the same time
call calc_ee_scale(ee_scale,ee,ee_scale_max)

delphi = 0.d0 ! for now this is nondimensionalised, but will dimensionalise before leaving the routine
r = ee ! set initial residual
rrr = dot_product_with_itself(r)
rrr_newt_tol = ptotal*(max(iterrestol,newtres*iterresreltol))**2
rrr_tol = rrr_newt_tol/(ee_scale_max**2)
rrr_o = rrr ! rrr_o is the last time the coefficients were calculated explicitly from r

if (debug) then
  call print_debug_vector(ee,"ee")
  call print_debug_vector(ee_scale,"ee_scale")
  call print_debug_vector(r,"r")
  write(93,'(a,g14.6)') 'rrr = ',rrr
  write(93,'(a,g14.6)') 'rrr_newt_tol = ',rrr_newt_tol
  write(93,'(a,g14.6)') 'rrr_tol = ',rrr_tol
  write(93,'(a,g14.6)') 'ee_scale_max = ',ee_scale_max
  write(93,'(a,i8,1(a,g14.7))') "ITERATIONS:        start iterstep = ",0,": rrr = ",rrr
  write(93,'(a)') repeat('-',80)
end if

if (singlegrid) then
  nl_lower = nmultigrid
else
  nl_lower = 1
end if

! start iteration loop
iterstep = 0
iterstepchecknext = 0
itersteproundoffnext = itersteproundoff
iteration_loop: do

  if (debug) then
    write(93,'(a,i10)') 'At start of iteration_loop, iterstep = ',iterstep
    rrr_newt = dot_product_with_itself_scaled(r,ee_scale)
    write(93,'(2(a,g14.7))') "rrr_newt = ",rrr_newt,": rrr = ",rrr
  end if

  if (iterstepchecknext == iterstep) then
    iterstepchecknext = iterstepchecknext + iterstepcheck
    rrr_newt = dot_product_with_itself_scaled(r,ee_scale)

    if (debug_sparse) then
      iterres = sqrt(rrr_newt/dble(ptotal))
      write(*,'(1(a,i8),3(a,g14.7))') "ITERATION: iterstep = ",iterstep, &
        ": iterres = ",iterres,": rrr = ",rrr,": rrr_newt = ",rrr_newt
      if (convergence_details_file) &
        write(fconverge,'(1(a,i8),3(a,g14.7))') "ITERATION: iterstep = ",iterstep, &
          ": iterres = ",iterres,": rrr = ",rrr,": rrr_newt = ",rrr_newt
    end if

    if (rrr_newt < rrr_newt_tol) then ! iterations have converged based on rrr_newt
      ierror = 0
      exit iteration_loop
    end if

    if (check_stopfile("stopback")) then
      write(*,'(a)') 'INFO: user requested simulation stop via "kill" file'
      ierror = -1
      exit iteration_loop
    end if

  end if

  if (rrr < rrr_tol) then ! iterations have converged based on rrr
    rrr_newt = dot_product_with_itself_scaled(r,ee_scale)
    ierror = 0
    exit iteration_loop
  end if

  if (iterstep == iterstepmax) exit iteration_loop ! maximum iterations have been performed without convergence

! now do the updates
  iterstep = iterstep + 1

! loop through all grids, depending on whether called with multigrid or not
  do nl = nl_lower, nmultigrid

    if (single_update) then
      deldelphi = 0.d0
      delr = 0.d0
    end if

! loop through all levels within this grid
    do ng = 1, multigrid(nl)%ngrid

! first calculate update to deldelphi for this grid
      deldelphi_grid = 0.d0
      do pppe = 1, multigrid(nl)%grid(ng)%ndelr
        ppe = multigrid(nl)%grid(ng)%delr_elements(pppe)
        deldelphi_grid = deldelphi_grid + multigrid(nl)%grid(ng)%delr(pppe)*r(ppe)
      end do
      deldelphi_grid = deldelphi_grid*multigrid(nl)%grid(ng)%minus_reciprocal_delr_dot_delr

! now apply this update to all delphis in this grid
      do pppu = 1, multigrid(nl)%grid(ng)%nunknown_elements
        ppu = multigrid(nl)%grid(ng)%unknown_elements(pppu)
        if (single_update) then
          deldelphi(ppu) = deldelphi(ppu) + deldelphi_grid
        else
          delphi(ppu) = delphi(ppu) + deldelphi_grid
        end if
      end do

! and apply it to all residuals
      do pppe = 1, multigrid(nl)%grid(ng)%ndelr
        ppe = multigrid(nl)%grid(ng)%delr_elements(pppe)
        if (single_update) then
          delr(ppe) = delr(ppe) + deldelphi_grid*multigrid(nl)%grid(ng)%delr(pppe)
        else
          r(ppe) = r(ppe) + deldelphi_grid*multigrid(nl)%grid(ng)%delr(pppe)
        end if
      end do

      if (debug.and..not.single_update) then
        write(93,*) 'GGGGGGGGGGGGGGGGGGGGG'
        write(93,'(3(a,i8))') "iterstep = ",iterstep,": level = ",nl,": grid = ",ng
        write(93,'(a,g14.6)') "deldelphi_grid = ",deldelphi_grid
        call print_debug_vector(r,"r")
        call print_debug_vector(delphi,"delphi")
      end if

    end do

    if (single_update) then

! perform a line search along the direction of deldelphi
      lambda = dot_product_with_itself(delr)
      if (lambda < d_min) then
        if (debug) write(93,'(a)') 'lambda too small, so exiting grid loop (possibly after convergence)'
        exit
      end if
      lambda = -dot_product_local(delr,r)/lambda

      if (debug) then
        write(93,*) 'MMMMMMMMMMMMMMMMMMMMM'
        write(93,'(2(a,i8))') "iterstep = ",iterstep,": level = ",nl
        write(93,'(a,g14.6)') "lambda = ",lambda
        call print_debug_vector(delr,"delr")
        call print_debug_vector(deldelphi,"deldelphi")
      end if

! now update solution based on this line search
      delphi = delphi + lambda*deldelphi
      r = r + lambda*delr

      if (debug) then
        call print_debug_vector(r,"r")
        call print_debug_vector(delphi,"delphi")
      end if

    else

      if (debug) then
        write(93,*) 'MMMMMMMMMMMMMMMMMMMMM'
        write(93,'(2(a,i8))') "iterstep = ",iterstep,": level = ",nl
        call print_debug_vector(r,"r")
        call print_debug_vector(delphi,"delphi")
      end if

    end if
    
  end do

  rrr = dot_product_with_itself(r) ! could calculate this incrementally

! if residual has decreased significantly, then recalculate the factors to guard against roundoff errors
! do this before convergence check, incase roundoff error has infected convergence
  if (rrr_o/rrr > roundoff_trigger.or.iterstep == itersteproundoffnext) then
    itersteproundoffnext = itersteproundoffnext + itersteproundoff
    rrr_o = rrr
    r = ee
    do ng = 1, multigrid(nmultigrid)%ngrid
      ppu = multigrid(nmultigrid)%grid(ng)%unknown_elements(1) ! at this level there should only be one unknown contained within this grid
      do pppe = 1, multigrid(nmultigrid)%grid(ng)%ndelr
        ppe = multigrid(nmultigrid)%grid(ng)%delr_elements(pppe)
        r(ppe) = r(ppe) + multigrid(nmultigrid)%grid(ng)%delr(pppe)*delphi(ppu)
      end do
    end do
    rrr = dot_product_with_itself(r)
    if (debug) then
      write(93,'(a)') 'in roundoff_trigger'
      call print_debug_vector(r,"r")
      write(93,'(a,g14.6)') 'rrr = ',rrr
      write(93,'(a,g14.6)') 'rrr_o = ',rrr_o
    end if
!   if (debug_sparse) write(*,'(a)') 'ITERATIONS: recalculating coefficients to avoid round-off errors'
    if (debug) write(*,'(a)') 'ITERATIONS: recalculating coefficients to avoid round-off errors'
  end if

end do iteration_loop
  
iterres = sqrt(rrr_newt/dble(ptotal))
if (ierror == 0) then
  if (debug_sparse) then
    write(*,'(a,i8,3(a,g14.7))') "ITERATIONS: convered iterstep = ",iterstep,": iterres = ",iterres, &
      ": rrr = ",rrr,": rrr_newt = ",rrr_newt
    if (convergence_details_file) &
      write(fconverge,'(a,i8,3(a,g14.7))') &
      "ITERATIONS: converged iterstep = ",iterstep,": iterres = ",iterres, &
      ": rrr = ",rrr,": rrr_newt = ",rrr_newt
  end if
else
  write(*,'(a,i8,3(a,g14.7))') "ITERATIONS WARNING: failed iterstep = ",iterstep,": iterres = ",iterres, &
    ": rrr = ",rrr,": rrr_newt = ",rrr_newt
  if (convergence_details_file) &
    write(fconverge,'(a,i8,3(a,g14.7))') &
    "ITERATIONS WARNING: failed iterstep = ",iterstep,": iterres = ",iterres, &
    ": rrr = ",rrr,": rrr_newt = ",rrr_newt
end if

!---------------------
! redimensionalise delphi
do ppu = 1, ptotal
  m = unknown_var_from_pp(ppu)
  delphi(ppu) = delphi(ppu)*var(m)%magnitude
end do

!---------------------

if (debug_sparse) write(*,'(a/80(1h-))') 'subroutine multigrid_mainsolver'

end subroutine multigrid_mainsolver

!-----------------------------------------------------------------

subroutine calc_equation_from_unknown(ppeonly)

! here we create/update the equation from unknown lookup structure
! if ppeonly is true, then we only create pp array

use general_module

integer :: ppe, mm, m, ns, pppu, ppu
logical :: ppeonly
logical :: debug_sparse = .false.

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
        if (.not.ppeonly) then
          call push_integer_array(array=equation_from_unknown(ppu)%equation_m,new_element=m)
          call push_integer_array(array=equation_from_unknown(ppu)%equation_ns,new_element=ns)
          call push_integer_array(array=equation_from_unknown(ppu)%within_equation_index,new_element=pppu)
        end if
        call push_integer_array(array=equation_from_unknown(ppu)%equation_pp,new_element=ppe)
      else
        if (.not.ppeonly) then
          equation_from_unknown(ppu)%equation_m(equation_from_unknown(ppu)%nequation)=m
          equation_from_unknown(ppu)%equation_ns(equation_from_unknown(ppu)%nequation)=ns
          equation_from_unknown(ppu)%within_equation_index(equation_from_unknown(ppu)%nequation)=pppu
        end if
        equation_from_unknown(ppu)%equation_pp(equation_from_unknown(ppu)%nequation)=ppe
      end if
    end do
  end do
end do

if (debug_sparse) write(*,'(a/80(1h-))') 'subroutine calc_equation_from_unknown'

end subroutine calc_equation_from_unknown

!-----------------------------------------------------------------

subroutine calc_multigrid(ee_scale)

! here we create/update the multigrid structure and the contained delr arrays

use general_module

double precision, dimension(:) :: ee_scale ! scale factor for each equation, pre-calculated
integer :: ncells, ppu_next, ppu_unallocated, ppu_last, pppe, ppe, m, ns, pppu, ppu, nm, ng, pp, mu, nsu, &
  nu, default_grid_size, delnu, ngsub, n, ndelr_element_list
integer, dimension(:), allocatable :: unknowns_marker
double precision :: coefficient_strength, next_coefficient_strength, delr_dot_delr
! the following are used as temporary arrays when finding delr contained within multigrid
double precision, dimension(:), allocatable, save :: delr_linear ! the delr values
logical, dimension(:), allocatable, save :: delr_marker ! a marker array showing what elements are used
integer, dimension(:), allocatable, save :: delr_element_list ! and list of the elements
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
! now move through each grid within each multigrid calculating delr
if (.not.allocated(delr_linear)) allocate(delr_linear(ptotal),delr_marker(ptotal),delr_element_list(ptotal))
delr_linear = 0.d0
delr_marker = .false.
delr_element_list = 0
ndelr_element_list = 0
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
        delr_linear(ppe) = delr_linear(ppe) + var(m)%funk(ns)%dv(n)/ee_scale(ppe)
        if (.not.delr_marker(ppe)) then
          ndelr_element_list = ndelr_element_list + 1
          delr_element_list(ndelr_element_list) = ppe
          delr_marker(ppe) = .true.
        end if
      end do
    end do

! now transfer back into multigrid structure
    multigrid(nm)%grid(ng)%ndelr = ndelr_element_list
    if (allocatable_integer_size(multigrid(nm)%grid(ng)%delr_elements) < ndelr_element_list) then
      call resize_integer_array(array=multigrid(nm)%grid(ng)%delr_elements,new_size=ndelr_element_list, &
        keep_data=.false.)
      call resize_double_precision_array(array=multigrid(nm)%grid(ng)%delr,new_size=ndelr_element_list, &
        keep_data=.false.)
    end if
    multigrid(nm)%grid(ng)%delr_elements(1:ndelr_element_list) = delr_element_list
! now assemble delr from delr_linear, while also resetting the temporary delr arrays
! and also calculating minus_reciprocal_delr_dot_delr
    delr_dot_delr = 0.d0
    do n = 1, ndelr_element_list
      multigrid(nm)%grid(ng)%delr(n) = delr_linear(delr_element_list(n))
      delr_marker(delr_element_list(n)) = .false.
      delr_linear(delr_element_list(n)) = 0.d0
      delr_element_list(n) = 0
      delr_dot_delr = delr_dot_delr + multigrid(nm)%grid(ng)%delr(n)**2
    end do
    if (delr_dot_delr < tiny(1.d0)) call error_stop("delr_dot_delr is <= tiny in subroutine calc multigrid")
    multigrid(nm)%grid(ng)%minus_reciprocal_delr_dot_delr = -1.d0/delr_dot_delr
    ndelr_element_list = 0
    
  end do
end do

if (debug_sparse) write(*,'(a/80(1h-))') 'subroutine calc_multigrid'

end subroutine calc_multigrid

!-----------------------------------------------------------------

subroutine bicg_mainsolver(ierror,stabilised)

! here we use a unpreconditioned bicg technique to solve the linear system, using equation funk data directly
! routine is based on pseudo-code from H. A. van der Vorst, SIAM J. Sci. and Stat. Comput., 13(2), 631–644., DOI:10.1137/0913035
! the logical stabilised determines whether bicg or bicgstab is used

use general_module
use equation_module

logical :: stabilised
integer :: ierror, m, j, iterstep, ppu, nelements, iterstepchecknext
double precision, dimension(:), allocatable, save :: r1, r2, p1, p2, v, t, s, ee, ee_scale ! allocate these once as their size doesn't change
double precision :: alpha, beta, iterres, alpha_demoninator, rho_o, rho, omega, omega_demoninator, ee_scale_max, &
  rrr_newt, rrr_tol, rrr, rrr_newt_tol
double precision, parameter :: alpha_max = 1.d6, beta_max = 1.d6, omega_max = 1.d6, &
  alpha_demoninator_min = tiny(1.d0), rho_o_min = tiny(1.d0), increment_multiplier = 1.d-3, omega_min = tiny(1.d0), &
  omega_demoninator_min = tiny(1.d0)
type(jacobian_type), save :: jacobian, jacobian_transpose
logical :: restart ! this is a flag to indicate that solution needs to be restarted
logical, parameter :: debug = .false.
logical :: debug_sparse = .true.

if (debug) debug_sparse = .true.
if (debug_sparse) write(*,'(80(1h+)/a)') 'subroutine bicg_mainsolver'

ierror = 4 ! this signals an error

!---------------------
! initialise and allocate loop variables
if (.not.allocated(r1)) then
  if (stabilised) then
    allocate(r1(ptotal),r2(ptotal),p1(ptotal),v(ptotal),t(ptotal),s(ptotal),ee(ptotal),ee_scale(ptotal))
  else
    allocate(r1(ptotal),r2(ptotal),p1(ptotal),p2(ptotal),v(ptotal),ee(ptotal),ee_scale(ptotal))
  end if
end if

! we need the reverse lookup structure, equation_from_unknown, but only the ppe indicies, for forming the jacobian_transpose
call calc_equation_from_unknown(ppeonly=.true.)

! and calculate the normalisation (iterative preconditioning) vector
call calc_ee_normalise(ee_scale,nelements)

! nondimensionalise jacobian, and setup ee and ee_scale
call setup_jacobians(jacobian,jacobian_transpose,ee_scale,nelements)

! convert ee_scale into a preconditioned -> nondimensionalised scale factor, forming ee and finding ee_scale_max at the same time
call calc_ee_scale(ee_scale,ee,ee_scale_max)

restart = .false.

! initial guess for delphi is the zero vector
delphi = 0.d0
! form the initial residual as b - A.x, but with b = -equation (=-var(m)%funk(ns)%v) and A.x stored as r1 from above
!   call aa_dot_vector_jacobian(jacobian,delphi,r1)
!   r1 = -r1-ee
r1 = -ee
r2 = r1
rho = 1.d0
p1 = 0.d0 
if (stabilised) then
  alpha = 1.d0
  omega = 1.d0
  v = 0.d0 
else
  p2 = 0.d0 
end if
rrr = dot_product_with_itself(r1)
rrr_newt_tol = ptotal*(max(iterrestol,newtres*iterresreltol))**2
rrr_tol = rrr_newt_tol/(ee_scale_max**2)

if (debug) then
  call print_debug_vector(r1,"r1 in bicg_mainsolver")
  call print_debug_vector(r2,"r2 in bicg_mainsolver")
  call print_debug_vector(p1,"p1 in bicg_mainsolver")
  if (stabilised) then
    call print_debug_vector(v,"v in bicg_mainsolver")
  else
    call print_debug_vector(p2,"p2 in bicg_mainsolver")
  end if
  call print_debug_vector(delphi,"delphi in bicg_mainsolver")
end if

! start iteration loop
iterstep = 0
iterstepchecknext = 0
iteration_loop: do

! if restart has been requested, do so now
  if (restart) then
    if (debug) write(93,'(a)') 'RRRRR RESTART REQUESTED'
    do j = 1, ptotal
!     delphi(j) = delphi(j) + increment_multiplier*random() ! add a random increment onto delphi, noting that delphi should be normalised
      delphi(j) = random() ! overwrite delphi with a random increment, noting that delphi should be normalised
    end do
! form the initial residual as b - A.x, but with b = -equation (=-var(m)%funk(ns)%v) and A.x stored as r1 from above
    call aa_dot_vector_jacobian(jacobian,delphi,r1)
    r1 = -r1-ee
    r2 = r1
    rho = 1.d0
    p1 = 0.d0 
    if (stabilised) then
      alpha = 1.d0
      omega = 1.d0
      v = 0.d0 
    else
      p2 = 0.d0 
    end if
    restart = .false.
  end if

  if (debug) then
    write(93,'(a,i10)') 'At start of iteration_loop, iterstep = ',iterstep
    rrr_newt = dot_product_with_itself_scaled(r1,ee_scale)
    write(93,'(2(a,g14.7))') "rrr_newt = ",rrr_newt,": rrr = ",rrr
  end if

  if (iterstepchecknext == iterstep) then
    iterstepchecknext = iterstepchecknext + iterstepcheck
    rrr_newt = dot_product_with_itself_scaled(r1,ee_scale)

    if (debug_sparse) then
      iterres = sqrt(rrr_newt/dble(ptotal))
      write(*,'(1(a,i8),3(a,g14.7))') "ITERATION: iterstep = ",iterstep, &
        ": iterres = ",iterres,": rrr = ",rrr,": rrr_newt = ",rrr_newt
      if (convergence_details_file) &
        write(fconverge,'(1(a,i8),3(a,g14.7))') "ITERATION: iterstep = ",iterstep, &
          ": iterres = ",iterres,": rrr = ",rrr,": rrr_newt = ",rrr_newt
    end if

    if (rrr_newt < rrr_newt_tol) then ! iterations have converged based on rrr_newt
      ierror = 0
      exit
    end if

    if (check_stopfile("stopback")) then
      write(*,'(a)') 'INFO: user requested simulation stop via "kill" file'
      ierror = -1
      exit
    end if

  end if

  if (rrr < rrr_tol) then ! iterations have converged based on rrr
    rrr_newt = dot_product_with_itself_scaled(r1,ee_scale)
    ierror = 0
    exit
  end if

  if (iterstep == iterstepmax) exit ! maximum iterations have been performed without convergence

! now do the updates
  iterstep = iterstep + 1

! save rho from the previous iteration as rho_o
  rho_o = rho
! and calculate the new rho
  rho = dot_product_local(r1,r2)
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
  call aa_dot_vector_jacobian(jacobian,p1,v) ! v = A.p1
  if (debug) call print_debug_vector(v,"A.p1")
  if (stabilised) then
    alpha_demoninator = dot_product_local(r2,v)
  else
    alpha_demoninator = dot_product_local(p2,v)
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
    call aa_dot_vector_jacobian(jacobian,s,t)
    if (debug) call print_debug_vector(t,"t updated")
    omega_demoninator = dot_product_with_itself(t)
    if (debug) write(93,'(a,g11.3)') 'omega_demoninator = ',omega_demoninator
    if (abs(omega_demoninator) < omega_demoninator_min) then
      write(*,'(a)') "WARNING: omega_demoninator small in bicg, restarting iterations"
      restart = .true.
      cycle iteration_loop
    end if
    omega = dot_product_local(t,s)/omega_demoninator
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
    call aa_dot_vector_jacobian(jacobian_transpose,p2,v) ! temporarily use v to store A^T.p2
    r2 = r2 - alpha*v
    if (debug) then
      call print_debug_vector(v,"A^T.p2")
      call print_debug_vector(r1,"r1")
      call print_debug_vector(r2,"r2")
    end if

  end if

  rrr = dot_product_with_itself(r1)

end do iteration_loop
  
iterres = sqrt(rrr_newt/dble(ptotal))
if (ierror == 0) then
  if (debug_sparse) then
    write(*,'(a,i8,3(a,g14.7))') "ITERATIONS: convered iterstep = ",iterstep,": iterres = ",iterres, &
      ": rrr = ",rrr,": rrr_newt = ",rrr_newt
    if (convergence_details_file) &
      write(fconverge,'(a,i8,3(a,g14.7))') &
      "ITERATIONS: converged iterstep = ",iterstep,": iterres = ",iterres, &
      ": rrr = ",rrr,": rrr_newt = ",rrr_newt
  end if
else
  write(*,'(a,i8,3(a,g14.7))') "ITERATIONS WARNING: failed iterstep = ",iterstep,": iterres = ",iterres, &
    ": rrr = ",rrr,": rrr_newt = ",rrr_newt
  if (convergence_details_file) &
    write(fconverge,'(a,i8,3(a,g14.7))') &
    "ITERATIONS WARNING: failed iterstep = ",iterstep,": iterres = ",iterres, &
    ": rrr = ",rrr,": rrr_newt = ",rrr_newt
end if

!---------------------
! redimensionalise delphi
do ppu = 1, ptotal
  m = unknown_var_from_pp(ppu)
  delphi(ppu) = delphi(ppu)*var(m)%magnitude
end do

!---------------------

if (debug_sparse) write(*,'(a/80(1h-))') 'subroutine bicg_mainsolver'

end subroutine bicg_mainsolver

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

subroutine descent_mainsolver(ierror,dogleg)

! here we use a descent algorithm to solve the linear system, using equation funk data directly
! depending on the dogleg logical, a proportion of the previous update vector may also be included in each update

use general_module
use equation_module
!$ use omp_lib

integer :: ierror, m, iterstep, ppu, iterstepchecknext, itersteproundoffnext, n, nelements
logical :: dogleg ! if this is false, then a pure descent algorithm is used
logical :: dogleg_l ! per iteration version of dogleg
double precision, dimension(:), allocatable, save :: delx, ee, w_x, delp, w_p, r, ee_scale ! allocate these once as their size doesn't change
type(jacobian_type), save :: jacobian, jacobian_transpose
double precision :: rrr, delrrr, delta, gamma, rrr_o, d, iterres, ee_scale_max, rrr_newt_tol, rrr_tol, rrr_newt
double precision :: a_xx, a_rx, a_rp, a_pp, a_xp ! these are mainly dot_products
double precision, parameter :: d_min = 1.d-60, roundoff_trigger = 1.d+4
integer, parameter :: itersteproundoff = 50 ! this is a second criterion which triggers recalculation of the coefficients
logical, parameter :: unwrap_vector_operations = .true.
logical, parameter :: debug = .false.
logical :: debug_sparse = .true.

if (debug) debug_sparse = .true.
if (debug_sparse) write(*,'(80(1h+)/a)') 'subroutine descent_mainsolver'

ierror = 4 ! this signals an error

! initialise and allocate loop variables
if (.not.allocated(delx)) then
  allocate(delx(ptotal),ee(ptotal),w_x(ptotal),delp(ptotal),w_p(ptotal),r(ptotal),ee_scale(ptotal))
! delphi = 0.d0 ! zero on the first iteration only
end if

! we need the reverse lookup structure, equation_from_unknown, but only the ppe indicies, for forming the jacobian_transpose
call calc_equation_from_unknown(ppeonly=.true.)

! and calculate the normalisation (iterative preconditioning) vector
call calc_ee_normalise(ee_scale,nelements)

! nondimensionalise jacobian, and setup ee and ee_scale
call setup_jacobians(jacobian,jacobian_transpose,ee_scale,nelements)

! convert ee_scale into a preconditioned -> nondimensionalised scale factor, forming ee and finding ee_scale_max at the same time
call calc_ee_scale(ee_scale,ee,ee_scale_max)

!---------------------
! initial guess for delphi is the zero vector
delphi = 0.d0 ! now keep previous solution as the first guess
! initialise other variables
delx = 0.d0
w_x = 0.d0
r = ee
rrr = dot_product_with_itself(r)
rrr_newt_tol = ptotal*(max(iterrestol,newtres*iterresreltol))**2
rrr_tol = rrr_newt_tol/(ee_scale_max**2)
a_xx = 0.d0
a_xp = 0.d0
a_rx = 0.d0
gamma = 0.d0
rrr_o = rrr ! rrr_o is the last time the coefficients were calculated explicitly from r

if (debug) then
  call print_debug_vector(ee,"ee")
  call print_debug_vector(ee_scale,"ee_scale")
  call print_debug_vector(r,"r")
  call print_scaled_jacobian_matrix(jacobian)
  write(93,'(a,g14.6)') 'rrr = ',rrr
  write(93,'(a,g14.6)') 'rrr_newt_tol = ',rrr_newt_tol
  write(93,'(a,g14.6)') 'rrr_tol = ',rrr_tol
  write(93,'(a,g14.6)') 'ee_scale_max = ',ee_scale_max
  write(93,'(a,i8,1(a,g14.7))') "ITERATIONS:        start iterstep = ",0,": rrr = ",rrr
  write(93,'(a)') repeat('-',80)
end if

! start iteration loop
iterstep = 0
iterstepchecknext = 0
itersteproundoffnext = itersteproundoff
do

  if (debug) then
    write(93,'(a,i10)') 'At start of iteration_loop, iterstep = ',iterstep
    rrr_newt = dot_product_with_itself_scaled(r,ee_scale)
    write(93,'(2(a,g14.7))') "rrr_newt = ",rrr_newt,": rrr = ",rrr
  end if

  if (iterstepchecknext == iterstep) then
    iterstepchecknext = iterstepchecknext + iterstepcheck
    rrr_newt = dot_product_with_itself_scaled(r,ee_scale)

    if (debug_sparse) then
      iterres = sqrt(rrr_newt/dble(ptotal))
      write(*,'(1(a,i8),3(a,g14.7))') "ITERATION: iterstep = ",iterstep, &
        ": iterres = ",iterres,": rrr = ",rrr,": rrr_newt = ",rrr_newt
      if (convergence_details_file) &
        write(fconverge,'(1(a,i8),3(a,g14.7))') "ITERATION: iterstep = ",iterstep, &
          ": iterres = ",iterres,": rrr = ",rrr,": rrr_newt = ",rrr_newt
    end if

    if (rrr_newt < rrr_newt_tol) then ! iterations have converged based on rrr_newt
      ierror = 0
      exit
    end if

    if (check_stopfile("stopback")) then
      write(*,'(a)') 'INFO: user requested simulation stop via "kill" file'
      ierror = -1
      exit
    end if

  end if

  if (rrr < rrr_tol) then ! iterations have converged based on rrr
    rrr_newt = dot_product_with_itself_scaled(r,ee_scale)
    ierror = 0
    exit
  end if

  if (iterstep == iterstepmax) exit ! maximum iterations have been performed without convergence

! now do the updates
  iterstep = iterstep + 1
! delp = J^T.r (actually delp^o in notation of notes)
  call aa_dot_vector_jacobian(jacobian_transpose,r,delp)
! w_p = J.delp
  call aa_dot_vector_jacobian(jacobian,delp,w_p) 
  if (unwrap_vector_operations) then
! ever so slightly faster to do a single loop for the three dot products
    a_rp = 0.d0
    a_pp = 0.d0
    a_xp = 0.d0
    !$omp parallel do private(n) reduction(+:a_rp,a_pp,a_xp)
    do n = 1, ptotal
      a_rp = a_rp + r(n)*w_p(n)
      a_pp = a_pp + w_p(n)**2
      a_xp = a_xp + w_x(n)*w_p(n)
    end do
    !$omp end parallel do
  else
    a_rp = dot_product_local(r,w_p)
    a_pp = dot_product_with_itself(w_p)
    a_xp = dot_product_local(w_x,w_p) ! not needed for non-dogleg, but efficiency for that case isn't a priority
  end if

  if (debug) then
    write(93,'(a,i4)') 'iterstep = ',iterstep
    call print_debug_vector(delp,"delp")
    call print_debug_vector(w_p,"w_p")
    write(93,'(a,g14.6)') 'a_rp = ',a_rp
    write(93,'(a,g14.6)') 'a_pp = ',a_pp
    write(93,'(a,g14.6)') 'a_xx = ',a_xx
  end if

  if (a_pp < d_min) then
    if (debug) write(93,'(a)') 'a_pp too small, so exiting iteration loop, presumably after convergence'
    exit
  end if

  dogleg_l = .false.
  if (dogleg) then
    if (a_xx > d_min) then
      d = (a_pp*a_xx-a_xp**2)
      if (abs(d) > d_min) dogleg_l = .true.
    end if
  end if

  if (dogleg_l) then
    delta = -(a_rp*a_xx-a_rx*a_xp)/d
    gamma = -(a_xp*delta+a_rx)/a_xx
!   a_xx = (a_xx*gamma + 2.d0*delta*a_xp)*gamma + delta**2*a_pp ! updating a_xx now
!   a_rx = delta*a_rp + gamma*a_rx
!   delrrr = a_xx+2.d0*a_rx
!   a_rx = a_xx + a_rx
!   delrrr = (a_xx*gamma + 2.d0*delta*a_xp)*gamma + delta**2*a_pp + 2.d0*(delta*a_rp + gamma*a_rx)
  else
    delta = -a_rp/a_pp
    gamma = 0.d0
!   a_xx = 0.d0
!   a_rx = 0.d0
!   delrrr = (a_pp*delta+2.d0*a_rp)*delta
  end if

  if (debug) then
    write(93,'(a)') 'after calculating delta/gamma'
    write(93,'(a,l)') 'dogleg_l = ',dogleg_l
    write(93,'(a,g14.6)') 'delta = ',delta
    write(93,'(a,g14.6)') 'gamma = ',gamma
    write(93,'(a,g14.6)') 'delrrr = ',delrrr
  end if

  if (dogleg) then
    a_xx = a_xx*gamma**2 + 2.d0*delta*gamma*a_xp + delta**2*a_pp
    a_rx = a_xx + delta*a_rp + gamma*a_rx
    delrrr = a_xx + 2.d0*(delta*a_rp + gamma*a_rx)
    if (unwrap_vector_operations) then
      !$omp parallel do private(n) shared(delta,gamma)
      do n = 1, ptotal
        w_x(n) = delta*w_p(n) + gamma*w_x(n)
        delx(n) = delta*delp(n) + gamma*delx(n)
        r(n) = r(n) + w_x(n)
        delphi(n) = delphi(n) + delx(n)
      end do
      !$omp end parallel do
    else
      w_x = delta*w_p + gamma*w_x
      delx = delta*delp + gamma*delx
      r = r + w_x
      delphi = delphi + delx
    end if
  else
! a_xx = a_rx = 0.d0 under the non-dogleg scheme, always
    delrrr = (a_pp*delta+2.d0*a_rp)*delta
    w_x = delta*w_p
    delx = delta*delp
    r = r + w_x
    delphi = delphi + delx
  end if

  rrr = rrr + delrrr

  if (debug) then
    write(93,'(a)') 'after updating w_x/delx and r/delphi'
    write(93,'(a,g14.6)') 'a_xx = ',a_xx
    write(93,'(a,g14.6)') 'a_rx = ',a_rx
    call print_debug_vector(w_x,"w_x")
    call print_debug_vector(delx,"delx")
    write(93,'(a)') 'after updating r/delphi'
    call print_debug_vector(r,"r")
    call print_debug_vector(delphi,"delphi")
    write(93,'(a,g14.6)') 'rrr = ',rrr
    write(93,'(a,g14.6)') 'rrr_o = ',rrr_o
  end if

  if (delrrr >= 0.d0) then
    write(*,*) 'delrrr > 0.d0'
  end if
  
! if residual has decreased significantly, then recalculate the factors to guard against roundoff errors
! do this before convergence check, incase roundoff error has infected convergence
  if (rrr_o/rrr > roundoff_trigger.or.iterstep == itersteproundoffnext) then
    itersteproundoffnext = itersteproundoffnext + itersteproundoff
    rrr_o = rrr
    call aa_dot_vector_jacobian(jacobian,delphi,r) 
    r = ee + r
    rrr = dot_product_with_itself(r)
    call aa_dot_vector_jacobian(jacobian,delx,w_x) 
    a_xx = dot_product_with_itself(w_x)
    a_rx = dot_product_local(r,w_x)
    if (debug) then
      write(93,'(a)') 'in roundoff_trigger'
      call print_debug_vector(r,"r")
      write(93,'(a,g14.6)') 'rrr = ',rrr
      call print_debug_vector(w_x,"w_x")
      write(93,'(a,g14.6)') 'a_xx = ',a_xx
      write(93,'(a,g14.6)') 'a_rx = ',a_rx
    end if
!   if (debug_sparse) write(*,'(a)') 'ITERATIONS: recalculating coefficients to avoid round-off errors'
    if (debug) write(*,'(a)') 'ITERATIONS: recalculating coefficients to avoid round-off errors'
  end if

end do
  
iterres = sqrt(rrr_newt/dble(ptotal))
if (ierror == 0) then
  if (debug_sparse) then
    write(*,'(a,i8,3(a,g14.7))') "ITERATIONS: convered iterstep = ",iterstep,": iterres = ",iterres, &
      ": rrr = ",rrr,": rrr_newt = ",rrr_newt
    if (convergence_details_file) &
      write(fconverge,'(a,i8,3(a,g14.7))') &
      "ITERATIONS: converged iterstep = ",iterstep,": iterres = ",iterres, &
      ": rrr = ",rrr,": rrr_newt = ",rrr_newt
  end if
else
  write(*,'(a,i8,3(a,g14.7))') "ITERATIONS WARNING: failed iterstep = ",iterstep,": iterres = ",iterres, &
    ": rrr = ",rrr,": rrr_newt = ",rrr_newt
  if (convergence_details_file) &
    write(fconverge,'(a,i8,3(a,g14.7))') &
    "ITERATIONS WARNING: failed iterstep = ",iterstep,": iterres = ",iterres, &
    ": rrr = ",rrr,": rrr_newt = ",rrr_newt
end if

!---------------------
! redimensionalise delphi
do ppu = 1, ptotal
  m = unknown_var_from_pp(ppu)
  delphi(ppu) = delphi(ppu)*var(m)%magnitude
end do

!---------------------

if (debug_sparse) write(*,'(a/80(1h-))') 'subroutine descent_mainsolver'

end subroutine descent_mainsolver

!-----------------------------------------------------------------

subroutine calc_ee_normalise(ee_scale,nelements)

! here we calculate the equation normalisation vector, which at the end of this routine, is used to convert the dimensional jacobian and rhs into
!  a (preconditioned) normalised matrix/vector, ready for the iterative routines
! at the end of this routine ee_scale is not ready to convert between dimensional and nondimensionalised by magnitudes - ee_scale is changed later to
!  do this within calc_ee_scale

use general_module
integer :: mm, m, ns, pppu, ppu, mu, ppe, nelements, n_positive, n_negative
double precision :: one_element, ee_positive, ee_negative
double precision, dimension(:) :: ee_scale
integer, parameter :: normalisation_method = 3 ! see below

! performance of normalisation methods:
! 0: no normalisation
!    >60000 iterations for heat_conduction_around_ellipse test problem + doglegdescent
! 1: normalise by largest (in magnitude) element, hence changing signs
!    6593 iterations for heat_conduction_around_ellipse test problem + doglegdescent
! 2: normalise without changing sign, and based on sum of row elements
!    7300 iterations for heat_conduction_around_ellipse test problem + doglegdescent
! 3: normalise with changing sign, and based on the sum of either positive or negative row elements, whichever is greater
!    6121 iterations for heat_conduction_around_ellipse test problem + doglegdescent
! 4: normalise with changing sign, and based on the average value of either positive or negative row elements, whichever is greater
!    24775 iterations for heat_conduction_around_ellipse test problem + doglegdescent
! 5: normalise with changing sign, and based on the sqrt of sum of squares of each of either positive or negative row elements, whichever is greater
!    6576 iterations for heat_conduction_around_ellipse test problem + doglegdescent
! 6: normalise with changing sign, and based on the rms value of either positive or negative row elements, whichever is greater
!    12500 iterations for heat_conduction_around_ellipse test problem + doglegdescent
! 7: as per 5, but doing sum of sqrts instead of sum of squares
!    22159 iterations for heat_conduction_around_ellipse test problem + doglegdescent

! first run through all elements maximum element size (stored in ee_scale) and counting total number of elements

ppe = 0
nelements = 0
ee_scale = 0.d0
do mm = 1, allocatable_size(var_list(var_list_number_equation)%list)
  m = var_list(var_list_number_equation)%list(mm) ! equation var number
  do ns = 1, ubound(var(m)%funk,1)
    ppe = ppe + 1
    if (normalisation_method == 0) ee_scale = 1.d0
    if (normalisation_method >= 3) then
      ee_positive = 0.d0
      ee_negative = 0.d0
    end if
    if (normalisation_method == 4.or.normalisation_method == 6) then
      n_positive = 0
      n_negative = 0
    end if
    do pppu = 1, var(m)%funk(ns)%ndv ! here we cycle through all the unknowns that are referenced within this equation
      ppu = var(m)%funk(ns)%pp(pppu) ! this is the unknown pp number
      mu = unknown_var_from_pp(ppu) ! unknown var number
      nelements = nelements + 1
      if (normalisation_method == 0) cycle
      one_element = var(m)%funk(ns)%dv(pppu)*var(mu)%magnitude
! calculate ee_scale, which is (according to notes of 28/4/16) actually a matrix normalisation factor
      if (normalisation_method == 1) then
        if (abs(one_element) > abs(ee_scale(ppe))) ee_scale(ppe) = one_element
      else if (normalisation_method == 2) then
        ee_scale(ppe) = ee_scale(ppe) + abs(one_element)
      else if (normalisation_method == 3) then
        if (one_element > 0.d0) then
          ee_positive = ee_positive + one_element
        else
          ee_negative = ee_negative - one_element
        end if
      else if (normalisation_method == 4) then
        if (one_element > 0.d0) then
          ee_positive = ee_positive + one_element
          n_positive = n_positive + 1
        else
          ee_negative = ee_negative - one_element
          n_negative = n_negative + 1
        end if
      else if (normalisation_method == 5) then
        if (one_element > 0.d0) then
          ee_positive = ee_positive + one_element**2
        else
          ee_negative = ee_negative + one_element**2
        end if
      else if (normalisation_method == 6) then
        if (one_element > 0.d0) then
          ee_positive = ee_positive + one_element**2
          n_positive = n_positive + 1
        else
          ee_negative = ee_negative + one_element**2
          n_negative = n_negative + 1
        end if
      else if (normalisation_method == 7) then
        if (one_element > 0.d0) then
          ee_positive = ee_positive + sqrt(one_element)
        else
          ee_negative = ee_negative + sqrt(-one_element)
        end if
      end if
    end do
    if (normalisation_method == 4.or.normalisation_method == 6) then
      ee_positive = ee_positive/dble(max(n_positive,1))
      ee_negative = ee_negative/dble(max(n_negative,1))
    end if
    if (normalisation_method >= 5 .and. normalisation_method <=6) then
      ee_positive = sqrt(ee_positive)
      ee_negative = sqrt(ee_negative)
    else if (normalisation_method >= 7) then
      ee_positive = ee_positive**2
      ee_negative = ee_negative**2
    end if
    if (normalisation_method >= 3) then
      if (ee_positive > ee_negative) then
        ee_scale(ppe) = ee_positive
      else
        ee_scale(ppe) = -ee_negative
      end if
    end if
  end do
end do

end subroutine calc_ee_normalise

!-----------------------------------------------------------------

subroutine calc_ee_scale(ee_scale,ee,ee_scale_max)

! here ee_scale is changed to make it suitable for calculating the newton residual, from the iterative solver scaled equation error
! prior to this ee is formed and scaled
! ee_scale_max is calculated at the same time

use general_module
integer :: ppe, mm, m, ns
double precision, dimension(:) :: ee_scale, ee
double precision :: ee_scale_max

ee_scale_max = 0.d0
ppe = 0
do mm = 1, allocatable_size(var_list(var_list_number_equation)%list)
  m = var_list(var_list_number_equation)%list(mm) ! equation var number
  do ns = 1, ubound(var(m)%funk,1)
    ppe = ppe + 1
    ee(ppe) = var(m)%funk(ns)%v/ee_scale(ppe)
! ee_scale is now used to calculate the newton-compatible residual, so also needs to be divided by equation magnitude
    ee_scale(ppe) = ee_scale(ppe)/var(m)%magnitude
! and this is the maximum magnitude of ee_scale across all equations
    ee_scale_max = max(ee_scale_max,abs(ee_scale(ppe)))
  end do
end do

end subroutine calc_ee_scale

!-----------------------------------------------------------------

subroutine setup_jacobians(jacobian,jacobian_transpose,ee_scale,nelements)

! here we create the fast lookup arrays for the jacobian (jacobian) and transpose (jacobian_transpose), doing the ee normalisation from
!  ee_scale at the same time

use general_module

integer :: mm, m, ns, pppu, ppu, mu, ppe, nelements, n, pppe, ii, i
double precision, dimension(:), allocatable :: ee_scale
type(jacobian_type) :: jacobian, jacobian_transpose
logical :: found

! now allocate fast-lookup jacobian and transpose matricies at the same time
if (.not.allocated(jacobian%ja)) allocate(jacobian%ja(ptotal+1),jacobian_transpose%ja(ptotal+1))
if (allocatable_integer_size(jacobian%i) < nelements) then
  if (allocated(jacobian%i)) deallocate(jacobian%v,jacobian%i,jacobian_transpose%v,jacobian_transpose%i)
  allocate(jacobian%v(nelements),jacobian%i(nelements),jacobian_transpose%v(nelements),jacobian_transpose%i(nelements))
end if

! run through equations again forming jacobian, ee and scaling equations

ppe = 0
n = 0
do mm = 1, allocatable_size(var_list(var_list_number_equation)%list)
  m = var_list(var_list_number_equation)%list(mm) ! equation var number
  do ns = 1, ubound(var(m)%funk,1)
    ppe = ppe + 1
    jacobian%ja(ppe) = n + 1 ! for normal jacobian now using compressed sparse row format (3 array variation), with 1 indexing (csr1) (as per mainsolver really)
    do pppu = 1, var(m)%funk(ns)%ndv ! here we cycle through all the unknowns that are referenced within this equation
      ppu = var(m)%funk(ns)%pp(pppu) ! this is the unknown pp number
      mu = unknown_var_from_pp(ppu) ! unknown var number
      n = n + 1
      jacobian%v(n) = var(m)%funk(ns)%dv(pppu)*var(mu)%magnitude/ee_scale(ppe)
      jacobian%i(n) = ppu
    end do
  end do
end do
jacobian%ja(ptotal+1) = nelements + 1

! using equation_from_unknown lookup form transpose
n = 0
do ppu = 1, ptotal
  jacobian_transpose%ja(ppu) = n + 1 ! for transpose jacobian also using compressed sparse row format (3 array variation), with 1 indexing (csr1) (as per mainsolver really)
  do pppe = 1, equation_from_unknown(ppu)%nequation
    n = n + 1
    ppe = equation_from_unknown(ppu)%equation_pp(pppe)
    jacobian_transpose%i(n) = ppe
! search through jacobian matrix for value
    found = .false.
    do ii = jacobian%ja(ppe), jacobian%ja(ppe+1)-1
      i = jacobian%i(ii)
      if (i == ppu) then
        jacobian_transpose%v(n) = jacobian%v(ii)
        found = .true.
        exit
      end if
    end do
    if (.not.found) call error_stop('not found')
  end do
end do
jacobian_transpose%ja(ptotal+1) = nelements + 1

! keep equation_from_unknown allocated

end subroutine setup_jacobians

!-----------------------------------------------------------------

subroutine aa_dot_vector_jacobian(jacobian,vector_to_multiply,vector_product)

! here we multiply a vector (vector_to_multiply) with the jacobian matrix stored as the jacobian entity
!  forming the product vector (vector_product)

!$ use omp_lib
use general_module
double precision, dimension(:), intent(in) :: vector_to_multiply
double precision, dimension(:) :: vector_product
type(jacobian_type), intent(in) :: jacobian
integer :: n, j

!---------------------

vector_product = 0.d0
!$omp parallel do private(j,n) shared(jacobian,vector_to_multiply,vector_product)
do j = 1, ptotal
  do n = jacobian%ja(j), jacobian%ja(j+1)-1
    vector_product(j) = vector_product(j) + jacobian%v(n)*vector_to_multiply(jacobian%i(n))
  end do
end do
!$omp end parallel do

end subroutine aa_dot_vector_jacobian

!-----------------------------------------------------------------

subroutine print_scaled_jacobian_matrix(jacobian)

use general_module
integer :: n, j
character(len=1000) :: formatline
character(len=20) :: ptotalformat
type(jacobian_type) :: jacobian

ptotalformat=dindexformat(ptotal)
formatline = '(g14.6,2(a,'//trim(ptotalformat)//'),a)' 
write(93,'(a/a)') repeat('+',10),"jacobian matrix"
do j = 1, ptotal
  do n = jacobian%ja(j), jacobian%ja(j+1)-1
    write(93,fmt=formatline) jacobian%v(n),' (',j,',',jacobian%i(n),')'
  end do
end do
write(93,'(a)') repeat('-',10)

end subroutine print_scaled_jacobian_matrix

!-----------------------------------------------------------------

function dot_product_with_itself(vector)

use general_module
double precision, dimension(:), intent(in) :: vector
double precision :: dot_product_with_itself
integer :: n

if (.false.) then
  dot_product_with_itself = dot_product(vector,vector)
else if (.false.) then
  dot_product_with_itself = 0.d0
  do n = 1, ptotal
    dot_product_with_itself = dot_product_with_itself + vector(n)**2
  end do
else
  dot_product_with_itself = 0.d0
  !$omp parallel do shared(vector) private(n) reduction(+:dot_product_with_itself)
  do n = 1, ptotal
    dot_product_with_itself = dot_product_with_itself + vector(n)**2
  end do
  !$omp end parallel do
end if
  
end function dot_product_with_itself

!-----------------------------------------------------------------

function dot_product_with_itself_scaled(vector,vector_scale)

use general_module
double precision, dimension(:), intent(in) :: vector, vector_scale
double precision :: dot_product_with_itself_scaled
integer :: n

if (.false.) then
  dot_product_with_itself_scaled = 0.d0
  do n = 1, ptotal
    dot_product_with_itself_scaled = dot_product_with_itself_scaled + (vector(n)*vector_scale(n))**2
  end do
else
  dot_product_with_itself_scaled = 0.d0
  !$omp parallel do shared(vector,vector_scale) private(n) reduction(+:dot_product_with_itself_scaled)
  do n = 1, ptotal
    dot_product_with_itself_scaled = dot_product_with_itself_scaled + (vector(n)*vector_scale(n))**2
  end do
  !$omp end parallel do
end if
  
end function dot_product_with_itself_scaled

!-----------------------------------------------------------------

function dot_product_local(vector1,vector2)

use general_module
double precision, dimension(:), intent(in) :: vector1, vector2
double precision :: dot_product_local
integer :: n

if (.false.) then
  dot_product_local = dot_product(vector1,vector2)
else if (.false.) then
  dot_product_local = 0.d0
  do n = 1, ptotal
    dot_product_local = dot_product_local + vector1(n)*vector2(n)
  end do
else
  dot_product_local = 0.d0
  !$omp parallel do shared(vector1,vector2) private(n) reduction(+:dot_product_local)
  do n = 1, ptotal
    dot_product_local = dot_product_local + vector1(n)*vector2(n)
  end do
  !$omp end parallel do
end if
  
end function dot_product_local

!-----------------------------------------------------------------

subroutine flexible_mainsolver(ierror)

! here we use a flexible algorithm to solve the linear system
! now based on normal and gradients of normal projection vectors
! this algorithm is not meant to be production ready - it is coded with flexibility as a priority over performance

use general_module
use equation_module
use lapack_module
!$ use omp_lib

integer :: ierror, m, iterstep, ppu, iterstepchecknext, itersteproundoffnext, n, nvectors_l, nelements, nvectors_ptotal
character(len=1000) :: formatline
double precision, dimension(:), allocatable, save :: delx, ee, r, ee_scale ! allocate these once as their size doesn't change
type(jacobian_type), save :: jacobian, jacobian_transpose
double precision :: rrr, rrr_o, iterres, ee_scale_max, rrr_newt_tol, rrr_tol, rrr_newt, a_determinant, delp_mag, &
  w_p_mag
double precision, parameter :: d_min = 1.d-60, roundoff_trigger = 1.d+4
integer, parameter :: itersteproundoff = 50 ! this is a second criterion which triggers recalculation of the coefficients
integer, parameter :: nvectors = 6 ! number of vectors to be used in the update
double precision, dimension(nvectors,nvectors) :: a_pp ! dot product of w_p's matrix
double precision, dimension(nvectors,nvectors) :: b_pp ! matrix used to pass to determinant matrix
double precision, dimension(nvectors) :: a_rp ! dot product of latest r_n with w_p's
double precision, dimension(nvectors) :: alpha ! multipliers of delp
double precision, dimension(nvectors,1) :: lapack_alpha ! shape required for lapack solver
double precision, dimension(nvectors,nvectors) :: lapack_a ! a array for lapack_solver
double precision, dimension(nvectors) :: a_pp_matrix_error ! error in a_pp matrix solution, only used for debugging
double precision, dimension(:,:), allocatable, save :: delp, w_p ! increment directions (delp) and 
logical :: lapack_error
logical, parameter :: unwrap_vector_operations = .false.
logical, parameter :: debug = .false.
logical :: debug_sparse = .true.

if (debug) debug_sparse = .true.
if (debug_sparse) write(*,'(80(1h+)/a)') 'subroutine flexible_mainsolver'

ierror = 4 ! this signals an error

! initialise and allocate loop variables
if (.not.allocated(delx)) then
  allocate(delx(ptotal),ee(ptotal),delp(nvectors,ptotal),w_p(nvectors,ptotal),r(ptotal),ee_scale(ptotal))
  delphi = 0.d0 ! zero on the first iteration only
end if

! we need the reverse lookup structure, equation_from_unknown, but only the ppe indicies, for forming the jacobian_transpose
call calc_equation_from_unknown(ppeonly=.true.)

! and calculate the normalisation (iterative preconditioning) vector
call calc_ee_normalise(ee_scale,nelements)

! nondimensionalise jacobian, and setup ee and ee_scale
call setup_jacobians(jacobian,jacobian_transpose,ee_scale,nelements)

! convert ee_scale into a preconditioned -> nondimensionalised scale factor, forming ee and finding ee_scale_max at the same time
call calc_ee_scale(ee_scale,ee,ee_scale_max)

!---------------------
! initial guess for delphi is the zero vector
! delphi = 0.d0 ! now keep previous solution as the first guess
! initialise other variables
delx = 0.d0
r = ee
rrr = dot_product_with_itself(r)
rrr_newt_tol = ptotal*(max(iterrestol,newtres*iterresreltol))**2
rrr_tol = rrr_newt_tol/(ee_scale_max**2)
a_pp = 0.d0
a_rp = 0.d0
alpha = 0.d0
delp = 0.d0
w_p = 0.d0
rrr_o = rrr ! rrr_o is the last time the coefficients were calculated explicitly from r
nvectors_ptotal = min(nvectors,ptotal) ! maximum number of vectors that we can have, recognising that it must be less than ptotal

if (debug) then
  call print_debug_vector(ee,"ee")
  call print_debug_vector(ee_scale,"ee_scale")
  call print_debug_vector(r,"r")
  call print_scaled_jacobian_matrix(jacobian)
  write(93,'(a,g14.6)') 'rrr = ',rrr
  write(93,'(a,g14.6)') 'rrr_newt_tol = ',rrr_newt_tol
  write(93,'(a,g14.6)') 'rrr_tol = ',rrr_tol
  write(93,'(a,g14.6)') 'ee_scale_max = ',ee_scale_max
  write(93,'(a,g14.6)') 'nvectors_ptotal = ',nvectors_ptotal
  write(93,'(a,i8,1(a,g14.7))') "ITERATIONS:        start iterstep = ",0,": rrr = ",rrr
  write(93,'(a)') repeat('-',80)
end if

! start iteration loop
iterstep = 0
iterstepchecknext = 0
itersteproundoffnext = itersteproundoff
iteration_loop: do

  if (debug) then
    write(93,'(a,i10)') 'At start of iteration_loop, iterstep = ',iterstep
    rrr_newt = dot_product_with_itself_scaled(r,ee_scale)
    write(93,'(2(a,g14.7))') "rrr_newt = ",rrr_newt,": rrr = ",rrr
  end if

  if (iterstepchecknext == iterstep) then
    iterstepchecknext = iterstepchecknext + iterstepcheck
    rrr_newt = dot_product_with_itself_scaled(r,ee_scale)

    if (debug_sparse) then
      iterres = sqrt(rrr_newt/dble(ptotal))
      write(*,'(1(a,i8),3(a,g14.7))') "ITERATION: iterstep = ",iterstep, &
        ": iterres = ",iterres,": rrr = ",rrr,": rrr_newt = ",rrr_newt
      if (convergence_details_file) &
        write(fconverge,'(1(a,i8),3(a,g14.7))') "ITERATION: iterstep = ",iterstep, &
          ": iterres = ",iterres,": rrr = ",rrr,": rrr_newt = ",rrr_newt
    end if

    if (rrr_newt < rrr_newt_tol) then ! iterations have converged based on rrr_newt
      ierror = 0
      exit iteration_loop
    end if

    if (check_stopfile("stopback")) then
      write(*,'(a)') 'INFO: user requested simulation stop via "kill" file'
      ierror = -1
      exit iteration_loop
    end if

  end if

  if (rrr < rrr_tol) then ! iterations have converged based on rrr
    rrr_newt = dot_product_with_itself_scaled(r,ee_scale)
    ierror = 0
    exit iteration_loop
  end if

  if (iterstep == iterstepmax) exit iteration_loop ! maximum iterations have been performed without convergence

! now do the updates
  iterstep = iterstep + 1

! calculate new projection vectors
! nvectors_l = min(nvectors_ptotal,iterstep) ! on the first iterations we need to limit the number of vectors
  nvectors_l = nvectors_ptotal
  do m = 1, nvectors_l
    if (m == 1) then
! delp = J^T.r (actually delp^o in notation of notes)
      call aa_dot_vector_jacobian(jacobian_transpose,r,delp(m,:))
    else
      call aa_dot_vector_jacobian(jacobian_transpose,w_p(m-1,:),delp(m,:)) 
      w_p_mag = dot_product(w_p(m-1,:),w_p(m-1,:))
      delp(m,:) = -delp(m,:)+w_p_mag*delp(m-1,:)
    end if
! normalise delp
    delp_mag = sqrt(dot_product(delp(m,:),delp(m,:)))
    delp(m,:) = delp(m,:)/delp_mag
! w_p = J.delp
    call aa_dot_vector_jacobian(jacobian,delp(m,:),w_p(m,:)) 
  end do

  if (debug) then
    formatline = '(i8'//repeat(',1x,g14.6',nvectors_l)//')'
    write(93,'(a)') 'delp vectors:'
    do n = 1, ptotal
      write(93,fmt=formatline) n,(delp(m,n),m=1,nvectors_l)
    end do
    write(93,'(a)') 'w_p vectors:'
    do n = 1, ptotal
      write(93,fmt=formatline) n,(w_p(m,n),m=1,nvectors_l)
    end do
  end if

! calculate new coefficients
  do m = 1, nvectors_l
    a_rp(m) = dot_product(r,w_p(m,:))
    do n = 1, nvectors_l
      a_pp(m,n) = dot_product(w_p(m,:),w_p(n,:))
    end do
  end do

! if (savex) then
!   do m = 1, nvectors
!     a_rp(m) = dot_product(r,w_p(m,:))
!     do n = 1, nvectors
!       a_pp(m,n) = dot_product(w_p(m,:),w_p(n,:))
!     end do
!   end do
! else
! ! update dot products a_pp matrix and a_rp vector
!   do m = 1, nvectors
!     a_pp(activerow,m) = 0.d0
!     a_rp(m) = 0.d0
!   end do
!   !$omp parallel do private(n) reduction(+:a_rp,a_pp,a_xp)
!   do n = 1, ptotal
!     do m = 1, nvectors
!       a_pp(activerow,m) = a_pp(activerow,m) + w_p(activerow,n)*w_p(m,n)
!       a_rp(m) = a_rp(m) + r(n)*w_p(m,n)
!     end do
!   end do
!   !$omp end parallel do
!   do m = 1, nvectors
!     a_pp(m,activerow) = a_pp(activerow,m)
!   end do
! end if

! if (debug) then
!   write(93,'(a,i4)') 'iterstep = ',iterstep
!   call print_debug_vector(delp,"delp")
!   call print_debug_vector(w_p,"w_p")
!   write(93,'(a,g14.6)') 'a_rp = ',a_rp
!   write(93,'(a,g14.6)') 'a_pp = ',a_pp
!   write(93,'(a,g14.6)') 'a_xx = ',a_xx
! end if


! solve for alpha using cramer's rule
  alpha = 0.d0
  do
    a_determinant = determinant(a_pp(1:nvectors_l,1:nvectors_l))
    if (a_determinant < d_min) then
! if the determinant is too small then reduce the number of projection vectors
      if (nvectors_l > 1) then
        if (debug) write(93,'(a,i2)') 'a_determinant too small, so reducing nvectors_l to ',nvectors_l
        nvectors_l = nvectors_l - 1
      else
        if (debug) write(93,'(a)') 'a_determinant too small, so exiting iteration loop, presumably after convergence'
        exit iteration_loop
      end if
    else
      exit
    end if
  end do
  if (.false.) then
    b_pp = a_pp
    do n = 1, nvectors_l
      b_pp(:,n) = -a_rp
      alpha(n) = determinant(b_pp(1:nvectors_l,1:nvectors_l))/a_determinant
      b_pp(:,n) = a_pp(:,n)
    end do
  else
!   call lapack_linear_solver(a=a_pp(1:nvectors_l,1:nvectors_l),b=-a_rp(1:nvectors_l),error=lapack_error)
    lapack_alpha = 0.d0
    lapack_alpha(1:nvectors_l,1) = -a_rp
    lapack_a = 0.d0
    do n = 1, nvectors
      lapack_a(n,n) = 1.d0
    end do
    lapack_a(1:nvectors_l,1:nvectors_l) = a_pp(1:nvectors_l,1:nvectors_l)
    call lapack_linear_solver(a=lapack_a,b=lapack_alpha,error=lapack_error)
    if (lapack_error) call error_stop('error in lapack linear solver in flexible_mainsolver')
    alpha = 0.d0
    alpha(1:nvectors_l) = lapack_alpha(1:nvectors_l,1)
  end if

  if (debug) then
    a_pp_matrix_error = matmul(a_pp(1:nvectors_l,1:nvectors_l),alpha(1:nvectors_l))+a_rp(1:nvectors_l)
    write(93,'(a,i2)') 'nvectors_l = ',nvectors_l
    write(93,'(a,g14.7)') 'a_determinant = ',a_determinant
    write(93,*) 'alpha = ',alpha(1:nvectors_l)
    write(93,*) 'a_pp_matrix_error = ',a_pp_matrix_error(1:nvectors_l)
    write(93,'(a,g14.7)') 'a_pp_matrix_error mag = ',sqrt(dot_product(a_pp_matrix_error(1:nvectors_l),a_pp_matrix_error(1:nvectors_l)))
  end if

  if (unwrap_vector_operations) then
    !$omp parallel do private(n) shared(alpha)
    do n = 1, ptotal
      delx(n) = 0.d0
      do m = 1, nvectors_l
        delx(n) = delx(n) + alpha(m)*delp(m,n)
        r(n) = r(n) + alpha(m)*w_p(m,n)
      end do
      delphi(n) = delphi(n) + delx(n)
    end do
    !$omp end parallel do
  else
    delx = 0.d0
    do m = 1, nvectors_l
      delx = delx + alpha(m)*delp(m,:)
      r = r + alpha(m)*w_p(m,:)
    end do
    delphi = delphi + delx
  end if

  rrr = dot_product_with_itself(r)

! if (savex) then
!   delp(activerow,:) = delx
!   w_x = 0.d0
!   do m = 1, nvectors_l
!     w_x = w_x + alpha(m)*w_p(m,:)
!   end do
!   w_p(activerow,:) = w_x
! end if

! if (debug) then
!   write(93,'(a)') 'after updating w_x/delx and r/delphi'
!   write(93,'(a,g14.6)') 'a_xx = ',a_xx
!   write(93,'(a,g14.6)') 'a_rx = ',a_rx
!   call print_debug_vector(w_x,"w_x")
!   call print_debug_vector(delx,"delx")
!   write(93,'(a)') 'after updating r/delphi'
!   call print_debug_vector(r,"r")
!   call print_debug_vector(delphi,"delphi")
!   write(93,'(a,g14.6)') 'rrr = ',rrr
!   write(93,'(a,g14.6)') 'rrr_o = ',rrr_o
! end if

! if (delrrr >= 0.d0) then
!   write(*,*) 'delrrr > 0.d0'
! end if
  
! if residual has decreased significantly, then recalculate the factors to guard against roundoff errors
! do this before convergence check, incase roundoff error has infected convergence
  if (rrr_o/rrr > roundoff_trigger.or.iterstep == itersteproundoffnext) then
    itersteproundoffnext = itersteproundoffnext + itersteproundoff
    rrr_o = rrr
    call aa_dot_vector_jacobian(jacobian,delphi,r) 
    r = ee + r
    rrr = dot_product_with_itself(r)
!   call aa_dot_vector_jacobian(jacobian,delx,w_x) 
!   a_xx = dot_product_with_itself(w_x)
!   a_rx = dot_product_local(r,w_x)
!   if (debug) then
!     write(93,'(a)') 'in roundoff_trigger'
!     call print_debug_vector(r,"r")
!     write(93,'(a,g14.6)') 'rrr = ',rrr
!     call print_debug_vector(w_x,"w_x")
!     write(93,'(a,g14.6)') 'a_xx = ',a_xx
!     write(93,'(a,g14.6)') 'a_rx = ',a_rx
!   end if
!   if (debug_sparse) write(*,'(a)') 'ITERATIONS: recalculating coefficients to avoid round-off errors'
    if (debug) write(*,'(a)') 'ITERATIONS: recalculating coefficients to avoid round-off errors'
  end if

end do iteration_loop
  
iterres = sqrt(rrr_newt/dble(ptotal))
if (ierror == 0) then
  if (debug_sparse) then
    write(*,'(a,i8,3(a,g14.7))') "ITERATIONS: convered iterstep = ",iterstep,": iterres = ",iterres, &
      ": rrr = ",rrr,": rrr_newt = ",rrr_newt
    if (convergence_details_file) &
      write(fconverge,'(a,i8,3(a,g14.7))') &
      "ITERATIONS: converged iterstep = ",iterstep,": iterres = ",iterres, &
      ": rrr = ",rrr,": rrr_newt = ",rrr_newt
  end if
else
  write(*,'(a,i8,3(a,g14.7))') "ITERATIONS WARNING: failed iterstep = ",iterstep,": iterres = ",iterres, &
    ": rrr = ",rrr,": rrr_newt = ",rrr_newt
  if (convergence_details_file) &
    write(fconverge,'(a,i8,3(a,g14.7))') &
    "ITERATIONS WARNING: failed iterstep = ",iterstep,": iterres = ",iterres, &
    ": rrr = ",rrr,": rrr_newt = ",rrr_newt
end if

!---------------------
! redimensionalise delphi
do ppu = 1, ptotal
  m = unknown_var_from_pp(ppu)
  delphi(ppu) = delphi(ppu)*var(m)%magnitude
end do

!---------------------

if (debug_sparse) write(*,'(a/80(1h-))') 'subroutine flexible_mainsolver'

end subroutine flexible_mainsolver

!-----------------------------------------------------------------

recursive function determinant(matrix) result(determinant_result)

double precision :: determinant_result
double precision, dimension(:,:) :: matrix
double precision, dimension(:,:), allocatable :: reduced_matrix
integer :: nsize, n, multiplier

nsize = ubound(matrix,1)
!if (ubound(matrix,2) /= nsize) stop 'matrix in determinant not square'
if (nsize == 1) then
  determinant_result = matrix(1,1)
else
  allocate(reduced_matrix(nsize-1,nsize-1))
  determinant_result = 0.d0
  multiplier = 1
  reduced_matrix = matrix(2:nsize,2:nsize)
  do n = 1, nsize
    determinant_result = determinant_result + matrix(1,n)*dble(multiplier)*determinant(reduced_matrix)
    if (n == nsize) exit
    reduced_matrix(:,n) = matrix(2:nsize,n)
    multiplier = -multiplier
  end do
  deallocate(reduced_matrix)
end if

end function determinant

!-----------------------------------------------------------------

end module linear_module
!-----------------------------------------------------------------
