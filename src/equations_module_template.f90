! file src_equations/equations_module_template.f90
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
!
! this file has strings replaced by the script setup_equations.pl to form
! the fortran code equations_module.f90
!
! Simulation info generated by setup_equations when equations_module.f90 was created
!<sub_string:simulation_info>

module equations_module

! drag in only these data types from general_module
use general_module, only : vector_list_type, scalar_list_type, integer_list_type

implicit none
private
public allocate_meta_arrays, update_derived_and_equations, update_constants, update_unknowns, update_newtients, &
  update_initial_newtients, update_transients, update_initial_transients, update_outputs, &
  check_variable_constraints, check_condition, var_value, varcdivgrad, varcgrad, varcgrad_nodelimited, &
  setup_external_functions, read_initial_outputs

! include external functions preambles here
!<arb_external_preamble>

contains

!----------------------------------------------------------------------------

subroutine allocate_meta_arrays

! here we allocate array elements of the meta data that corresponds to the input files

use general_module
logical, parameter :: debug = .false.
                  
if (debug) write(*,'(80(1h+)/a)') 'subroutine allocate_meta_arrays'

! allocate meta arrays
!<sub_string:allocate_meta_arrays>

! setup msomeloop which is used to allocate the funks within the someloop (thread) containers
!<sub_string:set_msomeloop>

! setup mseparation_list which is used to allocate the separation_lists within the someloop (thread) containers
!<sub_string:set_mseparation_list>

! set transient_simulation logical
!<sub_string:transient_simulation>

! set newtient_simulation logical
!<sub_string:newtient_simulation>

! set kernel availability
!<sub_string:kernel_availability>

if (debug) write(*,'(a/80(1h-))') 'subroutine allocate_meta_arrays'

end subroutine allocate_meta_arrays

!----------------------------------------------------------------------------

subroutine setup_external_functions

! hook subroutine that will include setup for any external functions

use general_module
logical, parameter :: debug = .false.
integer :: n
                  
if (debug) write(*,'(80(1h+)/a)') 'subroutine setup_external_functions'

! include external functions setup here
!<arb_external_setup>

if (debug) write(*,'(a/80(1h-))') 'subroutine setup_external_functions'

end subroutine setup_external_functions

!-----------------------------------------------------------------

subroutine update_derived_and_equations(setup)

! here we update all the deriveds and equations

use general_module
use gmesh_module
!$ use omp_lib
logical :: setup ! now needs to know whether we are in the setup phase or not - if so, posibly read in data
integer :: nvar, m, ns, i, j, k
integer :: thread = 1
double precision :: derivative_multiplier
character(len=1000) :: formatline
character(len=1000) :: error_string
logical, parameter :: debug = .false.
                  
if (debug) write(*,'(80(1h+)/a)') 'subroutine update_derived_and_equations'

if (debug) write(*,'(40(1h+)/a)') 'derived updates'

do nvar = 1, allocatable_size(var_list(var_list_number(centring="all",type="derived"))%list)
  m = var_list(var_list_number(centring="all",type="derived"))%list(nvar)

  i = 0
  j = 0
  k = 0

  if (debug) write(*,*) 'updating var derived: m = ',m,': name = ',trim(var(m)%name),': centring = ',var(m)%centring
  error_string = "Error occurred while updating derived "//trim(var(m)%name)

  if (output_variable_update_times) call time_variable_update(thread,0,m)

! derived
!  <sub_string:derived>

  if (output_variable_update_times) call time_variable_update(thread,1,m)

! read from data file only (possibly) during variable setup
  if (setup) call read_gmesh(contents='data',var_number=m)

  if (debug) then
    formatline = '(a,'//trim(indexformat)//',a,i3,a,a)'
    do ns = 1, ubound(var(m)%funk,1)
      write(*,fmt=formatline) '(ns,m) = (',ns,',',m,'): var = ',trim(print_funk(var(m)%funk(ns)))
    end do
  end if

end do

if (debug) write(*,'(a/40(1h-))') 'derived updates'

!-----------------------------------------------------

if (debug) write(*,'(40(1h+)/a)') 'equation updates'

do nvar = 1, allocatable_size(var_list(var_list_number(centring="all",type="equation"))%list)
  m = var_list(var_list_number(centring="all",type="equation"))%list(nvar)

  i = 0
  j = 0
  k = 0

  if (debug) write(*,*) 'updating var equation: m = ',m,': name = ',trim(var(m)%name),': centring = ',var(m)%centring
  error_string = "Error occurred while updating equation "//trim(var(m)%name)

  if (output_variable_update_times) call time_variable_update(thread,0,m)

! equations
!  <sub_string:equation>

  if (output_variable_update_times) call time_variable_update(thread,1,m)

! read from data file only (possibly) during variable setup
  if (setup) call read_gmesh(contents='data',var_number=m)

  if (debug) then
    formatline = '(a,'//trim(indexformat)//',a,i3,a,a)'
    do ns = 1, ubound(var(m)%funk,1)
      write(*,fmt=formatline) '(ns,m) = (',ns,',',m,'): var = ',trim(print_funk(var(m)%funk(ns)))
    end do
  end if

end do

if (debug) write(*,'(a/40(1h-))') 'equation updates'

if (debug) write(*,'(a/80(1h-))') 'subroutine update_derived_and_equations'

end subroutine update_derived_and_equations

!-----------------------------------------------------------------

recursive subroutine update_someloop(thread,m,ilast,jlast,klast,error_string)

! here we update each of all the possible someloops
! bug wife again!!!

use general_module
integer :: m, ilast, jlast, klast, i, j, k, ns, region_number, flux_direction, to_ns, from_ns, thread, static_ns, ii, &
  nseparation, i2, ii2, iistart, iiend
double precision :: derivative_multiplier, reflect_multiplier
!character(len=*), optional :: error_string ! no longer optional
character(len=*) :: error_string
logical, parameter :: debug = .false.
                  
if (debug) write(*,'(80(1h+)/a)') 'subroutine update_someloop'
!if (.not.present(error_string)) call error_stop("something is calling update_someloop without an error_string")

! ilast and jlast these hold the indices from the calling routine, and cannot be changed since they are used in the
! loop of the calling routine - so save a copy of these that can be altered within this routine
i = ilast 
j = jlast
k = klast

call reset_funk(someloop(thread)%funk(m))

!<sub_string:someloop>

if (debug) write(*,'(a/80(1h-))') 'subroutine update_someloop'

end subroutine update_someloop

!-----------------------------------------------------------------

subroutine update_constants

! here we update all the constants that are calculated using equations

use general_module
use gmesh_module
!$ use omp_lib
integer :: nvar, m, ns, i, j, k
integer :: thread = 1
double precision :: derivative_multiplier
character(len=1000) :: formatline
character(len=1000) :: error_string
logical, parameter :: debug = .false.
                  
if (debug) write(*,'(80(1h+)/a)') 'subroutine update_constants'

do nvar = 1, allocatable_size(var_list(var_list_number(centring="all",type="constant"))%list)
  m = var_list(var_list_number(centring="all",type="constant"))%list(nvar)

  i = 0
  j = 0
  k = 0

  if (debug) write(*,*) 'updating var constant: m = ',m,': name = ',trim(var(m)%name),': centring = ',var(m)%centring
  error_string = "Error occurred while updating constant "//trim(var(m)%name)

  if (output_variable_update_times) call time_variable_update(thread,0,m)

! constants
!  <sub_string:constant>

  if (output_variable_update_times) call time_variable_update(thread,1,m)

! read from data file
  call read_gmesh(contents='data',var_number=m)

  if (debug) then
    formatline = '(a,'//trim(indexformat)//',a,i3,a,a)'
    do ns = 1, ubound(var(m)%funk,1)
      write(*,fmt=formatline) '(ns,m) = (',ns,',',m,'): var = ',trim(print_funk(var(m)%funk(ns)))
    end do
  end if

end do

if (debug) write(*,'(a/80(1h-))') 'subroutine update_constants'

end subroutine update_constants

!-----------------------------------------------------------------

subroutine update_unknowns

! here we update all the unknown initial values

use general_module
use gmesh_module
!$ use omp_lib
integer :: nvar, m, ns, i, j, k
integer :: thread = 1
double precision :: derivative_multiplier
character(len=1000) :: formatline
character(len=1000) :: error_string
logical, parameter :: debug = .false.
                  
if (debug) write(*,'(80(1h+)/a)') 'subroutine update_unknowns'

do nvar = 1, allocatable_size(var_list(var_list_number(centring="all",type="unknown"))%list)
  m = var_list(var_list_number(centring="all",type="unknown"))%list(nvar)

  i = 0
  j = 0
  k = 0

  if (debug) write(*,*) 'updating var unknown: m = ',m,': name = ',trim(var(m)%name),': centring = ',var(m)%centring
  error_string = "Error occurred while updating unknown "//trim(var(m)%name)

  if (output_variable_update_times) call time_variable_update(thread,0,m)

! unknowns
!  <sub_string:unknown>

  if (output_variable_update_times) call time_variable_update(thread,1,m)

! read from data file
  call read_gmesh(contents='data',var_number=m)

  if (debug) then
    formatline = '(a,'//trim(indexformat)//',a,i3,a,a)'
    do ns = 1, ubound(var(m)%funk,1)
      write(*,fmt=formatline) '(ns,m) = (',ns,',',m,'): var = ',trim(print_funk(var(m)%funk(ns)))
    end do
  end if

end do

if (debug) write(*,'(a/80(1h-))') 'subroutine update_unknowns'

end subroutine update_unknowns

!-----------------------------------------------------------------

subroutine update_newtients

! here we update all the newtients for a new timestep

use general_module
use gmesh_module
!$ use omp_lib
integer :: nvar, m, ns, i, j, k, relstep
integer :: thread = 1
double precision :: derivative_multiplier
character(len=1000) :: formatline
character(len=1000) :: error_string
logical, parameter :: debug = .false.
                  
if (debug) write(*,'(80(1h+)/a)') 'subroutine update_newtients'

do relstep = newtient_relstepmax, 0, -1 ! look through newtient variables in reverse order
  do nvar = 1, allocatable_size(var_list(var_list_number(centring="all",type="newtient"))%list)
    m = var_list(var_list_number(centring="all",type="newtient"))%list(nvar)

    if (var(m)%relstep /= relstep) cycle ! skip this variable if it isn't the correct relstep

    i = 0
    j = 0
    k = 0
    error_string = "Error occurred while updating newtient "//trim(var(m)%name)

    if (debug) write(*,*) 'updating var newtient: m = ',m,': name = ',trim(var(m)%name),': centring = ',var(m)%centring, &
      ": relstep = ",relstep

    if (output_variable_update_times) call time_variable_update(thread,0,m)

! newtients
!    <sub_string:newtient>

    if (output_variable_update_times) call time_variable_update(thread,1,m)

    if (debug) then
      formatline = '(a,'//trim(indexformat)//',a,i3,a,a)'
      do ns = 1, ubound(var(m)%funk,1)
        write(*,fmt=formatline) '(ns,m) = (',ns,',',m,'): var = ',trim(print_funk(var(m)%funk(ns)))
      end do
    end if

  end do
end do

if (debug) write(*,'(a/80(1h-))') 'subroutine update_newtients'

end subroutine update_newtients

!-----------------------------------------------------------------

subroutine update_initial_newtients

! here we initialised all the newtients

use general_module
use gmesh_module
!$ use omp_lib
integer :: nvar, m, ns, i, j, k, relstep
integer :: thread = 1
double precision :: derivative_multiplier
character(len=1000) :: formatline
character(len=1000) :: error_string
logical, parameter :: debug = .false.
                  
if (debug) write(*,'(80(1h+)/a)') 'subroutine update_initial_newtients'

do relstep = 0, newtient_relstepmax ! look through newtient variables in forward order
  do nvar = 1, allocatable_size(var_list(var_list_number(centring="all",type="newtient"))%list)
    m = var_list(var_list_number(centring="all",type="newtient"))%list(nvar)

    if (var(m)%relstep /= relstep) cycle ! skip this variable if it isn't the correct relstep

    i = 0
    j = 0
    k = 0

    if (debug) write(*,*) 'initialising var newtient: m = ',m,': name = ',trim(var(m)%name),': centring = ',var(m)%centring,": relstep = ",relstep
    error_string = "Error occurred while updating initial_newtient "//trim(var(m)%name)

    if (output_variable_update_times.and..not.ignore_initial_update_times) call time_variable_update(thread,0,m)

! initial_newtients
!    <sub_string:initial_newtient>

    if (output_variable_update_times.and..not.ignore_initial_update_times) call time_variable_update(thread,1,m)

! read from data file
    call read_gmesh(contents='data',var_number=m)

    if (debug) then
      formatline = '(a,'//trim(indexformat)//',a,i3,a,a)'
      do ns = 1, ubound(var(m)%funk,1)
        write(*,fmt=formatline) '(ns,m) = (',ns,',',m,'): var = ',trim(print_funk(var(m)%funk(ns)))
      end do
    end if

  end do
end do

if (debug) write(*,'(a/80(1h-))') 'subroutine update_initial_newtients'

end subroutine update_initial_newtients

!-----------------------------------------------------------------

subroutine update_transients

! here we update all the transients for a new timestep

use general_module
use gmesh_module
!$ use omp_lib
integer :: nvar, m, ns, i, j, k, relstep
integer :: thread = 1
double precision :: derivative_multiplier
character(len=1000) :: formatline
character(len=1000) :: error_string
logical, parameter :: debug = .false.
                  
if (debug) write(*,'(80(1h+)/a)') 'subroutine update_transients'

do relstep = transient_relstepmax, 0, -1 ! look through transient variables in reverse order
  do nvar = 1, allocatable_size(var_list(var_list_number(centring="all",type="transient"))%list)
    m = var_list(var_list_number(centring="all",type="transient"))%list(nvar)

    if (var(m)%relstep /= relstep) cycle ! skip this variable if it isn't the correct relstep

    i = 0
    j = 0
    k = 0

    if (debug) write(*,*) 'updating var transient: m = ',m,': name = ',trim(var(m)%name),': centring = ',var(m)%centring, &
      ": relstep = ",relstep
    error_string = "Error occurred while updating transient "//trim(var(m)%name)

    if (output_variable_update_times) call time_variable_update(thread,0,m)

! transients
!    <sub_string:transient>

    if (output_variable_update_times) call time_variable_update(thread,1,m)

    if (debug) then
      formatline = '(a,'//trim(indexformat)//',a,i3,a,a)'
      do ns = 1, ubound(var(m)%funk,1)
        write(*,fmt=formatline) '(ns,m) = (',ns,',',m,'): var = ',trim(print_funk(var(m)%funk(ns)))
      end do
    end if

  end do
end do

if (debug) write(*,'(a/80(1h-))') 'subroutine update_transients'

end subroutine update_transients

!-----------------------------------------------------------------

subroutine update_initial_transients

! here we initialised all the transients

use general_module
use gmesh_module
!$ use omp_lib
integer :: nvar, m, ns, i, j, k, relstep
integer :: thread = 1
double precision :: derivative_multiplier
character(len=1000) :: formatline
character(len=1000) :: error_string
logical, parameter :: debug = .false.
                  
if (debug) write(*,'(80(1h+)/a)') 'subroutine update_initial_transients'

do relstep = 0, transient_relstepmax ! look through transient variables in forward order
  do nvar = 1, allocatable_size(var_list(var_list_number(centring="all",type="transient"))%list)
    m = var_list(var_list_number(centring="all",type="transient"))%list(nvar)

    if (var(m)%relstep /= relstep) cycle ! skip this variable if it isn't the correct relstep

    i = 0
    j = 0
    k = 0

    if (debug) write(*,*) 'initialising var transient: m = ',m,': name = ',trim(var(m)%name),': centring = ',var(m)%centring,": relstep = ",relstep
    error_string = "Error occurred while updating initial_transient "//trim(var(m)%name)

    if (output_variable_update_times.and..not.ignore_initial_update_times) call time_variable_update(thread,0,m)

! initial_transients
!    <sub_string:initial_transient>

    if (output_variable_update_times.and..not.ignore_initial_update_times) call time_variable_update(thread,1,m)

! read from data file
    call read_gmesh(contents='data',var_number=m)

    if (debug) then
      formatline = '(a,'//trim(indexformat)//',a,i3,a,a)'
      do ns = 1, ubound(var(m)%funk,1)
        write(*,fmt=formatline) '(ns,m) = (',ns,',',m,'): var = ',trim(print_funk(var(m)%funk(ns)))
      end do
    end if

  end do
end do

if (debug) write(*,'(a/80(1h-))') 'subroutine update_initial_transients'

end subroutine update_initial_transients

!-----------------------------------------------------------------

subroutine update_outputs(stepoutput)

! here we update all the outputs that are calculated using equations

use general_module
!$ use omp_lib
logical, optional, intent(in) :: stepoutput
logical :: stepoutput_local
integer :: nvar, m, ns, i, j, k
integer :: thread = 1
double precision :: derivative_multiplier
character(len=1000) :: formatline
character(len=1000) :: error_string
logical, parameter :: debug = .false.
                  
if (debug) write(*,'(80(1h+)/a)') 'subroutine update_outputs'

stepoutput_local = .false.
if (present(stepoutput)) then
  stepoutput_local = stepoutput
end if

do nvar = 1, allocatable_size(var_list(var_list_number(centring="all",type="output"))%list)
  m = var_list(var_list_number(centring="all",type="output"))%list(nvar)

  i = 0
  j = 0
  k = 0

  if (debug) write(*,*) 'updating var output: m = ',m,': name = ',trim(var(m)%name),': centring = ',var(m)%centring
  error_string = "Error occurred while updating output "//trim(var(m)%name)

! if stepoutput is not on for either the component or compound and this is being called from output_step, then skip
!  updating this variable
  if (stepoutput_local) then
    if (.not.( trim(check_option(compound(var(m)%compound_number)%options,stepoutput_options)) == "stepoutput" .or. &
               trim(check_option(var(m)%options,stepoutput_options)) == "stepoutput" )) cycle
  end if

  if (output_variable_update_times) call time_variable_update(thread,0,m)

! equations
!  <sub_string:output>

  if (output_variable_update_times) call time_variable_update(thread,1,m)

  if (debug) then
    formatline = '(a,'//trim(indexformat)//',a,i3,a,a)'
    do ns = 1, ubound(var(m)%funk,1)
      write(*,fmt=formatline) '(ns,m) = (',ns,',',m,'): var = ',trim(print_funk(var(m)%funk(ns)))
    end do
  end if

end do

if (debug) write(*,'(a/80(1h-))') 'subroutine update_outputs'

end subroutine update_outputs

!-----------------------------------------------------------------

subroutine read_initial_outputs

! here we read any output data from previous files

use general_module
use gmesh_module
integer :: nvar, m, ns
character(len=1000) :: formatline
character(len=1000) :: error_string
logical, parameter :: debug = .false.
                  
if (debug) write(*,'(80(1h+)/a)') 'subroutine read_initial_outputs'

do nvar = 1, allocatable_size(var_list(var_list_number(centring="all",type="output"))%list)
  m = var_list(var_list_number(centring="all",type="output"))%list(nvar)

  if (debug) write(*,*) 'updating var output: m = ',m,': name = ',trim(var(m)%name),': centring = ',var(m)%centring
  error_string = "Error occurred while updating output "//trim(var(m)%name)

! read from data file
  call read_gmesh(contents='data',var_number=m)

  if (debug) then
    formatline = '(a,'//trim(indexformat)//',a,i3,a,a)'
    do ns = 1, ubound(var(m)%funk,1)
      write(*,fmt=formatline) '(ns,m) = (',ns,',',m,'): var = ',trim(print_funk(var(m)%funk(ns)))
    end do
  end if

end do

if (debug) write(*,'(a/80(1h-))') 'subroutine read_initial_outputs'

end subroutine read_initial_outputs

!-----------------------------------------------------------------

function check_condition(condition_type)

! here we check whether a condition is satisfied for action = output|stop|convergence|bell
! do this in series as if in parallel all threads would be attempting to alter the one logical
! could be done but not worth it (conditions are usually none centred anyway)

use general_module
character(len=*) :: condition_type
logical :: check_condition
integer :: nvar, m, ns, i, j, k
integer :: thread = 1
double precision :: derivative_multiplier
character(len=1000) :: formatline
character(len=1000) :: error_string
logical, parameter :: debug = .false.
                  
if (debug) write(*,'(80(1h+)/a)') 'function check_condition'

check_condition = .false.

do nvar = 1, allocatable_size(var_list(var_list_number(centring="all",type="condition"))%list)
  m = var_list(var_list_number(centring="all",type="condition"))%list(nvar)

  i = 0
  j = 0
  k = 0

  if (debug) write(*,*) 'checking condition var: m = ',m,': name = ',trim(var(m)%name),': centring = ',var(m)%centring
  error_string = "Error occurred while updating condition "//trim(var(m)%name)

  if (output_variable_update_times) call time_variable_update(thread,0,m)

! conditions
!  <sub_string:condition>

  if (output_variable_update_times) call time_variable_update(thread,1,m)

  if (debug) then
    formatline = '(a,'//trim(indexformat)//',a,i3,a,a)'
    do ns = 1, ubound(var(m)%funk,1)
      write(*,fmt=formatline) '(ns,m) = (',ns,',',m,'): var = ',trim(print_funk(var(m)%funk(ns)))
    end do
  end if

end do

if (debug) write(*,'(a/80(1h-))') 'function check_condition'

end function check_condition

!-----------------------------------------------------------------

subroutine check_variable_constraints(type,ierror)

! here we check on the magnitudes of any variables against specified options

use general_module
character(len=*) :: type
integer :: ns, ierror
character(len=1000) :: formatline
integer :: thread = 1
logical, parameter :: debug = .false.
                  
if (debug) write(*,'(80(1h+)/a)') 'subroutine check_variable_constraints'

ierror = 0

if (trim(type) == 'unknown') then
  if (debug) write(*,*) 'subroutine check_variable_constraints called with type = '//trim(type)
!  <sub_string:unknown_constraints>
else if (trim(type) == 'derived') then
  if (debug) write(*,*) 'subroutine check_variable_constraints called with type = '//trim(type)
!  <sub_string:derived_constraints>
else if (trim(type) == 'equation') then
  if (debug) write(*,*) 'subroutine check_variable_constraints called with type = '//trim(type)
!  <sub_string:equation_constraints>
else if (trim(type) == 'local') then
  if (debug) write(*,*) 'subroutine check_variable_constraints called with type = '//trim(type)
!  <sub_string:local_constraints>
else
  call error_stop("subroutine check_variable_constraints called with variable type = "//trim(type))
end if

if (debug) write(*,'(a/80(1h-))') 'subroutine check_variable_constraints'

end subroutine check_variable_constraints

!-----------------------------------------------------------------

function var_value(m,ns,noerror)

! little function to find value of var, irrespective of whether it is local (ie not stored) or not

use general_module
integer, intent(in) :: m ! var number
integer, intent(in) :: ns ! ns reference for region of var
logical, optional, intent(in) :: noerror ! if ns is passed as zero just output result as zero rather than flagging an error
logical :: noerror_l
integer :: i, j, k
integer :: thread = 1
double precision :: var_value
character(len=1000) :: error_string

if (ns == 0) then

  noerror_l = .true.
  if (present(noerror)) noerror_l = noerror
  if (noerror_l) then
    var_value = 0.d0
  else
    call error_stop('ns is equal to 0 in var_value:  Generally this means that an attempt is being made '// &
      'to reference '//trim(var(m)%centring)//' centred variable '//trim(var(m)%name)//' outside of the region '// &
      trim(var(m)%region)//' in which it is defined. '// &
      'Look at the use of this variable in the equations and check that its region context is correct.')
  end if
    
else if (var(m)%type == 'local') then

  i = 0
  j = 0
  k = 0
  if (var(m)%centring == 'cell') then
    i = region(var(m)%region_number)%ijk(ns)
    if (i == 0) call error_stop('i is equal to 0 in var_value: problem with use of '//trim(var(m)%name))
  else if (var(m)%centring == 'face') then
    j = region(var(m)%region_number)%ijk(ns)
    if (j == 0) call error_stop('j is equal to 0 in var_value: problem with use of '//trim(var(m)%name))
  else if (var(m)%centring == 'node') then
    k = region(var(m)%region_number)%ijk(ns)
    if (k == 0) call error_stop('k is equal to 0 in var_value: problem with use of '//trim(var(m)%name))
  end if
  error_string = "Error occurred while updating someloop "//trim(var(m)%name)//" from subroutine var_value.  It is likely that "// &
    "this is being called from an output routine while outputting a local variable."

  call update_someloop(thread,var(m)%someloop,i,j,k,error_string)
  var_value = someloop(thread)%funk(var(m)%someloop)%v

else

  var_value = var(m)%funk(ns)%v

end if

end function var_value

!-----------------------------------------------------------------

function varcdivgrad(i,m)

! same as cellgrad but references a var function (which must be cell centred)
! calculate gradient (vector) of cell-centred data var(m) in cell i

use general_module
integer, intent(in) :: i, m
integer :: ii, j, jj, ik, ns
double precision, dimension(totaldimensions) :: varcdivgrad
logical, parameter :: debug = .false.

if (debug) write(*,'(80(1h+)/a)') 'function varcdivgrad'

varcdivgrad = [ 0.d0, 0.d0, 0.d0 ]

do jj = 1, ubound(cell(i)%jface,1)
  j = cell(i)%jface(jj)
  do ii = 1, ubound(face(j)%kernel(0)%ijk,1)
    ik = face(j)%kernel(0)%ijk(ii) 
    if (ik == 0) cycle
    ns = region(var(m)%region_number)%ns(ik)
    if (ns == 0) ns = region(var(m)%region_number)%ns(i) ! if point doesn't lie in region then use centre
    varcdivgrad = varcdivgrad + face(j)%norm(:,1)*divop(i,j)*face(j)%kernel(0)%v(ii)*var_value(m,ns)
  end do
end do

if (debug) write(*,'(a/80(1h-))') 'function varcdivgrad'

end function varcdivgrad

!-----------------------------------------------------------------

function varcgrad(i,m)

! same as cellgrad but references a var function (which must be cell centred)
! calculate gradient (vector) of cell-centred data var(m) in cell i

use general_module
integer, intent(in) :: i, m
integer :: l, ii, ik, ns, nsc
double precision, dimension(totaldimensions) :: varcgrad
logical, parameter :: debug = .false.

if (debug) write(*,'(80(1h+)/a)') 'function varcgrad'

varcgrad = [ 0.d0, 0.d0, 0.d0 ]
nsc = region(var(m)%region_number)%ns(i)
if (nsc == 0) call error_stop("trying to evaluate a cell centred variable in varcgrad in a cell in which it is not defined")

do l = 1, totaldimensions
  do ii = 1, allocatable_integer_size(cell(i)%kernel(l)%ijk)
    if (debug) then
      write(90,*) 'i = ',i,': l = ',l,': ii = ',ii,': m = ',m,': centring = ',trim(var(m)%centring)
      flush(90)
    end if
    ik = cell(i)%kernel(l)%ijk(ii) 
    if (ik == 0) cycle
    ns = region(var(m)%region_number)%ns(ik)
    if (ns == 0) ns = nsc ! if point doesn't lie in region then use centre
    varcgrad(l) = varcgrad(l) + cell(i)%kernel(l)%v(ii)*var_value(m,ns)
  end do
end do

if (debug) write(*,'(a/80(1h-))') 'function varcgrad'

end function varcgrad

!-----------------------------------------------------------------

function varcgrad_nodelimited(i,m)

! same as varcgrad but limits the gradient so that all node values lie within the range of 
!  node-neighbouring surrounding cell values
! calculate limited gradient (vector) of cell-centred data var(m) in cell i

use general_module
integer, intent(in) :: i, m
integer :: kk, k, ii2, i2, ns
double precision, dimension(totaldimensions) :: varcgrad_nodelimited
double precision :: varmax, varmin, varc, varl, alpha
logical, parameter :: debug = .false.

if (debug) write(*,'(80(1h+)/a)') 'function varcgrad_nodelimited'

varcgrad_nodelimited = varcgrad(i,m)

! find var in the central cell and set default limits
ns = region(var(m)%region_number)%ns(i)
if (ns == 0) call error_stop("trying to evaluate a cell centred variable in varcgrad in a cell in which it is not defined")
varc = var_value(m,ns)
varmax = 0.d0
varmin = 0.d0

! cycle through each node of the cell, calculating the (relative) maximum and minimum node values
do kk = 1, allocatable_integer_size(cell(i)%knode)
  k = cell(i)%knode(kk)
  do ii2 = 1, allocatable_integer_size(node(k)%icell)
    i2 = node(k)%icell(ii2)
    if (i2 == i) cycle ! central cell already included
    ns = region(var(m)%region_number)%ns(i2)
    if (ns == 0) cycle
    varl = var_value(m,ns) - varc
    varmax = max(varmax,varl)
    varmin = min(varmin,varl)
  end do
end do
    
! cycle through the nodes again, calculating the non-limited var values at each
alpha = 1.d0 ! this is the multiplier that will eventually limit the gradient
do kk = 1, allocatable_integer_size(cell(i)%knode)
  k = cell(i)%knode(kk)
  varl = dot_product(node(k)%x-cell(i)%x,varcgrad_nodelimited)
  if (varl > varmax) then
    alpha = min(alpha,varmax/max(varl,tinyish))
  else if (varl < varmin) then
    alpha = min(alpha,-varmin/max(-varl,tinyish))
  end if
end do

!alpha = max(alpha,0.d0) 
if (alpha < 1.d0) varcgrad_nodelimited = alpha*varcgrad_nodelimited 

if (debug) write(*,'(a/80(1h-))') 'function varcgrad_nodelimited'

end function varcgrad_nodelimited

!-----------------------------------------------------------------

! include external functions contents here
!<arb_external_contents>

!-----------------------------------------------------------------

end module equations_module

!----------------------------------------------------------------------------
