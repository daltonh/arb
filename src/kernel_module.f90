! file src/kernel_module.f90
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
module kernel_module

implicit none

! only the setup_kernels subroutine is accessible from outside this module
private
public setup_kernels

! no need to save this stuff as it is only called once

! type for weight fluxing
type weight_flux_type
  integer :: ndonating ! the number of faces that are donating from this cell
  integer, dimension(:), allocatable :: donating ! the ii index of the cell that this face donates to
  double precision, dimension(:), allocatable :: proportion ! proportion of flux from cell ii in kernel that will be fluxed through this (cell(i)%face) face
end type weight_flux_type

! some general diagnostic variables used in all setup subroutines
integer :: total_masks, check_minw_increase, check_minw_limited, total_mask_separations 

! specifications for the kernels follow:
! recommended defaults for each parameter are in braces
! 'userable variables' can be set from input file using the KERNEL_OPTIONS keyword: names to use in input file are (for the main) the same as the fortran names, but with the underscores removed.  The subroutine process_kernel_options has more info

! ref: kernel options
! both mls and optimisation method options:
character(len=100) :: kernel_method = 'optimisation' ! (optimisation, userable) polynomial basis kernels calculated via nonlinear optimisation
!character(len=100) :: kernel_method = 'mls' ! (optimisation) polynomial basis kernels calculated via moving least squares technique
!character(len=100) :: kernel_method = 'simple' ! (optimisation) simple over face differencing, only suitable for 1D problems.
!character(len=100) :: kernel_method = 'none' ! don't calculate kernels
logical, parameter :: domain_cell_from_face_kernels = .false. ! (.false.) calculate the domain cell centred derivatives by averaging the face centred derivatives to the cell centre.  This seems to work well, both from a computational efficiency and stability perspective.
logical, parameter :: boundary_cell_from_face_kernels = .false. ! (.false.) copy the boundary cell centred derivatives from the face ones.  The slight difference between the two is in the weighting of the surrounding cells.
logical, parameter :: domain_node_from_face_kernels = .false. ! (.true.) for 1D domains only, use the face kernels (which are coincident with the nodes) for the node kernels
logical, parameter :: boundary_node_from_face_kernels = .false. ! (.true.) for 1D domains only, and only for the derivative kernels (as the averaging kernels will be the same anyway)
integer :: minimum_domain_separation = 1, minimum_boundary_separation = 1 ! (1,1, userable) these are the default separations for domain/boundary faces that specify the size of the kernels.  Large numbers produce large kernels.
integer :: maximum_domain_separation = 3, maximum_boundary_separation = 3 ! (3,3, userable) these are the maximum separations for domain/boundary faces that specify the size of the kernels.  These are the maximum sizes that can be used if adaptive_mask_size is on.
integer :: maximum_cell_domain_separation = 3, maximum_cell_boundary_separation = 3 ! (3,3, userable) these are the maximum separations for domain/boundary faces that specify the size of the kernels.  These are for the cell derivative kernels.
logical :: auto_maximum_separation = .false. ! (.false., userable, changed default to false for v0.50) set the maximum_separations automatically based on polynomial_order.  Note, overwrites any previous maximum_separation specifications, but now (v0.50) does not alter minw
logical :: limit_kernel_mask_to_shared_nodes = .false. ! (.false., userable) cells within kernel mask must share a node with the originating face or cell - this is a locality constraint that all cells in mask must share at least one node with the relevant face
logical :: boundary_node_separations = .true. ! (.true., userable) boundary cells separated by nodes are considered to be adjacent to each other when calculating cell separations
! v0.50 trial increase in the default kernel order
integer :: polynomial_order = 2 ! (2, userable) maximum order (power index) within polynomial basis functions
integer :: polynomial_average_order = 2 ! (2, userable) maximum order (power index) within polynomial basis functions, used just for the face averaging kernels
integer :: polynomial_cell_order = 1 ! (1, userable) maximum order (power index) within polynomial basis functions, used just for the cell based gradient kernels
integer :: polynomial_node_order = 1 ! (1, userable) maximum order (power index) within polynomial basis functions, used just for the node based gradient kernels
double precision, parameter :: kernel_dx_multiplier = 1.d0 ! (1.d0) multiply dx_kernel by this
logical, parameter :: remove_small_elements = .true. ! (.true.) kernel values below small_element_minimum will be removed
double precision, parameter :: small_element_minimum = 1.d-10 ! (1.d-10) minimum kernel size allowed (otherwise set kernel element to zero to save memory) - also used throughout at a nonzero kernel test
logical, parameter :: conservative_weighting = .false. ! (.false.) use a conservation principle to calculate weights that are connected, otherwise use formula based on absolute separation
logical, parameter :: orientation_dependent_weights = .true. ! (.true.) the kernel weights are different depending on the direction of the kernel
logical :: zero_nonoriented_weights = .false. ! (.false., userable) for the orientation_dependent_weights and face kernels, zero cell weights that do not have the highest weighting importance.  In effect this will compact face kernels, but may increase the number of negative elements.  Probably a good idea on structured meshes which generally have a high degree of symmetry.
logical :: average_stability_corrections = .false. ! (.false., userable) whether to zero negative averaging kernel elements, right now only applied to face kernels
logical :: gradient_stability_corrections = .false. ! (.false., userable) whether to zero negative direction-equivalent gradient kernel elements, right now only applied to face kernels

! partial_hyperbolic_kernel for v0.42 (040614)
!logical :: partial_hyperbolic_kernel = .true. ! (.true., userable) use hyperbolic kernel for boundary elements, or averaging domain elements, but not for domain derivative kernels
!double precision :: weight_separation_multiplier = 0.25d0 ! (0.3 for nonhyperbolic, 0.5d0 for hyperbolic, userable) for non-centred kernels this specifies the how the weights of each separation are related - the smaller the number, the tighter the kernel - use is slightly different for conservative and nonconservative weighting - for conservative_weighting this is the proportion of each cell weight that is fluxed to further away neighbours - now not used if radial_kernel_weighting on
!logical :: shift_boundary_weight_centre = .true. ! (.false., userable) for the purposes of calculating the face kernel weights (only), shift the centre of the kernel to the mid point between the two adjacent cells (only affects nonhyperbolic kernels)
!double precision :: shift_hyperbolic_distance = -0.5d0 ! (0.5d0, userable) proportion of difference in distance between both sides of kernel to shift the derivative weighting function (set to <= to remove this feature)
!logical :: hyperbolic_kernel = .false. ! (.true., userable) use a hyperbolic radial weighting function, with the optimisation process changed to suit - only for optimisation kernels (not mls)
!double precision :: hyperbolic_b = 0.5d0 ! (0.5d0, userable) for hyperbolic kernel, specify how compact the kernel is (the smaller b is, the more compact the kernel)
!logical :: separation_multiplied_trial_kernels = .true. ! (.true., userable) for non-hyperbolic kernel, decide whether trial kernels are separation multiplied by weight_separation_multiplier (inverse of)

! these options were channged/added for v0.42 (171213)
! problems with these on cells near boundaries in high aspect ratio structured meshes, so reverting back to v0.41 nonhyperbolic kernels as the default for the time being (300514)
!logical :: partial_hyperbolic_kernel = .false. ! (.true., userable) use hyperbolic kernel for boundary elements, or averaging domain elements, but not for domain derivative kernels
!double precision :: weight_separation_multiplier = 0.5d0 ! (0.5d0, userable) for non-centred kernels this specifies the how the weights of each separation are related - the smaller the number, the tighter the kernel - use is slightly different for conservative and nonconservative weighting - for conservative_weighting this is the proportion of each cell weight that is fluxed to further away neighbours - now not used if radial_kernel_weighting on
!logical :: shift_boundary_weight_centre = .false. ! (.false., userable) for the purposes of calculating the face kernel weights (only), shift the centre of the kernel to the mid point between the two adjacent cells (only affects nonhyperbolic kernels)
!double precision :: shift_hyperbolic_distance = -0.5d0 ! (0.5d0, userable) proportion of difference in distance between both sides of kernel to shift the derivative weighting function (set to <= to remove this feature)
!logical :: hyperbolic_kernel = .true. ! (.true., userable) use a hyperbolic radial weighting function, with the optimisation process changed to suit - only for optimisation kernels (not mls)
!double precision :: hyperbolic_b = 0.5d0 ! (0.5d0, userable) for hyperbolic kernel, specify how compact the kernel is (the smaller b is, the more compact the kernel)
!logical :: separation_multiplied_trial_kernels = .true. ! (.true., userable) for non-hyperbolic kernel, decide whether trial kernels are separation multiplied by weight_separation_multiplier (inverse of)

! the following recovers v0.41 kernels (note, all userable, so can be specified from input file)
! logical :: partial_hyperbolic_kernel = .false. ! (.true., userable) use hyperbolic kernel for boundary elements, or averaging domain elements, but not for domain derivative kernels
! ! working
! !double precision :: weight_separation_multiplier = 0.3d0 ! (0.25d0, userable) for non-centred kernels this specifies the how the weights of each separation are related - the smaller the number, the tighter the kernel - use is slightly different for conservative and nonconservative weighting - for conservative_weighting this is the proportion of each cell weight that is fluxed to further away neighbours - now not used if radial_kernel_weighting on
! double precision :: weight_separation_multiplier = 0.25d0 ! (0.25d0, userable) for non-centred kernels this specifies the how the weights of each separation are related - the smaller the number, the tighter the kernel - use is slightly different for conservative and nonconservative weighting - for conservative_weighting this is the proportion of each cell weight that is fluxed to further away neighbours - now not used if radial_kernel_weighting on
! ! not working - ouch - nonhyperbolic kernels very sensitive to this parameter
! !double precision :: weight_separation_multiplier = 0.5d0 ! (0.25d0, userable) for non-centred kernels this specifies the how the weights of each separation are related - the smaller the number, the tighter the kernel - use is slightly different for conservative and nonconservative weighting - for conservative_weighting this is the proportion of each cell weight that is fluxed to further away neighbours - now not used if radial_kernel_weighting on
! logical :: shift_boundary_weight_centre = .true. ! (.false., userable) for the purposes of calculating the face kernel weights (only), shift the centre of the kernel to the mid point between the two adjacent cells (only affects nonhyperbolic kernels)
! double precision :: shift_hyperbolic_distance = 0.5d0 ! (0.5d0, userable) proportion of difference in distance between both sides of kernel to shift the derivative weighting function (set to <= to remove this feature)
! logical :: hyperbolic_kernel = .false. ! (.true., userable) use a hyperbolic radial weighting function, with the optimisation process changed to suit - only for optimisation kernels (not mls)
! double precision :: hyperbolic_b = 0.5d0 ! (0.5d0, userable) for hyperbolic kernel, specify how compact the kernel is (the smaller b is, the more compact the kernel)
! logical :: separation_multiplied_trial_kernels = .true. ! (.true., userable) for non-hyperbolic kernel, decide whether trial kernels are separation multiplied by weight_separation_multiplier (inverse of)

! v0.50 defaults:
! the default is now the v0.41 nonhyperbolic kernels
! sometimes better alternative to the nonhyperbolic kernels, producing stronger gradient functions at boundaries
logical :: hyperbolic_kernel = .false. ! (.false., userable) use a hyperbolic radial weighting function, with the optimisation process changed to suit - only for optimisation kernels (not mls)
! rarely successful combination of the hyperbolic kernels for averaging everywhere and all kernels on the boundary elements, nonhyperbolic everywhere else
logical :: partial_hyperbolic_kernel = .false. ! (.false., userable) use hyperbolic kernel for boundary elements, or averaging domain elements, but not for domain derivative kernels
! hyperbolic and nonhyperbolic parameters:
! this was increased for v0.42 to 0.5d0 from 0.3d0 (accidentally, which was a disaster). Now decreased to 0.2 which seems to be OK for both hyperbolic and nonhyperbolic kernels
double precision :: weight_separation_multiplier = 0.2d0 ! (0.2d0, userable) for non-centred kernels this specifies the how the weights of each separation are related - the smaller the number, the tighter the kernel - use is slightly different for conservative and nonconservative weighting - for conservative_weighting this is the proportion of each cell weight that is fluxed to further away neighbours - now not used if radial_kernel_weighting on
! hyperbolic specific parameters: 
! introduced in late v0.42 to improve hyperbolic kernel performance.  Value decreased to 0.3d0 from 0.5d0 for v0.50
double precision :: shift_hyperbolic_distance = 0.3d0 ! (0.3d0, userable) proportion of difference in distance between both sides of kernel to shift the derivative weighting function (set to <= to remove this feature)
! changed from 0.5d0 in v0.42 to 0.3d0 in v0.50
double precision :: hyperbolic_b = 0.3d0 ! (0.3d0, userable) for hyperbolic kernel, specify how compact the kernel is (the smaller b is, the more compact the kernel)
! nonhyperbolic specific parameters: 
logical :: separation_multiplied_trial_kernels = .true. ! (.true., userable) for non-hyperbolic kernel, decide whether trial kernels are separation multiplied by weight_separation_multiplier (inverse of)
logical :: shift_boundary_weight_centre = .false. ! (.false., userable) for the purposes of calculating the face kernel weights (only), shift the centre of the kernel to the mid point between the two adjacent cells (only affects nonhyperbolic kernels)


logical, parameter :: radial_kernel_weighting = .true. ! (.true.) the kernel weighting is also influenced by the radial distance
logical, parameter :: stretched_radial_kernel = .true. ! (.true.) the radial kernel is locally stretched to reflect average distances in each kernel direction
logical, parameter :: scaled_radial_kernel = .true. ! (.true.) the radial kernel is locally scaled to reflect average distances in each kernel direction
double precision, parameter :: stretched_uniformity = 0.1d0 ! (0.1d0) stretched_uniformity controls the amount of kernel uniformity across the dimensions (for stretched_radial_kernel), stretched_uniformity = 0 gives no uniformity, stretched_uniformity = 1 makes the effective lengthscales approximately the same and equal to one in all dimensions
logical, parameter :: uniform_cell_averaging_kernels = .false. ! (.false.) the face->cell and node->cell kernels are uniform - seems to happen anyway using a linear fit, but have not proven this generally
logical :: check_minw = .true. ! (.true., userable) check that the minw value is large enough
double precision :: minimum_minw = 1.0d0 ! (1.0d0, userable, changed default from 1.d0 to 0.5d0, and then back to 1.d0 for v0.50) minimum value of SVD minw allowed for mask to be acceptable when using adaptive_mask_size

! optimisation options:
logical, parameter :: optimise_positise = .false. ! (.false.) constrain kernel values in an attempt at getting positive kernels
logical, parameter :: optimise_positise_cautiously = .false. ! (.false.) only allow change to kernel if it directly reduces negative index of kernel
double precision, parameter :: optimise_positise_cautiously_multiplier = 1.d0 ! (1.d0) when optimising cautiously, allow changes if negative index is less than this multiple of last negative index - a large number reproduces a non-cautious approach - 1.d0 is a cautious approach - less than one will be super cautious (ie, the kernel will not be changed much)
integer, parameter :: optimise_additional_elements = 0 ! (0) setting this to zero will mean that each kernel has at least as many elements present as in the minimum separation level - a large negative number will give the smallest kernel, a positive number will increase the kernel size (really needs positise on for /= 0 on this variable - setting to a negative number is not advisable as kernel may not be structurally symmetric)
logical, parameter :: optimise_positise_sum_index = .true. ! (.true.) base negative index (which measures kernel positivity) on sum of negative elements, rather than maximum negative element
double precision, parameter :: small_pp = 1.d-8 ! (1.d-8) used as a cut-off for elements of the polynomial basis

! kernel usage notes:
! 1) zero_wayward_boundary_weights only needs to be set for mls, or optimisation with optimise_positise off
!-----------------------------------------------------------------
contains

!-----------------------------------------------------------------

subroutine setup_kernels

! here we calculate the cell, face and node centred kernels
! face kernels:
! kernel(0) is the average based on surrounding cell values (faceave)
! kernel(1-3) is the derivative in the 1-3rd direction based on surrounding cell values (facegrad)
! kernel(4-6) is the derivative in the face normal directions based on surrounding cell values (facegrad)
! cell kernels:
! kernel(0) is the average based on surrounding face values (cellave)
! kernel(1-3) is the derivative in the 1-3rd direction based on surrounding cell values (cellgrad)
! kernel(4) is the average based on surrounding node values (cellfromnodeave)
! kernel(5-7) is the derivative in the 1-3rd direction based on surrounding node values (cellfromnodegrad) - NB, not implemented yet!
! node kernels:
! kernel(0) is the average based on surrounding cell values (nodeave)
! kernel(1-3) is the derivative in the 1-3rd direction based on surrounding cell values (nodegrad)

use general_module
integer :: i, j, k, l, jj, ijk, ii2, i2, ierror, n
integer :: n_domain_kernels, n_domain_elements, n_boundary_kernels, n_boundary_elements, n_elements, &
  min_location, max_location, nmax
double precision :: xtmp, cross_kernel, overall_cross_kernel, central_kernel, overall_central_kernel, kernel_sum, &
  overall_kernel_sum, min_value, max_value, value, dx_kernel
double precision, dimension(:), allocatable :: kernel_error
double precision, dimension(totaldimensions) :: norm
logical :: any_error
character(len=10000) :: filename, formatline
type(kernel_type), dimension(10) :: temporary_kernel ! for debugging the advection routine
integer, dimension(2) :: new_size_2d ! 2d array for passing to 2d array routines
logical, parameter :: debug = .false.
logical :: debug_sparse = .false.

!------------------------------------------
if (debug) debug_sparse = .true.

if (debug_sparse) write(*,'(80(1h+)/a)') 'subroutine setup_kernels'

filename = trim(output_dir)//"kernel_warnings.txt"
open(fwarn,file=trim(filename),status='replace',iostat=ierror)
if (ierror /= 0) call error_stop('problem opening file '//trim(filename))

call process_kernel_options ! to interpret options passed from user input file

! zero separation and check_minw statistics that are common to all kernels
total_masks = 0
total_mask_separations = 0
check_minw_increase = 0
check_minw_limited = 0

!------------------------------------------
! run through cells, faces and nodes setting kernels, each in a separate subroutine

call setup_face_kernels
call setup_cell_kernels ! has to go after setup_face_kernels as may use face kernels to construct cell kernels
call setup_node_kernels ! ditto to this one, that may also use face kernels

!------------------------------------------
! start kernel health checks

if (debug_sparse.or..true.) write(*,'(a)') 'INFO: checking kernel consistencies and printing statistics: kernel advection parameters'

! checking on kernel health for advection bits
! this check has to be performed before elements are removed (done next) as these checks assume that the kernel support region is the same for all cell kernels

overall_cross_kernel = 0.d0
overall_central_kernel = 0.d0
overall_kernel_sum = 0.d0
do i = 1, itotal
  cell(i)%cross_kernel = 0.d0
  cell(i)%central_kernel = 0.d0
  cell(i)%kernel_sum = 0.d0
  if (.true.) then
    if (cell(i)%type == 2) cycle
    if (debug) write(85,'(a)') &
      'i,j,i2,kernel_sum,cross_kernel,central_kernel,(temporary_kernel(jj)%v(ii2),ii2=1,ubound(temporary_kernel(jj)%ijk,1))'
    do jj = 1, ubound(cell(i)%jface,1)
      j = cell(i)%jface(jj)
      call copy_kernel(original=cell(i)%kernel(1),copy=temporary_kernel(jj))
      temporary_kernel(jj)%v = 0.d0
! temporary kernel is now only the coefficients to evaluate the change in phi from the centre to the face (ie, the high order bits)
!     temporary_kernel(jj)%v(1) = 1.d0 
      do l = 1, 3
        if (allocatable_integer_size(cell(i)%kernel(l)%ijk) == 0) cycle
        xtmp = face(j)%x(l) - cell(i)%x(l)
        do ii2 = 1, ubound(cell(i)%kernel(l)%ijk,1)
          if (temporary_kernel(jj)%ijk(ii2) /= cell(i)%kernel(l)%ijk(ii2)) call error_stop('problem')
          temporary_kernel(jj)%v(ii2) = temporary_kernel(jj)%v(ii2) + cell(i)%kernel(l)%v(ii2)*xtmp
        end do
      end do
!     cross_kernel = max(-minval(temporary_kernel(jj)%v(2:ubound(temporary_kernel(jj)%v,1))),0.d0)
!     cross_kernel = maxval(abs(temporary_kernel(jj)%v)) ! now cross_kernel is the maximum kernel element that goes to make up delphi
      cross_kernel = maxval(abs(temporary_kernel(jj)%v(2:ubound(temporary_kernel(jj)%v,1)))) ! now cross_kernel is the maximum kernel element that goes to make up delphi, except for the central element
      central_kernel = abs(temporary_kernel(jj)%v(1)) ! central_kernel is the contribution from the central cell
      kernel_sum = sum(abs(temporary_kernel(jj)%v))
      if (debug) then
        i2 = cell(i)%icell(jj+1)
        formatline = '(2('//trim(indexformat)//',1x),'//trim(indexformat)// &
          repeat(',1x,f7.3',ubound(temporary_kernel(jj)%ijk,1)+3)//')'
        write(85,fmt=formatline) i,j,i2,kernel_sum,cross_kernel,central_kernel, &
          (temporary_kernel(jj)%v(ii2),ii2=1,ubound(cell(i)%kernel(1)%ijk,1))
      end if
      cell(i)%cross_kernel = max(cross_kernel,cell(i)%cross_kernel)
      cell(i)%central_kernel = max(central_kernel,cell(i)%central_kernel)
      cell(i)%kernel_sum = max(kernel_sum,cell(i)%kernel_sum)
    end do
    if (debug) write(85,*) 'cross_kernel = ',cell(i)%cross_kernel
    if (debug) write(85,*) 'central_kernel = ',cell(i)%central_kernel
    if (debug) write(85,*) 'kernel_sum = ',cell(i)%kernel_sum
    overall_cross_kernel = max(cell(i)%cross_kernel,overall_cross_kernel)
    overall_central_kernel = max(cell(i)%central_kernel,overall_central_kernel)
    overall_kernel_sum = max(cell(i)%kernel_sum,overall_kernel_sum)
  end if
end do
formatline = '(a,'//trim(compactformat)//')'
if (debug) write(85,fmt=formatline) 'overall_cross_kernel = ',overall_cross_kernel
write(*,fmt=formatline) 'INFO: overall_cross_kernel = ',overall_cross_kernel
write(fwarn,fmt=formatline) 'INFO: overall_cross_kernel = ',overall_cross_kernel
if (debug) write(85,fmt=formatline) 'overall_central_kernel = ',overall_central_kernel
write(*,fmt=formatline) 'INFO: overall_central_kernel = ',overall_central_kernel
write(fwarn,fmt=formatline) 'INFO: overall_central_kernel = ',overall_central_kernel
if (debug) write(85,fmt=formatline) 'overall_kernel_sum = ',overall_kernel_sum
write(*,fmt=formatline) 'INFO: overall_kernel_sum = ',overall_kernel_sum
write(fwarn,fmt=formatline) 'INFO: overall_kernel_sum = ',overall_kernel_sum

!------------------------------------------
! perform efficiency measures

if (remove_small_elements) then

  if (debug_sparse.or..true.) write(*,'(a)') 'INFO: removing small kernel elements'

! count and remove zeroish kernel functions
! removal of elements is based on the kernel value scaled by dx_kernel, which is a maximum length scale for
!  the kernel surrounds

!------------
! face

  do j = 1, jtotal
    do l = 0, 6
      dx_kernel = 1.d0
! derivative kernels should have a size of order 1/dx_kernel
      if (l >= 1) dx_kernel = face(j)%dx_kernel
      n_elements = 0
      do n = 1, allocatable_integer_size(face(j)%kernel(l)%ijk)
        if (abs(face(j)%kernel(l)%v(n)*dx_kernel) >= small_element_minimum) then
          n_elements = n_elements + 1
          face(j)%kernel(l)%v(n_elements) = face(j)%kernel(l)%v(n)
          face(j)%kernel(l)%ijk(n_elements) = face(j)%kernel(l)%ijk(n)
          if (allocated(face(j)%kernel(l)%reflect_multiplier)) &
            face(j)%kernel(l)%reflect_multiplier(:,n_elements) = face(j)%kernel(l)%reflect_multiplier(:,n)
        end if
      end do
! resize kernel arrays (keeping data), unless the kernel is empty
! don't have to reallocate reflect_multiplier
! if empty now deallocate ijk and v arrays
      if (n_elements == 0) then
! this will actually deallocate these arrays
        call resize_integer_array(keep_data=.false.,array=face(j)%kernel(l)%ijk,new_size=n_elements)
        call resize_double_precision_array(keep_data=.false.,array=face(j)%kernel(l)%v,new_size=n_elements)
        if (allocated(face(j)%kernel(l)%reflect_multiplier)) deallocate(face(j)%kernel(l)%reflect_multiplier)
      else
        call resize_integer_array(keep_data=.true.,array=face(j)%kernel(l)%ijk,new_size=n_elements)
        call resize_double_precision_array(keep_data=.true.,array=face(j)%kernel(l)%v,new_size=n_elements)
        if (allocated(face(j)%kernel(l)%reflect_multiplier)) then
          new_size_2d = [totaldimensions,n_elements]
          call resize_integer_2d_array &
            (keep_data=.true.,array=face(j)%kernel(l)%reflect_multiplier,new_size=new_size_2d)
        end if
      end if
    end do
  end do
      
!------------
! cell

  do i = 1, itotal
    do l = 0, 4
      n_elements = 0
      dx_kernel = 1.d0
! derivative kernels should have a size of order 1/dx_kernel
      if (l >= 1.and.l <= 3) dx_kernel = cell(i)%dx_kernel
      do n = 1, allocatable_integer_size(cell(i)%kernel(l)%ijk)
        if (abs(cell(i)%kernel(l)%v(n)*dx_kernel) >= small_element_minimum) then
          n_elements = n_elements + 1
          cell(i)%kernel(l)%v(n_elements) = cell(i)%kernel(l)%v(n)
          cell(i)%kernel(l)%ijk(n_elements) = cell(i)%kernel(l)%ijk(n)
          if (allocated(cell(i)%kernel(l)%reflect_multiplier)) &
            cell(i)%kernel(l)%reflect_multiplier(:,n_elements) = cell(i)%kernel(l)%reflect_multiplier(:,n)
        end if
      end do
! resize kernel arrays (keeping data), unless the kernel is empty
! if empty now deallocate ijk and v arrays
      if (n_elements == 0) then
        call resize_integer_array(keep_data=.false.,array=cell(i)%kernel(l)%ijk,new_size=n_elements)
        call resize_double_precision_array(keep_data=.false.,array=cell(i)%kernel(l)%v,new_size=n_elements)
        if (allocated(cell(i)%kernel(l)%reflect_multiplier)) deallocate(cell(i)%kernel(l)%reflect_multiplier)
      else
        call resize_integer_array(keep_data=.true.,array=cell(i)%kernel(l)%ijk,new_size=n_elements)
        call resize_double_precision_array(keep_data=.true.,array=cell(i)%kernel(l)%v,new_size=n_elements)
        if (allocated(cell(i)%kernel(l)%reflect_multiplier)) then
          new_size_2d = [totaldimensions,n_elements]
          call resize_integer_2d_array &
            (keep_data=.true.,array=cell(i)%kernel(l)%reflect_multiplier,new_size=new_size_2d)
        end if
      end if
    end do
  end do

!------------
! node

  do k = 1, ktotal
    do l = 0, 3
      n_elements = 0
      dx_kernel = 1.d0
! derivative kernels should have a size of order 1/dx_kernel
      if (l >= 1.and.l <= 3) dx_kernel = node(k)%dx_kernel
      do n = 1, allocatable_integer_size(node(k)%kernel(l)%ijk)
        if (abs(node(k)%kernel(l)%v(n)*dx_kernel) >= small_element_minimum) then
          n_elements = n_elements + 1
          node(k)%kernel(l)%v(n_elements) = node(k)%kernel(l)%v(n)
          node(k)%kernel(l)%ijk(n_elements) = node(k)%kernel(l)%ijk(n)
          if (allocated(node(k)%kernel(l)%reflect_multiplier)) &
            node(k)%kernel(l)%reflect_multiplier(:,n_elements) = node(k)%kernel(l)%reflect_multiplier(:,n)
        end if
      end do
! resize kernel arrays (keeping data), unless the kernel is empty
! if empty now deallocate ijk and v arrays
      if (n_elements == 0) then
        call resize_integer_array(keep_data=.false.,array=node(k)%kernel(l)%ijk,new_size=n_elements)
        call resize_double_precision_array(keep_data=.false.,array=node(k)%kernel(l)%v,new_size=n_elements)
        if (allocated(node(k)%kernel(l)%reflect_multiplier)) deallocate(node(k)%kernel(l)%reflect_multiplier)
      else
        call resize_integer_array(keep_data=.true.,array=node(k)%kernel(l)%ijk,new_size=n_elements)
        call resize_double_precision_array(keep_data=.true.,array=node(k)%kernel(l)%v,new_size=n_elements)
        if (allocated(node(k)%kernel(l)%reflect_multiplier)) then
          new_size_2d = [totaldimensions,n_elements]
          call resize_integer_2d_array &
            (keep_data=.true.,array=node(k)%kernel(l)%reflect_multiplier,new_size=new_size_2d)
        end if
      end if
    end do
  end do

end if

!---------------------------
! continue with kernel health checks
! print warnings of any negative zeroth order kernels or boundary gradient kernel elements with the wrong sign

if (debug_sparse.or..true.) write(*,'(a)') 'INFO: checking kernel consistencies and printing statistics: kernel element signs'

any_error = .false.

formatline = '(a,'//trim(indexformat)//',a,g13.6,a,'//trim(indexformat)//')'
n = 0
min_value = 0.d0
min_location = 0
do i = 1, itotal
  if (allocatable_integer_size(cell(i)%kernel(0)%ijk) == 0) cycle
  value = minval(cell(i)%kernel(0)%v)
  if (value < -small_element_minimum) then
    n = n + 1
    if (value < min_value) then
      min_value = value
      min_location = i
    end if
  end if
end do
if (n > 0) write(fwarn,fmt=formatline) &
  'WARNING: ',n,' negative cell averaging kernel elements detected: minimum kernel element = ',min_value,': at i = ',min_location

n = 0
min_value = 0.d0
min_location = 0
do j = 1, jtotal
  if (allocatable_integer_size(face(j)%kernel(0)%ijk) == 0) cycle
  value = minval(face(j)%kernel(0)%v)
  if (value < -small_element_minimum) then
    n = n + 1
    if (value < min_value) then
      min_value = value
      min_location = j
    end if
  end if
end do
if (n > 0) write(fwarn,fmt=formatline) &
  'WARNING: ',n,' negative face averaging kernel elements detected: minimum normalised kernel element = ',min_value,': at j = ', &
  min_location

n = 0
min_value = 0.d0
min_location = 0
do k = 1, ktotal
  if (allocatable_integer_size(node(k)%kernel(0)%ijk) == 0) cycle
  value = minval(node(k)%kernel(0)%v)
  if (value < -small_element_minimum) then
    n = n + 1
    if (value < min_value) then
      min_value = value
      min_location = k
    end if
  end if
end do
if (n > 0) write(fwarn,fmt=formatline) &
  'WARNING: ',n,' negative node averaging kernel elements detected: minimum normalised kernel element = ',min_value,': at k = ', &
  min_location

! also run a check on the facenorm derivatives on the boundaries, looking for positive elements in all of them except for the 1st
if (.true.) then
  n = 0
  max_value = 0.d0
  max_location = 0
  do j = 1, jtotal
    if (face(j)%type /= 2) cycle
    if (allocatable_integer_size(face(j)%kernel(4)%ijk) == 0) cycle
    if (minval(face(j)%kernel(4)%reflect_multiplier) /= 1) cycle ! skip reflect boundaries as they're too hard to deal with
! look for maximum cell value that is not on a boundary
    value = 0.d0
    do ii2 = 1, allocatable_integer_size(face(j)%kernel(4)%ijk)
      i2 = face(j)%kernel(4)%ijk(ii2)
      if (cell(i2)%type == 1) value = max(value,face(j)%kernel(4)%v(ii2)*face(j)%dx_kernel)
    end do
!   value = max(maxval(face(j)%kernel(4)%v(3:ubound(face(j)%kernel(4)%v,1))),face(j)%kernel(4)%v(1))*face(j)%dx_kernel
    if (value > small_element_minimum) then
      n = n + 1
      if (value > max_value) then
        max_value = value
        max_location = j
      end if
    end if
  end do
  if (n > 0) write(fwarn,fmt=formatline) &
    'INFO: ',n,' positive boundary face normal derivative kernel elements detected: maximum normalised kernel element = ', &
    max_value,': at j = ',max_location,': this may not indicate a problem for curved boundaries'
end if

!---------------------------
! print warnings if not enough kernels have been constructed for averaging and in each direction
! kernel values should be >= small_element_minimum to be considered nonzero

if (debug_sparse.or..true.) write(*,'(a)') 'INFO: checking kernel consistencies and printing statistics: completeness'

do i = 1, itotal
  formatline = '(a,'//trim(indexformat)//')'
  if (kernel_availability_cellave) then
    if (allocatable_integer_size(cell(i)%kernel(0)%ijk) == 0) then
      write(fwarn,fmt=formatline) 'ERROR: missing cell averaging kernel for cell: i = ',i
      any_error = .true.
    else if (maxval(abs(cell(i)%kernel(0)%v)) < small_element_minimum) then ! 
      write(fwarn,fmt=formatline) 'ERROR: missing cell averaging kernel for cell: i = ',i
      any_error = .true.
    end if
  end if
  if (kernel_availability_cellfromnodeave) then
    if (allocatable_integer_size(cell(i)%kernel(4)%ijk) == 0) then
      write(fwarn,fmt=formatline) 'ERROR: missing node averaging kernel for cell: i = ',i
      any_error = .true.
    else if (maxval(abs(cell(i)%kernel(4)%v)) < small_element_minimum) then
      write(fwarn,fmt=formatline) 'ERROR: missing node averaging kernel for cell: i = ',i
      any_error = .true.
    end if
  end if
  if (kernel_availability_cellgrad) then
    n = 0
    do l = 1, totaldimensions
      if (allocatable_integer_size(cell(i)%kernel(l)%ijk) > 0) then
        if (maxval(abs(cell(i)%kernel(l)%v))*cell(i)%dx_kernel >= small_element_minimum) n = n + 1
      end if
    end do
    if (n < cell(i)%dimensions) then
      formatline = '(a,'//trim(indexformat)//',a,i1,a,i1)'
      write(fwarn,fmt=formatline) 'ERROR: not enough derivative kernels have been constructed for cell: i = ',i, &
        ': number constructed = ',n,': cell dimensions = ',cell(i)%dimensions
      any_error = .true.
    end if
  end if
end do
  
do j = 1, jtotal
  formatline = '(a,'//trim(indexformat)//')'
  if (kernel_availability_faceave) then
    if (allocatable_integer_size(face(j)%kernel(0)%ijk) == 0) then
      write(fwarn,fmt=formatline) 'ERROR: missing averaging kernel for face: j = ',j
      any_error = .true.
    else if (maxval(abs(face(j)%kernel(0)%v)) < small_element_minimum) then
      write(fwarn,fmt=formatline) 'ERROR: missing averaging kernel for face: j = ',j
      any_error = .true.
    end if
  end if
  if (kernel_availability_facegrad) then
    n = 0
    do l = 1, 2*totaldimensions
      if (allocatable_integer_size(face(j)%kernel(l)%ijk) > 0) then
        if (maxval(abs(face(j)%kernel(l)%v))*face(j)%dx_kernel >= small_element_minimum) n = n + 1
      end if
    end do
  ! TODO
    if (n < 2*(face(j)%dimensions+1)) then
      formatline = '(a,'//trim(indexformat)//',a,i1,a,i1)'
      write(fwarn,fmt=formatline) 'ERROR: not enough derivative kernels have been constructed for face: j = ',j, &
        ': number constructed = ',n,': face dimensions = ',face(j)%dimensions
      any_error = .true.
    end if
  end if
end do
  
do k = 1, ktotal
  formatline = '(a,'//trim(indexformat)//')'
  if (kernel_availability_nodeave) then
    if (allocatable_integer_size(node(k)%kernel(0)%ijk) == 0) then
      write(fwarn,fmt=formatline) 'ERROR: missing node averaging kernel for node: k = ',k
      any_error = .true.
    else if (maxval(abs(node(k)%kernel(0)%v)) < small_element_minimum) then ! 
      write(fwarn,fmt=formatline) 'ERROR: missing node averaging kernel for node: k = ',k
      any_error = .true.
    end if
  end if
  if (kernel_availability_nodegrad) then
    n = 0
    do l = 1, totaldimensions
      if (allocatable_integer_size(node(k)%kernel(l)%ijk) > 0) then
        if (maxval(abs(node(k)%kernel(l)%v))*node(k)%dx_kernel >= small_element_minimum) n = n + 1
      end if
    end do
    if (n < node(k)%domain_dimensions) then
      formatline = '(a,'//trim(indexformat)//',a,i1,a,i1)'
      write(fwarn,fmt=formatline) 'ERROR: not enough derivative kernels have been constructed for node: k = ',k, &
        ': number constructed = ',n,': domain dimensions = ',node(k)%domain_dimensions
      any_error = .true.
    end if
  end if
end do
  
!---------------------------
! check and print warnings if kernels are inconsistent

if (debug_sparse.or..true.) write(*,'(a)') 'INFO: checking kernel consistencies and printing statistics: consistency'

! cell kernels
formatline = '(a,'//trim(indexformat)//',a,i1,a,i1,2(a,'//trim(floatformat)//'),a,i1)'
allocate(kernel_error(1:4))
CELL_LOOP: do i = 1, itotal
  do l = 0, 4
    kernel_error = 0.d0
    dx_kernel = 1.d0
    nmax = 4
! derivative kernels should have a size of order 1/dx_kernel
    if (l >= 1.and. l <= 3) then
      dx_kernel = cell(i)%dx_kernel
      if (allocated(glue_face)) nmax = 1 ! if there are glued faces present then we don't report errors for any cell centred kernel first order components
    end if
!   if (allocatable_integer_size(cell(i)%kernel(l)%ijk) > 0.and. &
!     maxval(abs(cell(i)%kernel(l)%v))*dx_kernel > small_element_minimum) then ! this identifies non-zero kernels
! now zero kernels are not allocated, so all allocated kernels should be valid
    if (allocatable_integer_size(cell(i)%kernel(l)%ijk) > 0) then
      if (l == 0) then ! averaging kernel l = 0, face centred
        kernel_error(1) = sum(cell(i)%kernel(l)%v) - 1.d0
        do n = 1, 3
          do ijk = 1, ubound(cell(i)%kernel(l)%ijk,1)
            kernel_error(n+1) = kernel_error(n+1) + cell(i)%kernel(l)%v(ijk)*face(cell(i)%kernel(l)%ijk(ijk))%x(n)
          end do
          kernel_error(n+1) = kernel_error(n+1) - cell(i)%x(n)
        end do
      else if (l == 4) then ! averaging kernel l = 4, node centred
        kernel_error(1) = sum(cell(i)%kernel(l)%v) - 1.d0
        do n = 1, 3
          do ijk = 1, ubound(cell(i)%kernel(l)%ijk,1)
            kernel_error(n+1) = kernel_error(n+1) + cell(i)%kernel(l)%v(ijk)*node(cell(i)%kernel(l)%ijk(ijk))%x(n)
          end do
          kernel_error(n+1) = kernel_error(n+1) - cell(i)%x(n)
        end do
      else ! derivative kernels l = 1,2,3, cell centred
        kernel_error(1) = sum(cell(i)%kernel(l)%v)
        do n = 1, 3
          do ijk = 1, ubound(cell(i)%kernel(l)%ijk,1)
            kernel_error(n+1) = kernel_error(n+1) + cell(i)%kernel(l)%v(ijk)*cell(cell(i)%kernel(l)%ijk(ijk))%x(n)
          end do
          if (n == l) kernel_error(n+1) = kernel_error(n+1) - 1.d0
        end do
      end if
!     do n = 1, 4
      do n = 1, nmax
        if (abs(kernel_error(n))*dx_kernel > 1.d-6) then ! errors are absolute values
          write(fwarn,fmt=formatline) 'ERROR: for cell(',i,')%kernel(',l,') equation ', &
            n,' is inconsistent: kernel_error(n) = ',kernel_error(n),': dx_kernel = ',dx_kernel,': cell%type = ',cell(i)%type
          any_error = .true.
          if (average_stability_corrections.or.gradient_stability_corrections) then
            write(fwarn,'(a)') 'WARNING: as one of averagestabilitycorrections or gradientstabilitycorrections are on (.true.) '// &
              'error messages similar to the above will not be repeated for other cell kernels'
            exit CELL_LOOP
          end if
        end if
      end do
    end if
  end do
end do CELL_LOOP
deallocate(kernel_error)
  
! face kernels
allocate(kernel_error(1:4))
nmax = 4
if (allocated(glue_face)) nmax = 1 ! if there are glued faces present then we don't report errors for any first order components
FACE_LOOP: do j = 1, jtotal
  do l = 0, 6
    dx_kernel = 1.d0
! derivative kernels should have a size of order 1/dx_kernel
    if (l >= 1) dx_kernel = face(j)%dx_kernel
    kernel_error = 0.d0
    if (allocatable_integer_size(face(j)%kernel(l)%ijk) > 0) then
!   if (allocatable_integer_size(face(j)%kernel(l)%ijk) > 0.and. &
!     maxval(abs(face(j)%kernel(l)%v))*dx_kernel > small_element_minimum) then ! this identifies non-zero kernels
      if (l == 0) then ! averaging kernel l = 0, cell centred
        kernel_error(1) = sum(face(j)%kernel(l)%v) - 1.d0
        do n = 1, 3
          do ijk = 1, ubound(face(j)%kernel(l)%ijk,1)
            kernel_error(n+1) = kernel_error(n+1) + face(j)%kernel(l)%v(ijk)*cell(face(j)%kernel(l)%ijk(ijk))%x(n)
          end do
          kernel_error(n+1) = kernel_error(n+1) - face(j)%x(n)
        end do
      else ! derivative kernels l = 1,2,3,4,5,6, cell centred
        kernel_error(1) = sum(face(j)%kernel(l)%v)
        do n = 1, 3
          do ijk = 1, ubound(face(j)%kernel(l)%ijk,1)
            kernel_error(n+1) = kernel_error(n+1) + face(j)%kernel(l)%v(ijk)*cell(face(j)%kernel(l)%ijk(ijk))%x(n)
          end do
          if (l < 4) then
            if (n == l) kernel_error(n+1) = kernel_error(n+1) - 1.d0
          else
            kernel_error(n+1) = kernel_error(n+1) - face(j)%norm(n,l-3)
          end if
        end do
      end if
      do n = 1, nmax
        if (abs(kernel_error(n))*dx_kernel > 1.d-6) then
          write(fwarn,fmt=formatline) 'ERROR: for face(',j,')%kernel(',l,') equation ', &
            n,' is inconsistent: kernel_error(n) = ',kernel_error(n),': dx_kernel = ',dx_kernel,': face%type = ',face(j)%type
          any_error = .true.
          if (average_stability_corrections.or.gradient_stability_corrections) then
            write(fwarn,'(a)') 'WARNING: as one of averagestabilitycorrections or gradientstabilitycorrections are on (.true.) '// &
              'error messages similar to the above will not be repeated for other face kernels'
            exit FACE_LOOP
          end if
        end if
      end do
    end if
  end do
end do FACE_LOOP
deallocate(kernel_error)
  
! node kernels
allocate(kernel_error(1:4))
nmax = 4
if (allocated(glue_face)) nmax = 1 ! if there are glued faces present then we don't report errors for any first order components
NODE_LOOP: do k = 1, ktotal
  do l = 0, 3
    dx_kernel = 1.d0
! derivative kernels should have a size of order 1/dx_kernel
    if (l >= 1) dx_kernel = node(k)%dx_kernel
    kernel_error = 0.d0
!   if (allocatable_integer_size(node(k)%kernel(l)%ijk) > 0.and. &
!     maxval(abs(node(k)%kernel(l)%v))*dx_kernel > small_element_minimum) then ! this identifies non-zero kernels
    if (allocatable_integer_size(node(k)%kernel(l)%ijk) > 0) then ! this identifies non-zero kernels
      if (l == 0) then ! averaging kernel l = 0, cell centred
        kernel_error(1) = sum(node(k)%kernel(l)%v) - 1.d0
        do n = 1, 3
          do ijk = 1, ubound(node(k)%kernel(l)%ijk,1)
            kernel_error(n+1) = kernel_error(n+1) + node(k)%kernel(l)%v(ijk)*cell(node(k)%kernel(l)%ijk(ijk))%x(n)
          end do
          kernel_error(n+1) = kernel_error(n+1) - node(k)%x(n)
        end do
      else ! derivative kernels l = 1,2,3 cell centred
        kernel_error(1) = sum(node(k)%kernel(l)%v)
        do n = 1, 3
          do ijk = 1, ubound(node(k)%kernel(l)%ijk,1)
            kernel_error(n+1) = kernel_error(n+1) + node(k)%kernel(l)%v(ijk)*cell(node(k)%kernel(l)%ijk(ijk))%x(n)
          end do
          if (n == l) kernel_error(n+1) = kernel_error(n+1) - 1.d0
        end do
      end if
      do n = 1, nmax
        if (abs(kernel_error(n))*dx_kernel > 1.d-6) then
          write(fwarn,fmt=formatline) 'ERROR: for node(',k,')%kernel(',l,') equation ', &
            n,' is inconsistent: kernel_error(n) = ',kernel_error(n),': dx_kernel = ',dx_kernel,': node%type = ',node(k)%type
          any_error = .true.
          if (average_stability_corrections.or.gradient_stability_corrections) then
            write(fwarn,'(a)') 'WARNING: as one of averagestabilitycorrections or gradientstabilitycorrections are on (.true.) '// &
              'error messages similar to the above will not be repeated for other node kernels'
            exit NODE_LOOP
          end if
        end if
      end do
    end if
  end do
end do NODE_LOOP
deallocate(kernel_error)
  
!---------------------------
! run through all kernels setting the logical reflect and possibly deallocating any reflect_multiplier strings
! doing this after kernel checks, but before kernel counting

if (debug_sparse.or..true.) write(*,'(a)') 'INFO: checking kernel consistencies and printing statistics: reflection bookkeeping'

! faces
do j = 1, jtotal
  do l = 0, 6
    if (allocated(face(j)%kernel(l)%reflect_multiplier)) then
      if (minval(face(j)%kernel(l)%reflect_multiplier) /= 1) then  ! the array only needs to be kept if a reflect_multiplier value is not 1 (ie, -1)
        face(j)%kernel(l)%reflect_present = .true.
      else
        deallocate(face(j)%kernel(l)%reflect_multiplier)
        face(j)%kernel(l)%reflect_present = .false.
      end if
    else
      face(j)%kernel(l)%reflect_present = .false.
    end if
  end do
end do
        
! cells
do i = 1, itotal
  do l = 0, 4
    if (allocated(cell(i)%kernel(l)%reflect_multiplier)) then
      if (minval(cell(i)%kernel(l)%reflect_multiplier) /= 1.and.l >= 1.and.l <= 3) then  ! the array only needs to be kept if a reflect_multiplier value is not 1 (ie, -1)
        cell(i)%kernel(l)%reflect_present = .true.
      else
        deallocate(cell(i)%kernel(l)%reflect_multiplier)
        cell(i)%kernel(l)%reflect_present = .false.
      end if
    else
      cell(i)%kernel(l)%reflect_present = .false.
    end if
  end do
end do

! nodes
do k = 1, ktotal
  do l = 0, 3
    if (allocated(node(k)%kernel(l)%reflect_multiplier)) then
      if (minval(node(k)%kernel(l)%reflect_multiplier) /= 1.and.l >= 1.and.l <= 3) then  ! the array only needs to be kept if a reflect_multiplier value is not 1 (ie, -1)
        node(k)%kernel(l)%reflect_present = .true.
      else
        deallocate(node(k)%kernel(l)%reflect_multiplier)
        node(k)%kernel(l)%reflect_present = .false.
      end if
    else
      node(k)%kernel(l)%reflect_present = .false.
    end if
  end do
end do

! also deallocate face%reflect_multiplier, cell%reflect_multiplier, face%r and cell%r whenever there isn't any glue operations happening to save memory
! same with nodes
new_size_2d = [totaldimensions,2]
if (.true.) then
  do i = 1, itotal
    if (.not.cell(i)%reflect_present) deallocate(cell(i)%reflect_multiplier)
    if (.not.cell(i)%glue_present) deallocate(cell(i)%r)
  end do
  do j = 1, jtotal
    if (.not.face(j)%reflect_present) deallocate(face(j)%reflect_multiplier)
    if (.not.face(j)%glue_present) call resize_double_precision_2d_array(array=face(j)%r,new_size=new_size_2d,keep_data=.true.) ! keep the first two elements for use as facedxup etc
  end do
  do k = 1, ktotal
    if (.not.node(k)%reflect_present) deallocate(node(k)%reflect_multiplier)
    if (.not.node(k)%glue_present) deallocate(node(k)%r)
  end do
end if

!------------------------------------------
! calculate some kernel statistics

if (debug_sparse.or..true.) write(*,'(a)') 'INFO: checking kernel consistencies and printing statistics: calculating statistics'

formatline = '(a,i1,a,'//trim(indexformat)//',a,f5.1)'

n_elements = 0
do l = 0, 6
  n_boundary_kernels = 0
  n_boundary_elements = 0
  n_domain_kernels = 0
  n_domain_elements = 0
  do j = 1, jtotal
    if (face(j)%type == 2) then
      n_boundary_kernels = n_boundary_kernels + 1
      n_boundary_elements = n_boundary_elements + allocatable_integer_size(face(j)%kernel(l)%ijk)
    else
      n_domain_kernels = n_domain_kernels + 1
      n_domain_elements = n_domain_elements + allocatable_integer_size(face(j)%kernel(l)%ijk)
    end if
    n_elements = n_elements + allocatable_integer_size(face(j)%kernel(l)%ijk)
  end do
  write(fwarn,fmt=formatline) 'INFO: for face kernel ',l, &
    ': number of face domain element kernels = ',n_domain_kernels, &
    ': average number of elements = ',dble(n_domain_elements)/dble(max(n_domain_kernels,1))
  write(fwarn,fmt=formatline) 'INFO: for face kernel ',l, &
    ': number of face boundary element kernels = ',n_boundary_kernels, &
    ': average number of elements = ',dble(n_boundary_elements)/dble(max(n_boundary_kernels,1))
end do

do l = 0, 4
  n_boundary_kernels = 0
  n_boundary_elements = 0
  n_domain_kernels = 0
  n_domain_elements = 0
  do i = 1, itotal
    if (cell(i)%type == 2) then
      n_boundary_kernels = n_boundary_kernels + 1
      n_boundary_elements = n_boundary_elements + allocatable_integer_size(cell(i)%kernel(l)%ijk)
    else
      n_domain_kernels = n_domain_kernels + 1
      n_domain_elements = n_domain_elements + allocatable_integer_size(cell(i)%kernel(l)%ijk)
    end if
    n_elements = n_elements + allocatable_integer_size(cell(i)%kernel(l)%ijk)
  end do
  write(fwarn,fmt=formatline) 'INFO: for cell kernel ',l, &
    ': number of cell domain element kernels = ',n_domain_kernels, &
    ': average number of elements = ',dble(n_domain_elements)/dble(max(n_domain_kernels,1))
  write(fwarn,fmt=formatline) 'INFO: for cell kernel ',l, &
    ': number of cell boundary element kernels = ',n_boundary_kernels, &
    ': average number of elements = ',dble(n_boundary_elements)/dble(max(n_boundary_kernels,1))
end do

do l = 0, 3
  n_boundary_kernels = 0
  n_boundary_elements = 0
  n_domain_kernels = 0
  n_domain_elements = 0
  do k = 1, ktotal
    if (node(k)%type == 2) then
      n_boundary_kernels = n_boundary_kernels + 1
      n_boundary_elements = n_boundary_elements + allocatable_integer_size(node(k)%kernel(l)%ijk)
    else
      n_domain_kernels = n_domain_kernels + 1
      n_domain_elements = n_domain_elements + allocatable_integer_size(node(k)%kernel(l)%ijk)
    end if
    n_elements = n_elements + allocatable_integer_size(node(k)%kernel(l)%ijk)
  end do
  write(fwarn,fmt=formatline) 'INFO: for node kernel ',l, &
    ': number of node domain element kernels = ',n_domain_kernels, &
    ': average number of elements = ',dble(n_domain_elements)/dble(max(n_domain_kernels,1))
  write(fwarn,fmt=formatline) 'INFO: for node kernel ',l, &
    ': number of node boundary element kernels = ',n_boundary_kernels, &
    ': average number of elements = ',dble(n_boundary_elements)/dble(max(n_boundary_kernels,1))
end do

!if (debug) write(*,'(a,i10)') 'INFO: total number of kernel elements = ',n_kernels
write(*,'(a,i10)') 'INFO: total number of kernel elements = ',n_elements
write(fwarn,'(a,i10)') 'INFO: total number of kernel elements = ',n_elements
if (any_error) write(*,'(a)') 'WARNING: Errors were detected in the calculated kernels.  Check the file kernel_warnings.txt '// &
  'within the output directory for details.  Common causes for these types of errors: 1) an error in the mesh somewhere with '// &
  'disconnected or malformed cells; 2) kernel masks which are too small or cells which have very poor aspect ratios; or '// &
  '3) stability corrections are turned on (averagestabilitycorrections or gradientstabilitycorrections). '// &
  'To cure 2) you could try increasing the size of the kernel masks by increasing the maximum_separation and by using '// &
  'the check_minw with an increased minimum_minw.'
if (trim(kernel_method) == 'mls' .or. trim(kernel_method) == 'optimisation') then
  if (check_minw) then
    write(*,'(a,g12.5)') 'INFO: check minw resulted in an average separation increase across all kernel masks of ', &
      dble(check_minw_increase)/dble(max(total_masks,1))
    write(*,'(a,g12.5)') 'INFO: the proportion of kernel masks that need more elements to satisfy the minimum_minw is ', &
      dble(check_minw_limited)/dble(max(total_masks,1))
    write(fwarn,'(a,g12.5)') 'INFO: check minw resulted in an average separation increase across all kernel masks of ', &
      dble(check_minw_increase)/dble(max(total_masks,1))
    write(fwarn,'(a,g12.5)') 'INFO: the proportion of kernel masks that need more elements to satisfy the minimum_minw is ', &
      dble(check_minw_limited)/dble(max(total_masks,1))
    if (check_minw_limited > 0) then
      write(*,'(a)') "NOTE: increasing the maximum_separation will increase the proportion of kernel masks that satisfy the "// &
        "minimum_minw criterion"
      write(fwarn,'(a)') "NOTE: increasing the maximum_separation will increase the proportion of kernel masks that satisfy the "// &
        "minimum_minw criterion"
    end if
  end if
  write(*,'(a,g12.5)') 'INFO: the average separation across all kernel masks is ', &
    dble(total_mask_separations)/dble(max(total_masks,1))
  write(fwarn,'(a,g12.5)') 'INFO: the average separation across all kernel masks is ', &
    dble(total_mask_separations)/dble(max(total_masks,1))
end if

close(fwarn)

!------------------------------------------
! write out kernel details if requested

if (kernel_details_file) then
  if (debug_sparse.or..true.) write(*,'(a)') 'INFO: writing kernel details to kernel_details.txt file'

  filename = trim(output_dir)//"kernel_details.txt"
  open(fdetail,file=trim(filename),status='replace',iostat=ierror)
  if (ierror /= 0) then
    write(*,*) 'ERROR: problem opening file ',filename
    stop
  end if

  write(fdetail,'(a)') '-----------------------------------------------------------------------'
  write(fdetail,'(a)') 'CELL KERNEL DETAILS:'
  do i = 1, itotal
    do l = lbound(cell(i)%kernel,1), ubound(cell(i)%kernel,1)
      formatline = '(a,'//trim(indexformat)//',a,i1,a,i1,a,i1,a,a,a'//repeat(',a,'//trim(floatformat)//',a,'// &
        trim(indexformat)//',a',allocatable_integer_size(cell(i)%kernel(l)%ijk))//',a)'
      write(fdetail,fmt=formatline) ' i = ',i,'i: type = ',cell(i)%type,': dimension = ',cell(i)%dimensions,': l = ',l, &
        ': centring = ',cell(i)%kernel(l)%centring,': kernel v(ijk) =', &
        (' ',cell(i)%kernel(l)%v(n),'(',cell(i)%kernel(l)%ijk(n),')',n=1,allocatable_integer_size(cell(i)%kernel(l)%ijk)), &
        trim(print_kernel_reflect(cell(i)%kernel(l)))
!     call flush(fdetail)
    end do
  end do
  write(fdetail,'(a)') '-----------------------------------------------------------------------'
  write(fdetail,'(a)') 'FACE KERNEL DETAILS:'
  do j = 1, jtotal
    do l = lbound(face(j)%kernel,1), ubound(face(j)%kernel,1)
      formatline = '(a,'//trim(indexformat)//',a,i1,a,i1,a,i1,a,a,a'//repeat(',a,'//trim(floatformat)//',a,'// &
        trim(indexformat)//',a',allocatable_integer_size(face(j)%kernel(l)%ijk))//',a)'
      write(fdetail,fmt=formatline) ' j = ',j,'j: type = ',face(j)%type,': dimension = ',face(j)%dimensions,': l = ',l, &
        ': centring = ',face(j)%kernel(l)%centring,': kernel v(ijk) =', &
        (' ',face(j)%kernel(l)%v(n),'(',face(j)%kernel(l)%ijk(n),')',n=1,allocatable_integer_size(face(j)%kernel(l)%ijk)), &
        trim(print_kernel_reflect(face(j)%kernel(l)))
!     call flush(fdetail)
    end do
  end do
  write(fdetail,'(a)') '-----------------------------------------------------------------------'
  write(fdetail,'(a)') 'NODE KERNEL DETAILS:'
  do k = 1, ktotal
    do l = lbound(node(k)%kernel,1), ubound(node(k)%kernel,1)
      formatline = '(a,'//trim(indexformat)//',a,i1,a,i1,a,i1,a,a,a'//repeat(',a,'//trim(floatformat)//',a,'// &
        trim(indexformat)//',a',allocatable_integer_size(node(k)%kernel(l)%ijk))//',a)'
      write(fdetail,fmt=formatline) ' k = ',k,'k: type = ',node(k)%type,': domain_dimensions = ',node(k)%domain_dimensions, &
         ': l = ',l,': centring = ',node(k)%kernel(l)%centring,': kernel v(ijk) =', &
        (' ',node(k)%kernel(l)%v(n),'(',node(k)%kernel(l)%ijk(n),')',n=1,allocatable_integer_size(node(k)%kernel(l)%ijk)), &
        trim(print_kernel_reflect(node(k)%kernel(l)))
!     call flush(fdetail)
    end do
  end do

  close(fdetail)
end if

!------------------------------------------

if (debug_sparse) write(*,'(a/80(1h-))') 'subroutine setup_kernels'

end subroutine setup_kernels

!-----------------------------------------------------------------

subroutine construct_orthogonal_basis(centring,r,norm,error)

! here we find orthogonal basis vectors for the space spanned by r, and put those vectors
!  and the optional norm array in terms of this new basis
! r and norm get resized within this routine

! two cases of basis choice (explanation from v0.50 when putting in node kernels):
! 1) if the norm is given, the chosen basis will be based on this norm, but modified so that only the basis vectors that span the kernel space are kept
! (so actually the norm passed in must be special in that it contains orthogonal vectors that span the kernel space)
! this is the case for all face kernels, the cell derivative kernels and all node kernels
! note that for the optimisation method, (I think from memory that) the basis vectors must align with the derivative directions
! hence, for the optimisation method, a 2D mesh must be co-planar with 2 coordinate directions
!
! 2) for the node->cell and face->cell (cell l = 0 and 4) kernels any basis can be used

! TODO: could modify so that mls kernels are allow on planar surfaces that are not aligned with coordinate directions

use general_module
use lapack_module
use numerical_recipes_module
character(len=*) :: centring
integer :: i, j, m, n, n_plus, nchange, basis_dimension, null_dimension, nnorm
double precision, dimension(:,:), allocatable, optional :: norm
double precision, dimension(:,:), allocatable :: r, a
double precision, dimension(:), allocatable :: w
double precision, dimension(:,:), allocatable :: u, v, r_basis
integer, dimension(:), allocatable :: basis_list, null_list
double precision, dimension(totaldimensions,totaldimensions) :: basis_vectors
logical :: error
double precision, parameter :: small_element = 1.d-7 ! had problems when this was 1.d-10 - think that lack of precision in .msh file may be an issue
logical, parameter :: numrec = .false.
logical, parameter :: debug = .false.

if (debug) write(83,'(80(1h+)/a)') 'subroutine construct_orthogonal_basis'

error = .false.
m = totaldimensions ! number of rows in r
n = ubound(r,2) ! number of vectors in r
if (ubound(r,1) /= totaldimensions) call error_stop('dimensions of r incorrect in construct_orthonormal_basis')
nnorm = 0
if (present(norm)) then
  nnorm = ubound(norm,2) ! number of vectors in norm
  if (ubound(norm,1) /= m) call error_stop('dimensions of norm incorrect in construct_orthonormal_basis')
end if
if (debug) then
  write(83,'(a)') 'centring = '//trim(centring)
  write(83,*) 'j:r'
  do j = 1, m
    write(83,'(i2,100(a,g9.2))') j,':',(r(j,i),' ',i=1,n)
  end do
  if (present(norm)) then
    write(83,*) 'j:norm'
    do j = 1, m
      write(83,'(i2,100(a,g9.2))') j,':',(norm(j,i),' ',i=1,nnorm)
    end do
  end if
end if

n_plus = max(m,n) ! increase n given to svd so that a minimum of m (3) basis vectors are returned
allocate(a(m,n_plus),u(m,m),w(n_plus),v(n_plus,n_plus))
a = 0.d0
a(:,1:n) = r ! copy the first n columns of r to a
! for svd dimensions should be: a(m,n_plus), u(m,m), w(n_plus), v(n_plus,n_plus)
if (numrec) then
  call numerical_recipes_singular_value_decomposition(a=a,u=u,w=w,v=v,error=error)
else
  call lapack_singular_value_decomposition(a=a,u=u,w=w,v=v,error=error)
end if
if (error) call error_stop('problem performing singular value decomposition in construct_orthogonal_basis')
if (debug) then
  write(83,*) 'done svd decomposition'
  write(83,*) 'j:u'
  do j = 1, m
    write(83,'(i2,100(a,g9.2))') j,':',(u(j,i),' ',i=1,m)
  end do
  write(83,*) 'j:w:v'
  do j = 1, n_plus
    write(83,'(i2,100(a,g9.2))') j,':',w(j),':',(v(j,i),' ',i=1,n_plus)
  end do
end if

do j = 1, m
  if (abs(w(j)) >= small_element) then
    call push_integer_array(array=basis_list,new_element=j)
! else
  else if (vector_magnitude(u(:,j)) >= small_element ) then ! null_list vectors must be nonzero
    call push_integer_array(array=null_list,new_element=j)
  end if
end do
! either of these could be zero
basis_dimension = allocatable_integer_size(basis_list)
null_dimension = allocatable_integer_size(null_list)

! check that a basis has been found that spans fully three dimensional test
if (basis_dimension+null_dimension /= totaldimensions) then
  write(*,*) 'ERROR: singular value decomposition was not able to find three orthonormal basis vectors'
  if (debug) write(83,*) 'ERROR: singular value decomposition was not able to find three orthonormal basis vectors'
  error = .true.
end if

! now assemble the basis vectors matrix
basis_vectors = 0.d0
do i = 1, basis_dimension
  basis_vectors(:,i) = u(:,basis_list(i))
end do
do i = basis_dimension+1, basis_dimension+null_dimension
  basis_vectors(:,i) = u(:,null_list(i-basis_dimension))
end do
  
if (debug) then
  write(83,*) 'basis_dimension = ',basis_dimension,': basis_list = ',basis_list
  write(83,*) 'null_dimension = ',null_dimension,': null_list = ',null_list
  write(83,*) 'j:basis_vectors'
  do j = 1, totaldimensions
    write(83,'(i2,100(a,g9.2))') j,':',(basis_vectors(j,i),' ',i=1,totaldimensions)
  end do
end if

! if the kernel is for a face, use these as a basis instead (so that facegrad[l=4] returns gradient normal to face for example)
if (trim(centring) == 'face'.and.present(norm)) then
  if (nnorm /= 2*totaldimensions) call error_stop('face norm has wrong number of dimensions in construct_orthogonal_basis')
! need to check that the space spanned by the non-null svd basis vectors is the same as that spanned
!  by the same number of norm vectors
! only have to worry about 2 and 1 dimensional spaces - in the former check that the nullspace is equivalent, in the latter check the
!  the basis space is equivalent
! need to think about face kernels on curvlinear meshes
  if (basis_dimension == 2.and.abs(sqrt(abs(dot_product(basis_vectors(:,3),norm(:,6))))-1.d0) > small_element) then
    write(*,*) 'basis_vectors(:,3) = ',basis_vectors(:,3)
    write(*,*) 'norm(:,6) = ',norm(:,6)
    call error_stop('space spanned by svd basis vectors and that spanned by face norm vectors is not the same in '// &
      'construct_orthogonal_basis')
  else if (basis_dimension == 1.and.abs(sqrt(abs(dot_product(basis_vectors(:,1),norm(:,4))))-1.d0) > small_element) then
    write(*,*) 'basis_vectors(:,1) = ',basis_vectors(:,1)
    write(*,*) 'norm(:,4) = ',norm(:,4)
    call error_stop('space spanned by svd basis vectors and that spanned by face norm vectors is not the same in '// &
      'construct_orthogonal_basis')
  end if
! TODO: maybe there are smarter solutions here
! TODO: if there is an error could leave basis as svd one - no, optimise kernels requires this to work
  basis_vectors(:,1:3) = norm(:,4:6)
else if ((trim(centring) == 'cell'.or.trim(centring) == 'node').and.present(norm)) then
  if (nnorm /= totaldimensions) then
    write(*,*) 'nnorm = ',nnorm,': totaldimensions = ',totaldimensions
    call error_stop(trim(centring)//' norm has wrong number of dimensions in construct_orthogonal_basis')
  end if
  if (basis_dimension == 2) then
    nchange = maxloc(abs(basis_vectors(:,3)),dim=1)
    if (debug) write(83,*) 'test variable (should be small) = ',abs(sqrt(abs(dot_product(basis_vectors(:,3),norm(:,nchange))))-1.d0)
    if (abs(sqrt(abs(dot_product(basis_vectors(:,3),norm(:,nchange))))-1.d0) > small_element) then
      write(*,*) 'basis_vectors(:,3) = ',basis_vectors(:,3)
      write(*,*) 'norm(:,nchange) = ',norm(:,nchange)
      call error_stop('space spanned by svd basis vectors is not representable by coordinate direction vectors in '// &
        'construct_orthogonal_basis: probably the mesh is not aligned with the coordinate directions which is required '// &
        'in this routine, although this could be modified to be more flexible under the mls method')
    end if
! set basis vectors to the norm ones, swapping the null_direction one to the back
    basis_vectors(:,1:3) = norm(:,1:3)
    basis_vectors(:,3) = norm(:,nchange)
    basis_vectors(:,nchange) = norm(:,3)
  else if (basis_dimension == 1) then
    nchange = maxloc(abs(basis_vectors(:,1)),dim=1)
    if (abs(sqrt(abs(dot_product(basis_vectors(:,1),norm(:,nchange))))-1.d0) > small_element) then
      write(*,*) 'basis_vectors(:,1) = ',basis_vectors(:,1)
      write(*,*) 'norm(:,nchange) = ',norm(:,nchange)
      call error_stop('space spanned by svd basis vectors is not representable by coordinate direction vectors in '// &
        'construct_orthogonal_basis: probably the mesh is not aligned with the coordinate directions which is required '// &
        'in this routine, although this could be modified to be more flexible under the mls method')
    end if
! set basis vectors to the norm ones, swapping the basis_direction one to the front
    basis_vectors(:,1:3) = norm(:,1:3)
    basis_vectors(:,1) = norm(:,nchange)
    basis_vectors(:,nchange) = norm(:,1)
  else
    basis_vectors(:,1:3) = norm(:,1:3)
  end if
! put option here to be able to continue with mls kernels??
end if

if (debug) then
  write(83,*) 'j:basis_vectors after attempting to use the normal vectors'
  do j = 1, totaldimensions
    write(83,'(i2,100(a,g9.2))') j,':',(basis_vectors(j,i),' ',i=1,totaldimensions)
  end do
end if

deallocate(a,u,v,w)
if (allocated(null_list)) deallocate(null_list)
if (allocated(basis_list)) deallocate(basis_list)
if (error) return

! now to express all the r and norm vectors in terms of the new basis
allocate(r_basis(totaldimensions,n+nnorm))
r_basis(:,1:n) = r(:,1:n)
if (present(norm)) r_basis(:,n+1:n+nnorm) = norm(:,1:nnorm)
if (.true.) then
! noting that the basis_vectors matrix is orthogonal, its inverse is equal to its transpose
  r_basis = matmul(transpose(basis_vectors),r_basis)
else if (numrec) then
  call numerical_recipes_linear_solver(a=basis_vectors,b=r_basis,error=error)
else
  call lapack_linear_solver(a=basis_vectors,b=r_basis,error=error)
end if
if (error) then
  if (debug) write(83,*) 'ERROR: problem when converting vectors to new basis in construct_orthogonal_basis'
  deallocate(r_basis)
  return
end if

if (debug) then
  write(83,*) 'j:r_basis'
  do j = 1, totaldimensions
    write(83,'(i2,100(a,g9.2))') j,':',(r_basis(j,i),' ',i=1,n+nnorm)
  end do
end if

! check that basis_dimension is consistent with r_basis
do j = basis_dimension+1, totaldimensions
  do i = 1, n
    if (abs(r_basis(j,i)) >= small_element) call error_stop('ERROR: non-zero r_basis r component beyond basis_dimension')
  end do
  do i = n+1, n+nnorm
    if (abs(r_basis(j,i)) >= small_element) then
      if (debug) write(83,'(a,i1,a)') &
        'WARNING: normal vector ',i-n,' cannot be expressed in the r space basis vectors'
      r_basis(:,i) = 0.d0 ! zero all elements of this normal as a marker to calling routine
    end if
  end do
end do

! reshape norm and r to return
! keep number of rows of r greater than zero
deallocate(r)
allocate(r(max(basis_dimension,1),n))
r(:,:) = r_basis(1:max(basis_dimension,1),1:n)
if (present(norm)) then
  deallocate(norm)
  allocate(norm(max(basis_dimension,1),nnorm))
  norm(:,:) = r_basis(1:max(basis_dimension,1),n+1:n+nnorm)
end if
deallocate(r_basis)

if (debug) then
  write(83,*) 'j:r'
  do j = 1, max(basis_dimension,1)
    write(83,'(i2,100(a,g9.2))') j,':',(r(j,i),' ',i=1,n)
  end do
  if (present(norm)) then
    write(83,*) 'j:norm'
    do j = 1, max(basis_dimension,1)
      write(83,'(i2,100(a,g9.2))') j,':',(norm(j,i),' ',i=1,nnorm)
    end do
  end if
end if

if (debug) write(83,'(a/80(1h-))') 'subroutine construct_orthogonal_basis'

end subroutine construct_orthogonal_basis

!-----------------------------------------------------------------

subroutine mls_kernel(centring,ijk,l_kernel,l_coor,rr,norm,pp,kernel,local_polynomial_order,minimum_separation,separation_array, &
  separation_index,error,hyperbolic_kernel_local)

! here we calculate the zeroth and first order kernels using a mls method, adapted from
!  papers of L. Cueto-Felgueroso, 2006ish

use general_module
use lapack_module
use numerical_recipes_module

character(len=*), intent(in) :: centring ! face|cell|node that the kernel is associated with (not the location of the data)
integer, intent(in) :: ijk ! index of element
integer, intent(in) :: l_kernel ! number of kernel
integer, intent(in) :: l_coor ! coordinate of rr which is direction of the kernel if this is a derivative kernel
double precision, dimension(:,:), allocatable :: rr ! array of surrounding points relative to kernel centre
double precision, dimension(:), optional :: norm ! normal for first order direction
double precision, dimension(:), allocatable :: weight ! weighting function
integer, dimension(:), allocatable :: weight_importance ! weighting function importance (not used here)
integer, dimension(:), allocatable :: separation_array
integer, dimension(:), allocatable :: separation_index
logical :: hyperbolic_kernel_local
double precision, dimension(:), allocatable :: kernel ! kernel to calculate
double precision, dimension(:,:), allocatable :: pp, mmm, mmm_inv, ww, identity
double precision, dimension(:), allocatable :: rr_centre
integer :: local_polynomial_order ! local polynomial_order
integer :: minimum_separation ! largest separation that will be considered in this kernel
integer :: spatial_dimensions, mm, nn, n, i, j, nn_total
logical :: error
double precision :: trace
logical, parameter :: numrec = .false.
logical, parameter :: debug = .false.

if (debug) write(83,'(80(1h+)/a)') 'subroutine mls_kernel'

kernel = 0.d0
error = .false.

! find spatial dimensions
!spatial_dimensions = dimensions
spatial_dimensions = ubound(rr,1)
if (present(norm)) then
  if (spatial_dimensions /= ubound(norm,1)) call error_stop('dimensions of rr and norm do not match in subroutine mls_kernel')
end if
! total number of kernel nodes, including those that may not be used as their separation_index is higher than minimum_separation
nn_total = ubound(rr,2)

! polynomial basis dimension from the pp tensor (number of rows in pp)
mm = ubound(pp,1)
if (ubound(pp,2) /= nn_total) call error_stop('dimensions of pp and rr do not match in mls_kernel')

! calculate the weights
if (trim(centring) == 'face') then
  call calc_weight(centring,ijk,l_coor,rr,minimum_separation,face(ijk)%kernel(l_kernel)%ijk,separation_array, &
    separation_index,weight,weight_importance,hyperbolic_kernel_local)
else if (trim(centring) == 'cell') then
  call calc_weight(centring,ijk,l_coor,rr,minimum_separation,cell(ijk)%kernel(l_kernel)%ijk,separation_array, &
    separation_index,weight,weight_importance,hyperbolic_kernel_local)
else
  call calc_weight(centring,ijk,l_coor,rr,minimum_separation,node(ijk)%kernel(l_kernel)%ijk,separation_array, &
    separation_index,weight,weight_importance,hyperbolic_kernel_local)
end if
nn = ubound(weight,1) ! set number of elements that are in use, which corresponds to the size of weight

if (debug) then
  write(83,*) 'spatial_dimensions,mm,nn,nn_total'
  write(83,*) spatial_dimensions,mm,nn,nn_total
  if (present(norm)) then
    write(83,*) 'derivative: norm = ',norm
  else
    write(83,*) 'average: no norm'
  end if
end if

if (debug) then
  write(83,*) 'j:rr'
  do j = 1, ubound(rr,1)
    write(83,'(i2,100(a,g9.2))') j,':',(rr(j,i),' ',i=1,ubound(rr,2))
  end do
end if

! allocate various
allocate(mmm(mm,mm),mmm_inv(mm,mm),ww(nn,nn),identity(mm,mm))

if (debug) then
  write(83,*) 'j:pp'
  do j = 1, ubound(pp,1)
    write(83,'(i2,100(a,g9.2))') j,':',(pp(j,i),' ',i=1,ubound(pp,2))
  end do
end if

! form ww, which is the tensor diagonal of the weight vector
ww = 0.d0
do n = 1, nn
  ww(n,n) = weight(n)
end do

if (debug) then
  write(83,*) 'j:ww'
  do j = 1, ubound(ww,1)
    write(83,'(i2,100(a,g9.2))') j,':',(ww(j,i),' ',i=1,ubound(ww,2))
  end do
end if

! form mmm
mmm = matmul(matmul(pp(:,1:nn),ww),transpose(pp(:,1:nn)))

! invert mm using lu decompostion by inverting identity matrix column by column
! setup mm_inv as identity matrix
identity = 0.d0
do n = 1, mm
  identity(n,n) = 1.d0
end do
mmm_inv = identity
if (numrec) then
  call numerical_recipes_linear_solver(a=mmm,b=mmm_inv,error=error,overwritea=.false.)
else
  call lapack_linear_solver(a=mmm,b=mmm_inv,error=error,overwritea=.false.)
end if

! check accuracy of inversion - guarding against numerical singularity
!trace = norm2(matmul(mmm,mmm_inv)-identity) norm2 not supported by gfortran 4.5.1
trace = sqrt(sum((matmul(mmm,mmm_inv)-identity)**2))

if (debug) write(83,*) 'trace = ',trace
if (trace > 1.d-10) error = .true.

if (error) then
  if (debug) write(83,*) 'error occured when trying to invert mmm - returning'
  deallocate(mmm,mmm_inv,ww,identity)
  return
end if

if (debug) then
  write(83,*) 'j:mmm_inv'
  do j = 1, ubound(mmm_inv,1)
    write(83,'(i2,100(a,g9.2))') j,':',(mmm_inv(j,i),' ',i=1,ubound(mmm_inv,2))
  end do
end if

allocate(rr_centre(spatial_dimensions))
rr_centre = 0.d0
if (.not.present(norm)) then
  kernel(1:nn) = matmul(polynomial_basis_vector(rr=rr_centre,local_polynomial_order=local_polynomial_order), &
    matmul(mmm_inv,matmul(pp(:,1:nn),ww)))
else
  kernel(1:nn) = matmul(polynomial_basis_vector(rr=rr_centre,norm=norm,local_polynomial_order=local_polynomial_order), &
    matmul(mmm_inv,matmul(pp(:,1:nn),ww)))
end if

if (debug) then
  write(83,*) 'j:kernel'
  do j = 1, ubound(kernel,1)
    write(83,'(i2,100(a,g9.2))') j,':',kernel(j)
  end do
end if

deallocate(mmm,mmm_inv,ww,rr_centre,identity)

if (debug) write(83,'(a/80(1h-))') 'subroutine mls_kernel'

end subroutine mls_kernel

!-----------------------------------------------------------------

double precision function radial_kernel(rr,derivative_order,hyperbolic_kernel_local)

! this is the kernel required for the mls kernel method
! this kernel is 1 at rr=0, 1/2 at rr=1, approaches 0 as rr approaches infinity

integer :: derivative_order
logical :: hyperbolic_kernel_local
double precision :: rr
double precision :: pi = 4.d0*atan(1.d0)
! this seems to be good
integer :: n = 1
double precision :: a = .3725159907783194d0 ! this value gives maximum gradient at rr=1, which is consistent with the kernel distance scalings
!double precision :: a = 0.5d0
!integer :: n = 2
!double precision :: a = 1.50d0
! was using this before but seems to be too peaky for high aspect ratios elements though without stretched_radial_kernel on
! also suffers from/causes round-off errors
!integer :: n = 3
!double precision :: a = 1.20d0
!double precision :: gamma = 1.4d0

radial_kernel = 0.d0

if (derivative_order == 0) then
  if (hyperbolic_kernel_local) then
    radial_kernel = (1.d0+hyperbolic_b)/(rr+hyperbolic_b)
  else
!   radial_kernel = (cos(pi*rr/(1.d0+rr))+1.d0)/2.d0
!   radial_kernel = ((cos(pi*rr/(rr+1))+1)/2)**n
!   radial_kernel = (cos(pi*a*rr/(a*rr+1))+1)**n/2**n
!   radial_kernel = exp(-(rr/gamma)**2)
    radial_kernel = ((cos(pi*a*rr/(a*rr+1))+1)/2.d0)**n ! current
  end if
else if (derivative_order == 1) then
  if (hyperbolic_kernel_local) then
    radial_kernel = -(1.d0+hyperbolic_b)/(rr+hyperbolic_b)**2
  else
!  radial_kernel = -pi*sin(pi*rr/(1.d0+rr))/(2.d0*(1.d0+rr)**2)
!  radial_kernel = -pi*n*(cos(pi*rr/(rr+1))+1)**(n-1)*sin(pi*rr/(rr+1))/(2**n*(rr+1)**2)
!  radial_kernel = -pi*a*n*(cos(pi*a*rr/(a*rr+1))+1)**(n-1)*sin(pi*a*rr/(a*rr+1))/(2**n*(a*rr+1)**2)
   radial_kernel = -pi*a*dble(n)*(cos(pi*a*rr/(a*rr+1))+1)**(n-1)*sin(pi*a*rr/(a*rr+1))/(2**n*(a*rr+1)**2) ! current
  end if
end if

end function radial_kernel

!-----------------------------------------------------------------

function polynomial_basis_vector(rr,norm,local_polynomial_order)

! here we calculate the polynomial basis vector evaluated at rr
! if norm is present, calculate the derivative (gradient) of this vector in the direction of norm

use general_module
double precision, dimension(:) :: rr ! single location vector of unknown dimensions
double precision, dimension(:), optional :: norm ! normal for first order direction
double precision, dimension(:), allocatable :: polynomial_basis_vector ! result returned
integer :: local_polynomial_order ! local polynomial_order
integer :: spatial_dimensions, vector_dimension, n
logical, parameter :: debug = .false.

if (debug) write(83,'(80(1h+)/a)') 'function polynomial_basis_vector'

if (local_polynomial_order > 3) call error_stop &
  ('polynomial_basis_vector function can handle atmost 3rd order polynomials right now')
if (local_polynomial_order < 0) call error_stop &
  ('polynomial_basis_vector function must have non-negative polynomial_order')

spatial_dimensions = ubound(rr,1)
if (present(norm)) then
  if (ubound(norm,1) /= spatial_dimensions) then
    write(*,*) 'norm and rr spatial dimensions don''t match in polynomial_basis_vector'
    stop
  end if
end if
if (spatial_dimensions < 1.or.spatial_dimensions > 3) call error_stop("spatial dimensions incorrect in polynomial_basis_vector")

! find size of basis vector
vector_dimension = 1
if (local_polynomial_order >= 1) vector_dimension = vector_dimension + spatial_dimensions
if (local_polynomial_order >= 2) then
  if (spatial_dimensions == 1) then
    vector_dimension = 3
  else if (spatial_dimensions == 2) then
    vector_dimension = 6
  else
    vector_dimension = 10
  end if
end if
if (local_polynomial_order >= 3) then
  if (spatial_dimensions == 1) then
    vector_dimension = 4
  else if (spatial_dimensions == 2) then
    vector_dimension = 10
  else
    vector_dimension = 20
  end if
end if

allocate(polynomial_basis_vector(vector_dimension))

if (.not.present(norm)) then
  polynomial_basis_vector(1) = 1.d0
  if (local_polynomial_order >= 1) polynomial_basis_vector(2:spatial_dimensions+1) = rr
  if (local_polynomial_order >= 2) then
    do n = 1, spatial_dimensions
      polynomial_basis_vector(n+spatial_dimensions+1) = rr(n)**2
    end do
    if (spatial_dimensions == 2) then
      polynomial_basis_vector(6) = rr(1)*rr(2)
    else if (spatial_dimensions == 3) then
      polynomial_basis_vector(8) = rr(1)*rr(2)
      polynomial_basis_vector(9) = rr(1)*rr(3)
      polynomial_basis_vector(10) = rr(2)*rr(3)
    end if
  end if
  if (local_polynomial_order >= 3) then
    if (spatial_dimensions == 1) then
      polynomial_basis_vector(4) = rr(1)**3
    else if (spatial_dimensions == 2) then
      polynomial_basis_vector(7) = rr(1)**3
      polynomial_basis_vector(8) = rr(2)**3
      polynomial_basis_vector(9) = rr(1)**2*rr(2)
      polynomial_basis_vector(10) = rr(1)*rr(2)**2
    else if (spatial_dimensions == 3) then
      polynomial_basis_vector(11) = rr(1)**3
      polynomial_basis_vector(12) = rr(2)**3
      polynomial_basis_vector(13) = rr(3)**3
      polynomial_basis_vector(14) = rr(1)**2*rr(2)
      polynomial_basis_vector(15) = rr(1)*rr(2)**2
      polynomial_basis_vector(16) = rr(1)**2*rr(3)
      polynomial_basis_vector(17) = rr(1)*rr(3)**2
      polynomial_basis_vector(18) = rr(2)**2*rr(3)
      polynomial_basis_vector(19) = rr(2)*rr(3)**2
      polynomial_basis_vector(20) = rr(1)*rr(2)*rr(3)
    end if
  end if
else
  polynomial_basis_vector(1) = 0.d0
  if (local_polynomial_order >= 1) polynomial_basis_vector(2:spatial_dimensions+1) = norm
  if (local_polynomial_order >= 2) then
    do n = 1, spatial_dimensions
      polynomial_basis_vector(n+spatial_dimensions+1) = 2.d0*rr(n)*norm(n)
    end do
    if (spatial_dimensions == 2) then
      polynomial_basis_vector(6) = rr(1)*norm(2)+rr(2)*norm(1)
    else if (spatial_dimensions == 3) then
      polynomial_basis_vector(8) = rr(1)*norm(2)+rr(2)*norm(1)
      polynomial_basis_vector(9) = rr(1)*norm(3)+rr(3)*norm(1)
      polynomial_basis_vector(10) = rr(2)*norm(3)+rr(3)*norm(2)
    end if
  end if
  if (local_polynomial_order >= 3) then
    if (spatial_dimensions == 1) then
      polynomial_basis_vector(4) = 3.d0*rr(1)**2*norm(1)
    else if (spatial_dimensions == 2) then
      polynomial_basis_vector(7) = 3.d0*rr(1)**2*norm(1)
      polynomial_basis_vector(8) = 3.d0*rr(2)**2*norm(2)
      polynomial_basis_vector(9) = 2.d0*rr(1)*rr(2)*norm(1) + rr(1)**2*norm(2)
      polynomial_basis_vector(10) = rr(2)**2*norm(1) + rr(1)*2.d0*rr(2)*norm(2)
    else if (spatial_dimensions == 3) then
      polynomial_basis_vector(11) = 3.d0*rr(1)**2*norm(1)
      polynomial_basis_vector(12) = 3.d0*rr(2)**2*norm(2)
      polynomial_basis_vector(13) = 3.d0*rr(3)**2*norm(3)
      polynomial_basis_vector(14) = 2.d0*rr(1)*rr(2)*norm(1) + rr(1)**2*norm(2)
      polynomial_basis_vector(15) = rr(2)**2*norm(1) + rr(1)*2.d0*rr(2)*norm(2)
      polynomial_basis_vector(16) = 2.d0*rr(1)*rr(3)*norm(1) + rr(1)**2*norm(3)
      polynomial_basis_vector(17) = rr(3)**2*norm(1) + rr(1)*2.d0*rr(3)*norm(3)
      polynomial_basis_vector(18) = 2.d0*rr(2)*rr(3)*norm(2) + rr(2)**2*norm(3)
      polynomial_basis_vector(19) = rr(3)**2*norm(2) + rr(2)*2.d0*rr(3)*norm(3)
      polynomial_basis_vector(20) = rr(2)*rr(3)*norm(1)+rr(1)*rr(3)*norm(2)+rr(1)*rr(2)*norm(3)
    end if
  end if
end if
  
if (debug) write(83,'(a/80(1h-))') 'function polynomial_basis_vector'

end function polynomial_basis_vector

!-----------------------------------------------------------------

subroutine calc_weight(centring,ijk,l_coor,rr,minimum_separation,imask,separation_array,separation_index, &
  weight,weight_importance,hyperbolic_kernel_local)

! here we calculate the weight scaling factor for all cells in the kernel
! weight is either a true weighting (mls method) or a trial kernel value (optimisation method)
! also calculate weight_importance which is an integer measure of how much want the kernel to be weighted there
!  (0=high weighting, >0=lower weighting)

use general_module
character(len=*), intent(in) :: centring ! face|cell|node that the kernel is associated with (not the location of the data)
integer, intent(in) :: ijk ! index of element
integer, intent(in) :: l_coor ! coordinate of rr which is direction of the kernel if this is a derivative kernel
double precision, dimension(:), allocatable :: weight ! the list of weights that is passed out
integer, dimension(:), allocatable :: weight_importance ! this specifies the importance (separation) index of each element
integer, dimension(:), allocatable :: imask ! list of cells that could be in the kernel
integer :: minimum_separation ! only elements with this separation will have non-zero weights for the mls method
integer, dimension(:), allocatable :: separation_index ! these are the final elements having that separation
integer, dimension(:), allocatable :: separation_array ! this is the separation indices of each element
double precision, dimension(:,:), allocatable :: rr ! array of cell locations, first index is spatial dimension, second is point number
integer :: nn_total, nn, separation, i, ii, start_index, end_index, i2, ii2, flux_to_cells, nn_minimum_separation, mm, m, &
  maximum_separation, nn_local_separation, ll
double precision :: weight_sum, rr_mag, s_uniform, xx_min
double precision, dimension(2) :: xx_num,xx_den,xx_length
logical :: hyperbolic_kernel_local, shift_local
double precision, dimension(:), allocatable :: h_local ! average separation in each dimension
double precision, dimension(:), allocatable :: s_local ! scaling length in each dimension, calculated from h_local if 
double precision, dimension(:,:), allocatable :: rr_local ! local copy of rr that is scaled by h_local
logical, parameter :: debug = .false.

if (debug) write(83,'(80(1h+)/a)') 'subroutine calc_weight'

if (debug) then
  write(83,*) 'calling parameters: centring = '//trim(centring)
  write(83,*) 'calling parameters: ijk = ',ijk
  write(83,*) 'calling parameters: l_coor = ',l_coor
  write(83,*) 'calling parameters: minimum_separation = ',minimum_separation
  write(83,*) 'calling parameters: imask = ',imask
  write(83,*) 'calling parameters: separation_index = ',separation_index
  write(83,*) 'calling parameters: separation_array = ',separation_array
end if
  
! total number of elements
nn_total = ubound(rr,2)

! maximum separation of elements in separation_index array (which should be the same as the main routine's variable)
maximum_separation = ubound(separation_index,1)

! bound minimum separation by the maximum separation available (just to make sure)
minimum_separation = min(minimum_separation,maximum_separation)

! number of elements up to and including minimum separation
nn_minimum_separation = separation_index(minimum_separation)

! calc nn, which is the number of elements that should be included in the kernel calc
! different for mls versus optimisation 
if (trim(kernel_method) == 'optimisation') then
  nn = nn_total ! all cells included at this stage
else
  nn = nn_minimum_separation
end if

! only include local cells (up to and including separation 2) in scaling calculations
! v0.42
!nn_local_separation = separation_index(min(minimum_separation,2))
!nn_local_separation = separation_index(min(minimum_separation,3))
! for v0.50, trying this
nn_local_separation = separation_index(minimum_separation)

! number of direction dimensions of rr
mm = ubound(rr,1)

! resize weight and zero value
call resize_double_precision_array(keep_data=.false.,array=weight,new_size=nn)
weight = 0.d0

! find weight_importance here, for elements of all separations
call copy_integer_array(original=separation_array,copy=weight_importance)
weight_importance = weight_importance - 1 ! 0 is the minimum in the weight index
! orientation_dependent_weights means that for face kernels and tangential derivatives, the cells adjacent to the face are given less weighting
if (orientation_dependent_weights.and.trim(centring)=='face'.and.l_coor >= 2) then
  if (maximum_separation >= 1) weight_importance(1:separation_index(1)) = 1 ! these are now less important
! if (maximum_separation >= 2) weight_importance(separation_index(1)+1:separation_index(2)) = 0 ! than these
! now only increase this cell's importance (ie, set weight_importance = 0) if the cell shares a node with the face
  if (maximum_separation >= 2) then
    do ii = separation_index(1)+1, separation_index(2)
      i = imask(ii) ! this is a cell in the kernel that has a separation of 2
      if (location_in_list_dummy(array=face(ijk)%icell,element=i) /= 0) weight_importance(ii) = 0
    end do
  end if
else if (orientation_dependent_weights.and.trim(centring)=='cell'.and.l_coor >= 1) then
  if (cell(ijk)%type == 1.and.maximum_separation >= 2) weight_importance(1:separation_index(1)) = 1 ! these are now less important
  if (maximum_separation >= 2) weight_importance(separation_index(1)+1:separation_index(2)) = 0 ! than these for derivatives
! unless we are on a boundary cell where all separation 1 and 2 cells have equal importance (of 0)
end if

if (conservative_weighting.and.trim(centring)=='face'.and.l_coor == 0) then
! not recommended anymore

  weight(1:2) = 0.5d0 ! initialise first separation weights
  end_index = 0
  do separation = 1, minimum_separation-1 ! loop through separations that are to donate weights
    start_index = end_index+1
    end_index = separation_index(separation)
    do ii = start_index, end_index
      i = imask(ii) ! i is a cell that is about to possibly loose some material

! calculate/find the cells that are about to be fluxed-to from cell i
      flux_to_cells = 0
      do ii2 = separation_index(separation)+1, separation_index(separation+1) ! this is all cells having a separation 1 greater
        i2 = imask(ii2)
        if (location_in_list_dummy(array=cell(i)%icell(2:ubound(cell(i)%jface,1)+1),element=i2) /= 0) then ! we have found a neighbour
          flux_to_cells = flux_to_cells + 1 ! if so then it will receive some weight from i
        end if
      end do

      if (debug) write(83,*) 'for cell i = ',i,' having separation = ',separation,' found flux-to_cells = ',flux_to_cells

! if any cells will receive some from i then go ahead and do the fluxing
      if (flux_to_cells > 0) then

        do ii2 = separation_index(separation)+1, separation_index(separation+1) ! this is all cells having a separation 1 greater
          i2 = imask(ii2)
          if (location_in_list_dummy(array=cell(i)%icell(2:ubound(cell(i)%jface,1)+1),element=i2) /= 0) then ! we have found a neighbour
            if (debug) write(83,*) 'adding weight to cell i2 = ',i2,' with current weight = ',weight(ii2)
            weight(ii2) = weight(ii2) + weight(ii)*weight_separation_multiplier/dble(flux_to_cells)
          end if
        end do

        if (debug) write(83,*) 'removing weight from cell i = ',i,' with current weight = ',weight(ii)
        weight(ii) = weight(ii)*(1.d0 - weight_separation_multiplier)
      end if

    end do

  end do

else
! these are based on the weight_importance
! if orientation_dependent_weights is set then these are different for each l_coor direction
! for the optimisation method, these weights are infact the trial kernels

! first make a copy of rr that will be manipulated for each (this) particular kernel direction
  allocate(rr_local(mm,nn_total),h_local(mm),s_local(mm))
  rr_local = rr

! shift the rr vectors of boundary kernels so that the kernel's centre is halfway between cells 1 and 2
  if (.not.hyperbolic_kernel_local.and.shift_boundary_weight_centre) then ! not for hyperbolic kernels
    if (trim(centring) == 'face' .or. trim(centring) == 'node') then ! shift boundary face kernels
      shift_local = .false.
      if (trim(centring) == 'face') then
        if (face(ijk)%type == 2) shift_local = .true.
      else
        if (node(ijk)%type == 2) shift_local = .true.
      end if
      if (shift_local) then
        if (debug.and..false.) then
          do i = 1, nn_total
            write(83,*) 'before shifting: i, rr_local(:,i) = ',i,rr_local(:,i)
          end do
        end if
        do i = nn_total, 1, -1
          rr_local(:,i) = rr_local(:,i) - rr_local(:,1)/2.d0 ! rr(:,1) is the neighbouring cell
        end do
        if (debug.and..false.) then
          do i = 1, nn_total
            write(83,*) 'after shifting: i, rr_local(:,i) = ',i,rr_local(:,i)
          end do
        end if
      end if
    else if (trim(centring) == 'cell') then ! shift boundary cell kernels
      if (cell(ijk)%type == 2) then
        if (debug.and..false.) then
          do i = 1, nn_total
            write(83,*) 'before shifting: i, rr_local(:,i) = ',i,rr_local(:,i)
          end do
        end if
        do i = nn_total, 1, -1
          if (i == 2) cycle
          rr_local(:,i) = rr_local(:,i) - rr_local(:,2)/2.d0 ! rr(:,2) is the neighbouring cell
        end do
        rr_local(:,2) = rr_local(:,2)/2.d0
        if (debug.and..false.) then
          do i = 1, nn_total
            write(83,*) 'after shifting: i, rr_local(:,i) = ',i,rr_local(:,i)
          end do
        end if
      end if
    end if
  end if
    
  if (stretched_radial_kernel.or.scaled_radial_kernel) then
!--------------
! find h_local, which is a local length scale for each coordinate direction, now based only on the local_separation elements
! logic here is to not decrease the kernel size relative to the mesh as the minimum separation increases
    h_local = 0.d0
    if (.true.) then ! based on average distance
      do m = 1, mm
        do i = 1, nn_local_separation
          h_local(m) = h_local(m) + abs(rr_local(m,i))
        end do
      end do
!     h_local = sqrt(2.d0)*h_local/dble(nn_local_separation)
      h_local = h_local/dble(nn_local_separation)
    else ! based on maximum distance
      do m = 1, mm
        do i = 1, nn_local_separation
          h_local(m) = max(h_local(m),abs(rr_local(m,i)))
        end do
      end do
    end if
! if there is a problem with any of the dimensions then don't do any scaling
    if (minval(h_local) < 1.d-10) then
      if (debug) write(83,*) 'Problem determining h_local, so not performing any scaling for this kernel: h_local = ',h_local
! temp &&&&
      write(83,*) 'Problem determining h_local, so not performing any scaling for this kernel: h_local = ',h_local
      h_local = 1.d0 
    end if
!--------------
! find the uniform scaling length, and set s_local for each dimension to this
    if (.not.scaled_radial_kernel) then
      s_uniform = 1.d0
    else if (l_coor >= 1) then
! for derivatives set uniform scaling length to lengthscale in derivative direction
      s_uniform = h_local(l_coor)
    else
! for averaging kernels set uniform scaling length to average lengthscale over all directions
      s_uniform = 0.d0
      do m = 1, mm
        s_uniform = s_uniform + h_local(m)**2
      end do
      s_uniform = sqrt(s_uniform/dble(mm))
    end if
!--------------
! now create scaling lengths
    if (.not.stretched_radial_kernel) then
      s_local = s_uniform
    else
      do m = 1, mm
        s_local(m) = s_uniform*(h_local(m)/s_uniform)**(stretched_uniformity)
      end do
    end if
!--------------
    if (debug) then
      write(83,*) 'h_local = ',h_local
      write(83,*) 's_local = ',s_local
      write(83,*) 's_uniform = ',s_uniform
    end if
! now scale rr by these lengths
    do m = 1, mm
      if (debug) write(83,*) 'scaling dimension m = ',m
      if (debug) write(83,*) 'before scaling: rr_local(m,:) = ',rr_local(m,:)
      rr_local(m,:) = rr_local(m,:)/s_local(m)
      if (debug) write(83,*) 'after scaling: rr_local(m,:) = ',rr_local(m,:)
    end do
  else
    s_local = 1.d0
  end if

! for derivative kernels, the maximum kernel magnitudes should be away from the kernel centre, unless the kernel is one sided
! the xx_min parameter attempts to capture how one-sided a kernel is, and is defined as the minimum second moment/first moment (approx) from each side of the kernel
! a one sided kernel (eg, boundary kernel) has xx_min = 0, so the derivative weighting function can peak at the kernel centre
! for a two sided kernel, we shift the weighting peak away from the kernel centre, so that the peak is located at xx_min (multiplied by shift_hyperbolic_distance)
! this feature can be switched off by setting shift_hyperbolic_distance <= 0.d0
  xx_min = 0.d0
  if (hyperbolic_kernel_local.and.shift_hyperbolic_distance > 0.d0.and.l_coor >= 1.and.l_coor <= 3) then
    xx_num = 0.d0
    xx_den = 0.d0
    xx_length = 0.d0
    do i = 1, nn
      if (rr_local(l_coor,i) > 0.d0) then
        ll = 1
      else if (rr_local(l_coor,i) < 0.d0) then
        ll = 2
      else
        cycle
      end if
      rr_mag = vector_magnitude(rr_local(:,i))
      xx_num(ll) = xx_num(ll) + radial_kernel(rr_mag,0,hyperbolic_kernel_local)*(rr_local(l_coor,i)**2)
      xx_den(ll) = xx_den(ll) + radial_kernel(rr_mag,0,hyperbolic_kernel_local)*abs(rr_local(l_coor,i))
    end do
    if (debug) write(83,*) 'll,xx_num,xx_den,xx_length'
    do ll = 1,2
      xx_length(ll) = xx_num(ll)/max(xx_den(ll),1.d-20)
      if (debug) write(83,*) ll,xx_num(ll),xx_den(ll),xx_length(ll)
    end do
    xx_min = shift_hyperbolic_distance*min(xx_length(1),xx_length(2))
    if (debug) then
      write(83,*) 'l_coor = ',l_coor
      write(83,*) 'xx_min = ',xx_min
    end if
  end if

  weight_sum = 0.d0
  do i = 1, nn

    rr_mag = vector_magnitude(rr_local(:,i))

    if (zero_nonoriented_weights.and.orientation_dependent_weights.and.trim(centring)=='face'.and.weight_importance(i) > 0) then
! if zero_nonoriented_weights is on and we are on a face then only the highest importance cells have a weight assigned
      weight(i) = 0.d0
    else if (radial_kernel_weighting) then
      if (trim(kernel_method) == 'optimisation'.and.l_coor >= 1) then
! for a derivative kernel
        if (hyperbolic_kernel_local) then
! from 290514
! still not found the best compromise at performance on unstructured V on structured
          weight(i) = abs(radial_kernel(max(rr_mag,xx_min),1,hyperbolic_kernel_local))* &
            (weight_separation_multiplier**(weight_importance(i)/2.d0))
!         weight(i) = abs(radial_kernel(max(rr_mag,xx_min),0))*(weight_separation_multiplier**(weight_importance(i)/2.d0))
! previous to 290514 - no good
!         weight(i) = abs(radial_kernel(abs(rr_mag-xx_min),1))*(weight_separation_multiplier**(weight_importance(i)/2.d0))
! before fiddling 260514
!         weight(i) = abs(radial_kernel(rr_mag,1))*(weight_separation_multiplier**(weight_importance(i)/2.d0))
!         weight_sum = weight_sum + weight(i)
        else if (separation_multiplied_trial_kernels) then
          weight(i) = (-radial_kernel(rr_mag,1,hyperbolic_kernel_local)*rr_local(l_coor,i)/max(rr_mag*s_local(l_coor),1.d-20))* &
            (weight_separation_multiplier**weight_importance(i))
!         weight_sum = weight_sum + weight(i)*rr(l_coor,i)
        else
          weight(i) = (-radial_kernel(rr_mag,1,hyperbolic_kernel_local)*rr_local(l_coor,i)/max(rr_mag*s_local(l_coor),1.d-20))
!         weight_sum = weight_sum + weight(i)*rr(l_coor,i)
        end if
        weight_sum = weight_sum + abs(weight(i)*rr(l_coor,i)) ! note, for hyperbolic kernels the sign has no meaning, so need to make sure these quantities are positive
      else
! for an average kernel or if we are doing mls which requries pure weights rather than a trial kernel
        if (hyperbolic_kernel_local) then
          weight(i) = radial_kernel(rr_mag,0,hyperbolic_kernel_local)*(weight_separation_multiplier**(weight_importance(i)/2.d0))
        else if (separation_multiplied_trial_kernels) then
          weight(i) = radial_kernel(rr_mag,0,hyperbolic_kernel_local)*(weight_separation_multiplier**weight_importance(i))
        else
          weight(i) = radial_kernel(rr_mag,0,hyperbolic_kernel_local)
        end if
        weight_sum = weight_sum + weight(i)
      end if
    else
! for no radial kernel function
      weight(i) = weight_separation_multiplier**weight_importance(i)
      weight_sum = weight_sum + weight(i)
    end if
    if (debug) write(83,'(2(a,i2),10(a,g11.4))') &
      'i = ',i,': import. = ',weight_importance(i),': rr_mag = ',rr_mag,': weight (not scaled) = ',weight(i), &
      ': rr_local =',(rr_local(m,i),' ',m=1,mm)
!   write(83,*) 'rr_mag = ',rr_mag
!   write(83,*) 'rr(:,i) = ',rr(:,i)
!   write(83,*) 'rr_local(:,i) = ',rr_local(:,i)
!   write(83,*) 'weight(i) = ',weight(i)

  end do
! special case normal boundary derivative - overwrite the centred trial kernel (which is zero anyway) to preserve straight sum
! TODO: same with node kernels if 1D? - no too tricky to pick these out from here, just use face kernels for node kernels if domain is 1D
  if (.not.shift_boundary_weight_centre.and.radial_kernel_weighting.and. &
    trim(kernel_method) == 'optimisation'.and.trim(centring) == 'face'.and.l_coor == 1.and..not.hyperbolic_kernel_local) then
    if (face(ijk)%type == 2) weight(2) = -sum(weight)
  end if
  weight = weight/weight_sum

  deallocate(rr_local,h_local,s_local)

end if

if (debug) then
  write(83,*) 'final parameters: minimum_separation = ',minimum_separation
  write(83,*) 'final parameters: weight = ',weight
  write(83,*) 'final parameters: weight_importance = ',weight_importance
end if
  
if (debug) write(83,'(a/80(1h-))') 'subroutine calc_weight'

end subroutine calc_weight

!-----------------------------------------------------------------

subroutine check_mask_minw(pp,separation_index,minimum_separation,minw)

use general_module
use lapack_module
use numerical_recipes_module
double precision, dimension(:,:), allocatable :: pp ! array of polynomial basis vectors
integer, dimension(:), allocatable :: separation_index
integer :: minimum_separation
double precision :: minw ! minimum w found representing the singularity of the mask (ref chenoweth09, JCP, 228, p5592)
logical :: error ! whether there are any problems with this mask or not
logical :: svd_error
double precision, dimension(:,:), allocatable :: bb, u, v
double precision, dimension(:), allocatable :: w
integer :: minimum_separation_in, i, j, mm, nn, nn_tot
logical :: numrec = .false.
logical, parameter :: debug = .false.
logical :: debug_sparse = .false.

if (debug) debug_sparse = .true.

if (debug_sparse) write(83,'(80(1h+)/a)') 'subroutine check_mask_minw'

error = .false.
minw = -1.d0 ! represents not set

mm = ubound(pp,1) ! this is the number of elements in each polynomial vector
nn_tot = ubound(pp,2) ! this is the total number of elements in the kernel
minimum_separation_in = min(minimum_separation,ubound(separation_index,1)) ! limit the minimum_separation to the separations available

if (debug_sparse) write(83,*) 'mm = ',mm,': nn_tot = ',nn_tot,': minimum_separation_in = ',minimum_separation_in

do minimum_separation = minimum_separation_in, ubound(separation_index,1)

  nn = separation_index(minimum_separation) ! find the number of elements in the kernel up to and including the minimum separation

  allocate(bb(nn,mm),u(nn,nn),w(mm),v(mm,mm))

  bb = transpose(pp(:,1:nn))

  if (debug) then
    write(83,*) 'before svd decomposition'
    write(83,*) 'j:bb'
    do j = 1, nn
      write(83,'(i2,100(a,g9.2))') j,':',(bb(j,i),' ',i=1,mm)
    end do
  end if

! for svd dimensions should be: a(m,n), u(m,m), w(n), v(n,n)
  if (numrec) then
    call numerical_recipes_singular_value_decomposition(a=bb,u=u,w=w,v=v,error=svd_error)
  else
    call lapack_singular_value_decomposition(a=bb,u=u,w=w,v=v,error=svd_error)
  end if
  if (svd_error) call error_stop('problem performing svd in check_kernel_mask')

  if (debug) then
    write(83,*) 'done svd decomposition'
    write(83,*) 'j:u'
    do j = 1, nn
      write(83,'(i2,100(a,g9.2))') j,':',(u(j,i),' ',i=1,mm)
    end do
    write(83,*) 'j:w:v'
    do j = 1, nn
      write(83,'(i2,100(a,g9.2))') j,':',w(j),':',(v(j,i),' ',i=1,nn)
    end do
  end if

  minw = minval(w)
  if (debug_sparse) write(83,*) 'minw = ',minw,': nn = ',nn,': minimum_separation = ',minimum_separation

  deallocate(bb,u,w,v)

  if (minw < minimum_minw) then
    if (nn == nn_tot) then
      if (debug_sparse) then
        write(83,*) 'WARNING: no more elements available within maximum separation to further increase minw'
      end if
      exit
    end if
    if (debug_sparse) write(83,*) 'Adding additional elements to kernel to increase minw'
! temp &&&
!   if (.true.) write(83,*) 'Adding additional elements to kernel to increase minw'
  else
    exit
  end if

end do

if (debug_sparse) write(83,*) 'final minimum_separation = ',minimum_separation,': minw = ',minw

if (debug_sparse) write(83,'(a/80(1h-))') 'subroutine check_mask_minw'

end subroutine check_mask_minw

!-----------------------------------------------------------------

subroutine construct_polynomial_basis_tensor(r,local_polynomial_order,pp,minimum_separation,separation_index,error)

! subroutine to allocate and construct polynomial basis tensor

double precision, dimension(:,:), allocatable :: r
integer :: local_polynomial_order
double precision, dimension(:,:), allocatable :: pp
integer :: minimum_separation
integer, dimension(:), allocatable :: separation_index
logical :: error
integer :: nn, mm, j, i, n, minimum_separation_original
logical, parameter :: debug = .false.

if (debug) write(83,'(80(1h-)/a)') 'subroutine construct_polynomial_basis_tensor'

error = .false.
! the r tensor consists of nn columns of m-dimensional location vectors

! number of kernel nodes
nn = ubound(r,2) ! equal to separation_index(ubound(separation_index,1))

! find polynomial basis dimension by sending a test vector to polynomial_basis_vector
mm = ubound(polynomial_basis_vector(rr=r(:,1),local_polynomial_order=local_polynomial_order),1)

! there must be atleast as many nodes as the polynomial basis dimension in the minimum_separation
! check this and increase minimum_separation if necessary
minimum_separation_original = minimum_separation
if (debug) write(83,*) 'minimum_separation = ',minimum_separation
if (debug) write(83,*) 'ubound(separation_index,1) = ',ubound(separation_index,1)
if (debug) write(83,*) 'minimum points required mm = ',mm

do minimum_separation = minimum_separation_original, ubound(separation_index,1)
  if (separation_index(minimum_separation) >= mm) exit ! if the number of nodes is larger than mm, exit loop
end do
minimum_separation = min(minimum_separation,ubound(separation_index,1))
if (debug) then
  if (separation_index(minimum_separation) < mm) then
    write(83,*) 'not enough points (dim r) in kernel mask for the requested polynomial_order: mm = ',mm,': nn = ',nn
    write(83,*) 'minimum_separation,minimum_separation_original,separation_index(minimum_separation)'
    write(83,*) minimum_separation,minimum_separation_original,separation_index(minimum_separation)
  else if (minimum_separation /= minimum_separation_original) then
    write(83,*) 'minimum_separation has been increased in '// &
      'construct_polynomial_basis_tensor from ',minimum_separation_original,' to ',minimum_separation
  end if
end if
! also check that there are enough nodes in the full maximum_separation mask
if (separation_index(minimum_separation) < mm) then
  error = .true.
  return
end if

if (debug) then
  write(83,*) 'mm,nn,separation_index(minimum_separation),local_polynomial_order'
  write(83,*) mm,nn,separation_index(minimum_separation),local_polynomial_order
end if

if (debug) then
  write(83,*) 'j:r'
  do j = 1, ubound(r,1)
    write(83,'(i2,100(a,g9.2))') j,':',(r(j,i),' ',i=1,ubound(r,2))
  end do
end if

! allocate pp if it doesn't have the correct size already
if (allocated(pp)) then
  if (ubound(pp,1) /= mm.or.ubound(pp,2) /= nn) deallocate(pp)
end if
if (.not.allocated(pp)) allocate(pp(mm,nn))

! form pp
do n = 1, nn
  pp(:,n) = polynomial_basis_vector(rr=r(:,n),local_polynomial_order=local_polynomial_order)
end do

if (debug) then
  write(83,*) 'j:pp'
  do j = 1, ubound(pp,1)
    write(83,'(i2,100(a,g9.2))') j,':',(pp(j,i),' ',i=1,ubound(pp,2))
  end do
end if

if (debug) write(83,'(a/80(1h-))') 'subroutine construct_polynomial_basis_tensor'

end subroutine construct_polynomial_basis_tensor

!-----------------------------------------------------------------

subroutine optimisation_kernel(centring,ijk,l_kernel,l_coor,rr,norm,pp,kernel,minimum_separation, &
  separation_array,separation_index,error,hyperbolic_kernel_local)

! here we use an optimisation technique to calculate kernels which have a polynomial basis

use general_module
use lapack_module
use numerical_recipes_module
character(len=*), intent(in) :: centring ! face|cell|node|none that the kernel is associated with (not the location of the data)
integer, intent(in) :: ijk ! index of element
integer, intent(in) :: l_kernel ! (directional) number of the kernel
integer, intent(in) :: l_coor ! coordinate direction of derivative if this will be a derivative kernel
double precision, dimension(:,:), allocatable :: rr ! array of surrounding points relative to kernel centre
double precision, dimension(:), optional :: norm ! normal for first order direction
double precision, dimension(:,:), allocatable :: pp ! polynomial basis tensor, arranged as columns corresponding to each point in the kernel
double precision, dimension(:), allocatable :: kernel ! kernel to calculate
integer :: minimum_separation ! largest separation that will be considered in this kernel
integer, dimension(:), allocatable :: separation_array
integer, dimension(:), allocatable :: separation_index
double precision, dimension(:), allocatable :: weight ! weighting function
integer, dimension(:), allocatable :: weight_importance ! weighting function importance (not used here)
integer :: nn, mm, d, i, j, n_kernel_steps
double precision :: llnorm, l, llnorm_initial
double precision, dimension(:), allocatable :: y
double precision, dimension(:,:), allocatable :: ll, lll
integer, dimension(:), allocatable :: active
logical :: error, active_change, linear_error, hyperbolic_kernel_local
character(len=10) :: phase ! tells the constraints algorithm what phase we are in
!double precision, parameter :: llnorm_tol = 1.d-8 ! tolerance indicating linear solver has been successful
! relaxed for v0.42
!double precision, parameter :: llnorm_tol = 1.d-6 ! tolerance indicating linear solver has been successful
! and increased again due to normalisation, v0.42
double precision, parameter :: llnorm_tol = 1.d-20 ! relative tolerance indicating linear solver has been successful
integer, parameter :: maximum_constraint_steps = 100
logical, parameter :: numrec = .false.
logical, parameter :: debug = .false.

error = .false.

if (debug) write(83,'(80(1h+)/a)') 'subroutine optimisation_kernel'

d = l_coor + 1

! find d, which is the element within the polynomial basis that this kernel refers to
if (present(norm)) then
! a present norm indicates that we are calculating a derivative in the direction corresponding to d
!  d = maxloc(norm,dim=1) + 1 ! now this is based on l_coor
! check that the norm direction is in one of the coordinate directions
  if (abs(norm(d-1)-1.d0) > 1.d-10) call error_stop('the optimisation kernel routine cannot (yet) calculate '// &
    'derivatives that are not in the coordinate direction.  You could try: a) orientating your 1D or 2D mesh to be coincident '// &
    'with the coordinates; b) turn on cell_from_face_kernels (in kernel_module.f90); '// &
    'or c) generalise the optimisation routine and fix construct_orthogonal_basis error checks!')
!else
!  d = 1
end if
! now find some array dimensions
mm = ubound(pp,1) ! this is the number of elements in each polynomial basis vector, which is the number of rows in the pp tensor
if (d > mm) call error_stop('norm in optimisation_kernel inconsistent with polynomial basis')
nn = ubound(pp,2) ! this corresponds to the number of points in the kernel, which is the number of columns in the pp tensor
if (nn /= ubound(kernel,1)) call error_stop('the dimensions of the pp tensor and kernel vector do not match in optimisation_kernel')

! calculate the weights
if (trim(centring) == 'face') then
  call calc_weight(centring,ijk,l_coor,rr,minimum_separation,face(ijk)%kernel(l_kernel)%ijk,separation_array, &
    separation_index,weight,weight_importance,hyperbolic_kernel_local)
else if (trim(centring) == 'cell') then
  call calc_weight(centring,ijk,l_coor,rr,minimum_separation,cell(ijk)%kernel(l_kernel)%ijk,separation_array, &
    separation_index,weight,weight_importance,hyperbolic_kernel_local)
else
  call calc_weight(centring,ijk,l_coor,rr,minimum_separation,node(ijk)%kernel(l_kernel)%ijk,separation_array, &
    separation_index,weight,weight_importance,hyperbolic_kernel_local)
end if

if (debug) then
  write(83,*) 'd = ',d,': mm = ',mm,': nn = ',nn
  write(83,'(a)') 'weight = '
  write(83,'(100(f8.2,a))') (weight(i),' ',i=1,ubound(weight,1))
  write(83,'(a)') 'weight_importance = '
  write(83,'(100(i8,a))') (weight_importance(i),' ',i=1,nn)
  write(83,'(a)') 'pp = '
  do j = 1, ubound(pp,1)
    write(83,'(100(f8.2,a))') (pp(j,i),' ',i=1,ubound(pp,2))
  end do
end if

! allocate arrays for the optimisation procedure
allocate(y(2*nn+mm))
allocate(ll(ubound(y,1),1))
allocate(lll(ubound(y,1),ubound(y,1)))
allocate(active(ubound(y,1)))

y = 1.d0 ! this sets all kernels and all lagrangian functions to 1

phase = 'first'
linear_error = .false.
call optimisation_kernel_constraints(pp,y,separation_array,minimum_separation,d,nn,mm,active,active_change, &
  phase,linear_error,error)

if (error) call error_stop('unresolvable error during '//trim(phase)//' phase of optimisation_kernel_constraints')

call optimisation_kernel_update(pp,y,weight,weight_importance,d,nn,mm,active,ll(:,1),lll,llnorm,l)

if (debug) write(83,'(2(a,g13.6))') 'INFO: entering constraint loop: initial llnorm = ',llnorm
! save llnorm
llnorm_initial = llnorm
n_kernel_steps = 0

constraint_loop: do

  n_kernel_steps = n_kernel_steps + 1

  lll = -lll

  if (numrec) then
    call numerical_recipes_linear_solver(a=lll,b=ll,error=linear_error)
  else
    call lapack_linear_solver(a=lll,b=ll,error=linear_error)
  end if

  if (debug.and.linear_error) write(83,*) 'linear error coming out of linear solver routine'

! check on validity of ll elements
  if (.not.linear_error) then
    do j = 1, ubound(ll,1)
      if (.not.number_is_valid(ll(j,1))) then
        linear_error = .true.
        exit
      end if
    end do
    if (debug.and.linear_error) write(83,*) 'linear error from NaN'
  end if

! also check on size of ll kernel elements
  if (.not.linear_error) then
    if (maxval(abs(ll(1:nn,1))) > 1.d+20) linear_error = .true.
    if (debug.and.linear_error) write(83,*) 'linear error from size'
  end if

! if all OK then update y and check on llnorm
  if (.not.linear_error) then
    y = y + ll(:,1)
    call optimisation_kernel_update(pp,y,weight,weight_importance,d,nn,mm,active,ll(:,1),lll,llnorm,l)
    if (debug) write(83,'(2(a,g12.5))') 'linear error llnorm = ',llnorm,': normalised llnorm = ',llnorm/max(llnorm_initial,1.d0)
! temp &&&
!   write(84,'(2(a,g12.5))') 'linear error llnorm = ',llnorm,': normalised llnorm = ',llnorm/max(llnorm_initial,1.d0)
!   if (llnorm > llnorm_tol) linear_error = .true.
! changed to relative error for v0.42
    if (llnorm/max(llnorm_initial,1.d0) > llnorm_tol) linear_error = .true.
    if (debug.and.linear_error) write(83,*) 'linear error from large llnorm = ',llnorm,': llnorm_initial = ',llnorm_initial
  end if

! run check on differential kernels
  if (.false.) then
    call optimisation_kernel_check(pp,y,weight,weight_importance,d,nn,mm,active)
  end if

  call optimisation_kernel_constraints(pp,y,separation_array,minimum_separation,d,nn,mm, &
    active,active_change,phase,linear_error,error)

  if (error) call error_stop('unresolvable error during '//trim(phase)//' phase of optimisation_kernel_constraints: '// &
    'Most likely this is a symptom of this particular kernel mask not including enough elements.  If checkminw is on and '// &
    'minimumminw is set to a reasonable number, most likely you already have a warning message about this particular kernel '// &
    'mask that suggests possible solutions.  As a first resort you can try increasing the minimumseparation, '// &
    'maximumseparation and/or ensuring that limitkernelmasktosharednodes is .false..  As a second resort '// &
    'set debug=.true. in kernel_module.f90 to produce a more detailed debug file.')

  if (.not.active_change) exit
  if (n_kernel_steps > maximum_constraint_steps) &
    call error_stop('constraint loop is out of control in optimisation_kernel_constraints')

  call optimisation_kernel_update(pp,y,weight,weight_importance,d,nn,mm,active,ll(:,1),lll,llnorm,l)

end do constraint_loop

if (debug) write(83,*) ' CONSTRAINT LOOP CONVERGED'

kernel = y(1:nn)

deallocate(y,ll,lll,active,weight,weight_importance)

if (debug) write(83,'(a/80(1h-))') 'subroutine optimisation_kernel'

end subroutine optimisation_kernel

!-----------------------------------------------------------------

subroutine optimisation_kernel_update(pp,y,weight,weight_importance,d,nn,mm,active,ll,lll,llnorm,l)

! here we calculate the f vector and lll tensor from the latest y vect
! this is all for solving the optimisation kernel problem

use general_module
integer :: d, nn, mm, i, j
integer, dimension(:), allocatable :: active, weight_importance
double precision, dimension(:) :: ll
double precision, dimension(:), allocatable :: y, weight
double precision, dimension(:,:), allocatable :: pp, lll
double precision :: llnorm, l, pp_sign, weight_factor
logical :: debug = .false.

if (debug) write(83,'(80(1h+)/a)') 'subroutine optimisation_kernel_update'

l = 0.d0
ll = 0.d0
lll = 0.d0

!----------------
! this stuff due to weight maximisation
do i = 1, nn

! rr = vector_magnitude(pp(min(2,mm):min(4,mm),i))
! weight_factor = rr/weight(i)**2
! weight_factor = rr/weight(i) ! this is the one
! weight_factor = rr/(weight(i)*radial_kernel(rr,0))
! weight_factor = 1.d0/weight(i)
! weight_factor = rr
! weight_factor = 1.d0
! weight_factor = 1.d0/weight(i) - 1.d0/max(maxval(weight),0.5d0)
! weight_factor = 1.d0/weight(i) - 0.99d0/maxval(weight)
! weight_factor = weight_factor**2
    
! weight_factor = 1.d0
! if (weight_importance(i) == 0) weight_factor = 1.d-4
! weight_factor = 10.d0**weight_importance(i)
! was using this one
!  weight_factor = 1.d0/(weight_separation_multiplier**weight_importance(i))
! weight_factor = 1.d0/(max(abs(weight(i)),1.d-2))**2
!   rr = vector_magnitude(pp(min(2,mm):min(4,mm),i))
!   weight_factor = 1.d0/(weight(i)**2)/(optimisation_rigidity**weight_importance(i))

! this was the standard, 041213
!  weight_factor = 1.d0/(weight_separation_multiplier**weight_importance(i))

  if (hyperbolic_kernel) then
    weight_factor = 1.d0/(weight(i)**2)
    l = l + y(i)**2*weight_factor
    ll(i) = ll(i) + 2*y(i)*weight_factor
    lll(i,i) = lll(i,i) + 2*weight_factor
  else if (radial_kernel_weighting) then
    weight_factor = 1.d0/(weight_separation_multiplier**weight_importance(i))
    l = l + (y(i)-weight(i))**2*weight_factor
    ll(i) = ll(i) + 2*(y(i)-weight(i))*weight_factor
    lll(i,i) = lll(i,i) + 2*weight_factor
  else
    weight_factor = 1.d0/(weight_separation_multiplier**weight_importance(i))
    l = l + (y(i)*pp(d,i)-weight(i))**2*weight_factor
    ll(i) = ll(i) + 2*(y(i)*pp(d,i)-weight(i))*pp(d,i)*weight_factor
    lll(i,i) = lll(i,i) + 2*pp(d,i)**2*weight_factor
!   l = l + (y(i)*pp(d,i))**2*weight_factor
!   ll(i) = ll(i) + 2*(y(i)*pp(d,i))*pp(d,i)*weight_factor
!   lll(i,i) = lll(i,i) + 2*pp(d,i)**2*weight_factor
  end if

! if (debug) then
!   write(83,'(a)') '*********************'
!   write(83,*) 'rr = ',rr
!   write(83,*) '1.d0/radial_kernel(rr,0)**2 = ',1.d0/radial_kernel(rr,0)**2
!   write(83,*) 'weight(i) = ',weight(i)
!   write(83,*) 'weight_factor = ',weight_factor
! end if

end do

!----------------
! this stuff due to the polynomial conditions
do j = 1, nn
  do i = nn+1, nn+mm
    ll(j) = ll(j) + y(i)*pp(i-nn,j)
    lll(j,i) = lll(j,i) + pp(i-nn,j)
    lll(i,j) = lll(i,j) + pp(i-nn,j)
  end do
end do

do i = nn+1, nn+mm
  do j = 1, nn
    l = l + y(i)*y(j)*pp(i-nn,j)
    ll(i) = ll(i) + y(j)*pp(i-nn,j)
  end do
  if (i-nn == d) then
    l = l - y(i)
    ll(i) = ll(i) - 1.d0
  end if
end do

!----------------
! this stuff due to the positivity conditions
do i = 1, nn
  if (active(i) == 1) then
!   pp_sign = sign(1.d0,pp(d,i))
    pp_sign = sign(max(abs(pp(d,i)),small_pp),pp(d,i)) ! this gives something that has the same sign as pp(d,i), but a magnitude that is >= small_pp
    l = l - y(i+mm+nn)*y(i)*pp_sign
    ll(i) = ll(i) - y(i+mm+nn)*pp_sign
    ll(i+nn+mm) = ll(i+nn+mm) - y(i)*pp_sign
    lll(i,i+mm+nn) = lll(i,i+mm+nn) - pp_sign
    lll(i+mm+nn,i) = lll(i+mm+nn,i) - pp_sign
  else
    ll(i+nn+mm) = ll(i+nn+mm) + y(i+nn+mm) - 1.d0
    lll(i+nn+mm,i+nn+mm) = lll(i+nn+mm,i+nn+mm) + 1.d0
  end if
end do

!----------------
  
llnorm = 0.d0
do j = 1, ubound(ll,1)
  llnorm = llnorm + ll(j)**2
end do
! normalise llnorm by maximum element size in ll now
llnorm = llnorm/maxval(abs(y))

if (debug) then
  write(83,*) 'j:active:k/e/i:k*pp/-/-:ll:lll'
  do j = 1, nn
    write(83,'(i2,100(a,g9.2))') j,':-:k',y(j),':',y(j)*pp(d,j),':',ll(j),':',(lll(j,i),' ',i=1,ubound(ll,1))
  end do
  do j = nn+1, nn+mm
    write(83,'(i2,100(a,g9.2))') j,':-:e',y(j),':---------:',ll(j),':',(lll(j,i),' ',i=1,ubound(ll,1))
  end do
  do j = mm+nn+1, mm+2*nn
    write(83,'(i2,a,i1,99(a,g9.2))') j,':',active(j-mm-nn),':i',y(j),':---------:',ll(j),':',(lll(j,i),' ',i=1,ubound(ll,1))
  end do
  write(83,*) 'llnorm = ',llnorm
  write(83,*) 'l = ',l
end if

if (debug) write(83,'(a/80(1h-))') 'subroutine optimisation_kernel_update'

end subroutine optimisation_kernel_update

!-----------------------------------------------------------------

subroutine optimisation_kernel_check(pp,y,weight,weight_importance,d,nn,mm,active)

! here we check the analytical optimisaion kernel derivatives against differences

use general_module
integer :: d, nn, mm, i, j
double precision, dimension(:), allocatable :: ll, ll_difference, ll_u, ll_d
double precision, dimension(:), allocatable :: y, weight, y_difference
double precision, dimension(:,:), allocatable :: pp, lll, lll_difference
integer, dimension(:), allocatable :: active, weight_importance
double precision :: llnorm, l, l_u, l_d, ll_error, lll_error
integer :: jj
logical :: debug = .false.
double precision :: y_eps = 1.d-6 ! small difference increment to y

if (debug) write(83,'(80(1h+)/a)') 'subroutine optimisation_kernel_check'

jj = 2*nn+mm
allocate(ll(jj),lll(jj,jj))
allocate(y_difference(jj),ll_difference(jj),lll_difference(jj,jj))
allocate(ll_u(jj),ll_d(jj))

! calculate difference approximations
do j = 1, jj
  y_difference = y
  y_difference(j) = y_difference(j) + y_eps
  call optimisation_kernel_update(pp,y_difference,weight,weight_importance,d,nn,mm,active,ll_u,lll, &
    llnorm,l_u)
  y_difference(j) = y_difference(j) - 2.d0*y_eps
  call optimisation_kernel_update(pp,y_difference,weight,weight_importance,d,nn,mm,active,ll_d,lll, &
    llnorm,l_d)
  ll_difference(j) = (l_u-l_d)/(2.d0*y_eps)
  do i = 1, jj
    lll_difference(i,j) = (ll_u(i)-ll_d(i))/(2.d0*y_eps)
  end do
end do

! calculate differential result
call optimisation_kernel_update(pp,y,weight,weight_importance,d,nn,mm,active,ll,lll,llnorm,l)

if (debug) then
  write(83,*) 'j:y:ll:lll: differential result'
  do j = 1, jj
    write(83,'(i2,100(a,f8.2))') j,':',y(j),':',ll(j),':',(lll(j,i),' ',i=1,jj)
  end do
  write(83,*) 'j:y:ll:lll: difference result'
  do j = 1, jj
    write(83,'(i2,100(a,f8.2))') j,':',y(j),':',ll_difference(j),':',(lll_difference(j,i),' ',i=1,jj)
  end do
  write(83,*) 'j:y:ll:lll: error'
  do j = 1, jj
    write(83,'(i2,100(a,f8.2))') j,':',y(j),':',ll(j)-ll_difference(j),':',(lll(j,i)-lll_difference(j,i),' ',i=1,jj)
  end do
  ll_error = 0.d0
  lll_error = 0.d0
  do j = 1, jj
    ll_error = ll_error + abs(ll(j)-ll_difference(j))
    do i = 1, jj
      lll_error = lll_error + abs(lll(j,i)-lll_difference(j,i))
    end do
  end do
  write(83,*) 'll_error = ',ll_error
  write(83,*) 'lll_error = ',lll_error
end if

deallocate(ll,lll,ll_difference,lll_difference,ll_u,ll_d)

if (debug) write(83,'(a/80(1h-))') 'subroutine optimisation_kernel_check'

end subroutine optimisation_kernel_check

!-----------------------------------------------------------------

subroutine optimisation_kernel_constraints(pp,y,separation_array,minimum_separation,d,nn,mm,active, &
  active_change,phase,linear_error,error)

! here we calculate set any constraints on the kernel variables

use general_module
integer :: d, nn, mm, i, nfree, minimum_separation, answer, i_min, i_negative
integer, dimension(:), allocatable :: active
integer, dimension(:), allocatable, save :: active_last, active_best
double precision, dimension(:), allocatable :: y
double precision, dimension(:), allocatable, save :: y_last, y_best
double precision, dimension(:,:), allocatable :: pp
integer, dimension(:), allocatable :: separation_array
double precision :: lambda_index, lambda_index_min, negative_index, &
  local_negative_index, maximum_negative_index
double precision, save :: negative_index_last, negative_index_best
integer, save :: i_negative_last, minimum_kernel_size, i_negative_best
logical :: active_change ! indicates whether any changes have been made to the constraints
character(len=10) :: phase ! first|second|expand|contract|finish : this is the phase of development we are in
logical :: linear_error ! error logical from the linear system solution
logical :: error
logical :: debug = .false.

if (debug) write(83,'(80(1h+)/a)') 'subroutine optimisation_kernel_constraints'

active_change = .false.
error = .false.

if (debug) then
  write(83,*) 'entering:'
  write(83,*) 'phase = ',phase
  write(83,*) 'linear_error = ',linear_error
  write(83,*) 'negative_index_last = ',negative_index_last
  write(83,*) 'i_negative_last = ',i_negative_last
  write(83,*) 'minimum_kernel_size = ',minimum_kernel_size
end if

!----------------
! setup constraints

if (trim(phase) == 'first'.or.(trim(phase) == 'second'.and.linear_error)) then

  active_change = .true.
  active = 0 ! initialise all to zero 
  i_negative = -1 ! dummy value
  i_negative_last = -1 ! dummy value
  i_negative_best = -1 ! dummy value
  negative_index = -huge(1.d0) ! dummy value
  negative_index_last = negative_index
  negative_index_best = negative_index

! setup initial constraints
  do i = 1, nn
    if (separation_array(i) > minimum_separation) then
      active(i) = 1 ! start by putting constraints on all elements beyond the minimum_separation
    else if (.not.radial_kernel_weighting.and.abs(pp(d,i)) < small_pp) then
      if (vector_magnitude(pp(min(2,mm):mm,i)) < small_pp) then
        active(i) = 0 ! if polymer basis in this dimension is small and we ontop of the kernel centre then make constraint inactive
      else
        active(i) = 1 ! otherwise make the constraint active to set this kernel element to zero
      end if
    end if
  end do

! now check on the number of free variables: at least as many variables must be free as the minimum_kernel_size
  nfree = nn - sum(active) ! this is the number of kernel elements within the unconstrained kernel

! set the minimum number of kernel elements
  if (trim(phase) == 'first') then
! mm is the smallest possible kernel size that could satisfy the polynomial equations
! nfree is at this stage the number of kernel elements that result from all the applicable minimum_separation elements include in the kernel
! optimise_additional_elements is a factor to increase the kernel size
    minimum_kernel_size = max(mm,nfree+optimise_additional_elements)
  else
! on subsequent calls increase the kernel size based on what was used last time
    minimum_kernel_size = nn - sum(active_last) + 1 ! if there was an error then we need to increase the number of free variables
  end if

! in first instance try to remove some constraints above the minimum_separation
  if (nfree < minimum_kernel_size) then !
    do i = 1, nn
      if (separation_array(i) > minimum_separation.and.(abs(pp(d,i)) >= small_pp.or.radial_kernel_weighting)) then
        active(i) = 0
        nfree = nfree + 1
        if (nfree == minimum_kernel_size) exit
      end if
    end do
  end if

! if we still have a problem then move in from the outer elements setting constaints off irrespective of pp magnitude
  if (nfree < minimum_kernel_size) then
    do i = nn, 1, -1
      if (active(i) == 1) then
        active(i) = 0
        nfree = nfree + 1
        if (nfree == minimum_kernel_size) exit
      end if
    end do
  end if

! so by now if there is still a problem then not enough cells are available and we have an unresolvable problem
  if (nfree < minimum_kernel_size) then
    if (debug) write(83,*) 'ran out of elements in kernel mask without a successful linear solution being obtained: '// &
      'Either you should make the minimum kernel size larger, or something is amiss with the dimensions of the mesh.'
    error = .true.
  end if

  if (.not.error) then
    call copy_integer_array(original=active,copy=active_last)
    call copy_integer_array(original=active,copy=active_best)
    phase = 'second' ! this means that no kernel has been solved yet by the linear solver
  end if

!----------------
!else if (.false..or.d==1) then
else if (.false.) then
! this is a debugging tool for changing the constraints by hand

  write(*,*) 'introduce (+) or remove (-) new constraint?'
  read(*,*) answer
  if (answer > 0) then
    active(answer) = 1
    write(*,*) 'active(',answer,') = ',active(answer)
  else if (answer < 0) then
    active(-answer) = 0
    write(*,*) 'active(',-answer,') = ',active(-answer)
  end if
  active_change = .true.

!----------------
else if (.true..and.optimise_positise) then ! restrained kernel expansion

  if (trim(phase) == 'second') phase = 'expand' ! change phase to signify that one linear solution worked

! calculate latest statistics if a solution was found
  if (.not.linear_error) then
! loop through all free variables seeing if there are any negative pp*k elements, and find the location and value of the largest
    i_negative = 0
    maximum_negative_index = 0.d0
    negative_index = 0.d0
    do i = nn, 1, -1
      local_negative_index = y(i)*pp(d,i)
!     local_negative_index = y(i)*pp(d,i)/weight(i)
      if (active(i) == 0) then
        if (local_negative_index < maximum_negative_index) then ! this means that the element has current a negative pp*k which is largest in magnitude
          i_negative = i
          maximum_negative_index = local_negative_index
        end if
        if (local_negative_index < 0.d0) negative_index = negative_index + local_negative_index
      end if
    end do
  end if

  if (.not.optimise_positise_sum_index) negative_index = maximum_negative_index ! use maximum kernel value rather than sum

  if (linear_error.and.sum(active) > sum(active_last)) then
! this signifies that we have tried to increase the kernel size, and an error has occurred.  There is no way to resolve this, so exit with the currently-best kernel
! set the kernel to the best one we achieved and exit
    active = active_best
    y = y_best
    negative_index = negative_index_best
    i_negative = i_negative_best
    phase = 'finish'

  else if (linear_error.or.(trim(phase) == 'contract'.and.i_negative /= 0)) then
! we are here because there was an error in the linear solution after a contraction operation in the expand phase, or
!  during the contract phase a negative kernel element appeared
! eitherway, we need to rewind to the last saved kernel
    active = active_last
    y = y_last
    i_negative = i_negative_last
    negative_index = negative_index_last
! set minimum_kernel_size to current size to prevent the same step reoccurring
    minimum_kernel_size = nn - sum(active)
    if (trim(phase) == 'contract') phase = 'finish' ! the minimum_kernel_size would also achieve this, but make it explicit

  else if (trim(phase) /= 'contract'.and.optimise_positise_cautiously.and. &
    negative_index < optimise_positise_cautiously_multiplier*negative_index_last) then
! using the optimise_positise_cautiously option we only allow a change if it decreases the negative index
    active = active_best
    y = y_best
    i_negative = i_negative_best
    negative_index = negative_index_best
    phase = 'finish'

  else
! otherwise we are progressing, so save the current kernel
    call copy_integer_array(original=active,copy=active_last)
    call copy_double_precision_array(original=y,copy=y_last)
    i_negative_last = i_negative
    negative_index_last = negative_index
    if (negative_index >= negative_index_best) then
      call copy_integer_array(original=active,copy=active_best)
      call copy_double_precision_array(original=y,copy=y_best)
      i_negative_best = i_negative
      negative_index_best = negative_index
    end if
  end if
    
! now do any changes to decrease the negative index and/or kernel size
  if (trim(phase) /= 'finish') then

    if (nn-sum(active) > minimum_kernel_size.and.i_negative /= 0) then
! there is a negative kernel element and we have space to remove it
      active(i_negative) = 1
      active_change = .true.

    else if (i_negative /= 0) then
! there is still a negative kernel element that we will try to remove by adding negative lambda constrained cells, if possible

      lambda_index_min = 1.d0
      i_min = 0
      do i = 1, nn ! loop through all cells looking for a constrained element that has the largest negative lambda
!       if (separation_array(i) /= separation_array(max(i-1,1)).and.i_min > 0) exit ! we have found a candidate in the current separation level so use this
        if (separation_array(i) /= separation_array(max(i-1,1)).and.i_min > 0.and.separation_array(i) > minimum_separation) exit ! we have found a candidate in the current separation level so use this
        if (active(i) == 1.and.y(nn+mm+i) < 0.d0) then
          if (.not.radial_kernel_weighting.and.abs(pp(d,i)) < small_pp.and.vector_magnitude(pp(min(2,mm):mm,i)) > small_pp) cycle ! do not relax constraint on these elements that have same pp(d,i) as origin
          lambda_index = y(nn+mm+i) ! fiddle with this
!         lambda_index = y(nn+mm+i)/weight(i) ! fiddle with this
          if (lambda_index < lambda_index_min) then
            i_min = i
            lambda_index_min = lambda_index
          end if
        end if
      end do
      
      if (i_min > 0) then
! we have found a variable that could help the situation, so relax this constraint
        active(i_min) = 0
        active_change = .true.
      else
! otherwise no variables are left which could help the situation and there is nothing more that can be done so we never enter the contract phase
! set the kernel to the best one we achieved and exit
        active = active_best
        y = y_best
        negative_index = negative_index_best
        i_negative = i_negative_best
        phase = 'finish' ! not necessary to set this, but...
      end if

    else
! we are now in the contraction phase as i_negative == 0
      phase = 'contract'

    end if

  end if

end if
      
!if (debug.and.(.not.active_change.or.trim(phase)=='finish')) then
if (debug) then
  write(83,*) 'exiting:'
  write(83,*) 'phase = ',phase
  write(83,*) 'negative_index = ',negative_index
  write(83,*) 'negative_index_best = ',negative_index_best
  write(83,*) 'i_negative = ',i_negative
  write(83,*) 'minimum_kernel_size = ',minimum_kernel_size
  write(83,*) 'active_change = ',active_change
  write(83,*) 'i:active:active_last:active_best:sep:k:k*pp:lambda'
  do i = 1, nn
    write(83,'(i2,3(a,i1),a,i2,100(a,f12.2))') i,':',active(i),':',active_last(i),':',active_best(i), &
      ':',separation_array(i),':',y(i),':',y(i)*pp(d,i),':',y(i+mm+nn)
  end do
end if

if (debug) write(83,'(a/80(1h-))') 'subroutine optimisation_kernel_constraints'

end subroutine optimisation_kernel_constraints

!-----------------------------------------------------------------

subroutine expand_kernel_mask_old(iarray,maximum_separation,imask,separation_index,separation_array)

! here we find the kernel mask, and calculate the separation, defined as the number of faces you have to pass through
!  to get to that cell
! iarray is the list of neighbours that could possibly be included in the mask
! the imask array is ordered in increasing separation
! the separation information is stored in array separation_index, which specifies the last index for each separation
! separation_array specifies the separation of each element
! maximum_separation is the maximum separation of cells that should be included in the kernel

use general_module
integer, dimension(:), allocatable :: iarray ! the array of all surrounding cells, passed in as one of the cell or face arrays, not ordered
integer :: maximum_separation ! this is the maximum separation that we will consider
integer, dimension(:), allocatable :: imask ! this is the list of cells that is passed out, with a changed size
integer, dimension(:), allocatable :: separation_index ! this is the list of separation indices is passed out, with a changed size
integer, dimension(:), allocatable :: separation_array ! this the separation indices per element passed out, with a changed size
integer :: separation, i, ii, start_index, end_index, i2, ii2, k, kk, common_nodes
logical, parameter :: debug = .false.

if (debug) write(83,'(80(1h+)/a)') 'subroutine expand_kernel_mask'

separation = ubound(separation_index,1) ! this is the previously highest defined separation

if (ubound(imask,1) /= separation_index(separation)) call error_stop( &
  'imask size does not match with final separation_index in calling arguments to expand_kernel_mask')
if (ubound(separation_array,1) /= separation_index(separation)) call error_stop( &
  'separation_array size does not match with final separation_index in calling arguments to expand_kernel_mask')

if (debug) then
  write(83,*) 'calling parameters: iarray = ',iarray
  write(83,*) 'calling parameters: maximum_separation = ',maximum_separation
  write(83,*) 'calling parameters: imask = ',imask
  write(83,*) 'calling parameters: separation_index = ',separation_index
  write(83,*) 'calling parameters: separation_array = ',separation_array
end if
  
if (separation == 1) then
  end_index = 0
else
  end_index = separation_index(separation-1)
end if

! loop through separations until we reach the requested maximum_separation
separation_loop: do while (separation < maximum_separation.and. &
  (limit_kernel_mask_to_shared_nodes.and.ubound(iarray,1) > ubound(imask,1).or..not.limit_kernel_mask_to_shared_nodes))

! define indices of imask that bound last separation
  start_index = end_index + 1
  end_index = separation_index(separation)
  if (end_index < start_index) call error_stop('indices out of order in expand_kernel_masks')

  separation = separation + 1
  call resize_integer_array(keep_data=.true.,array=separation_index,change=1)
  separation_index(separation) = separation_index(separation-1)

  if (debug) then
    write(83,'(3(a,i3))') 'separation = ',separation,': start_index = ',start_index,': end_index = ',end_index
    write(83,*) 'previous separation (separation = ',separation-1,') list = ',imask(start_index:end_index)
  end if

! loop through all cells which have the last separation looking for ones that have no separation and which are adjacent
  last_separation_loop: do ii = start_index, end_index
    i = imask(ii)

! loop through neighbours of this cell that share a face element
    neighbour_loop: do ii2 = 2, ubound(cell(i)%jface,1)+1
      i2 = cell(i)%icell(ii2)
      if (location_in_list(array=imask,element=i2) /= 0) cycle neighbour_loop ! cell is already in list
      if (limit_kernel_mask_to_shared_nodes.and.location_in_list(array=iarray,element=i2) == 0) cycle neighbour_loop ! cell is not in overall iarray list
! if we are here then cell i2 has the current separation
      if (debug) write(83,*) 'found new imask element = ',i2,' having separation = ',separation
      call push_integer_array(array=imask,new_element=i2)
      separation_index(separation) = ubound(imask,1)
    end do neighbour_loop

! include boundary cells that share a node with other boundary cells
    if (boundary_node_separations.and.cell(i)%type == 2) then
      boundary_neighbour_loop: do ii2 = ubound(cell(i)%jface,1)+2, ubound(cell(i)%icell,1)
        i2 = cell(i)%icell(ii2)
        if (location_in_list(array=imask,element=i2) /= 0) cycle boundary_neighbour_loop ! cell is already in list
        if (limit_kernel_mask_to_shared_nodes.and.location_in_list(array=iarray,element=i2) == 0) cycle boundary_neighbour_loop ! cell is not in overall iarray list
        if (.not.cell(i2)%type == 2) cycle boundary_neighbour_loop ! cell is also a boundary cell
! if the boundary cell has a dimension of 2, we need the cells to share atleast two nodes to be considered adjacent
        if (cell(i)%dimensions == 2) then
          common_nodes = 0
          do kk = 1, ubound(cell(i)%knode,1)
            k = cell(i)%knode(kk)
            if (location_in_list(array=cell(i2)%knode,element=k) /= 0) common_nodes = common_nodes + 1
            if (common_nodes == 2) exit
          end do
          if (common_nodes < 2) cycle boundary_neighbour_loop
        end if
! if we are here then cell i2 has the current separation and is an adjacect boundary cell to a boundary cell
        if (debug) write(83,*) 'found new boundary to boundary imask element = ',i2,' having separation = ',separation
        call push_integer_array(array=imask,new_element=i2)
        separation_index(separation) = ubound(imask,1)
      end do boundary_neighbour_loop
    end if

  end do last_separation_loop

! as cells are separated by common faces, if no cells are found at a given separation level then this is an error
  if (separation_index(separation) == separation_index(separation-1)) call error_stop( &
    'no elements are found at a separation level in expand_kernel_mask, indicating a mesh problem of some sort')
  
  if (debug) write(83,*) 'calculated separation (separation = ',separation,') list = ', &
    imask(separation_index(separation-1)+1:separation_index(separation))
  
end do separation_loop

! update separation_array
! TODO: could keep original data here but not much worth
! TODO: could put it in the main loop but would have to think about array size allocation - probably not worth it efficiency wise
call resize_integer_array(keep_data=.false.,array=separation_array,new_size=separation_index(ubound(separation_index,1)))
end_index = 0
do separation = 1, ubound(separation_index,1)
  start_index = end_index + 1
  end_index = separation_index(separation)
  separation_array(start_index:end_index) = separation
end do

if (debug) then
  write(83,*) 'final parameters: imask = ',imask
  write(83,*) 'final parameters: separation_index = ',separation_index
  write(83,*) 'final parameters: maximum_separation = ',maximum_separation
  write(83,*) 'final parameters: ubound(iarray,1) = ',ubound(iarray,1)
  write(83,*) 'final parameters: ubound(imask,1) = ',ubound(imask,1)
  write(83,*) 'final parameters: separation_array = ',separation_array
end if
  
if (debug) write(83,'(a/80(1h-))') 'subroutine expand_kernel_mask'

end subroutine expand_kernel_mask_old

!-----------------------------------------------------------------

subroutine process_kernel_options

! run through requested kernel options, including user set ones (from input files)
! NB, as a general rule, options do not have underscores in them
! ref: kernel options

use general_module
integer :: n, max_polynomial_order
character(len=100) :: option_name
logical :: error

!do n = allocatable_character_size(kernel_options), 1, -1
do n = 1, allocatable_character_size(kernel_options) ! precedence is now as read, that is, options will be applied in the order that they are written
  option_name = extract_option_name(kernel_options(n),error)
  if (error) then
    write(*,'(a)') "WARNING: could not determine what the desired kernel option is from the following: "//trim(kernel_options(n))
  else if (trim(option_name) == "kernelmethod") then
! kernel_method
    kernel_method = trim(extract_option_string(kernel_options(n),error))
    if (error) then
      call error_stop("could not determine the required kernelmethod from the kernel option "//trim(kernel_options(n)))
    else if (trim(kernel_method) == 'mls' .or. trim(kernel_method) == 'optimisation' .or. trim(kernel_method) == 'simple' .or. trim(kernel_method) == 'none') then
      write(*,'(a)') 'INFO: setting kernelmethod = '//trim(kernel_method)
    else
      call error_stop("requested kernelmethod "//trim(kernel_method)//" is not valid")
    end if
  else if (trim(option_name) == "polynomialorder") then
! polynomial_order
    polynomial_order = extract_option_integer(kernel_options(n),error)
    if (error) then
      call error_stop("could not determine the required polynomialorder from the kernel option "//trim(kernel_options(n)))
    else if (polynomial_order > 3 .or. polynomial_order < 1) then
      call error_stop("requested kernel polynomialorder outside of allowable range 1 -> 3")
    end if
    write(*,'(a,i1)') 'INFO: setting kernel polynomialorder = ',polynomial_order
  else if (trim(option_name) == "polynomialaverageorder") then
! polynomial_average_order
    polynomial_average_order = extract_option_integer(kernel_options(n),error)
    if (error) then
      call error_stop("could not determine the required polynomialaverageorder from the kernel option "//trim(kernel_options(n)))
    else if (polynomial_average_order > 3 .or. polynomial_average_order < 1) then
      call error_stop("requested kernel polynomialaverageorder outside of allowable range 1 -> 3")
    end if
    write(*,'(a,i1)') 'INFO: setting kernel polynomialaverageorder = ',polynomial_average_order
  else if (trim(option_name) == "polynomialcellorder") then
! polynomial_cell_order
    polynomial_cell_order = extract_option_integer(kernel_options(n),error)
    if (error) then
      call error_stop("could not determine the required polynomialcellorder from the kernel option "//trim(kernel_options(n)))
    else if (polynomial_cell_order > 3 .or. polynomial_cell_order < 1) then
      call error_stop("requested kernel polynomialcellorder outside of allowable range 1 -> 3")
    end if
    write(*,'(a,i1)') 'INFO: setting kernel polynomialcellorder = ',polynomial_cell_order
  else if (trim(option_name) == "polynomialnodeorder") then
! polynomial_node_order
    polynomial_node_order = extract_option_integer(kernel_options(n),error)
    if (error) then
      call error_stop("could not determine the required polynomialnodeorder from the kernel option "//trim(kernel_options(n)))
    else if (polynomial_node_order > 3 .or. polynomial_node_order < 1) then
      call error_stop("requested kernel polynomialnodeorder outside of allowable range 1 -> 3")
    end if
    write(*,'(a,i1)') 'INFO: setting kernel polynomialnodeorder = ',polynomial_node_order
  else if (trim(option_name) == "minimumseparation") then
! minimum_separation
    minimum_domain_separation = extract_option_integer(kernel_options(n),error)
    if (error) then
      call error_stop("could not determine the required kernel minimumseparation from the kernel option "//trim(kernel_options(n)))
    else if (minimum_domain_separation < 1) then
      call error_stop("requested kernel minimumseparation should be greater than zero")
    end if
    write(*,'(a,i1)') 'INFO: setting kernel minimumseparation = ',minimum_domain_separation
    minimum_boundary_separation = minimum_domain_separation
  else if (trim(option_name) == "maximumseparation") then
! maximum_separation
    maximum_domain_separation = extract_option_integer(kernel_options(n),error)
    if (error) then
      call error_stop("could not determine the required kernel maximumseparation from the kernel option "//trim(kernel_options(n)))
    else if (maximum_domain_separation < 1) then
      call error_stop("requested kernel maximumseparation should be greater than zero")
    end if
    write(*,'(a,i1)') 'INFO: setting kernel maximumseparation = ',maximum_domain_separation
    maximum_boundary_separation = maximum_domain_separation
  else if (trim(option_name) == "maximumcellseparation") then
! maximum_cell_separation
    maximum_cell_domain_separation = extract_option_integer(kernel_options(n),error)
    if (error) then
      call error_stop("could not determine the required kernel maximumcellseparation from the kernel option "// &
      trim(kernel_options(n)))
    else if (maximum_cell_domain_separation < 1) then
      call error_stop("requested kernel maximumcellseparation should be greater than zero")
    end if
    write(*,'(a,i1)') 'INFO: setting kernel maximumcellseparation = ',maximum_cell_domain_separation
    maximum_cell_boundary_separation = maximum_cell_domain_separation
  else if (trim(option_name) == "limitkernelmasktosharednodes") then
! limit_kernel_mask_to_shared_nodes
    limit_kernel_mask_to_shared_nodes = extract_option_logical(kernel_options(n),error)
    if (error) call error_stop("could not determine the limitkernelmasktosharednodes from the kernel option "// &
      trim(kernel_options(n)))
    write(*,'(a,l1)') 'INFO: setting kernel limitkernelmasktosharednodes = ',limit_kernel_mask_to_shared_nodes
  else if (trim(option_name) == "boundarynodeseparations") then
! boundary_node_separations
    boundary_node_separations = extract_option_logical(kernel_options(n),error)
    if (error) call error_stop("could not determine the boundarynodeseparations from the kernel option "// &
      trim(kernel_options(n)))
    write(*,'(a,l1)') 'INFO: setting kernel boundarynodeseparations = ',boundary_node_separations
  else if (trim(option_name) == "automaximumseparation") then
! auto_maximum_separation
    auto_maximum_separation = extract_option_logical(kernel_options(n),error)
    if (error) call error_stop("could not determine the automaximumseparation from the kernel option "//trim(kernel_options(n)))
    write(*,'(a,l1)') 'INFO: setting kernel automaximumseparation = ',auto_maximum_separation
  else if (trim(option_name) == "checkminw") then
! check_minw
    check_minw = extract_option_logical(kernel_options(n),error)
    if (error) call error_stop("could not determine the checkminw from the kernel option "//trim(kernel_options(n)))
    write(*,'(a,l1)') 'INFO: setting kernel checkminw = ',check_minw
  else if (trim(option_name) == "minimumminw") then
! minimum_minw
    minimum_minw = extract_option_double_precision(kernel_options(n),error)
    if (error) call error_stop("could not determine the minimumminw from the kernel option "//trim(kernel_options(n)))
    if (minimum_minw < 0.d0) call error_stop("attempting to make minimumminw too small")
    write(*,'(a,g10.3)') 'INFO: setting kernel minimumminw = ',minimum_minw
  else if (trim(option_name) == "weightseparationmultiplier") then
! weight_separation_multiplier
    weight_separation_multiplier = extract_option_double_precision(kernel_options(n),error)
    if (error) call error_stop("could not determine the weightseparationmultiplier from the kernel option "// &
      trim(kernel_options(n)))
    if (weight_separation_multiplier <= 0.d0) call error_stop("attempting to make weightseparationmultiplier too small")
    if (weight_separation_multiplier > 1.d0) call error_stop("attempting to make weightseparationmultiplier too large")
    write(*,'(a,g10.3)') 'INFO: setting kernel weightseparationmultiplier = ',weight_separation_multiplier
  else if (trim(option_name) == "hyperbolicb") then
! hyperbolic_b
    hyperbolic_b = extract_option_double_precision(kernel_options(n),error)
    if (error) call error_stop("could not determine the hyperbolicb from the kernel option "// &
      trim(kernel_options(n)))
    if (hyperbolic_b <= 0.d0) call error_stop("attempting to make hyperbolicb too small")
    write(*,'(a,g10.3)') 'INFO: setting kernel hyperbolicb = ',hyperbolic_b
  else if (trim(option_name) == "shifthyperbolicdistance") then
! shift_hyperbolic_distance
    shift_hyperbolic_distance = extract_option_double_precision(kernel_options(n),error)
    if (error) call error_stop("could not determine the shifthyperbolicdistance from the kernel option "// &
      trim(kernel_options(n)))
    write(*,'(a,g10.3)') 'INFO: setting kernel shifthyperbolicdistance = ',shift_hyperbolic_distance
  else if (trim(option_name) == "separationmultipliedtrialkernels") then
! separation_multiplied_trial_kernels
    separation_multiplied_trial_kernels = extract_option_logical(kernel_options(n),error)
    if (error) call error_stop("could not determine the separationmultipliedtrialkernels from the kernel option "// &
      trim(kernel_options(n)))
    write(*,'(a,l1)') 'INFO: setting kernel separationmultipliedtrialkernels = ',separation_multiplied_trial_kernels
  else if (trim(option_name) == "hyperbolickernel") then
! hyperbolic_kernel
    hyperbolic_kernel = extract_option_logical(kernel_options(n),error)
    if (error) call error_stop("could not determine the hyperbolickernel from the kernel option "//trim(kernel_options(n)))
    write(*,'(a,l1)') 'INFO: setting kernel hyperbolickernel = ',hyperbolic_kernel
  else if (trim(option_name) == "partialhyperbolickernel") then
! hyperbolic_kernel
    partial_hyperbolic_kernel = extract_option_logical(kernel_options(n),error)
    if (error) call error_stop("could not determine the partialhyperbolickernel from the kernel option "//trim(kernel_options(n)))
    write(*,'(a,l1)') 'INFO: setting kernel partialhyperbolickernel = ',partial_hyperbolic_kernel
  else if (trim(option_name) == "shiftboundaryweightcentre") then
! shift_boundary_weight_centre
    shift_boundary_weight_centre = extract_option_logical(kernel_options(n),error)
    if (error) call error_stop("could not determine the shiftboundaryweightcentre from the kernel option "//trim(kernel_options(n)))
    write(*,'(a,l1)') 'INFO: setting kernel shiftboundaryweightcentre = ',shift_boundary_weight_centre
  else if (trim(option_name) == "zerononorientedweights") then
! zero_nonoriented_weights
    zero_nonoriented_weights = extract_option_logical(kernel_options(n),error)
    if (error) call error_stop("could not determine the zerononorientedweights from the kernel option "// &
      trim(kernel_options(n)))
    write(*,'(a,l1)') 'INFO: setting kernel zerononorientedweights = ',zero_nonoriented_weights
  else if (trim(option_name) == "averagestabilitycorrections") then
! average_stability_corrections
    average_stability_corrections = extract_option_logical(kernel_options(n),error)
    if (error) call error_stop("could not determine the averagestabilitycorrections from the kernel option "//trim(kernel_options(n)))
    write(*,'(a,l1)') 'INFO: setting kernel averagestabilitycorrections = ',average_stability_corrections
  else if (trim(option_name) == "gradientstabilitycorrections") then
! gradient_stability_corrections
    gradient_stability_corrections = extract_option_logical(kernel_options(n),error)
    if (error) call error_stop("could not determine the gradientstabilitycorrections from the kernel option "//trim(kernel_options(n)))
    write(*,'(a,l1)') 'INFO: setting kernel gradientstabilitycorrections = ',gradient_stability_corrections
  else
!   write(*,'(a)') "WARNING: "//trim(option_name)//" is not a (valid) kernel option that can be set from the input file"
    call error_stop(trim(option_name)//" is not a (valid) kernel option that can be set from the input file")
  end if
end do

!------------------------------------------
! run some checks on selected options and setup automatic maximum_separations if required
if (auto_maximum_separation) then
  max_polynomial_order = max(polynomial_order,polynomial_average_order,polynomial_cell_order,polynomial_node_order)
  maximum_domain_separation = max(max_polynomial_order,1)+3
  if (max_polynomial_order == 1) maximum_domain_separation = 3 ! make this a bit tighter
  maximum_boundary_separation = maximum_domain_separation
  maximum_cell_domain_separation = maximum_domain_separation ! now making these the same as the face ones
  maximum_cell_boundary_separation = maximum_domain_separation
  write(*,'(a,i1,a,i1,a)') 'INFO: setting maximumseparation = ',maximum_domain_separation, &
    ' based on maxpolynomialorder = ',max_polynomial_order, &
    ': Note, this will overwrite any previous maximum_separation specifications'
! trial removal of this minw increase, v0.5
  if (max_polynomial_order > 1.and..false.) then
!   limit_kernel_mask_to_shared_nodes = .false.
!   write(*,'(a)') 'INFO: as maxpolynomialorder > 1, also setting limitkernelmasktosharednodes to false'
    if (minimum_minw < 2.d0) then
      minimum_minw = 2.d0
      write(*,'(a)') 'INFO: as maxpolynomialorder > 1, increasing minimumminw to 2.d0'
    end if
  end if
end if
if (trim(kernel_method) == 'optimisation') then
  if (.not.radial_kernel_weighting) write(*,'(a)') 'WARNING: inadvisable combination of optimisation kernel method without '// &
    'radial_kernel_weighting'
end if

end subroutine process_kernel_options

!-----------------------------------------------------------------

subroutine setup_face_kernels

! ref: face kernels
! setting up face kernels

use general_module
integer :: i, j, ii, l, l2, separation, minimum_separation_before, maximum_separation, minimum_separation, &
  local_polynomial_order, l_coor, sepd, nverror
double precision :: dx_kernel, minw, value, dx1, dx2, maxvalue, verror
logical :: minw_error, hyperbolic_kernel_local, error
double precision, dimension(:,:), allocatable :: r, norm, pp, r_dim, norm_dim
integer, dimension(:), allocatable :: separation_index, separation_array
double precision, dimension(:,:), allocatable :: max_rel_kernel ! maximum kernel value in separation level / maximum kernel value in all separation levels
integer, dimension(:,:), allocatable :: max_rel_ijk
character(len=10000) :: formatline
logical, parameter :: debug = .false.
logical :: debug_sparse = .false.

if (debug) debug_sparse = .true.

if (debug_sparse) write(*,'(80(1h+)/a)') 'subroutine setup_face_kernels'

verror = 0.d0 ! this is the cummulative stability corrections applied
nverror = 0 ! number of corrections applied

!------------------------
! find dx_kernel now for all methods
! and set none kernel values as defaults as placeholders incase they are not needed

do j = 1, jtotal
  formatline = '(a,'//trim(indexformat)//',a,i1)'
  if (debug) write(83,fmt=formatline) 'FACE dx_kernel: j = ',j,'j: face type = ',face(j)%type
! dx_kernel is now based on volume of surrounding elements
! find average volume of surrounding non-boundary elements
  if (face(j)%type == 1) then
    dx_kernel = (cell(face(j)%icell(1))%vol+cell(face(j)%icell(2))%vol)/2.d0
  else
    dx_kernel = cell(face(j)%icell(1))%vol
  end if
! find lengthscale for the adjacent cell elements based their dimensions
! dx_kernel is half this lengthscale as the distance from the face to the first cell centre is about this
  dx_kernel = (dx_kernel**(1.d0/dble(cell(face(j)%icell(1))%dimensions)))/2.d0
  dx_kernel = kernel_dx_multiplier*dx_kernel
  face(j)%dx_kernel = dx_kernel ! save for use below in warnings and zeroing
  if (debug) write(83,*) 'j = ',j,': dx_kernel = ',face(j)%dx_kernel
  do l = 0, 6
!   allocate(face(j)%kernel(l)%ijk(0),face(j)%kernel(l)%v(0))
    face(j)%kernel(l)%centring = 'cell'
!   face(j)%kernel(l)%v = 0.d0
!   face(j)%kernel(l)%ijk = 0
  end do
end do

!------------------------

if (.not.(kernel_availability_faceave.or.kernel_availability_facegrad)) then
  if (debug_sparse) write(83,'(a)') 'INFO: skipping constructing face kernels as none have any kernel_availability'
  if (debug_sparse.or..true.) write(*,'(a)') 'INFO: skipping constructing face kernels as none have any kernel_availability'
  if (debug_sparse) write(*,'(a/80(1h-))') 'subroutine setup_face_kernels'
  return
end if

if (debug_sparse.or..true.) write(*,'(a)') 'INFO: constructing face kernels using '//trim(kernel_method)//' method'

! mls and optimisation kernels
if (trim(kernel_method) == 'mls' .or. trim(kernel_method) == 'optimisation') then

  minw_error = .false. ! this signals any minw error
  hyperbolic_kernel_local = hyperbolic_kernel ! this only changes if partial_hyperbolic_kernel is on

! zero separation level specific kernel maximums
  allocate(max_rel_ijk(0:6,1:max(maximum_domain_separation,maximum_boundary_separation)))
  allocate(max_rel_kernel(0:6,1:max(maximum_domain_separation,maximum_boundary_separation)))
  max_rel_ijk = 0
  max_rel_kernel = 0.d0

! temp &&&& for debugging single kernels
  do j = 1, jtotal
! do j = 1, 100 
! do j = 415, 415

    if (debug) write(83,*) '----------------------------'
    formatline = '(a,'//trim(indexformat)//',a,i1,a,i1)'
    if (debug) write(83,fmt=formatline) 'FACE: j = ',j,'j: face type = ',face(j)%type,': face dimensions = ',face(j)%dimensions

! set the (maximum) default separations
    if (face(j)%type == 2) then
      maximum_separation = maximum_boundary_separation
    else 
      maximum_separation = maximum_domain_separation
    end if

! setup the kernel mask which is the same for all kernel directions

! include first two elements and assign their separations locally (specific to the face)
! make sure that first two elements are as per icell so that boundary values correctly applied
    face(j)%kernel(0)%centring = 'cell'
    call resize_integer_array(keep_data=.false.,array=face(j)%kernel(0)%ijk,new_size=2)
    face(j)%kernel(0)%ijk(1:2) = face(j)%icell(1:2)
    call resize_integer_array(keep_data=.false.,array=separation_index,new_size=1)
    separation_index(1) = 2 ! last index in kernel%ijk that has a cell with separation 1
    call resize_integer_array(keep_data=.false.,array=separation_array,new_size=2)
    separation_array = 1
! allocate the reflect_multiplier array and populate it with values
! if all values turn out to be 1, then deallocate it later and set the reflect logical for the kernel appropriately
    if (allocated(r)) deallocate(r)

! add elements to the kernel mask in increasing order of separation up to the maximum_separation
    if (.true.) then
! now using stored reflect_multiplier and r arrays
      allocate(face(j)%kernel(0)%reflect_multiplier(totaldimensions,2))
      face(j)%kernel(0)%reflect_multiplier = face(j)%reflect_multiplier(:,1:2)
      allocate(r(totaldimensions,2))
      r = face(j)%r(:,1:2)
      call expand_mask(jcentre=j,have_icell=.true.,limit_mask_to_shared_nodes=limit_kernel_mask_to_shared_nodes, &
        include_adjacent_boundary_cells=boundary_node_separations,maximum_separation=maximum_separation,imask=face(j)%kernel(0)%ijk, &
        separation_index=separation_index,separation_array=separation_array, &
        reflect_multiplier=face(j)%kernel(0)%reflect_multiplier,r=r,dx=face(j)%dx_kernel)
    else
! older routine does not handle glued faces
      call expand_kernel_mask_old(iarray=face(j)%icell,maximum_separation=maximum_separation,imask=face(j)%kernel(0)%ijk, &
        separation_index=separation_index,separation_array=separation_array)
! create r for all cells in the mask
      allocate(r(totaldimensions,ubound(face(j)%kernel(0)%ijk,1)))
      do ii = 1, ubound(face(j)%kernel(0)%ijk,1)
        r(:,ii) = cell(face(j)%kernel(0)%ijk(ii))%x - face(j)%x
      end do
      allocate(face(j)%kernel(0)%reflect_multiplier(totaldimensions,ubound(face(j)%kernel(0)%ijk,1)))
      face(j)%kernel(0)%reflect_multiplier = 1
    end if

! scale r with dx_kernel
    r = r/face(j)%dx_kernel
! also size value array
    call resize_double_precision_array(keep_data=.false.,array=face(j)%kernel(0)%v,new_size=ubound(face(j)%kernel(0)%ijk,1))

! convert r to a consistent basis
! construct norm, find an orthogonal basis for r and convert r and the norm to this basis
    if (allocated(norm)) deallocate(norm)
    allocate(norm(totaldimensions,2*totaldimensions))
    norm = 0.d0
    norm(1,1) = 1.d0
    norm(2,2) = 1.d0
    norm(3,3) = 1.d0
    norm(:,4) = face(j)%norm(:,1)
    norm(:,5) = face(j)%norm(:,2)
    norm(:,6) = face(j)%norm(:,3)
    if (average_stability_corrections.or.gradient_stability_corrections) then ! save copies for stability corrections routine if required
      call copy_double_precision_2d_array(original=norm,copy=norm_dim)
      call copy_double_precision_2d_array(original=r,copy=r_dim)
    end if
    call construct_orthogonal_basis('face',r=r,norm=norm,error=error)
    if (error) call error_stop('unable to construct orthogonal basis vectors for face kernel')

! loop through all the directions required, doing face relative directions first

    face_direction_loop: do l = 6, 0, -1

      if ((.not.kernel_availability_facegrad.and.l >= 1).or.(.not.kernel_availability_faceave.and.l == 0)) then
        if (debug) write(83,*) 'SKIPPING face direction_loop based on kernel_availability: l = ',l
        cycle face_direction_loop
      end if

      if (debug) write(83,*) 'START direction_loop: l = ',l

! set hyperbolic_kernel_local based on face type
      if (partial_hyperbolic_kernel) then
        if (face(j)%type == 2.or.l == 0) then
!       if (l == 0) then
!       if (face(j)%type == 2) then
!       if (.true.) then
!       if (.false.) then
          hyperbolic_kernel_local = .true.
        else
          hyperbolic_kernel_local = .false.
        end if
      end if

! copy and reset kernel from the l=0 one, which is the last one set, and which will have the maximum number of elements right now
      if (l /= 0) call copy_kernel(original=face(j)%kernel(0),copy=face(j)%kernel(l))
      face(j)%kernel(l)%v = 0.d0

! apply setup changes to the derivative and averaging kernels separately, once for each
      if (l == 6.or.l == 0) then

! higher order kernels are only used on derivatives, not averages
        local_polynomial_order = polynomial_order
        if (l == 0) local_polynomial_order = polynomial_average_order

! set the (minimum) default separations
        if (face(j)%type == 2) then
          minimum_separation = minimum_boundary_separation
        else 
          minimum_separation = minimum_domain_separation
        end if
        if (minimum_separation > maximum_separation) call error_stop('problem when constructing a face kernel.  The requested '// &
          'minimum_separation is greater than the maximum_separation of cells that surround this face.  Reasons for this error '// &
          'could include a maximum_separation that is set too low, or the kernel option limitkernelmasktosharednodes is set to '// &
          'true (the default for polynomialorder=1), and the structure of the mesh does not allow enough cells to be included '// &
          'in the mask.  Problem face is '//trim(print_face(j)))

! calculate polynomial basis pp tensor from list of r vectors
        call construct_polynomial_basis_tensor(r,local_polynomial_order,pp,minimum_separation,separation_index,error)
        if (error) call error_stop('unable to construct pp basis tensor for face kernel: try increasing the maximum_separation in '// &
          'kernel_module.f90 to allow more cells to be included in each kernel mask')
        if (debug) then
          write(83,*) 'after construct_polynomial_basis_tensor'
          write(83,'(6(a,i3))') 'local_polynomial_order = ',local_polynomial_order,': ubound(pp,1) = ',ubound(pp,1), &
            ': minimum_sep. = ',minimum_separation,': separation_index(minimum_sep.) = ',separation_index(minimum_separation), &
            ': maximum_sep. = ',maximum_separation,': separation_index(maximum_sep.) = ',separation_index(maximum_separation)
        end if

! check minw, enlarging the minimum_separation if required
        if (check_minw) then
          minimum_separation_before = minimum_separation
          call check_mask_minw(pp,separation_index,minimum_separation,minw)
          if (minimum_separation /= minimum_separation_before) check_minw_increase = check_minw_increase + minimum_separation - &
            minimum_separation_before
          if (minw < minimum_minw) check_minw_limited = check_minw_limited + 1
          if (debug) then
            write(83,*) 'after check_mask_minw: minw = ',minw
            write(83,'(6(a,i3))') 'local_polynomial_order = ',local_polynomial_order,': ubound(pp,1) = ',ubound(pp,1), &
              ': minimum_sep. = ',minimum_separation,': separation_index(minimum_sep.) = ',separation_index(minimum_separation), &
              ': maximum_sep. = ',maximum_separation,': separation_index(maximum_sep.) = ',separation_index(maximum_separation)
          end if
          if (.not.minw_error.and.minw < 1.d-8) then
            write(*,'(a)') 'WARNING:  A very low value of minw was found when constructing face '// &
            'kernels.  This indicates that this kernel mask likely does not have enough kernel elements.  Consider '// &
            'increasing the minimumseparation, the maximumseparation, and ensuring that limitkernelmasktosharednodes is '// &
            'set to false.  This message will not be repeated for subsequent errors.  First error occurred at face '// &
            trim(print_face(j))
            minw_error = .true.
          end if
        end if

        total_masks = total_masks + 1
        total_mask_separations = total_mask_separations + minimum_separation

      end if

      if (l == 0.and.face(j)%type == 2) then
! for boundary cells averaging kernel don't do mls
        face(j)%kernel(0)%v(2) = 1.d0
        if (debug) write(83,*) 'boundary averaging kernel: type = ',face(j)%type

!     else if (l >= 1.and.vector_magnitude(norm(:,l)) < 1.d-10) then
      else if (l >= 1.and.vector_magnitude(norm(:,max(l,1))) < 1.d-10) then ! reference l=1 vector within norm for convienience here when l=0
! if the norm is zero in this direction don't do either
        if (debug) then
          write(83,'(a)') 'norm component when expressed in basis is zero: skipping kernel construction'
          write(83,*) 'l = ',l,': norm(:,l) = ',norm(:,l),': vector_magnitude(norm(:,l)) = ',vector_magnitude(norm(:,l)) 
        end if

      else if (l >= 1.and.l <= 3) then ! NB, l loop direction is such that l >= 4 already calculated
! construct these absolute kernels (1->3) from the relative ones calculated earlier (4->6)
        if (debug) write(83,*) 'constructing kernel from previous derivative kernels: l = ',l
        do ii = 1, ubound(face(j)%kernel(l)%ijk,1)
          do l2 = 4,6
            face(j)%kernel(l)%v(ii) = face(j)%kernel(l)%v(ii) + face(j)%norm(l,l2-3)*face(j)%kernel(l2)%v(ii)
          end do
        end do

      else
! create kernels via mls or optimisation method

! find l_coor, which is either 0 to indicate an averaging kernel, or else is the component of r that represents the
!  direction for the derivative
        l_coor = 0 ! default is an averaging kernel
        if (l >= 4) l_coor = maxloc(abs(norm(:,l)),dim=1) ! NB, zero norms dealt with above, as too l < 4
            
        if (debug) write(83,*) 'calculating face kernel: l = ',l,': l_coor = ',l_coor,': j = ',j, &
          'j: method = '//trim(kernel_method)

        if (trim(kernel_method) == 'mls') then
          if (l == 0) then
            call mls_kernel(centring='face',ijk=j,l_kernel=l,l_coor=l_coor,rr=r,pp=pp,kernel=face(j)%kernel(l)%v, &
              local_polynomial_order=local_polynomial_order,minimum_separation=minimum_separation,separation_array=separation_array, &
              separation_index=separation_index,error=error,hyperbolic_kernel_local=hyperbolic_kernel_local)
          else
            call mls_kernel(centring='face',ijk=j,l_kernel=l,l_coor=l_coor,rr=r,norm=norm(:,l),pp=pp,kernel=face(j)%kernel(l)%v, &
              local_polynomial_order=local_polynomial_order,minimum_separation=minimum_separation,separation_array=separation_array, &
              separation_index=separation_index,error=error,hyperbolic_kernel_local=hyperbolic_kernel_local)
          end if
        else
          if (l == 0) then
            call optimisation_kernel(centring='face',ijk=j,l_kernel=l,l_coor=l_coor,rr=r,pp=pp,kernel=face(j)%kernel(l)%v, &
              minimum_separation=minimum_separation,separation_array=separation_array, &
              separation_index=separation_index,error=error,hyperbolic_kernel_local=hyperbolic_kernel_local)
          else if (l >= 4.and.l <= 6) then
            call optimisation_kernel(centring='face',ijk=j,l_kernel=l,l_coor=l_coor,rr=r,norm=norm(:,l),pp=pp, &
              kernel=face(j)%kernel(l)%v,minimum_separation=minimum_separation, &
              separation_array=separation_array,separation_index=separation_index,error=error, &
              hyperbolic_kernel_local=hyperbolic_kernel_local)
          end if
        end if
        if (error) call error_stop('error in calculating a face '//trim(kernel_method)//' kernel')

! rescale recently-formed derivative kernels
        if (l >= 4.and.l <= 6) face(j)%kernel(l)%v=face(j)%kernel(l)%v/face(j)%dx_kernel

      end if
  
! TODO: do stability checks here by saving a copy of r, instead of in main routine
      if (l == 0.and.average_stability_corrections) then
        nverror = nverror + 1
        call kernel_stability_corrections(verror,one_kernel=face(j)%kernel(l))
      else if (l > 0.and.gradient_stability_corrections) then
  !     call kernel_stability_corrections(verror,one_kernel=face(j)%kernel(l),norm=face(j)%norm(:,l-3),xc=face(j)%x, &
  !       dx_kernel=face(j)%dx_kernel)
        nverror = nverror + 1
        call kernel_stability_corrections(verror,one_kernel=face(j)%kernel(l),norm=norm_dim(:,l),r=r_dim, &
          dx_kernel=face(j)%dx_kernel)

      end if

! find maximum values for each separation level
      maxvalue = maxval(abs(face(j)%kernel(l)%v)) ! this is the maximum kernel magnitude for this node and direction over any separation level
      do separation = 1, allocatable_integer_size(separation_index)
        if (separation == 1) then
          sepd = 1
        else
          sepd = separation_index(separation-1)+1
        end if
        value = maxval(abs(face(j)%kernel(l)%v(sepd:separation_index(separation))))/maxvalue
        if (value > max_rel_kernel(l,separation)) then
          max_rel_kernel(l,separation) = value
          max_rel_ijk(l,separation) = j
        end if
      end do
          
    end do face_direction_loop

! print some debugging info about kernels

    if (debug_sparse.and.kernel_availability_facegrad.and.kernel_availability_faceave) then
      formatline = '(a,'//trim(indexformat)//',a,i1,a,g9.2,2(a,i2),a,i1)'
      write(83,fmt=formatline) 'END separation_loop: all kernels calculated for: j = ',j,'j: type = ',face(j)%type, &
        ': dx_kernel = ',face(j)%dx_kernel,': face dimensions = ',face(j)%dimensions
! print out details of all cells that are in the kernel
      do ii = 1, ubound(face(j)%kernel(0)%ijk,1)
        i = face(j)%kernel(0)%ijk(ii)
        formatline = '(a,i3,a,'//trim(dindexformat(i))//',a,i2,a,g9.2,a'//repeat(',1x,f8.4',7)//')'
        write(83,fmt=formatline) 'ii = ',ii,': i = ',i,': sep. = ',separation_array(ii),': rmag = ',vector_magnitude(r(:,ii)), &
          ': v = ',face(j)%kernel(0)%v(ii),(face(j)%kernel(l)%v(ii)*face(j)%dx_kernel,l=1,6)
      end do
!     if (trim(kernel_method) == 'mls'.and.check_minw) write(83,*) 'minw = ',minw
      if (check_minw) write(83,*) 'minw = ',minw

! temp &&&
!     if (.false..and.j == 12294) then
!       write(*,*) 'WARNING: writing out kernel debugging file for j = ',j
!       open(unit=84,file='kernel_debugging.msh')
!       write(84,'(a/a/a)') '$MeshFormat','2.2 0 8','$EndMeshFormat'
!       write(84,'(a,3(/a))') '$Nodes','1','1 0. 0. 0.','$EndNodes'
!       write(84,'(a/i2)') '$Elements',ubound(r,2)
!       do ii = 1, ubound(r,2)
!         write(84,'(i2,a)') ii,' 15 2 0 0 1'
!       end do
!       write(84,'(a)') '$EndElements'
!       if (ubound(r,1) /= 3) stop "only 3d vectors can be handles right now"

!       write(84,'(a,6(/a))') '$ElementData','1','"<r>"','0','3','0','3'
!       write(84,'(i2)') ubound(r,2)
!       do ii = 1, ubound(r,2)
!         write(84,'(i1,3(1x,f10.5))') ii,(real(r(l,ii)),l=1,3)
!       end do
!       write(84,'(a)') '$EndElementData'
!         
!       write(84,'(a,6(/a))') '$ElementData','1','"<facenorm>"','0','3','0','3'
!       write(84,'(i2)') 1
!       write(84,'(i1,3(1x,f10.5))') 1,(real(norm(l,4)),l=1,3)
!       write(84,'(a)') '$EndElementData'
!         
!       close(unit=84)
!     end if
    end if

  end do

! print out some summary statements for each kernel and separation level combo
  if (.true.) then
    write(fwarn,'(a)') &
      'FACE: maximum abs kernel values at each separation level normalised by maximum abs values over all separation levels before removing small elements:'
    do l = 0, ubound(face(j)%kernel,1)
      do separation = 1, ubound(max_rel_kernel,2)
        write(fwarn,'(a,i1,a,i1,a,g10.3,a,i8)') 'l = ',l,': separation = ',separation,': max_rel_kernel = ', &
          max_rel_kernel(l,separation),': j = ',max_rel_ijk(l,separation)
      end do
    end do
  end if

  deallocate(max_rel_ijk,max_rel_kernel)

!----------------------
! ref: simple face kernels
! uber simple masks suitable for 1D applications only
! only 0 (average) and 4 (gradient in face direction) defined correctly
! if the face is normal to one of the coordinate directions then that direction will also be defined correctly
! anything else may have a value but will be nonsense
else if (trim(kernel_method) == 'simple') then
  do j = 1, jtotal
    dx1 = abs(dot_product( face(j)%r(:,1) , face(j)%norm(:,1) ))
    dx2 = abs(dot_product( face(j)%r(:,2) , face(j)%norm(:,1) ))
    face(j)%kernel(l)%centring = 'cell'
    do l = 0, 6
      allocate(face(j)%kernel(l)%ijk(2),face(j)%kernel(l)%v(2))
      face(j)%kernel(l)%ijk = face(j)%icell(1:2)
      face(j)%kernel(l)%v = 0.d0
      allocate(face(j)%kernel(l)%reflect_multiplier(totaldimensions,2))
      face(j)%kernel(l)%reflect_multiplier = 1
! a glue_reflect will be nonzero only if the face is glued to another
      if (face(j)%glue_reflect /= 0) face(j)%kernel(l)%reflect_multiplier(face(j)%glue_reflect,2) = -1
      if (l == 0) then
        face(j)%kernel(l)%v(1) = dx2/(dx1+dx2)
        face(j)%kernel(l)%v(2) = dx1/(dx1+dx2)
      else if (l == 4) then
        face(j)%kernel(l)%v(1) = -1.d0/(dx1+dx2)
        face(j)%kernel(l)%v(2) = 1.d0/(dx1+dx2)
      else if (l >= 1.and.l <= 3) then ! quick-and-dirty to get 1d coordinate-aligned problems working
        face(j)%kernel(l)%v(1) = -face(j)%norm(l,1)/(dx1+dx2)
        face(j)%kernel(l)%v(2) = face(j)%norm(l,1)/(dx1+dx2)
      end if
    end do
  end do
!----------------------
end if

if (allocated(r)) deallocate(r)
if (allocated(norm)) deallocate(norm)
if (allocated(r_dim)) deallocate(r_dim)
if (allocated(norm_dim)) deallocate(norm_dim)
if (allocated(pp)) deallocate(pp)
if (allocated(separation_index)) deallocate(separation_index)
if (allocated(separation_array)) deallocate(separation_array)

! temp &&&& for debugging single kernels
!stop

if (average_stability_corrections.or.gradient_stability_corrections) then
  if (verror > small_element_minimum) then
    write(*,'(a,g13.6)') 'WARNING: average stability corrections applied to face kernels is ',verror/max(dble(nverror),1.d0)
    write(fwarn,'(a,g13.6)') 'WARNING: average stability corrections applied to face kernels is ',verror/max(dble(nverror),1.d0)
  else
    write(*,'(a)') 'INFO: no stablity correction applied to face kernels'
  end if
end if

if (debug_sparse) write(*,'(a/80(1h-))') 'subroutine setup_face_kernels'

end subroutine setup_face_kernels

!-----------------------------------------------------------------

subroutine setup_cell_kernels

! ref: cell kernels
! setting up cell kernels

use general_module
integer :: i, ii, j, jj, ii2, i2, l, n, minimum_separation_before, maximum_separation, minimum_separation, &
  local_polynomial_order, l_coor, i_kernel, sepd, separation
double precision :: dx_kernel, minw, value, maxvalue
logical :: minw_error, hyperbolic_kernel_local, error
double precision, dimension(:,:), allocatable :: max_rel_kernel ! maximum kernel value in separation level / maximum kernel value in all separation levels
integer, dimension(:,:), allocatable :: max_rel_ijk
double precision, dimension(:,:), allocatable :: r, norm, pp
integer, dimension(:), allocatable :: separation_index, separation_array
integer, dimension(2) :: new_size_2d ! 2d array for passing to 2d array routines
character(len=10000) :: formatline
logical, parameter :: debug = .false.
logical :: debug_sparse = .false.

if (debug) debug_sparse = .true.

if (debug_sparse) write(*,'(80(1h+)/a)') 'subroutine setup_cell_kernels'

!----------------
! set dx_kernel for all methods
! and setup none kernels as placeholders

do i = 1, itotal
  formatline = '(a,'//trim(indexformat)//',a,i1)'
  if (debug) write(83,fmt=formatline) 'CELL dx_kernel: i = ',i,'i: cell type = ',cell(i)%type
! dx_kernel is now based on volume of surrounding elements
! find index of closest non-boundary cell
  if (cell(i)%type == 1) then
    i_kernel = i
  else
    i_kernel = cell(i)%icell(2)
  end if
! find lengthscale for this cell element based on its dimensions
! dx_kernel is first half a cell dimension as the first kernel considered is the cell to face averaging one and half a cell dimension is the appropriate length
  dx_kernel = (cell(i_kernel)%vol**(1.d0/dble(cell(i_kernel)%dimensions)))/2.d0

  dx_kernel = kernel_dx_multiplier*dx_kernel
  cell(i)%dx_kernel = dx_kernel ! save for use below in warnings and zeroing
  if (debug) write(83,*) 'dx_kernel = ',cell(i)%dx_kernel

  do l = 0, 4
    if (l == 0) then
      cell(i)%kernel(l)%centring = 'face'
    else if (l == 4) then
      cell(i)%kernel(l)%centring = 'node'
    else
      cell(i)%kernel(l)%centring = 'cell'
    end if
!   allocate(cell(i)%kernel(l)%v(0),cell(i)%kernel(l)%ijk(0))
!   cell(i)%kernel(l)%v = 0.d0
!   cell(i)%kernel(l)%ijk = 0
  end do
end do

!-------------------
if ((debug_sparse.or..true.).and..not.(kernel_availability_cellave.or.kernel_availability_cellgrad.or. &
  kernel_availability_cellfromnodeave.or.kernel_availability_cellfromnodegrad)) then
  if (debug_sparse) write(83,'(a)') 'INFO: skipping constructing cell kernels as none have any kernel_availability'
  if (debug_sparse.or..true.) write(*,'(a)') 'INFO: skipping constructing cell kernels as none have any kernel_availability'
  if (debug_sparse) write(*,'(a/80(1h-))') 'subroutine setup_cell_kernels'
  return
end if

if (debug_sparse.or..true.) write(*,'(a)') 'INFO: constructing cell kernels using '//trim(kernel_method)//' method'
if (trim(kernel_method) == 'mls' .or. trim(kernel_method) == 'optimisation') then

  minw_error = .false.
  hyperbolic_kernel_local = hyperbolic_kernel ! this only changes if partial_hyperbolic_kernel is on

! zero separation level specific kernel maximums
  allocate(max_rel_ijk(0:4,1:max(maximum_domain_separation,maximum_boundary_separation)))
  allocate(max_rel_kernel(0:4,1:max(maximum_domain_separation,maximum_boundary_separation)))
  max_rel_ijk = 0
  max_rel_kernel = 0.d0

  do i = 1, itotal

    if (debug_sparse) then
      write(83,*) '----------------------------'
      formatline = '(a,'//trim(indexformat)//',a,i1,a,i1)'
      write(83,fmt=formatline) 'CELL: i = ',i,'i: cell type = ',cell(i)%type,': cell dimensions = ',cell(i)%dimensions
      if (debug) write(83,*) 'surrounding faces: ',cell(i)%jface
    end if

    cell_direction_loop: do l = 0, 4

! skip kernel if no kernel_availability
      if ((.not.kernel_availability_cellgrad.and.l >= 1.and.l <= 3).or.(.not.kernel_availability_cellave.and.l == 0).or. &
        (.not.kernel_availability_cellfromnodeave.and.l == 4)) then
!       (.not.kernel_availability_cellfromnodeave.and.l == 4).or.(.not.kernel_availability_cellfromnodegrad.and.l >= 5.and.l <= 7)) then ! not implemented yet
        if (debug) write(83,*) 'SKIPPING cell direction_loop based on kernel_availability: l = ',l
        cycle cell_direction_loop
      end if

! change dx_kernel to reflect the appropriate lengthscales for each kernel - now set for every l
      if (l >= 1.and.l <= 3) then
        dx_kernel = cell(i)%dx_kernel*2.d0
      else
        dx_kernel = cell(i)%dx_kernel
      end if

! set hyperbolic_kernel_local if partial_hyperbolic_kernel is on
      if (partial_hyperbolic_kernel) then
        if (cell(i)%type == 2.or.l == 0.or.l == 4) then
!       if (l == 0.or.l == 4) then
!       if (cell(i)%type == 2) then
!       if (.true.) then
!       if (.false.) then
          hyperbolic_kernel_local = .true.
        else
          hyperbolic_kernel_local = .false.
        end if
      end if

! initialise averaging kernels
      if (l == 0) then
        call copy_integer_array(original=cell(i)%jface,copy=cell(i)%kernel(l)%ijk)
        allocate(cell(i)%kernel(l)%v(ubound(cell(i)%kernel(l)%ijk,1)))
        cell(i)%kernel(l)%v = 0.d0
      else if (l == 4) then
        call copy_integer_array(original=cell(i)%knode,copy=cell(i)%kernel(l)%ijk)
        allocate(cell(i)%kernel(l)%v(ubound(cell(i)%kernel(l)%ijk,1)))
        cell(i)%kernel(l)%v = 0.d0
      end if

      if (debug) then
        write(83,*)
        write(83,*) 'NEW KERNEL: i = ',i,': l = ',l,': kernel centring = '//trim(cell(i)%kernel(l)%centring)
      end if

!---------------
! boundary cells take on value from boundary face as set above as they are coincident
! also, in 1D boundary cells are coincident with boundary nodes also so take on that value
      if ((l == 0.or.(l == 4.and.cell(i)%dimensions == 0)).and.cell(i)%type == 2) then

        cell(i)%kernel(l)%v(1) = 1.d0 
        
!---------------
! the averaging kernels are just uniform, which seems (!) to be correct for the linear interpolation (which must be used here anyway due to number of points)
      else if (uniform_cell_averaging_kernels.and.(l == 0.or.l == 4)) then
        cell(i)%kernel(l)%v = 1.d0/ubound(cell(i)%kernel(l)%ijk,1)

!---------------
! boundary cells take on derivatives from boundary face as they are coincident
      else if (l >= 1.and.l <= 3.and.cell(i)%type == 2.and.boundary_cell_from_face_kernels) then

        j = cell(i)%jface(1)
        call copy_kernel(original=face(j)%kernel(l),copy=cell(i)%kernel(l))
        cell(i)%kernel(l)%centring = 'cell' ! have to rewrite this after above copying
        cell(i)%kernel(l)%v = cell(i)%kernel(l)%v*dx_kernel ! rescaling here so that all derivative kernels can be unscaled later
        if (debug) write(83,*) 'pulling face boundary kernel from j = ',j

!---------------
! construct derivative kernels from surrounding face derivative and cell averaging kernels
      else if (l >= 1.and.l <= 3.and.cell(i)%type == 1.and.domain_cell_from_face_kernels) then

! TODO: won't work with reflect right now
        call error_stop("domain_cell_from_face_kernels needs fixing in kernel_module to work with glued faces - comment "// &
          "this out if you don''t need this feature")
! TODO: also needs to be fixed for kernel_availability

! first create mask from surrounding face masks of the same derivative
        allocate(cell(i)%kernel(l)%ijk(1))
        cell(i)%kernel(l)%ijk(1) = i
        do jj = 1, ubound(cell(i)%kernel(0)%ijk,1) ! this is equivalent to cell(i)%jface
          j = cell(i)%kernel(0)%ijk(jj)
          do ii2 = 1, ubound(face(j)%kernel(l)%ijk,1)
            i2 = face(j)%kernel(l)%ijk(ii2)
            if (location_in_list(array=cell(i)%kernel(l)%ijk,element=i2) == 0) &
              call push_array(array=cell(i)%kernel(l)%ijk,new_element=i2) ! add element if it is not already on the list
          end do
        end do

! create value array and zero it
        allocate(cell(i)%kernel(l)%v(ubound(cell(i)%kernel(l)%ijk,1)))
        cell(i)%kernel(l)%v = 0.d0

! now use averaging kernel to create derivative entries
        do jj = 1, ubound(cell(i)%kernel(0)%ijk,1)
          j = cell(i)%kernel(0)%ijk(jj)
          do ii2 = 1, ubound(face(j)%kernel(l)%ijk,1)
            i2 = face(j)%kernel(l)%ijk(ii2)
            n = location_in_list(array=cell(i)%kernel(l)%ijk,element=i2)
            if (n == 0) stop 'ERROR: member of face kernel not found in cell mask kernel when summing face elements'
            cell(i)%kernel(l)%v(n) = cell(i)%kernel(l)%v(n) + cell(i)%kernel(0)%v(jj)*face(j)%kernel(l)%v(ii2)
          end do
        end do

        cell(i)%kernel(l)%v = cell(i)%kernel(l)%v*dx_kernel ! rescaling here so that all derivative kernels can be unscaled later

!---------------
! otherwise use mls or optimisation method to create new kernels
      else 

! calculate separation arrays and for the derivatives create the mask
        if (l == 1) then
! set the (minimum) default separations
          if (cell(i)%type == 2) then
            minimum_separation = minimum_boundary_separation
            maximum_separation = maximum_cell_boundary_separation
          else 
            minimum_separation = minimum_domain_separation
            maximum_separation = maximum_cell_domain_separation
          end if
          if (minimum_separation > maximum_separation) call error_stop('problem when constructing a cell kernel.  The '// &
            'requested minimum_separation is greater than the maximum_separation of cells that surround this cell.  '// &
            'Reasons for this error could include a maximum_separation that is set too low, or the kernel option '// &
            'limitkernelmasktosharednodes is set to true (the default for polynomialorder=1), and the structure of '// &
            'the mesh does not allow enough cells to be included in the mask.  Problem cell is '//trim(print_cell(i)))

          call resize_integer_array(keep_data=.false.,array=separation_index,new_size=1,default_value=1) ! last index in kernel%ijk that has a cell with separation 1
          call resize_integer_array(keep_data=.false.,array=separation_array,new_size=1,default_value=1)
          call resize_integer_array(keep_data=.false.,array=cell(i)%kernel(l)%ijk,new_size=1,default_value=i)
          if (.true.) then
! expand the mask
            new_size_2d = [totaldimensions,1]
            call resize_integer_2d_array(array=cell(i)%kernel(l)%reflect_multiplier,new_size=new_size_2d, &
              keep_data=.false.,default_value=1)
            call resize_double_precision_2d_array(array=r,new_size=new_size_2d,keep_data=.false.,default_value=0.d0)
            call expand_mask(icentre=i,have_icell=.true.,limit_mask_to_shared_nodes=limit_kernel_mask_to_shared_nodes, &
              include_adjacent_boundary_cells=boundary_node_separations,maximum_separation=maximum_separation,imask=cell(i)%kernel(l)%ijk, &
              separation_index=separation_index,separation_array=separation_array, &
              reflect_multiplier=cell(i)%kernel(l)%reflect_multiplier,r=r,dx=dx_kernel)
          else
! expand the separation_arrays to include all cells up to and including the maximum_separation
! old routine does not work with glued cells
            call expand_kernel_mask_old(iarray=cell(i)%icell,maximum_separation=maximum_separation,imask=cell(i)%kernel(1)%ijk, &
              separation_index=separation_index,separation_array=separation_array)
          end if
          call resize_double_precision_array(keep_data=.false.,array=cell(i)%kernel(l)%v,new_size=ubound(cell(i)%kernel(l)%ijk,1))
!         allocate(cell(i)%kernel(l)%v(ubound(cell(i)%kernel(l)%ijk,1)))
          cell(i)%kernel(l)%v = 0.d0
        else if (l == 2.or.l == 3) then
          call copy_kernel(original=cell(i)%kernel(1),copy=cell(i)%kernel(l))
          cell(i)%kernel(l)%v = 0.d0
!         call copy_integer_array(original=cell(i)%kernel(1)%ijk,copy=cell(i)%kernel(l)%ijk)
! NB: separation_index and separation_array just get reused from the l = 1 case
        else ! (l == 0.or.l == 4)
! there is only one separation level in these averaging kernels
          minimum_separation = 1
          maximum_separation = 1
          call resize_integer_array(keep_data=.false.,array=separation_index,new_size=1, &
            default_value=ubound(cell(i)%kernel(l)%ijk,1)) ! last index in kernel%ijk that has a cell with separation 1
          call resize_integer_array(keep_data=.false.,array=separation_array,new_size=separation_index(1),default_value=1)
        end if

! create r, scale it with dx_kernel, and then convert to a consistent basis
        if (l == 0.or.l == 1.or.l == 4) then
          if (.not.allocated(r)) then ! with new expand_mask routine r will already be formed/allocated for l==1
            allocate(r(totaldimensions,ubound(cell(i)%kernel(l)%ijk,1)))
            do ii = 1, ubound(cell(i)%kernel(l)%ijk,1)
              if (cell(i)%kernel(l)%centring.eq.'cell') then
                r(:,ii) = cell(cell(i)%kernel(l)%ijk(ii))%x - cell(i)%x
              else if (cell(i)%kernel(l)%centring.eq.'face') then
                r(:,ii) = face(cell(i)%kernel(l)%ijk(ii))%x - cell(i)%x
              else if (cell(i)%kernel(l)%centring.eq.'node') then
                r(:,ii) = node(cell(i)%kernel(l)%ijk(ii))%x - cell(i)%x
              else
                call error_stop('ERROR: problem in setup_kernels with cell kernel centring')
              end if
            end do
          end if
          r = r/dx_kernel
! construct norm if required and also convert to same basis
          if (l == 1) then ! only has to be calculated once
!           if (allocated(norm)) deallocate(norm)
            allocate(norm(totaldimensions,totaldimensions))
            norm = 0.d0
            norm(1,1) = 1.d0
            norm(2,2) = 1.d0
            norm(3,3) = 1.d0
            call construct_orthogonal_basis('cell',r=r,norm=norm,error=error)
          else
            call construct_orthogonal_basis('cell',r=r,error=error)
          end if
          if (error) call error_stop('ERROR: unable to construct orthogonal basis vectors for cell kernel')

! calculate polynomial basis pp tensor from list of r vectors
          if (l == 1) then
            local_polynomial_order = polynomial_cell_order
          else if (l == 0.or.l == 4) then
! the order of the averaging kernels is limited to be <= 1 as there are only a limited number of elements in the kernel mask
            local_polynomial_order = min(polynomial_cell_order,1)
          end if
          if (l == 1.or.l == 0.or.l == 4) then
            call construct_polynomial_basis_tensor(r,local_polynomial_order=local_polynomial_order,pp=pp, &
              minimum_separation=minimum_separation,separation_index=separation_index,error=error)
            if (error) call error_stop('unable to construct pp basis tensor for cell kernel: try increasing the '// &
              'maximum_separation in kernel_module.f90 to allow more cells to be included in each kernel mask')
            if (debug) then
              write(83,*) 'after construct_polynomial_basis_tensor'
              write(83,'(6(a,i3))') 'local_polynomial_order = ',local_polynomial_order,': ubound(pp,1) = ',ubound(pp,1), &
                ': minimum_sep. = ',minimum_separation,': separation_index(minimum_sep.) = ',separation_index(minimum_separation), &
                ': maximum_sep. = ',maximum_separation,': separation_index(maximum_sep.) = ',separation_index(maximum_separation)
            end if
          end if

! check minw, enlarging the minimum_separation if required
          if (check_minw.and.l == 1) then
            minimum_separation_before = minimum_separation
            call check_mask_minw(pp,separation_index,minimum_separation,minw)
            if (minimum_separation /= minimum_separation_before) check_minw_increase = check_minw_increase + minimum_separation - &
              minimum_separation_before
            if (minw < minimum_minw) check_minw_limited = check_minw_limited + 1
            if (debug) then
              write(83,*) 'after check_mask_minw: minw = ',minw
              write(83,'(6(a,i3))') 'local_polynomial_order = ',local_polynomial_order,': ubound(pp,1) = ',ubound(pp,1), &
                ': minimum_sep. = ',minimum_separation,': separation_index(minimum_sep.) = ',separation_index(minimum_separation), &
                ': maximum_sep. = ',maximum_separation,': separation_index(maximum_sep.) = ',separation_index(maximum_separation)
            end if
            if (.not.minw_error.and.minw < 1.d-8) then
              write(*,'(a)') 'WARNING:  A very low value of minw was found when constructing cell '// &
              'kernels.  This indicates that this kernel mask likely does not have enough kernel elements.  Consider '// &
              'increasing the minimumseparation, the maximumseparation, and ensuring that limitkernelmasktosharednodes is '// &
              'set to false.  This message will not be repeated for subsequent errors.  First error occurred at cell '// &
              trim(print_cell(i))
              minw_error = .true.
            end if
          end if

          if (l == 1) then
            total_masks = total_masks + 1
            total_mask_separations = total_mask_separations + minimum_separation
          end if

        end if

! use mls method to construct kernels
        if (l == 0.or.l == 4) then ! average from surrounding faces (l=0) or nodes (l=4)
          if (trim(kernel_method) == 'mls') then
            call mls_kernel(centring='cell',ijk=i,l_kernel=l,l_coor=0,rr=r,pp=pp,kernel=cell(i)%kernel(l)%v, &
              local_polynomial_order=local_polynomial_order,minimum_separation=minimum_separation, &
              separation_array=separation_array,separation_index=separation_index,error=error, &
              hyperbolic_kernel_local=hyperbolic_kernel_local)
            else
              call optimisation_kernel(centring='cell',ijk=i,l_kernel=l,l_coor=0,rr=r,pp=pp,kernel=cell(i)%kernel(l)%v, &
              minimum_separation=minimum_separation, &
              separation_array=separation_array,separation_index=separation_index,error=error, &
              hyperbolic_kernel_local=hyperbolic_kernel_local)
          end if
        else ! derivatives
          if (vector_magnitude(norm(:,l)) < 1.d-10) then
            if (debug) then
              write(83,'(a)') 'norm component when expressed in basis is zero: skipping kernel construction'
              write(83,*) 'l = ',l,': norm(:,l) = ',norm(:,l),': vector_magnitude(norm(:,l)) = ',vector_magnitude(norm(:,l)) 
            end if
          else
            l_coor = maxloc(abs(norm(:,l)),dim=1)
            if (trim(kernel_method) == 'mls') then
              call mls_kernel(centring='cell',ijk=i,l_kernel=l,l_coor=l_coor,rr=r,norm=norm(:,l),pp=pp,kernel=cell(i)%kernel(l)%v, &
                local_polynomial_order=local_polynomial_order,minimum_separation=minimum_separation, &
                separation_array=separation_array,separation_index=separation_index,error=error, &
                hyperbolic_kernel_local=hyperbolic_kernel_local)
            else
              call optimisation_kernel(centring='cell',ijk=i,l_kernel=l,l_coor=l_coor,rr=r,norm=norm(:,l),pp=pp, &
                kernel=cell(i)%kernel(l)%v,minimum_separation=minimum_separation, &
                separation_array=separation_array,separation_index=separation_index,error=error, &
                hyperbolic_kernel_local=hyperbolic_kernel_local)
            end if
          end if
        end if
        if (error) call error_stop('error in calculating a cell '//trim(kernel_method)//' kernel')

      end if
!---------------
! print some debugging info about kernels

      if (debug_sparse) then
        if (allocated(separation_array)) then
          formatline = '(a,i1,a,100(a,f8.4,a,'//trim(indexformat)//',a,i2,a))'
          write(83,fmt=formatline) 'l = ',l,': cent. = '//trim(cell(i)%kernel(l)%centring)//': value(ijk,sep.) =', &
            (' ',cell(i)%kernel(l)%v(ii),' (',cell(i)%kernel(l)%ijk(ii),',',separation_array(ii),')', &
            ii=1,ubound(cell(i)%kernel(l)%ijk,1))
        else
          formatline = '(a,i1,a,100(a,f8.4,a,'//trim(indexformat)//',a))'
          write(83,fmt=formatline) 'l = ',l,': cent. = '//trim(cell(i)%kernel(l)%centring)//': value(ijk) =', &
            (' ',cell(i)%kernel(l)%v(ii),' (',cell(i)%kernel(l)%ijk(ii),')',ii=1,ubound(cell(i)%kernel(l)%ijk,1))
        end if
!       if ((l <= 1.or.l == 4).and.trim(kernel_method) == 'mls'.and.check_minw) write(83,*) 'minw = ',minw
        if ((l <= 1.or.l == 4).and.check_minw) write(83,*) 'minw = ',minw
      end if

      if (debug) then
        do ii = 1, ubound(cell(i)%kernel(l)%ijk,1)
          i2 = cell(i)%kernel(l)%ijk(ii)
          if (.not.allocated(r)) then
            formatline = '(a,i3,a,'//trim(indexformat)//',a,g9.2)'
            write(83,fmt=formatline) 'ii = ',ii,': i2 = ',i2,': v = ',cell(i)%kernel(l)%v(ii)
          else if (allocated(r).and..not.allocated(norm)) then
            formatline = '(a,i3,a,'//trim(indexformat)//',a'//repeat(',1x,f6.2',ubound(r,1))//',2(a,g9.2))'
            write(83,fmt=formatline) 'ii = ',ii,': i2 = ',i2,': r =',r(:,ii), &
              ': rmag = ',vector_magnitude(r(:,ii)),': v = ',cell(i)%kernel(l)%v(ii)
          else
            formatline = '(a,i3,a,'//trim(indexformat)//',a'//repeat(',1x,f6.2',ubound(r,1))//',3(a,g9.2))'
            write(83,fmt=formatline) 'ii = ',ii,': i2 = ',i2,': r =',r(:,ii), &
              ': rmag = ',vector_magnitude(r(:,ii)),': r.norm = ',dot_product(r(:,ii),norm(:,l)),': v = ',cell(i)%kernel(l)%v(ii)
          end if
        end do
      end if

      if (l >= 1.and. l <= 3) cell(i)%kernel(l)%v=cell(i)%kernel(l)%v/dx_kernel

! find maximum values for each separation level
      maxvalue = maxval(abs(cell(i)%kernel(l)%v)) ! this is the maximum kernel magnitude for this node and direction over any separation level
      do separation = 1, allocatable_integer_size(separation_index)
        if (separation == 1) then
          sepd = 1
        else
          sepd = separation_index(separation-1)+1
        end if
        value = maxval(abs(cell(i)%kernel(l)%v(sepd:separation_index(separation))))/maxvalue
        if (value > max_rel_kernel(l,separation)) then
          max_rel_kernel(l,separation) = value
          max_rel_ijk(l,separation) = i
        end if
      end do

      if (allocated(r).and.(l == 0.or.l >= 3)) deallocate(r)
      if (allocated(norm).and.(l == 0.or.l >= 3)) deallocate(norm)
      if (allocated(separation_array).and.(l == 0.or.l >= 3)) deallocate(separation_array)
      if (allocated(separation_index).and.(l == 0.or.l >= 3)) deallocate(separation_index)

    end do cell_direction_loop

    if (debug_sparse) then
      formatline = '(a,'//trim(indexformat)//',a,i1)'
      write(83,fmt=formatline) 'END separation_loop: all kernels calculated for: i = ',i,'i: type = ',cell(i)%type
    end if

  end do

! print out some summary statements for each kernel and separation level combo
  if (.true.) then
    write(fwarn,'(a)') &
      'CELL: maximum abs kernel values at each separation level normalised by maximum abs values over all separation levels before removing small elements:'
    do l = 0, ubound(cell(i)%kernel,1)
      do separation = 1, ubound(max_rel_kernel,2)
        write(fwarn,'(a,i1,a,i1,a,g10.3,a,i8)') 'l = ',l,': separation = ',separation,': max_rel_kernel = ', &
          max_rel_kernel(l,separation),': i = ',max_rel_ijk(l,separation)
      end do
    end do
  end if

  deallocate(max_rel_ijk,max_rel_kernel)

else if (trim(kernel_method) == 'simple') then
!----------------------
! ref: simple cell kernels
! uber simple masks based on number of elements
! only 0 (average from surrounding faces) and 4 (average from surrounding nodes) defined
! average from face kernel (l=0) which is inverse of number of faces in kernel
! average from node kernel (l=4) which is inverse of number of nodes in kernel
  do i=1,itotal
    do l = 0, 4
      if (l == 0) then
        cell(i)%kernel(l)%centring = 'face'
        call copy_integer_array(original=cell(i)%jface,copy=cell(i)%kernel(l)%ijk)
        allocate(cell(i)%kernel(l)%v(ubound(cell(i)%kernel(l)%ijk,1)))
        cell(i)%kernel(l)%v = 1.d0/dble(ubound(cell(i)%kernel(l)%ijk,1))
      else if (l == 4) then
        cell(i)%kernel(l)%centring = 'node'
        call copy_integer_array(original=cell(i)%knode,copy=cell(i)%kernel(l)%ijk)
        allocate(cell(i)%kernel(l)%v(ubound(cell(i)%kernel(l)%ijk,1)))
        cell(i)%kernel(l)%v = 1.d0/dble(ubound(cell(i)%kernel(l)%ijk,1))
      else
        cell(i)%kernel(l)%centring = 'cell'
! as the face centred masks involve only adjacent values, we know the size of this mask and its values from the first and second tier icell entries
        allocate(cell(i)%kernel(l)%ijk(ubound(cell(i)%jface,1)+1))
        cell(i)%kernel(l)%ijk = cell(i)%icell(1:ubound(cell(i)%jface,1)+1)
! create value array and zero it
        allocate(cell(i)%kernel(l)%v(ubound(cell(i)%kernel(l)%ijk,1)))
        cell(i)%kernel(l)%v = 0.d0
        allocate(cell(i)%kernel(l)%reflect_multiplier(totaldimensions,ubound(cell(i)%jface,1)+1))
        cell(i)%kernel(l)%reflect_multiplier = 1
! now create derivative entries from surrounding face centred ones
        do jj = 1, ubound(cell(i)%kernel(0)%ijk,1)
          j = cell(i)%kernel(0)%ijk(jj)
          do ii2 = 1, 2 ! only two cells involved in these simple kernels
            i2 = face(j)%kernel(l)%ijk(ii2)
! if face is glued, then the normal must be pointing away from the cell, and a glue index will be allocated for the attached face
            if (face(j)%glue_jface /= 0.and.ii2 == 2) then ! face has another cell glued to it which will be the face's upcell
              cell(i)%kernel(l)%v(jj+1) = cell(i)%kernel(l)%v(jj+1) + cell(i)%kernel(0)%v(jj)*face(j)%kernel(l)%v(ii2)
              if (face(j)%glue_reflect /= 0) cell(i)%kernel(l)%reflect_multiplier(face(j)%glue_reflect,jj+1) = -1
            else if (i2 == i.or.face(j)%glue_jface /= 0) then ! central cell
              cell(i)%kernel(l)%v(1) = cell(i)%kernel(l)%v(1) + cell(i)%kernel(0)%v(jj)*face(j)%kernel(l)%v(ii2)
            else ! othercell, not glued
              cell(i)%kernel(l)%v(jj+1) = cell(i)%kernel(l)%v(jj+1) + cell(i)%kernel(0)%v(jj)*face(j)%kernel(l)%v(ii2)
            end if
          end do
        end do

      end if
    end do
  end do

!----------------------
end if

if (allocated(r)) deallocate(r)
if (allocated(norm)) deallocate(norm)
if (allocated(pp)) deallocate(pp)
if (allocated(separation_index)) deallocate(separation_index)
if (allocated(separation_array)) deallocate(separation_array)

! temp &&&& for debugging single kernels
!stop

if (debug_sparse) write(*,'(a/80(1h-))') 'subroutine setup_cell_kernels'

end subroutine setup_cell_kernels

!-----------------------------------------------------------------

subroutine setup_node_kernels

! ref: node kernels
! setting up node kernels

use general_module
integer :: i, j, k, ii, l, separation, minimum_separation_before, maximum_separation, minimum_separation, &
  local_polynomial_order, l_coor, n, nicell, sepd
double precision :: dx_kernel, minw, value, maxvalue
logical :: minw_error, hyperbolic_kernel_local, error, basis_constructed
double precision, dimension(:,:), allocatable :: r, norm, pp
integer, dimension(:), allocatable :: separation_index, separation_array
double precision, dimension(:,:), allocatable :: max_rel_kernel ! maximum kernel value in separation level / maximum kernel value in all separation levels
integer, dimension(:,:), allocatable :: max_rel_ijk
character(len=10000) :: formatline
logical, parameter :: debug = .false.
logical :: debug_sparse = .false.

if (debug) debug_sparse = .true.

if (debug_sparse) write(*,'(80(1h+)/a)') 'subroutine setup_node_kernels'

!--------------------------
! find dx_kernel for each node which is independent of kernel mask and direction
! also domain_dimensions now too
! now for all methods
! and setup none kernels as placeholders
if (debug_sparse) write(83,'(a)') 'INFO: calculating dx_kernel and domain_dimensions'

formatline = '(a,'//trim(indexformat)//',a,i1,a,i2)'

do k = 1, ktotal
! dx_kernel is now based on volume of surrounding domain-only cell elements
  nicell = allocatable_integer_size(node(k)%icell)
  if (debug) write(83,fmt=formatline) 'NODE dx_kernel: k = ',k,'k: node type = ',node(k)%type,': number of surrounding cells = ', &
    nicell
  if (nicell == 0) call error_stop('cannot calculate dx_kernel for a node as the node has no domain cell neighbours: '// &
    trim(print_node(k)))
  dx_kernel = 0.d0
  n = 0
  node(k)%domain_dimensions = 0
  do ii = 1, nicell
    i = node(k)%icell(ii)
    if (cell(i)%type == 1) then
      node(k)%domain_dimensions = cell(i)%dimensions
      n = n + 1
      dx_kernel = dx_kernel + cell(i)%vol
    end if
  end do
  if (node(k)%domain_dimensions == 0) call error_stop('cannot calculate dx_kernel for a node as the node has no domain cell '// &
    'neighbours: '//trim(print_node(k)))
  dx_kernel = kernel_dx_multiplier*((dx_kernel/dble(n))**(1.d0/dble(node(k)%domain_dimensions)))/2.d0
  node(k)%dx_kernel = dx_kernel ! save in node(k) object
  if (debug) write(83,*) 'dx_kernel = ',dx_kernel,': domain_dimensions = ',node(k)%domain_dimensions
  do l = 0, 3
!   allocate(node(k)%kernel(l)%ijk(0),node(k)%kernel(l)%v(0))
    node(k)%kernel(l)%centring = 'cell'
!   node(k)%kernel(l)%v = 0.d0
!   node(k)%kernel(l)%ijk = 0
  end do
end do

!--------------------------

if (.not.(kernel_availability_nodeave.or.kernel_availability_nodegrad)) then
  if (debug_sparse) write(83,'(a)') 'INFO: skipping constructing node kernels as none have any calculated kernel_availability'
  if (debug_sparse.or..true.) write(*,'(a)') 'INFO: skipping constructing node kernels as none have any calculated kernel_availability'
  if (debug_sparse) write(*,'(a/80(1h-))') 'subroutine setup_node_kernels'
  return
end if

if (debug_sparse.or..true.) write(*,'(a)') 'INFO: constructing node kernels using '//trim(kernel_method)//' method'

! mls and optimisation kernels
if (trim(kernel_method) == 'mls' .or. trim(kernel_method) == 'optimisation') then

  minw_error = .false. ! this signals any minw error
  hyperbolic_kernel_local = hyperbolic_kernel ! this only changes if partial_hyperbolic_kernel is on

! zero separation level specific kernel maximums
  allocate(max_rel_ijk(0:3,1:max(maximum_domain_separation,maximum_boundary_separation)))
  allocate(max_rel_kernel(0:3,1:max(maximum_domain_separation,maximum_boundary_separation)))
  max_rel_ijk = 0
  max_rel_kernel = 0.d0

  do k = 1, ktotal

    if (debug) write(83,*) '----------------------------'
    nicell = allocatable_integer_size(node(k)%icell)
    formatline = '(a,'//trim(indexformat)//',a,i1,a,i2)'
    if (debug) write(83,fmt=formatline) 'NODE: k = ',k,'k: node type = ',node(k)%type,': number of surrounding cells = ',nicell

! set the (maximum) default separations
    if (node(k)%type == 2) then
      maximum_separation = maximum_boundary_separation
    else 
      maximum_separation = maximum_domain_separation
    end if

! setup the kernel mask which is the same for all kernel directions

! include first nicell elements and assign their separations locally (specific to the node)
! make sure that first nicell elements are as per icell so that boundary values correctly applied
    node(k)%kernel(0)%centring = 'cell'
    call resize_integer_array(keep_data=.false.,array=node(k)%kernel(0)%ijk,new_size=nicell)
    node(k)%kernel(0)%ijk(1:nicell) = node(k)%icell(1:nicell)
    call resize_integer_array(keep_data=.false.,array=separation_index,new_size=1)
    separation_index(1) = nicell ! last index in kernel%ijk that has a cell with separation 1
    call resize_integer_array(keep_data=.false.,array=separation_array,new_size=nicell)
    separation_array = 1
! allocate the reflect_multiplier array and populate it with values
! if all values turn out to be 1, then deallocate it later and set the reflect logical for the kernel appropriately
    if (allocated(r)) deallocate(r)

! add elements to the kernel mask in increasing order of separation up to the maximum_separation
! now using stored reflect_multiplier and r arrays
    allocate(node(k)%kernel(0)%reflect_multiplier(totaldimensions,nicell))
    node(k)%kernel(0)%reflect_multiplier = node(k)%reflect_multiplier(:,1:nicell)
    allocate(r(totaldimensions,nicell))
    r = node(k)%r(:,1:nicell)
! already limited to the central nodes so don't use limit_mask_to_shared_nodes
    call expand_mask(have_icell=.true.,limit_mask_to_shared_nodes=.false., & 
      include_adjacent_boundary_cells=boundary_node_separations,maximum_separation=maximum_separation,imask=node(k)%kernel(0)%ijk, &
      separation_index=separation_index,separation_array=separation_array, &
      reflect_multiplier=node(k)%kernel(0)%reflect_multiplier,r=r,dx=node(k)%dx_kernel)

! scale r with dx_kernel
    r = r/node(k)%dx_kernel
! also size value array and zero it
    call resize_double_precision_array(keep_data=.false.,array=node(k)%kernel(0)%v,new_size=ubound(node(k)%kernel(0)%ijk,1))
    node(k)%kernel(0)%v = 0.d0

! construct norm, find an orthogonal basis for r and convert r and the norm to this basis
    if (allocated(norm)) deallocate(norm)
    allocate(norm(totaldimensions,totaldimensions))
    norm = 0.d0
    norm(1,1) = 1.d0
    norm(2,2) = 1.d0
    norm(3,3) = 1.d0
    call construct_orthogonal_basis('node',r=r,norm=norm,error=error)
    if (error) call error_stop('unable to construct orthogonal basis vectors for node kernel')

! loop through all the directions required, doing derivatives first
    basis_constructed = .false.

    node_direction_loop: do l = 3, 0, -1

      if ((.not.kernel_availability_nodegrad.and.l >= 1.and.l <= 3).or.(.not.kernel_availability_nodeave.and.l == 0)) then
        if (debug) write(83,*) 'SKIPPING node direction_loop based on kernel_availability: l = ',l
        cycle node_direction_loop
      end if

      if (debug) write(83,*) 'START direction_loop: l = ',l

! for 1D meshes nodes and faces are coincident, so make the respective kernels equal for:
! 1) an averaging boundary node, which has only a single kernel element = 1 for the coincident boundary cell
! 2) domain nodes if the relevant option is set, and
! 3) boundary nodes if the relevant option is set
      if (node(k)%domain_dimensions == 1.and.( (l == 0.and.node(k)%type == 2).or. &
        (domain_node_from_face_kernels.and.node(k)%type == 1).or. &
        (boundary_node_from_face_kernels.and.node(k)%type == 2) )) then

        j = node(k)%jface(1) ! this face must be coincident with the node
        if (allocatable_integer_size(face(j)%kernel(l)%ijk) == 0) then
!         call error_stop("trying to copy a kernel from a face "// &
!         "to a node but the face kernel has not been calculated.  Problem node is "//trim(print_node(k)))
          if (debug) write(83,*) '1D domain kernel cannot be copied from coincident face kernel: node type = ',node(k)%type, &
            ': coincident j = ',j
        else
          if (debug) write(83,*) '1D domain kernel being copied from coincident face kernel: node type = ',node(k)%type, &
            ': coincident j = ',j
! so just copy over this kernel
          call copy_kernel(original=face(j)%kernel(l),copy=node(k)%kernel(l))
! and shortcircuit rest of loop
          cycle node_direction_loop
        end if
      end if

! straight averaging kernel for coincident boundary node on 1D domain
      if (l == 0.and.node(k)%type == 2.and.node(k)%domain_dimensions == 1) then
        do ii = 1, allocatable_integer_size(node(k)%kernel(l)%ijk)
          if (cell(node(k)%kernel(l)%ijk(ii))%type == 2) then
            node(k)%kernel(l)%v(ii) = 1.d0 ! the first cell in the kernel to be a boundary cell is the one
            exit
          end if
        end do
        cycle node_direction_loop
      end if

! from here on we have to calculate kernels via the mls/optimisation methods
! copy kernel from the l=0 one, which is the last one set, and which will have a complete set of elements
      if (l /= 0) call copy_kernel(original=node(k)%kernel(0),copy=node(k)%kernel(l))

! construct the basis
! for the time being polynomial order for averaging and derivative kernels are the same
      if (.not.basis_constructed) then
        basis_constructed = .true.

        local_polynomial_order = polynomial_node_order

! set the (minimum) default separations
        if (node(k)%type == 2) then
          minimum_separation = minimum_boundary_separation
        else 
          minimum_separation = minimum_domain_separation
        end if
        if (minimum_separation > maximum_separation) call error_stop('problem when constructing a node kernel.  The requested '// &
          'minimum_separation is greater than the maximum_separation of cells that surround this node.  Reasons for this error '// &
          'could include a maximum_separation that is set too low, or the kernel option limitkernelmasktosharednodes is set to '// &
          'true (the default for polynomialorder=1), and the structure of the mesh does not allow enough cells to be included '// &
          'in the mask.  Problem node is '//trim(print_node(k)))

! calculate polynomial basis pp tensor from list of r vectors
        call construct_polynomial_basis_tensor(r,local_polynomial_order,pp,minimum_separation,separation_index,error)
        if (error) call error_stop('unable to construct pp basis tensor for node kernel: try increasing the maximum_separation in '// &
          'kernel_module.f90 to allow more cells to be included in each kernel mask')
        if (debug) then
          write(83,*) 'after construct_polynomial_basis_tensor'
          write(83,'(6(a,i3))') 'local_polynomial_order = ',local_polynomial_order,': ubound(pp,1) = ',ubound(pp,1), &
            ': minimum_sep. = ',minimum_separation,': separation_index(minimum_sep.) = ',separation_index(minimum_separation), &
            ': maximum_sep. = ',maximum_separation,': separation_index(maximum_sep.) = ',separation_index(maximum_separation)
        end if

! check minw, enlarging the minimum_separation if required
! TODO: separate subroutine?
        if (check_minw) then
          minimum_separation_before = minimum_separation
          call check_mask_minw(pp,separation_index,minimum_separation,minw)
          if (minimum_separation /= minimum_separation_before) check_minw_increase = check_minw_increase + minimum_separation - &
            minimum_separation_before
          if (minw < minimum_minw) check_minw_limited = check_minw_limited + 1
          if (debug) then
            write(83,*) 'after check_mask_minw: minw = ',minw
            write(83,'(6(a,i3))') 'local_polynomial_order = ',local_polynomial_order,': ubound(pp,1) = ',ubound(pp,1), &
              ': minimum_sep. = ',minimum_separation,': separation_index(minimum_sep.) = ',separation_index(minimum_separation), &
              ': maximum_sep. = ',maximum_separation,': separation_index(maximum_sep.) = ',separation_index(maximum_separation)
          end if
          if (.not.minw_error.and.minw < 1.d-8) then
            write(*,'(a)') 'WARNING:  A very low value of minw was found when constructing node '// &
            'kernels.  This indicates that this kernel mask likely does not have enough kernel elements.  Consider '// &
            'increasing the minimumseparation, the maximumseparation, and ensuring that limitkernelmasktosharednodes is '// &
            'set to false.  This message will not be repeated for subsequent errors.  First error occurred at node '// &
            trim(print_node(k))
            minw_error = .true.
          end if
        end if

        total_masks = total_masks + 1
        total_mask_separations = total_mask_separations + minimum_separation

      end if

! find kernels using mls or optimisation method
            
      if (debug) write(83,*) 'calculating node kernel: l = ',l,': k = ',k,'k: method = '//trim(kernel_method)

! use mls method to construct kernels
      if (l == 0) then ! average from surrounding cells (l=0)
        if (trim(kernel_method) == 'mls') then
          call mls_kernel(centring='node',ijk=k,l_kernel=l,l_coor=0,rr=r,pp=pp,kernel=node(k)%kernel(l)%v, &
            local_polynomial_order=local_polynomial_order,minimum_separation=minimum_separation, &
            separation_array=separation_array,separation_index=separation_index,error=error, &
            hyperbolic_kernel_local=hyperbolic_kernel_local)
          else
            call optimisation_kernel(centring='node',ijk=k,l_kernel=l,l_coor=0,rr=r,pp=pp,kernel=node(k)%kernel(l)%v, &
            minimum_separation=minimum_separation, &
            separation_array=separation_array,separation_index=separation_index,error=error, &
            hyperbolic_kernel_local=hyperbolic_kernel_local)
        end if
      else ! derivatives
        if (vector_magnitude(norm(:,l)) < 1.d-10) then
          if (debug) then
            write(83,'(a)') 'norm component when expressed in basis is zero: skipping kernel construction'
            write(83,*) 'l = ',l,': norm(:,l) = ',norm(:,l),': vector_magnitude(norm(:,l)) = ',vector_magnitude(norm(:,l)) 
          end if
        else
          l_coor = maxloc(abs(norm(:,l)),dim=1)
          if (trim(kernel_method) == 'mls') then
            call mls_kernel(centring='node',ijk=k,l_kernel=l,l_coor=l_coor,rr=r,norm=norm(:,l),pp=pp,kernel=node(k)%kernel(l)%v, &
              local_polynomial_order=local_polynomial_order,minimum_separation=minimum_separation, &
              separation_array=separation_array,separation_index=separation_index,error=error, &
              hyperbolic_kernel_local=hyperbolic_kernel_local)
          else
            call optimisation_kernel(centring='node',ijk=k,l_kernel=l,l_coor=l_coor,rr=r,norm=norm(:,l),pp=pp, &
              kernel=node(k)%kernel(l)%v,minimum_separation=minimum_separation, &
              separation_array=separation_array,separation_index=separation_index,error=error, &
              hyperbolic_kernel_local=hyperbolic_kernel_local)
          end if
! rescale derivative kernels
          node(k)%kernel(l)%v=node(k)%kernel(l)%v/node(k)%dx_kernel
        end if
        if (error) call error_stop('error in calculating a node '//trim(kernel_method)//' kernel')

      end if
  
! find maximum values for each separation level
      maxvalue = maxval(abs(node(k)%kernel(l)%v)) ! this is the maximum kernel magnitude for this node and direction over any separation level
      do separation = 1, allocatable_integer_size(separation_index)
        if (separation == 1) then
          sepd = 1
        else
          sepd = separation_index(separation-1)+1
        end if
        value = maxval(abs(node(k)%kernel(l)%v(sepd:separation_index(separation))))/maxvalue
        if (value > max_rel_kernel(l,separation)) then
          max_rel_kernel(l,separation) = value
          max_rel_ijk(l,separation) = k
        end if
      end do
          
    end do node_direction_loop

! print some debugging info about kernels

    if (debug_sparse.and.kernel_availability_nodeave.and.kernel_availability_nodegrad) then
      formatline = '(a,'//trim(indexformat)//',a,i1,a,g9.2,2(a,i2))'
      write(83,fmt=formatline) 'END separation_loop: all kernels calculated for: k = ',k,'k: type = ',node(k)%type, &
        ': dx_kernel = ',node(k)%dx_kernel
! print out details of all cells that are in the kernel
      do ii = 1, allocatable_integer_size(node(k)%kernel(0)%ijk)
        i = node(k)%kernel(0)%ijk(ii)
        formatline = '(a,i3,a,'//trim(dindexformat(i))//',a,i2,a,g9.2,a'//repeat(',1x,f8.4',4)//')'
        write(83,fmt=formatline) 'ii = ',ii,': i = ',i,': sep. = ',separation_array(ii),': rmag = ',vector_magnitude(r(:,ii)), &
          ': v = ',node(k)%kernel(0)%v(ii),(node(k)%kernel(l)%v(ii)*dx_kernel,l=1,3)
      end do
      if (check_minw) write(83,*) 'minw = ',minw

! temp &&&
!     if (.false..and.j == 12294) then
!       write(*,*) 'WARNING: writing out kernel debugging file for j = ',j
!       open(unit=84,file='kernel_debugging.msh')
!       write(84,'(a/a/a)') '$MeshFormat','2.2 0 8','$EndMeshFormat'
!       write(84,'(a,3(/a))') '$Nodes','1','1 0. 0. 0.','$EndNodes'
!       write(84,'(a/i2)') '$Elements',ubound(r,2)
!       do ii = 1, ubound(r,2)
!         write(84,'(i2,a)') ii,' 15 2 0 0 1'
!       end do
!       write(84,'(a)') '$EndElements'
!       if (ubound(r,1) /= 3) stop "only 3d vectors can be handles right now"

!       write(84,'(a,6(/a))') '$ElementData','1','"<r>"','0','3','0','3'
!       write(84,'(i2)') ubound(r,2)
!       do ii = 1, ubound(r,2)
!         write(84,'(i1,3(1x,f10.5))') ii,(real(r(l,ii)),l=1,3)
!       end do
!       write(84,'(a)') '$EndElementData'
!         
!       write(84,'(a,6(/a))') '$ElementData','1','"<facenorm>"','0','3','0','3'
!       write(84,'(i2)') 1
!       write(84,'(i1,3(1x,f10.5))') 1,(real(norm(l,4)),l=1,3)
!       write(84,'(a)') '$EndElementData'
!         
!       close(unit=84)
!     end if
    end if

  end do

! print out some summary statements for each kernel and separation level combo
  if (.true.) then
    write(fwarn,'(a)') &
      'NODE: maximum abs kernel values at each separation level normalised by maximum abs values over all separation levels before removing small elements:'
    do l = 0, ubound(node(k)%kernel,1)
      do separation = 1, ubound(max_rel_kernel,2)
        write(fwarn,'(a,i1,a,i1,a,g10.3,a,i8)') 'l = ',l,': separation = ',separation,': max_rel_kernel = ', &
          max_rel_kernel(l,separation),': k = ',max_rel_ijk(l,separation)
      end do
    end do
  end if

  deallocate(max_rel_ijk,max_rel_kernel)

!----------------------
! ref: simple node kernels and just copies of adjacent face kernels, only works if adjacent face is 0 dimensional
else if (trim(kernel_method) == 'simple') then

  do k = 1, ktotal
    j = node(k)%jface(1) ! if the node and face are coincident then the face must be 0D
    do l = 0, 3
      if (node(k)%domain_dimensions == 1) then
        if (debug) write(83,*) '1D domain kernel being copied from coincident face kernel (simple): node type = ',node(k)%type, &
          ': coincident j = ',j,': l = ',l
! so just copy over this kernel
        call copy_kernel(original=face(j)%kernel(l),copy=node(k)%kernel(l))
      else
        if (debug) write(83,*) &
          'simple kernel not possible as adjacent face is not 0D: node type = ',node(k)%type, &
          ': coincident j = ',j,': l = ',l
        allocate(node(k)%kernel(l)%ijk(0),node(k)%kernel(l)%v(0))
        node(k)%kernel(l)%centring = 'cell'
        node(k)%kernel(l)%v = 0.d0
        node(k)%kernel(l)%ijk = 0
      end if
    end do
  end do

!----------------------
end if

if (allocated(r)) deallocate(r)
if (allocated(norm)) deallocate(norm)
if (allocated(pp)) deallocate(pp)
if (allocated(separation_index)) deallocate(separation_index)
if (allocated(separation_array)) deallocate(separation_array)

! temp &&&& for debugging single kernels
!stop

if (debug_sparse) write(*,'(a/80(1h-))') 'subroutine setup_node_kernels'

end subroutine setup_node_kernels

!-----------------------------------------------------------------
! here we check that kernels obey some basic stability properties, at the expense of (polynomial) accuracy
! for averaging kernels ensure that each kernel value v > 0, otherwise the value is zeroed and rest decreased such that sum v = 1.d0
! for gradient kernels, each sign (v) = sign (r dot norm)), with some tolerance built in to ensure that kernel centre is not included in this test,
!  which is necessary for boundary kernels

subroutine kernel_stability_corrections(verror,one_kernel,norm,r,dx_kernel)

use general_module
double precision :: verror ! this is the cummulative amount of change that has happened to this series of kernels
type(kernel_type) :: one_kernel ! this is the particular kernel we're doing stability corrections to
! the following three are for gradient kernels
double precision, dimension(:), optional :: norm ! norm direction for gradient
double precision, dimension(:,:), optional :: r ! nondimensional (/dx_kernel) distance to each element, respecting glued faces
double precision, optional :: dx_kernel
double precision :: vsum, vsumpos, vsumneg, xrel
integer :: n
character(len=10000) :: formatline
logical, parameter :: debug = .false.
logical :: debug_sparse = .false.

if (debug) debug_sparse = .true.

if (debug_sparse) write(*,'(80(1h+)/a)') 'subroutine kernel_stability_corrections'

if (present(norm).and.present(r).and.present(dx_kernel)) then
! gradient correction
  vsumpos = 0.d0
  vsumneg = 0.d0
  vsum = 0.d0
  do n = 1, allocatable_integer_size(one_kernel%ijk)

    xrel = dot_product(r(:,n),norm)

! only correct the kernel entries if they are sufficiently away from the centre (in the direction of the norm)
    if (xrel > small_element_minimum) then
      if (one_kernel%v(n) < 0.d0) then
        verror = verror + abs(one_kernel%v(n))*dx_kernel
        one_kernel%v(n) = 0.d0
      end if
    else if (xrel < -small_element_minimum) then
      if (one_kernel%v(n) > 0.d0) then
        verror = verror + abs(one_kernel%v(n))*dx_kernel
        one_kernel%v(n) = 0.d0
      end if
    end if

    if (one_kernel%v(n) > 0.d0) vsumpos = vsumpos + one_kernel%v(n)
    if (one_kernel%v(n) < 0.d0) vsumneg = vsumneg - one_kernel%v(n)
    vsum = vsum + one_kernel%v(n)

  end do
  
! vsum is the sum of remaining elements
  if (vsumneg*dx_kernel > small_element_minimum.and.vsumpos*dx_kernel > small_element_minimum) then
    do n = 1, allocatable_integer_size(one_kernel%ijk)
      if (.false.) then
! if vsum > 0, increase negative elements
! if vsum < 0, increase positive elements
        if (vsum > 0.d0.and.one_kernel%v(n) < 0.d0) then
          one_kernel%v(n) = one_kernel%v(n)*vsumpos/vsumneg
        else if (vsum < 0.d0.and.one_kernel%v(n) > 0.d0) then
          one_kernel%v(n) = one_kernel%v(n)*vsumneg/vsumpos
        end if
      else
! if vsum > 0, decrease positive elements
! if vsum < 0, decrease negative elements
        if (vsum > 0.d0.and.one_kernel%v(n) > 0.d0) then
          one_kernel%v(n) = one_kernel%v(n)*vsumneg/vsumpos
        else if (vsum < 0.d0.and.one_kernel%v(n) < 0.d0) then
          one_kernel%v(n) = one_kernel%v(n)*vsumpos/vsumneg
        end if
      end if
    end do
  else if (vsumneg*dx_kernel > small_element_minimum.or.vsumpos*dx_kernel > small_element_minimum) then
    write(*,*) 'WARNING: problem with a stability correction in one of the gradient kernels'
    write(*,*) 'vsumneg,vsumpos,vsum'
    write(*,*) vsumneg,vsumpos,vsum
    write(*,*) 'one_kernel%v'
    write(*,*) one_kernel%v
    write(*,*) 'one_kernel%ijk'
    write(*,*) one_kernel%ijk
    write(*,*) 'r'
    write(*,*) r
    write(*,*) 'one_kernel%centring = ',one_kernel%centring
    call error_stop('problem using gradientstabilitycorrections, must be something unusual about the mesh')
  end if

else
! average correction
  vsum = 0.d0
  do n = 1, allocatable_integer_size(one_kernel%ijk)
    if (one_kernel%v(n) < 0.d0) then
      verror = verror + abs(one_kernel%v(n))
      one_kernel%v(n) = 0.d0
    else
      vsum = vsum + one_kernel%v(n)
    end if
  end do

  if (vsum > 1.d0) one_kernel%v = one_kernel%v/vsum
end if

if (debug_sparse) write(*,'(a/80(1h-))') 'subroutine kernel_stability_corrections'

end subroutine kernel_stability_corrections

!-----------------------------------------------------------------

end module kernel_module

!-----------------------------------------------------------------
