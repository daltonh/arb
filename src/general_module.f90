! file src/general_module.f90
!
! Copyright 2009-2018 Dalton Harvie (daltonh@unimelb.edu.au)
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
! module file for variables and routines common to most of the code
module general_module

! statements specifying different data types and parameters
implicit none

! allow all subroutines and functions to be public

! all variables referenced in the general_module should be saved
save ! note, under the 2008 fortran standard, I have read that all allocatable arrays in a module would automatically be saved anyway, but now including this specifically

!----------------------------------------------------------
! ref: general options
! the following are the main general option variables that can be set from the arb input file

! these two options are set from the perl, but are still userable (able to set from the arb file)
logical :: transient_simulation = .false. ! (.false., userable) whether simulation is transient or steady-state
logical :: newtient_simulation = .false. ! (.false., userable) whether simulation is newtient or not (ie, has variables that are evaluated only outside of the newton loop)

! timestep variables
integer :: timestep = 0 ! (0, userable) timestep index (with initial value being initial timestep, used for restarts)
integer :: timestepmax = huge(1) ! (huge, userable) maximum number of timesteps performed in total
integer :: timestepmin = 0 ! (0, userable) minimum number of timesteps performed in total
integer :: timestepadditional = 0 ! (0, userable) minimum number of timesteps that must be completed during current run, useful when restarting a simulation
integer :: timestepout = 0 ! (0, userable) maximum number of timesteps between output, with zero indicating no output
integer :: timesteprewind = 0 ! (0, userable) maximum number of timesteps to remember for timestep rewinding purposes - 0 turns off timestep rewinding
integer :: timesteprewindmax = 10 ! (10, userable) maximum number of consecutive timesteprewinds to do before giving up

! newtstep variables (newton loop)
integer :: newtstep = 0 ! (0, userable) newtstep index (with initial value being initial newtstep, used for restarts)
integer :: newtstepmax = 1000 ! (1000, userable) maximum number of steps performed by newton proceedure
integer :: newtstepmin = 1 ! (1, userable) minimum number of steps performed by newton proceedure
integer :: newtstepout = 0 ! (0, userable) maximum number of newtsteps between output, with zero indicating no output
integer :: newtsteprewind = 0 ! (0, userable) maximum number of newtsteps to remember for newtstep rewinding purposes - 0 turns off newtstep rewinding
integer :: newtstepdebugout = 990 ! (990, userable) after this many newtsteps newtstepout is set to 1 to produce debugging output
double precision :: newtrestol = 1.d-10 ! (1.d-10, userable) tolerance that indicates convergence of the newton proceedure
integer :: newtstepmaxiftimesteprewind = huge(1) ! (huge, userable) maximum number of steps performed by newton proceedure if timesteprewind is on and timesteprewindsdone < timesteprewindmax (ie, this is a way to jump out of the newton loop if it isn't converging)

! iterstep variables (iterative solver loop, if being used)
integer :: iterstepmax = 1000000 ! (1000000, userable) maximum number of steps performed by linear iteration solver
integer :: iterstepcheck = 100 ! (100, userable) how often linear iteration solver checks for kill file and outputs to screen residuals etc
double precision :: iterrestol = 1.d-11 ! (1.d-11, userable) tolerance that indicates convergence of the linear iteration solver
double precision :: iterresreltol = 0.d0 ! (0.d0, userable) tolerance that indicates convergence of the linear iteration solver, relative to the starting newtres - actual tolerance uses max(iterrestol,iterresreltol*newtres), so if iterresreltol is set to zero then it is inactive and only iterrestol is used

! the following are default values for various parameters which can be altered here (and not via user input options)
character(len=100) :: output_step_file = "default" ! (default, userable) whether to print output.step file or not: default|on, newtstep, timestep, output, final, off
logical :: output_timings = .true. ! (.true., userable) whether to time processes and output results to screen (see subroutine time_process)
logical :: output_timings_on_mesh_write = .false. ! (.false., userable) output timings each time a mesh file is written - requires that output_timings be on
logical :: output_detailed_timings = .false. ! (.false., userable) whether to give outputs for each routine (rather than just totals) - requires that output_timings be on
logical :: output_variable_update_times = .true. ! (.true., userable) time how long it takes to update each variable (on average) and report in output.stat
logical :: output_region_update_times = .true. ! (.true., userable) time how long it takes to update each dynamic region (on average) and report in output.stat
logical :: ignore_initial_update_times = .true. ! (.true., userable) ignore how long it takes to update each variable when initialising (ie, for initial_transients and initial_newtients)
logical :: kernel_details_file = .false. ! (.false., userable) print out a text file (kernel_details.txt) with all the kernel details
logical :: mesh_details_file = .false. ! (.false., userable) print out a text file (mesh_details.txt) with all the mesh details
logical :: region_details_file = .false. ! (.false., userable) print out a text file (region_details.txt) with all the region details
logical :: link_details_file = .false. ! (.false., userable) print out a text file (link_details.txt) with all the link details
logical :: convergence_details_file = .true. ! (.true., userable) write some convergence debugging data to convergence_details.txt

!----------------------------------------------------------
! set some numerical and mathematical parameters, non-userable

! code version details
real, parameter :: version = 0.58 ! current version
real, parameter :: minimum_version = 0.40 ! minimum version fortran_input.arb file that will still work with this version
character(len=100), parameter :: versionname = "roaming romesh"

! TODO: could make the following userable, at a small efficiency cost
double precision, parameter :: limitertolerance = 1.d-10 ! (1.d-10) tolerance used when calculating advection gradient limiting - set to small positive number
double precision, parameter :: limitercontgrad = 2.d0 ! factor that determines the gradient of the continuous advection limiter - set ~> 1.15 and ~< 2
double precision, parameter :: normalised_variable_limit = 1.d+10 ! ratio between unknown/equation magnitude and specified order of variable that signals an error 
double precision, parameter :: eps_dv = 1.d-40 ! minimum derivative magnitude to be considered when constructing jacobians, decreased from 1.d-20 -> 1.d-40 for v0.5

! mainly parameters, or almost parameters
integer, parameter :: totaldimensions=3 ! this is maximum number of dimensions: for all intents and purposes this can't be changed
double precision, parameter :: pi = 4.d0*atan(1.d0)
double precision, parameter :: tinyish = 1.d2*sqrt(tiny(0.d0)) ! something that is a bit bigger than tiny(0.d0)
double precision, parameter :: hugeish = 1.d-2*sqrt(huge(0.d0)) ! something that is a bit smaller than huge(0.d0)
character(len=100), parameter :: indexformat = 'i8' ! formating used for outputting integers throughout program, although now largely superseeding by dynamic format statements
character(len=100), parameter :: floatformat='g18.10' ! formating used for outputting double precision variables throughout program:  w = d+7 here (ie gw.d) as exponent may take three decimal places: now seems to be d+8 required
character(len=100), parameter :: compactformat='g11.4' ! compact formating used for outputting double precision variables throughout program:  w = d+7 here (ie gw.d) as exponent may take three decimal places
character(len=100), parameter :: realformat='g15.8' ! formating used for outputting variables truncated as reals throughout program (ie, treal), including variables in the msh files:  as exponent is truncated to two decimal places, w = d+6 here (gw.d)
character(len=100), parameter :: stringformat='a18' ! formating used for outputting strings (basically variable names) throughout program
! reals apparently have about 7 decimal places and width has to be d+7 (ifort)
integer, parameter :: fwarn = 11, fdetail = 12, foutput = 13, fgmsh = 14, finput = 15, fconverge = 16, foutputstep = 17 ! various file handles
integer :: backline = 6, newtline = 4, timeline = 2, totalline = 80 ! length of delimiter lines in the output
logical :: newtstepconverged = .true. ! reports on whether the last conducted newton loop step converged or not
logical :: newtstepfailed = .false. ! reports on whether there was an error during the last conducted newton loop step

! kernel availability (whether they are calculated or not) is calculated by the setup_equations.pl script, however, the settings can be overwritten (as true) here
logical :: kernel_availability_faceave = .false. ! (.false.) needed for varcdivgrad, but this routine itself is not needed under normal circumstances, so set false
logical :: kernel_availability_facegrad = .false. ! (.false.)
logical :: kernel_availability_cellfromnodegrad = .false. ! (.false.)
logical :: kernel_availability_cellgrad = .true. ! (.true.) needed for varcgrad used in variable output (for elementnodedata) so always keep on
logical :: kernel_availability_cellave = .false. ! (.false.)
logical :: kernel_availability_cellfromnodeave = .true. ! (.true.) needed when reading in elementnodedata so always keep on
logical :: kernel_availability_nodegrad = .false. ! (.false.)
logical :: kernel_availability_nodeave = .false. ! (.false.)

!----------------------------------------------------------
! now set some types and more complex data structures

! a generic kernel for calculating averages and derivatives
type kernel_type
  character(len=4) :: centring ! whether input values are cell (i), face (j) or node (k) centred
  integer, dimension(:), allocatable :: ijk ! array of cell/face/node indices that are used in this kernel
  double precision, dimension(:), allocatable :: v ! value of kernel in 1-to-1 correspondance with ijk
  logical :: reflect_present ! if any reflect_multipliers are not 1, and hence, the reflect_multipliers array is allocated
  integer, dimension(:,:), allocatable :: reflect_multiplier ! takes on values of +1 or -1 as multipliers to be applied during reflections (glued faces).  Second index refers to kernel element (ie, one-to-one correspondance with ijk and v) and first to reflection coordinate direction.  If not allocated then +1 values should be used.
  logical :: available ! as calculated by setup_equations, determines whether this kernel is actually required for a particular simulation
end type kernel_type

! this type specifies details of each node (vertex)
! nodes always have dimension = 0 and gtype = 15
type node_type
  integer :: type ! integer specifying whether node is within the domain (1) or on a boundary (2)
  double precision, dimension(totaldimensions) :: x ! location of node
  double precision :: dx_kernel ! characteristic dimension of mesh around this node to be used in kernel scaling - approximately equal to the equivalent radii of surrounding cells (not diameter)
  integer, dimension(:), allocatable :: jface ! array storing j indices of surrounding faces (directly connected, not via glue)
  integer, dimension(:), allocatable :: icell ! array storing i indices of surrounding cells (both directly connected and via glue)
  integer, dimension(:), allocatable :: glue_knode ! array storing k indices of any coincident nodes (due to faces being glued together) - unallocated if no nodes are glueds to this one
  logical :: glue_present ! signifies that some faces that are attached to this node are glued
  logical :: reflect_present ! signifies that some faces within the icells are not only glued, but also includes reflections (in practice means that reflect_multipliers should be allocated and have non-unity values)
  integer, dimension(:,:), allocatable :: reflect_multiplier ! reflect_multiplier for cells in the icell array, taking account of any glued faces.  First index is dimension (1:3), second is icell position
  double precision, dimension(:,:), allocatable :: r ! relative position of cells in the icell array, taking account of any glued faces.  First index is dimension (1:3), second is icell position
  type(kernel_type), dimension(0:3) :: kernel ! kernel(m) = kernel for the average (m=0) or derivative in the m'th coordinate direction, all based on cell data
  integer :: domain_dimensions ! number of dimensions of adjacent domain
end type node_type
  
! this type specifies details of each cell face
type face_type
  integer :: type ! integer specifying whether face is within the domain (1) or on a boundary (2)
  double precision, dimension(totaldimensions) :: x ! location of face centre
  double precision :: area ! area of face, (length in 1D, zero in 0D)
  double precision :: dx ! distance between adjacent cell centres
  double precision, dimension(totaldimensions) :: dx_unit ! unit vector from cell(idown) to cell(iup), adjacent to face
  double precision :: dx_kernel ! characteristic dimension of mesh around this face to be used in kernel scaling - approximately equal to the equivalent radii of surrounding cells (not diameter)
  double precision, dimension(totaldimensions,totaldimensions) :: norm ! orthogonal orientation vectors for face: first norm(:,1) points normal to face in direction from cell icell(1) -> icell(2), second norm(:,2) points from node(1)->node(2) on face (2d and 3d) (first tangent) and third norm(:,3) is normal to both (second tangent in 3d)
  integer, dimension(:), allocatable :: icell ! array storing i indices of the 2 adjacent cells
  integer, dimension(:), allocatable :: knode ! array storing k indices of surrounding nodes
  integer :: dimensions ! number of dimensions that cell is (0, 1 or 2)
  integer :: gtype ! gmsh element type for element geometry
  type(kernel_type), dimension(0:6) :: kernel ! kernel(m) = kernel for the average (m=0) or derivative in the m'th coordinate direction - m>=4 is for normals to the face for norm(:,m-3)
  integer :: glue_jface ! 0 if this face isn't glued to another, or the jface index of the face that it is glued to if it is
  integer :: glue_reflect ! 0 if no reflection is performed at this face during a glueing operation, or the coordinate direction of the reflection if the glueing involves a reflection
  logical :: glue_present ! signifies that some faces within the icells are glued (in practice means that the r array for this cell is allocated and contains glue-aware values)
  logical :: reflect_present ! signifies that some faces within the icells are not only glued, but also includes reflections (in practice means that reflect_multipliers should be allocated and have non-unity values)
  integer, dimension(:,:), allocatable :: reflect_multiplier ! reflect_multiplier for cells in the icell array, taking account of any glued faces.  First index is dimension (1:3), second is icell position
  double precision, dimension(:,:), allocatable :: r ! relative position of cells in the icell array, taking account of any glued faces.  First index is dimension (1:3), second is icell position
end type face_type

! this type specifies details of each cell
type cell_type
  integer :: type ! integer specifying whether cell is within the domain (1) or on a boundary (2)
  double precision, dimension(totaldimensions) :: x ! location of cell centre
  double precision, dimension(totaldimensions) :: dx ! maximum dimension of cell (node to node) in each dimension
  double precision :: vol ! volume (area in 2D, length in 1D)
  double precision :: dx_kernel ! characteristic dimension of mesh around this cell to be used in kernel scaling - approximately equal to the equivalent radius of the cell (not strictly, but definitely not the diameter either)
  double precision :: dx_max ! maximum cell centre to node distance for this cell
  double precision :: dx_min ! minimum cell centre to node distance for this cell
  integer, dimension(:), allocatable :: knode ! array storing k indices of surrounding nodes
  integer, dimension(:), allocatable :: jface ! array storing j indices of surrounding faces
  integer, dimension(:), allocatable :: icell ! array storing i indices of surrounding cells
  integer :: dimensions ! number of dimensions that cell is (1, 2 or 3)
  integer :: gtype ! gmsh element type for element geometry
  type(kernel_type), dimension(0:4) :: kernel ! kernel(m) = kernel for the average from faces (m=0) or derivative in the m'th coordinate direction, or average from nodes (m=4)
  double precision :: cross_kernel ! this is the maximum cross kernel component that would be used in this cell when calculating the face advective components - used for limiting the gradient used in the advection operation
  double precision :: central_kernel
  double precision :: kernel_sum ! this is the sum of all delta derivative bits
  logical :: glue_present ! signifies that some faces within the icells are glued (in practice means that the r array for this cell is allocated and contains glue-aware values)
  logical :: reflect_present ! signifies that some faces within the icells are not only glued, but also includes reflections (in practice means that reflect_multipliers should be allocated and have non-unity values)
  integer, dimension(:,:), allocatable :: reflect_multiplier ! reflect_multiplier for cells in the icell array, taking account of any glued faces.  First index is dimension (1:3), second is icell position
  double precision, dimension(:,:), allocatable :: r ! relative position of cells in the icell array, taking account of any glued faces.  First index is dimension (1:3), second is icell position
end type cell_type
  
! type for links between regions
type region_link_type
  character(len=1000) :: from_region ! name of the region which we are linking from
  character(len=1000) :: to_region ! name of the region which we are linking to
  integer :: from_region_number ! number of the region which we are linking from
  integer :: to_region_number ! number of the region which we are linking to
  character(len=4) :: from_centring ! centring of the region we are linking from
  character(len=4) :: to_centring ! centring of the region we are linking to
  integer, dimension(:), allocatable :: to_ns ! ns index within to_region from ns index of element within from_region
end type region_link_type

! type for glueing operations between two faces
type glue_face_type
  character(len=1000), dimension(2) :: region
  integer, dimension(2) :: region_number
  integer :: reflect ! same as glue_reflect for individual faces that are within the region
  double precision, dimension(totaldimensions) :: translate ! vector that indicates how second region is displaced from the first region, in order to match faces between both regions
  character(len=1000) :: option_line ! list of comma separated options as one line of text, taken straight from the input file
  character(len=100), dimension(:), allocatable :: options ! array of options for this var, with highest priority on the right
end type glue_face_type

! type for location specification within regions
type region_location_type
  logical :: active ! whether location is active or not
  character(len=1000) :: description ! string that describes location, as given by user in arb file
  character(len=100) :: type ! string that describes type of location description
  integer, dimension(:), allocatable :: regions ! list of region numbers used in this description
  integer, dimension(:), allocatable :: integers ! list of integers used in this description
  double precision, dimension(:), allocatable :: floats ! list of floats used in this description
  integer, dimension(:), allocatable :: variables ! list of fortran m numbers used in this description
  character(len=100), dimension(:), allocatable :: variabletypes ! list of fortran m numbers used in this description
  logical :: glue_face = .false. ! if a face-centred region, whether to include faces that are external to the region but glued to faces that are within the region
end type region_location_type

! type for regions
type region_type
  character(len=1000) :: name ! name of the region
  character(len=100) :: type ! region type: gmsh, static, constant, transient, newtient, derived, equation, output, condition
  character(len=4) :: centring ! whether cell or face centred
  integer :: dimensions = -1 ! maximum dimensions of the elements within the region (-1 means unset)
  character(len=100), dimension(:), allocatable :: options ! array of options for this region, with highest priority on the right
  logical :: dynamic = .false. ! whether region is static (user, static or gmsh) or a dynamic region
  integer :: relstep ! relative timestep, with relstep=0 being the current step
  type(region_location_type) :: location ! location of region
  type(region_location_type) :: initial_location ! initial_location of region if a dynamic transient or newtient region
  integer :: part_of ! fortran region index of the dynamic or static region that this region is solely contained within, for user (ie, non system) regions only
  integer :: parent ! fortran region index of the static region that this region is solely contained within, for user (ie, non system) regions only
  integer, dimension(:), allocatable :: ijk ! array of cell (i), face (j) or node (k) indices that are within this region - dimension of this is number of elements in region (seems to be true for dynamic regions too)
  integer, dimension(:), allocatable :: ns ! array that specifies data number from i, j or k index - dimension of this is either itotal, jtotal or ktotal - for all regions an ns(ijk) of zero indicates that the particular ijk element is not in the region
  real :: update_time = 0.d0 ! total cpu time that has been spent on updating this region (only for dynamic variables)
  integer :: update_number = 0 ! total number of times that this region has been updated (only for dynamic variables)
  integer :: nslast = 0 ! this is the last index of the cells one separation less than the maximum in region(m) - 0 means that there is no information stored about this separation level, as it is one below the minimum ijk index
  logical :: timesteprewind ! whether this region will be rewound to previous values on a timestep rewind
  logical :: newtsteprewind ! whether this region will be rewound to previous values on a newtstep rewind
  integer, dimension(:,:), allocatable :: ns_timesteprewind ! first index is ijk number, second is timesteprewind index (eg, timesteprewindearliest etc)
end type region_type

! data type for any functions that ultimately depend on field data
type funk_type
  double precision :: v ! value of function
  double precision, dimension(:), allocatable :: dv ! value of derivative, in 1:1 with pp
  integer, dimension(:), allocatable :: pp ! phi variable which derivative is taken wrt
  integer :: ndv ! number of derivative elements that currently contain valid data
  double precision, dimension(:), allocatable :: v_timesteprewind ! value of function in previous timesteps, with v_timesteprewind(timesteprewindhistory(timestepmax)) referencing the earliest held data, and v_timesteprewind(timesteprewindhistory(1)) referencing the latest held data
  double precision, dimension(:), allocatable :: v_newtsteprewind ! value of function in previous newtsteps
end type funk_type

! meta data type for general variables
type var_type
  character(len=1000) :: name ! name of the variable
  character(len=1000) :: units ! character string of the units
  double precision :: multiplier ! multiplier appended to units when interacting with outside world
  double precision :: magnitude ! an order of magnitude estimate of the variable, calculated from initial conditions or dynamically (option dynamicmagnitude/nodynamicmagnitude) and only for unknown and equation variables right now.  -1 indicates that this magnitude has not been set
  integer :: magnitude_constant = 0 ! fortran var number of a none-centred constant that is to be used as the magnitude of this variable, only for unknowns and equations, and if this is set, it takes precedence over any default magnitude values
  character(len=4) :: centring
  character(len=100) :: type ! variable type: constant, transient, newtient, unknown, derived, equation, output, condition, local
  integer :: relstep ! relative timestep, with relstep=0 being the current step
  character(len=6) :: rank ! specifies whether this is a component of a scalar, vector or tensor compound
  character(len=1000) :: region ! name of the region in which it is applied
  integer :: region_number ! number of the region in which it is applied
  character(len=1000) :: update_region ! name of the region in which it is updated, which is only different to the region if it was defined over a dynamic region
  integer :: update_region_number ! number of the region in which it is updated
  character(len=1000) :: compound_name ! name of the compound variable of which this scalar is a component
  integer :: compound_number ! number of the compound variable of which this scalar is a component
  integer, dimension(:), allocatable :: component ! ordered list of the compound variable of which this scalar is a component
  character(len=100), dimension(:), allocatable :: options ! array of options for this var, with highest priority on the right
  type(funk_type), dimension(:), allocatable :: funk ! an array of the data, which depending on centring, may contain 1 (none), itotal (cell), jtotal (face) or ktotal (node) elements
  integer :: someloop ! if this variable is not to be stored (ie, local is on) then this is the someloop number to be used instead (otherwise if local is off someloop = 0)
  logical :: dynamic_magnitude ! adjust the magnitude of the unknown or equation dynamically, under-relaxing it based on present values
  double precision :: dynamic_magnitude_multiplier ! multiplier that limits the change in each unknown/equation magnitude when being dynamically adjusted.  Set >1. (=1 is equivalent to having static magnitudes, =large places no restriction on the change in magnitude)
  real :: update_time = 0.d0 ! total cpu time that has been spent on updating this variable
  integer :: update_number = 0 ! total number of times that this variable has been updated
  logical :: timesteprewind ! whether this variable will be rewound to previous values on a timestep rewind
  double precision :: timesteprewindmultiplier ! multiplier used when this variable is rewound on a timestep rewind, unless timesteprewindmultiplier_variable is set
  integer :: timesteprewindmultiplier_variable = 0 ! fortran var number of a none-centred variable that is to be used as the timesteprewindmultiplier for this variable, and if this is set, it takes precedence over any default magnitude values
  logical :: newtsteprewind ! whether this variable will be rewound to previous values on a newtstep rewind
end type var_type

! data type for var_lists
type var_list_type
  character(len=4) :: centring
  character(len=100) :: type ! variable type
  logical :: include_regions ! whether compatible dynamic regions are also included in this list
  integer, dimension(:), allocatable :: list ! index of variable of region
  logical, dimension(:), allocatable :: region ! whether the corresponding index is a region (.true.) or variable (.false.)
end type var_list_type

! meta data type for all compound variables
type compound_type
  character(len=1000) :: name ! name of the variable
  character(len=1000) :: units ! character string of the units
  double precision :: multiplier ! multiplier appended to units when interacting with outside world
  character(len=4) :: centring
  character(len=100) :: type ! variable type: constant, transient, unknown, derived, equation, output, condition, local
  character(len=6) :: rank ! specifies whether this is a component of a scalar, vector or tensor compound
  integer :: relstep ! relative timestep, with relstep=0 being the current step
  character(len=1000) :: region ! name of the region in which it is applied
  character(len=1000) :: update_region ! name of the region in which it is updated
  integer :: region_number ! number of the region in which it is applied
  integer :: update_region_number ! number of the region in which it is updated
  integer, dimension(:), allocatable :: component ! ordered list of the compound variable of which this scalar is a component
  character(len=100), dimension(:), allocatable :: options ! array of options for this compound, with highest priority on the right
end type compound_type

! separation_list
type separation_list_type ! little type that is a list of cells used when looping through regions in order of increasing separation
  integer, dimension(:), allocatable :: icell ! list of cells
  integer, dimension(:), allocatable :: separation_index ! last icell index for cell having separation index-1
  integer, dimension(:,:), allocatable :: reflect_multiplier ! reflect_multiplier for cells in the icell array, taking account of any glued faces.  First index is dimension (1:3), second is icell position
  double precision, dimension(:,:), allocatable :: r ! relative position of cells in the icell array, taking account of any glued faces.  First index is dimension (1:3), second is icell position
  integer :: nseparation ! current separation for this loop
  integer :: iicurrent ! icell index of current cell in the loop - used for getting r and reflect_multiplier
end type separation_list_type

! now place all resources for each thread into a someloop container
type someloop_type
  type(funk_type), dimension(:), allocatable :: funk ! funk structure for local updating use to store someloop values and derivatives - index is someloop number
  type(separation_list_type), dimension(:), allocatable :: separation_list ! each separation loop gets its own separation_list, in each thread (someloop container)
  integer, dimension(:), allocatable :: current_separation_list ! array that specifies the current (last used from front of array) separation_lists that are being used in this someloop thread.  Size of array allocated at start, with 0s meaning empty
end type someloop_type
type(someloop_type), dimension(:), allocatable :: someloop ! one for each thread

! list of vectors, that grows to keep in elements
type vector_list_type
  double precision, dimension(:,:), allocatable :: elements ! first index is direction coordinate, second is node index
  integer :: length ! current length of this list, starting from 1
end type vector_list_type
  
! list of scalars, that grows to keep in elements
type scalar_list_type
  double precision, dimension(:), allocatable :: elements ! index is node index
  integer :: length ! current length of this list, starting from 1
end type scalar_list_type
  
! list of integers, that grows to keep in elements
type integer_list_type
  integer, dimension(:), allocatable :: elements ! index is node index
  integer :: length ! current length of this list, starting from 1
end type integer_list_type
  
! mesh arrays
type(node_type), dimension(:), allocatable :: node ! array of nodes
type(face_type), dimension(:), allocatable :: face ! array of faces
type(cell_type), dimension(:), allocatable :: cell ! array of cells

! now define actual data arrays which are all saved
! each variable has a corresponding data type
type(var_type), dimension(:), allocatable :: var ! var(m) = general variable of type m, itself containing array of funk data for each var
type(compound_type), dimension(:), allocatable :: compound ! list of compound (scalar, vector and tensor) general variables

! regions
type(region_type), dimension(:), allocatable :: region ! array of regions

! region links
type(region_link_type), dimension(:), allocatable :: region_link ! array of region links

! glue_faces
type(glue_face_type), dimension(:), allocatable :: glue_face ! array of glue_faces, used for periodic and reflection boundaries

! simulation info object, which just holds some description info carried over from the arb input file
type simulation_info_type
  character(len=200) :: title
  character(len=200) :: author
  character(len=200) :: date
  character(len=200) :: version
  character(len=4000) :: description
  character(len=200) :: filename
  character(len=400) :: absfilename
  character(len=200) :: rundate
  character(len=200) :: runversion
  character(len=200) :: runhost
end type simulation_info_type
type(simulation_info_type) :: simulation_info

! general purpose variables.  Default values specified here can in some cases be overwritten by user input statements
real :: last_cpu_time = 0.e0, last_wall_time = 0.e0, total_cpu_time = 0.e0, total_wall_time = 0.e0
real, dimension(:), allocatable :: update_time_start ! processor specific temporary variable update timing array
integer :: idomain, iboundary, itotal ! number of domain (type 1) and boundary (type 2) cells, also total
integer :: jdomain, jboundary, jtotal ! number of domain (type 1) and boundary (type 2) faces, also total
integer :: kdomain, kboundary, ktotal ! number of domain (type 1) and boundary (type 2) nodes, also total
integer :: ptotal ! number of equations and unknowns
double precision, dimension(:), allocatable :: phiold, delphi ! single dimension unknown-sized variables for newton proceedure
integer :: transient_relstepmax ! maximum relstep value for all transients (variables and dynamic regions)
integer :: newtient_relstepmax ! maximum relstep value for all newtients (variables and dynamic regions)
double precision :: newtres = 0.d0 ! last evaluated value of the newton residual
logical :: ignore_gmesh_step = .false. ! if a TIMESTEPSTART (for transient) or NEWTSTEPSTART (for steady-state) is specified in the input file then the step from any gmesh file is ignored
integer :: maximum_dimensions = 0 ! maximum dimensions of any region used in the simulation
integer :: maximum_celljfaces = 0 ! maximum number of faces that a cell has
integer :: maximum_cellknodes = 0 ! maximum number of nodes that a cell has
integer :: maximum_faceknodes = 0 ! maximum number of nodes that a face has
integer :: nthreads = 0 ! if > 1 is the number of openmp threads in use, if == 1 then this signifies either a serial calculation or omp calculation with one thread in use (distinction not important from programming point of view)
integer :: msomeloop = 0 ! set in allocate_meta_arrays, this is the maximum someloop number (ie, number of someloops)
integer :: mseparation_list = 0 ! set in allocate_meta_arrays, this is the maximum separation_list (ie, number of separation_lists)
double precision, dimension(:), allocatable :: newtsteprewind_lambda_history ! array of previously used lambdas
character(len=1000) :: input_file ! fortran specific input file generated by the arb script, set in subroutine setup_dirs
character(len=1000) :: output_dir ! path to the output_dir from the working_dir, set in subroutine setup_dirs
type(funk_type), dimension(:), allocatable :: funkt ! funk container which is used to assemble combined funk
integer, dimension(:), allocatable :: unknown_var_from_pp ! an array for fast lookup of the unknown var number from p
character(len=100), dimension(:), allocatable :: general_options ! list of general options, with highest priority on the right
character(len=100), dimension(:), allocatable :: kernel_options ! list of kernel options, with highest priority on the right
character(len=100), dimension(:), allocatable :: solver_options ! list of kernel options, with highest priority on the right
type(var_list_type), dimension(:), allocatable :: var_list ! array of var_lists, according to type, centring, and now also region
! these commonly used var_list_numbers are stored for quick access and calculated in equation_module
integer :: var_list_number_unknown, var_list_number_equation, var_list_number_condition ! these ones don't include regions
integer :: var_list_number_all_region, var_list_number_derived_region, var_list_number_equation_region, &
  var_list_number_constant_region, var_list_number_unknown_region, var_list_number_newtient_region, &
  var_list_number_transient_region, var_list_number_output_region ! these ones do include regions

! variables specific to the timestep and newtstep rewind capability
integer :: timesteprewindsdone = 0 ! number of consecutive timesteprewinds that have been done, to be compared against timesteprewindmax
integer :: newtstepmaxtimesteprewindlimited = huge(1) ! under a timesteprewind simulation this newtstepmax adjusts according to whether timesteprewindsdone < timesteprewindmax (ie, based on the above)
!integer, dimension(:), allocatable :: timesteprewindhistory ! array that stores the index of (eg) v_timesteprewind data that corresponds to each previous relative timestep
integer :: timesteprewindearliest ! index of the v_timesteprewind array that references the earliest stored data
integer :: timesteprewindlatest ! index of the v_timesteprewind array that references the latest stored data
integer :: timesteprewindstored ! number of timesteps stored in the v_timesteprewind arrays
! integer :: timesteprewind_history_first = 0 ! array index for v_timesteprewind_history in funks that corresponds to earliest data stored
! integer :: timesteprewind_history_last = 0 ! array index for v_timesteprewind_history in funks that corresponds to the latest data stored
! integer :: timesteprewind_newtstep = 10 ! (userable, newtstep) number of newtsteps that if exceeded in newton loop triggers timesteprewind
! integer :: newtsteprewind_history_first = 0 ! array index for v_newtsteprewind_history in funks that corresponds to earliest data stored
! integer :: newtsteprewind_history_last = 0 ! array index for v_newtsteprewind_history in funks that corresponds to the latest data stored
! integer :: newtsteprewind_lambda = 1.d-4 ! (userable, lambda) lambda limit that if reduced below this triggers a newtstep rewind
! integer :: newtsteprewind_lambdamultiplier = 0.5d0 ! (userable, lambdamultiplier) what previous lambda is multiplied by

! define some string parameter arrays
character(len=100), dimension(9), parameter :: var_types = [ "constant   ", "transient  ", "newtient   ", "unknown    ", &
  "derived    ", "equation   ", "output     ", "condition  ", "local      " ]
character(len=100), dimension(3), parameter :: stepoutput_options = ["stepoutput        ", "stepoutputnoupdate", &
  "nostepoutput      "]
character(len=100), dimension(2), parameter :: output_options = ["output  ", "nooutput"]
character(len=100), dimension(5), parameter :: output_gmesh_options = ["output            ", "centringoutput    ", &
  "meshoutput        ", "centringmeshoutput", "nooutput          "]
character(len=100), dimension(5), parameter :: vtkoutput_gmesh_options = ["vtkoutput            ", "centringvtkoutput    ", &
  "meshvtkoutput        ", "centringmeshvtkoutput", "novtkoutput          "]
character(len=100), dimension(5), parameter :: datoutput_gmesh_options = ["datoutput            ", "centringdatoutput    ", &
  "meshdatoutput        ", "centringmeshdatoutput", "nodatoutput          "]
character(len=100), dimension(5), parameter :: input_gmesh_options = ["input            ", "centringinput    ", &
  "meshinput        ", "centringmeshinput", "noinput          "]
character(len=100), dimension(3), parameter :: data_options = ["elementdata           ", "elementnodedata       ", &
  "elementnodelimiteddata"]
character(len=100), dimension(2), parameter :: input_options = ["input            ", "noinput          "]
character(len=8), dimension(7), parameter :: stopfilelist = [ "kill    ", "stopback", "stopiter", "stopnewt", "stop    ", &
  "stoptime", "halt    " ]
character(len=8), dimension(3), parameter :: dumpfilelist = [ "dumpnewt", "dump    ", "dumptime" ]
logical, dimension(totaldimensions) :: array_mask1 = [.true.,.false.,.false.], array_mask2 = [.false.,.true.,.false.], &
  array_mask3 = [.false.,.false.,.true.]

!----------------------------------------------------------------------------

! interface to make general array resizing procedure that can handle all standard data types
interface resize_array
  module procedure resize_double_precision_array, resize_real_array, resize_integer_array, resize_character_array, &
    resize_logical_array
end interface resize_array

interface push_array
  module procedure push_double_precision_array, push_real_array, push_integer_array, push_character_array, &
    push_logical_array
end interface push_array

interface allocatable_size
  module procedure allocatable_double_precision_size, allocatable_real_size, allocatable_integer_size, allocatable_character_size, &
    allocatable_logical_size
end interface allocatable_size

interface copy_array
  module procedure copy_double_precision_array, copy_real_array, copy_integer_array, copy_character_array, &
    copy_logical_array
end interface copy_array

contains

!----------------------------------------------------------------------------

subroutine remove_comments(textline)

character(len=*) :: textline
integer :: cut

cut=scan(textline,'#') ! find the position of the first comment character if it is there
if (cut>0) textline=textline(1:cut-1)//repeat(' ',len(textline)-cut) ! remove comment if there
!if (cut>0) textline=textline(1:cut-1) ! remove comment if there

end subroutine remove_comments

!----------------------------------------------------------------------------

function var_number_from_name(name)

integer :: var_number_from_name, m
character(len=*), intent(in) :: name

var_number_from_name = 0 ! default is an error code

do m=1,ubound(var,1)
  if (trim(var(m)%name) == trim(name)) then
    var_number_from_name = m
    return
  end if
end do

end function var_number_from_name

!----------------------------------------------------------------------------

function compound_number_from_name(name)

integer :: compound_number_from_name, m
character(len=*), intent(in) :: name

compound_number_from_name = 0 ! default is an error code

do m=1,ubound(compound,1)
  if (trim(compound(m)%name) == trim(name)) then
    compound_number_from_name = m
    return
  end if
end do

end function compound_number_from_name

!----------------------------------------------------------------------------

function region_number_from_name(name)

! finds the region number for the region name (no fancy features here anymore since regions implemented within perl)

integer :: region_number_from_name
character(len=*), intent(in) :: name
integer :: n

region_number_from_name = 0 ! default is an error code

! see if region already exists
if (allocated(region)) then
  do n=1,ubound(region,1)
    if (trim(region(n)%name) == trim(name)) then
      region_number_from_name = n
      exit
    end if
  end do
end if

end function region_number_from_name

!----------------------------------------------------------------------------

subroutine resize_node_array(new_element,change,new_size)

! of the allocatable arrays within node, this only copies jface and icell

type(node_type), optional, intent(in) :: new_element
integer, intent(in), optional :: change, new_size
type(node_type), dimension(:), allocatable :: node_old
integer :: old_size, min_size, new_size_l, change_l, k

change_l = 1
if (present(change)) change_l = change

old_size = 0
if (allocated(node)) old_size = ubound(node,1)

if (present(new_size)) then
  if (present(change)) stop 'ERROR: both change and new_size specified in resize_node_array'
  new_size_l = new_size
else
  new_size_l = old_size + change_l
end if
min_size = min(old_size,new_size_l)

! if any data is to be required in the future then save it now
if (min_size>0) then
  allocate(node_old(min_size))
  do k=1,min_size ! also create same allocatable structure within
    if (allocated(node(k)%jface)) allocate(node_old(k)%jface(ubound(node(k)%jface,1)))
    if (allocated(node(k)%icell)) allocate(node_old(k)%icell(ubound(node(k)%icell,1)))
  end do
  node_old=node(1:min_size)
end if

if (old_size>0) deallocate(node)

if (new_size_l>0) then
  allocate(node(new_size_l))
  if (min_size>0) then
    do k=1,min_size ! copy over allocatable structure within
      if (allocated(node_old(k)%jface)) allocate(node(k)%jface(ubound(node_old(k)%jface,1)))
      if (allocated(node_old(k)%icell)) allocate(node(k)%icell(ubound(node_old(k)%icell,1)))
    end do
    node(1:min_size)=node_old
    deallocate(node_old)
  end if
  if (min_size<new_size_l.and.present(new_element)) then
    do k=min_size+1,new_size_l ! copy over allocatable structure within
      if (allocated(new_element%jface)) allocate(node(k)%jface(ubound(new_element%jface,1)))
      if (allocated(new_element%icell)) allocate(node(k)%icell(ubound(new_element%icell,1)))
    end do
    node(min_size+1:new_size_l) = new_element ! initialise new values
  end if
end if

!write(*,*) 'at end of push_node_array with element = '//trim(print_node(node(new_size)))

end subroutine resize_node_array

!----------------------------------------------------------------------------

subroutine resize_face_array(new_element,change,new_size)

! of the allocatable arrays within face, this only copies knode and icell

type(face_type), optional, intent(in) :: new_element
integer, intent(in), optional :: change, new_size
type(face_type), dimension(:), allocatable :: face_old
integer :: old_size, min_size, new_size_l, change_l, j, l

change_l = 1
if (present(change)) change_l = change

old_size = 0
if (allocated(face)) old_size = ubound(face,1)

if (present(new_size)) then
  if (present(change)) stop 'ERROR: both change and new_size specified in resize_face_array'
  new_size_l = new_size
else
  new_size_l = old_size + change_l
end if
min_size = min(old_size,new_size_l)

! if any data is to be required in the future then save it now
if (min_size>0) then
  allocate(face_old(min_size))
  do j=1,min_size ! also create same allocatable structure within
    if (allocated(face(j)%knode)) allocate(face_old(j)%knode(ubound(face(j)%knode,1)))
    if (allocated(face(j)%icell)) allocate(face_old(j)%icell(ubound(face(j)%icell,1)))
    do l = lbound(face(j)%kernel,1), ubound(face(j)%kernel,1)
      if (allocated(face(j)%kernel(l)%ijk)) call error_stop("copying of kernels in resize_face_array untested")
      if (allocated(face(j)%kernel(l)%ijk)) allocate(face_old(j)%kernel(l)%ijk(ubound(face(j)%kernel(l)%ijk,1)))
      if (allocated(face(j)%kernel(l)%v)) allocate(face_old(j)%kernel(l)%v(ubound(face(j)%kernel(l)%v,1)))
    end do
  end do
  face_old=face(1:min_size)
end if

if (old_size>0) deallocate(face)

if (new_size_l>0) then
  allocate(face(new_size_l))
  if (min_size>0) then
    do j=1,min_size ! copy over allocatable structure within
      if (allocated(face_old(j)%knode)) allocate(face(j)%knode(ubound(face_old(j)%knode,1)))
      if (allocated(face_old(j)%icell)) allocate(face(j)%icell(ubound(face_old(j)%icell,1)))
      do l = lbound(face_old(j)%kernel,1), ubound(face_old(j)%kernel,1)
        if (allocated(face_old(j)%kernel(l)%ijk)) allocate(face(j)%kernel(l)%ijk(ubound(face_old(j)%kernel(l)%ijk,1)))
        if (allocated(face_old(j)%kernel(l)%v)) allocate(face(j)%kernel(l)%v(ubound(face_old(j)%kernel(l)%v,1)))
      end do
    end do
    face(1:min_size)=face_old
    deallocate(face_old)
  end if
  if (min_size<new_size_l.and.present(new_element)) then
    do j=min_size+1,new_size_l ! copy over allocatable structure within
      if (allocated(new_element%knode)) allocate(face(j)%knode(ubound(new_element%knode,1)))
      if (allocated(new_element%icell)) allocate(face(j)%icell(ubound(new_element%icell,1)))
      do l = lbound(new_element%kernel,1), ubound(new_element%kernel,1)
        if (allocated(new_element%kernel(l)%ijk)) allocate(face(j)%kernel(l)%ijk(ubound(new_element%kernel(l)%ijk,1)))
        if (allocated(new_element%kernel(l)%v)) allocate(face(j)%kernel(l)%v(ubound(new_element%kernel(l)%v,1)))
      end do
    end do
    face(min_size+1:new_size_l) = new_element ! initialise new values
  end if
end if

end subroutine resize_face_array

!----------------------------------------------------------------------------

subroutine resize_cell_array(new_element,change,new_size)

! of the allocatable arrays within cell, this copies knode, jface and icell

type(cell_type), optional, intent(in) :: new_element
integer, intent(in), optional :: change, new_size
type(cell_type), dimension(:), allocatable :: cell_old
integer :: old_size, min_size, new_size_l, change_l, i, l

change_l = 1
if (present(change)) change_l = change

old_size = 0
if (allocated(cell)) old_size = ubound(cell,1)

if (present(new_size)) then
  if (present(change)) stop 'ERROR: both change and new_size specified in resize_cell_array'
  new_size_l = new_size
else
  new_size_l = old_size + change_l
end if
min_size = min(old_size,new_size_l)

! if any data is to be required in the future then save it now
if (min_size>0) then
  allocate(cell_old(min_size))
  do i=1,min_size ! also create same allocatable structure within
    if (allocated(cell(i)%knode)) allocate(cell_old(i)%knode(ubound(cell(i)%knode,1)))
    if (allocated(cell(i)%jface)) allocate(cell_old(i)%jface(ubound(cell(i)%jface,1)))
    if (allocated(cell(i)%icell)) allocate(cell_old(i)%icell(ubound(cell(i)%icell,1)))
    do l = lbound(cell(i)%kernel,1), ubound(cell(i)%kernel,1)
      if (allocated(cell(i)%kernel(l)%ijk)) call error_stop("copying of kernels in resize_cell_array untested")
      if (allocated(cell(i)%kernel(l)%ijk)) allocate(cell_old(i)%kernel(l)%ijk(ubound(cell(i)%kernel(l)%ijk,1)))
      if (allocated(cell(i)%kernel(l)%v)) allocate(cell_old(i)%kernel(l)%v(ubound(cell(i)%kernel(l)%v,1)))
    end do
  end do
  cell_old=cell(1:min_size)
end if

if (old_size>0) deallocate(cell)

if (new_size_l>0) then
  allocate(cell(new_size_l))
  if (min_size>0) then
    do i=1,min_size ! copy over allocatable structure within
      if (allocated(cell_old(i)%knode)) allocate(cell(i)%knode(ubound(cell_old(i)%knode,1)))
      if (allocated(cell_old(i)%jface)) allocate(cell(i)%jface(ubound(cell_old(i)%jface,1)))
      if (allocated(cell_old(i)%icell)) allocate(cell(i)%icell(ubound(cell_old(i)%icell,1)))
      do l = lbound(cell_old(i)%kernel,1), ubound(cell_old(i)%kernel,1)
        if (allocated(cell_old(i)%kernel(l)%ijk)) allocate(cell(i)%kernel(l)%ijk(ubound(cell_old(i)%kernel(l)%ijk,1)))
        if (allocated(cell_old(i)%kernel(l)%v)) allocate(cell(i)%kernel(l)%v(ubound(cell_old(i)%kernel(l)%v,1)))
      end do
    end do
    cell(1:min_size)=cell_old
    deallocate(cell_old)
  end if
  if (min_size<new_size_l.and.present(new_element)) then
    do i=min_size+1,new_size_l ! copy over allocatable structure within
      if (allocated(new_element%knode)) allocate(cell(i)%knode(ubound(new_element%knode,1)))
      if (allocated(new_element%jface)) allocate(cell(i)%jface(ubound(new_element%jface,1)))
      if (allocated(new_element%icell)) allocate(cell(i)%icell(ubound(new_element%icell,1)))
      do l = lbound(new_element%kernel,1), ubound(new_element%kernel,1)
        if (allocated(new_element%kernel(l)%ijk)) allocate(cell(i)%kernel(l)%ijk(ubound(new_element%kernel(l)%ijk,1)))
        if (allocated(new_element%kernel(l)%v)) allocate(cell(i)%kernel(l)%v(ubound(new_element%kernel(l)%v,1)))
      end do
    end do
    cell(min_size+1:new_size_l) = new_element ! initialise new values
  end if
end if

end subroutine resize_cell_array

!----------------------------------------------------------------------------

subroutine resize_region_array(new_element,change,new_size)

type(region_type), optional, intent(in) :: new_element
integer, intent(in), optional :: change, new_size
type(region_type), dimension(:), allocatable :: region_old
integer :: old_size, min_size, new_size_l, change_l, i

change_l = 1
if (present(change)) change_l = change

old_size = 0
if (allocated(region)) old_size = ubound(region,1)

if (present(new_size)) then
  if (present(change)) stop 'ERROR: both change and new_size specified in resize_region_array'
  new_size_l = new_size
else
  new_size_l = old_size + change_l
end if
min_size = min(old_size,new_size_l)

! if any data is to be required in the future then save it now
if (min_size>0) then
  allocate(region_old(min_size))
  do i=1,min_size ! also create same allocatable structure within
    if (allocated(region(i)%ijk)) allocate(region_old(i)%ijk(ubound(region(i)%ijk,1)))
  end do
  region_old=region(1:min_size)
end if

if (old_size>0) deallocate(region)

if (new_size_l>0) then
  allocate(region(new_size_l))
  if (min_size>0) then
    do i=1,min_size ! copy over allocatable structure within
      if (allocated(region_old(i)%ijk)) allocate(region(i)%ijk(ubound(region_old(i)%ijk,1)))
    end do
    region(1:min_size)=region_old
    deallocate(region_old)
  end if
  if (min_size<new_size_l.and.present(new_element)) then
    do i=min_size+1,new_size_l ! copy over allocatable structure within
      if (allocated(new_element%ijk)) allocate(region(i)%ijk(ubound(new_element%ijk,1)))
    end do
    region(min_size+1:new_size_l) = new_element ! initialise new values
  end if
end if

end subroutine resize_region_array

!----------------------------------------------------------------------------

subroutine resize_sparse_matrix(keep_data,aa,icn,irn,change,new_size)

! here we change the size of a sparse matrix (by change or too new_size) while 
!  possibly (by default) keeping the data

double precision, dimension(:), allocatable :: aa
integer, dimension(:), allocatable :: icn, irn
integer, optional, intent(in) :: change, new_size
logical, optional, intent(in) :: keep_data
logical :: keep_data_l

! by default keep the data in the arrays
keep_data_l = .true.
if (present(keep_data)) then
  if (.not.keep_data) keep_data_l = .false.
end if

if (present(change)) then
  call resize_double_precision_array(keep_data=keep_data_l,array=aa,change=change)
  call resize_integer_array(keep_data=keep_data_l,array=icn,change=change)
  call resize_integer_array(keep_data=keep_data_l,array=irn,change=change)
else if (present(new_size)) then
  call resize_double_precision_array(keep_data=keep_data_l,array=aa,new_size=new_size)
  call resize_integer_array(keep_data=keep_data_l,array=icn,new_size=new_size)
  call resize_integer_array(keep_data=keep_data_l,array=irn,new_size=new_size)
end if

end subroutine resize_sparse_matrix

!----------------------------------------------------------------------------

subroutine resize_double_precision_array(keep_data,array,change,new_size,default_value)

! here we change the size of an array (by change) while maintaining its data

double precision, dimension(:), allocatable :: array, array_store
integer, intent(in), optional :: change, new_size
double precision, intent(in), optional :: default_value
integer :: change_l, old_size, new_size_l, min_size
logical, optional :: keep_data

change_l = 1
if (present(change)) change_l = change

old_size = 0
if (allocated(array)) old_size = ubound(array,1)

if (present(new_size)) then
  if (present(change)) stop 'ERROR: both change and new_size specified in resize_double_precision_array'
  new_size_l = new_size
else
  new_size_l = old_size + change_l
end if

min_size = min(old_size,new_size_l)

! if not keeping data (default is to keep data) then...
if (present(keep_data)) then
  if (.not.keep_data) min_size = 0
end if

! if any data is to be required in the future then save it now
if (min_size>0) then
  allocate(array_store(min_size))
  array_store = array(1:min_size) !bit pedantic - should be OK without indices
end if

!if (old_size>0) deallocate(array)
if (allocated(array)) deallocate(array)

if (new_size_l>0) then
  allocate(array(new_size_l))
  if (min_size>0) then
    array(1:min_size) = array_store !bit pedantic - should be OK without indices
    deallocate(array_store)
  end if
  if (min_size<new_size_l) then
    if (present(default_value)) then
      array(min_size+1:new_size_l) = default_value ! initialise new values
    else
      array(min_size+1:new_size_l) = 0.d0 ! initialise new values
    end if
  end if
end if

end subroutine resize_double_precision_array

!----------------------------------------------------------------------------

subroutine resize_integer_array(keep_data,array,change,new_size,default_value)

! here we change the size of an array (by change or to new_size) while maintaining its data
! if size = 0 then array is left unallocated

integer, dimension(:), allocatable :: array, array_store
integer, intent(in), optional :: change, new_size
integer, intent(in), optional :: default_value
integer :: change_l, old_size, new_size_l, min_size
logical, optional :: keep_data

change_l = 1
if (present(change)) change_l = change

old_size = 0
if (allocated(array)) old_size = ubound(array,1)

if (present(new_size)) then
  if (present(change)) stop 'ERROR: both change and new_size specified in resize_integer_array'
  new_size_l = new_size
else
  new_size_l = old_size + change_l
end if

min_size = min(old_size,new_size_l)

! if not keeping data (default is to keep data) then...
if (present(keep_data)) then
  if (.not.keep_data) min_size = 0
end if

! if any data is to be required in the future then save it now
if (min_size>0) then
  allocate(array_store(min_size))
  array_store = array(1:min_size) !bit pedantic - should be OK without indices
end if

!if (old_size>0) deallocate(array)
if (allocated(array)) deallocate(array)

if (new_size_l>0) then
  allocate(array(new_size_l))
  if (min_size>0) then
    array(1:min_size) = array_store !bit pedantic - should be OK without indices
    deallocate(array_store)
  end if
  if (min_size<new_size_l) then
    if (present(default_value)) then
      array(min_size+1:new_size_l) = default_value ! initialise new values
    else
      array(min_size+1:new_size_l) = 0 ! initialise new values
    end if
  end if
end if

end subroutine resize_integer_array

!----------------------------------------------------------------------------

subroutine resize_character_array(keep_data,array,change,new_size,default_value)

! here we change the size of an array (by change) while maintaining its data

character(len=*), dimension(:), allocatable :: array
character(len=len(array)), dimension(:), allocatable :: array_store
integer, intent(in), optional :: change, new_size
character(len=*), intent(in), optional :: default_value
integer :: change_l, old_size, new_size_l, min_size
logical, optional :: keep_data

change_l = 1
if (present(change)) change_l = change

old_size = 0
if (allocated(array)) old_size = ubound(array,1)

if (present(new_size)) then
  if (present(change)) stop 'ERROR: both change and new_size specified in resize_character_array'
  new_size_l = new_size
else
  new_size_l = old_size + change_l
end if

min_size = min(old_size,new_size_l)

! if not keeping data (default is to keep data) then...
if (present(keep_data)) then
  if (.not.keep_data) min_size = 0
end if

! if any data is to be required in the future then save it now
if (min_size>0) then
  allocate(array_store(min_size))
  array_store = array(1:min_size) !bit pedantic - should be OK without indices
end if

!if (old_size>0) deallocate(array)
if (allocated(array)) deallocate(array)

if (new_size_l>0) then
  allocate(array(new_size_l))
  if (min_size>0) then
    array(1:min_size) = array_store !bit pedantic - should be OK without indices
    deallocate(array_store)
  end if
  if (min_size<new_size_l) then
    if (present(default_value)) then
      array(min_size+1:new_size_l) = default_value ! initialise new values
    else
      array(min_size+1:new_size_l) = '' ! initialise new values
    end if
  end if
end if

end subroutine resize_character_array

!----------------------------------------------------------------------------

subroutine resize_logical_array(keep_data,array,change,new_size,default_value)

! here we change the size of an array (by change) while maintaining its data

logical, dimension(:), allocatable :: array
logical, dimension(:), allocatable :: array_store
integer, intent(in), optional :: change, new_size
logical, intent(in), optional :: default_value
integer :: change_l, old_size, new_size_l, min_size
logical, optional :: keep_data

change_l = 1
if (present(change)) change_l = change

old_size = 0
if (allocated(array)) old_size = ubound(array,1)

if (present(new_size)) then
  if (present(change)) stop 'ERROR: both change and new_size specified in resize_logical_array'
  new_size_l = new_size
else
  new_size_l = old_size + change_l
end if

min_size = min(old_size,new_size_l)

! if not keeping data (default is to keep data) then...
if (present(keep_data)) then
  if (.not.keep_data) min_size = 0
end if

! if any data is to be required in the future then save it now
if (min_size>0) then
  allocate(array_store(min_size))
  array_store = array(1:min_size) ! bit pedantic - should be OK without indices
end if

!if (old_size>0) deallocate(array)
if (allocated(array)) deallocate(array)

if (new_size_l>0) then
  allocate(array(new_size_l))
  if (min_size>0) then
    array(1:min_size) = array_store ! bit pedantic - should be OK without indices
    deallocate(array_store)
  end if
  if (min_size<new_size_l) then
    if (present(default_value)) then
      array(min_size+1:new_size_l) = default_value ! initialise new values
    else
      array(min_size+1:new_size_l) = .false. ! initialise new values
    end if
  end if
end if

end subroutine resize_logical_array

!----------------------------------------------------------------------------

subroutine resize_real_array(keep_data,array,change,new_size,default_value)

! here we change the size of an array (by change) while maintaining its data

real, dimension(:), allocatable :: array, array_store
integer, intent(in), optional :: change, new_size
real, intent(in), optional :: default_value
integer :: change_l, old_size, new_size_l, min_size
logical, optional :: keep_data

change_l = 1
if (present(change)) change_l = change

old_size = 0
if (allocated(array)) old_size = ubound(array,1)

if (present(new_size)) then
  if (present(change)) stop 'ERROR: both change and new_size specified in resize_real_array'
  new_size_l = new_size
else
  new_size_l = old_size + change_l
end if

min_size = min(old_size,new_size_l)

! if not keeping data (default is to keep data) then...
if (present(keep_data)) then
  if (.not.keep_data) min_size = 0
end if

! if any data is to be required in the future then save it now
if (min_size>0) then
  allocate(array_store(min_size))
  array_store = array(1:min_size) !bit pedantic - should be OK without indices
end if

!if (old_size>0) deallocate(array)
if (allocated(array)) deallocate(array)

if (new_size_l>0) then
  allocate(array(new_size_l))
  if (min_size>0) then
    array(1:min_size) = array_store !bit pedantic - should be OK without indices
    deallocate(array_store)
  end if
  if (min_size<new_size_l) then
    if (present(default_value)) then
      array(min_size+1:new_size_l) = default_value ! initialise new values
    else
      array(min_size+1:new_size_l) = 0.e0 ! initialise new values
    end if
  end if
end if

end subroutine resize_real_array

!----------------------------------------------------------------------------

subroutine push_character_array(array,new_element,reverse)

! add element to an array, by default at the end, or with reverse to the start
character(len=*), dimension(:), allocatable :: array
character(len=*) :: new_element
logical, optional :: reverse

call resize_character_array(array=array,change=1)
array(ubound(array,1))=new_element

! if reverse is specified then add the element to the beginning of the array rather than the start
if (present(reverse)) then
  if (reverse) then
! now using intrinsic function
    array=cshift(array=array,shift=-1) ! this is an intrinsic function that will shift elements to the right (to the left -1 places).  It is a circular shift so the newly added element at the far right will end up at the front, as required
!   do n = ubound(array,1)-1, 1, -1
!     array(n+1) = array(n)
!   end do
!   array(1) = new_element
  end if
end if

end subroutine push_character_array

!----------------------------------------------------------------------------

subroutine push_logical_array(array,new_element,reverse)

! add element to an array, by default at the end, or with reverse to the start
logical, dimension(:), allocatable :: array
logical :: new_element
logical, optional :: reverse

call resize_logical_array(array=array,change=1)
array(ubound(array,1))=new_element

! if reverse is specified then add the element to the beginning of the array rather than the start
if (present(reverse)) then
  if (reverse) then
! now using intrinsic function
    array=cshift(array=array,shift=-1) ! this is an intrinsic function that will shift elements to the right (to the left -1 places).  It is a circular shift so the newly added element at the far right will end up at the front, as required
!   do n = ubound(array,1)-1, 1, -1
!     array(n+1) = array(n)
!   end do
!   array(1) = new_element
  end if
end if

end subroutine push_logical_array

!----------------------------------------------------------------------------

subroutine shift_integer_array(array,new_element)

! add element to the start of an array, but as opposed to push
! 1) does not change sie of array, unless required to expand
! 2) goes from front rather than back by default
! assumes that a value of 0 is empty, and that array already has atleast one allocated element
integer, dimension(:), allocatable :: array
integer :: new_element

if (array(ubound(array,1)) /= 0) then ! if we need to expand array then use push routine
  call push_integer_array(array=array,new_element=new_element,reverse=.true.)
else
  array=eoshift(array=array,shift=-1,boundary=new_element) ! this is an intrinsic function that will shift elements to the right (to the left -1 places).  Default new value is zero
end if

end subroutine shift_integer_array

!----------------------------------------------------------------------------

subroutine unshift_integer_array(array)

! remove element from the start of an array, by moving everything to the left one place
! here array dimensions do not change, with far right value becoming 0
integer, dimension(:), allocatable :: array

array=eoshift(array=array,shift=+1) ! performs shift to the left, making far right value 0 by default

end subroutine unshift_integer_array

!----------------------------------------------------------------------------

subroutine push_integer_array(array,new_element,reverse)

! push a new element on to the back (or front, if reverse is specified) of an array

integer, dimension(:), allocatable :: array
integer :: new_element
logical, optional :: reverse

call resize_integer_array(array=array,change=1)
array(ubound(array,1))=new_element

! if reverse is specified then add the element to the beginning of the array rather than the start
if (present(reverse)) then
  if (reverse) then
! now using intrinsic function
    array=cshift(array=array,shift=-1) ! this is an intrinsic function that will shift elements to the right (to the left -1 places).  It is a circular shift so the newly added element at the far right will end up at the front, as required
!   do n = ubound(array,1)-1, 1, -1
!     array(n+1) = array(n)
!   end do
!   array(1) = new_element
  end if
end if

end subroutine push_integer_array

!----------------------------------------------------------------------------

subroutine push_double_precision_array(array,new_element,reverse)

double precision, dimension(:), allocatable :: array
double precision :: new_element
logical, optional :: reverse

call resize_double_precision_array(array=array,change=1)
array(ubound(array,1))=new_element

! if reverse is specified then add the element to the beginning of the array rather than the start
if (present(reverse)) then
  if (reverse) then
! now using intrinsic function
    array=cshift(array=array,shift=-1) ! this is an intrinsic function that will shift elements to the right (to the left -1 places).  It is a circular shift so the newly added element at the far right will end up at the front, as required
!   do n = ubound(array,1)-1, 1, -1
!     array(n+1) = array(n)
!   end do
!   array(1) = new_element
  end if
end if

end subroutine push_double_precision_array

!----------------------------------------------------------------------------

subroutine push_real_array(array,new_element,reverse)

real, dimension(:), allocatable :: array
real :: new_element
logical, optional :: reverse

call resize_real_array(array=array,change=1)
array(ubound(array,1))=new_element

! if reverse is specified then add the element to the beginning of the array rather than the start
if (present(reverse)) then
  if (reverse) then
! now using intrinsic function
    array=cshift(array=array,shift=-1) ! this is an intrinsic function that will shift elements to the right (to the left -1 places).  It is a circular shift so the newly added element at the far right will end up at the front, as required
!   do n = ubound(array,1)-1, 1, -1
!     array(n+1) = array(n)
!   end do
!   array(1) = new_element
  end if
end if

end subroutine push_real_array

!----------------------------------------------------------------------------

function allocatable_integer_size(array)

! find size of integer array that may or may not be allocated

integer, dimension(:), allocatable :: array
integer :: allocatable_integer_size

if (allocated(array)) then
  allocatable_integer_size = size(array)
else
  allocatable_integer_size = 0
end if

end function allocatable_integer_size

!----------------------------------------------------------------------------

function allocatable_double_precision_size(array)

! find size of double precision array that may or may not be allocated

double precision, dimension(:), allocatable :: array
integer :: allocatable_double_precision_size

if (allocated(array)) then
  allocatable_double_precision_size = size(array)
else
  allocatable_double_precision_size = 0
end if

end function allocatable_double_precision_size

!----------------------------------------------------------------------------

function allocatable_real_size(array)

! find size of real array that may or may not be allocated

real, dimension(:), allocatable :: array
integer :: allocatable_real_size

if (allocated(array)) then
  allocatable_real_size = size(array)
else
  allocatable_real_size = 0
end if

end function allocatable_real_size

!----------------------------------------------------------------------------

function allocatable_character_size(array)

! find size of character array that may or may not be allocated

character(len=*), dimension(:), allocatable :: array
integer :: allocatable_character_size

if (allocated(array)) then
  allocatable_character_size = size(array)
else
  allocatable_character_size = 0
end if

end function allocatable_character_size

!----------------------------------------------------------------------------

function allocatable_logical_size(array)

! find size of logical array that may or may not be allocated

logical, dimension(:), allocatable :: array
integer :: allocatable_logical_size

if (allocated(array)) then
  allocatable_logical_size = size(array)
else
  allocatable_logical_size = 0
end if

end function allocatable_logical_size

!----------------------------------------------------------------------------

function print_node(k,compact)

integer :: k ! knode index
logical, optional :: compact
logical :: compact_l
character(len=1000) :: print_node
integer :: n, error, njface, nicell, nglue_knode, nr, nreflect_multiplier
character(len=1000) :: formatline

compact_l = .false.
if (present(compact)) compact_l = compact

if (compact_l) then

  formatline = '(a,'//trim(indexformat)//',a,i1,a,'//repeat(',a,g12.5',totaldimensions)//')'
  write(print_node,fmt=formatline,iostat=error) 'k = ',k,'k: type = ',node(k)%type, &
    ': x =',(' ',node(k)%x(n),n=1,totaldimensions)

else

  njface = allocatable_integer_size(node(k)%jface)
  nicell = allocatable_integer_size(node(k)%icell)
  nglue_knode = allocatable_integer_size(node(k)%glue_knode)
  nr = 0
  if (allocated(node(k)%r)) nr = ubound(node(k)%r,2)
  nreflect_multiplier = 0
  if (allocated(node(k)%reflect_multiplier)) nreflect_multiplier = ubound(node(k)%reflect_multiplier,2)

  formatline = '(a,'//trim(indexformat)//',a,i1,a'//repeat(',a,g12.5',totaldimensions)// &
    ',a'//repeat(',a,'//trim(indexformat),njface)// &
    ',a'//repeat(',a,'//trim(indexformat),nicell)// &
    ',a'//repeat(',a,'//trim(indexformat),nglue_knode)// &
    ',a,l1'// &
    ',a'//repeat(',a,3(1x,'//trim(compactformat)//'),a',nr)// &
    ',a,l1'// &
    ',a'//repeat(',a,3(1x,i2),a',nreflect_multiplier)//')'

  write(print_node,fmt=formatline,iostat=error) 'k = ',k,'k: type = ',node(k)%type, &
    ': x =',(' ',node(k)%x(n),n=1,totaldimensions), &
    ': jface =',(' ',node(k)%jface(n),n=1,njface), &
    ': icell =',(' ',node(k)%icell(n),n=1,nicell), &
    ': glue_knode =',(' ',node(k)%glue_knode(n),n=1,nglue_knode), &
    ': glue_present = ',node(k)%glue_present, &
    ': r =',(' [',node(k)%r(:,n),']',n=1,nr), &
    ': reflect_present = ',node(k)%reflect_present, &
    ': reflect_multiplier =',(' [',node(k)%reflect_multiplier(:,n),']',n=1,nreflect_multiplier)

end if

if (error /= 0) print_node = 'ERROR: problem in print_node: formatline = '//trim(formatline)

end function print_node

!----------------------------------------------------------------------------

function print_face(j,compact)

integer :: j ! jface index
logical, optional :: compact
logical :: compact_l
character(len=10000) :: print_face, rup, rdown
integer :: n, error, nknode, nicell, l, nr, nreflect_multiplier
character(len=10000) :: formatline

compact_l = .false.
if (present(compact)) compact_l = compact

if (compact_l) then

  formatline = '(a,'//trim(indexformat)//',a,i1,a,i1,a,i2,a,'//repeat(',a,g12.5',totaldimensions)//')'
  write(print_face,fmt=formatline,iostat=error) 'j = ',j,'j: type = ',face(j)%type, &
    ': dimensions = ',face(j)%dimensions, &
    ': gtype = ',face(j)%gtype, &
    ': x =',(' ',face(j)%x(n),n=1,totaldimensions)

else

  nknode = allocatable_integer_size(face(j)%knode)
  nicell = allocatable_integer_size(face(j)%icell)
  nr = 0
  rdown = ' not allocated'
  rup = ' not allocated'
  if (allocated(face(j)%r)) then
    nr = ubound(face(j)%r,2)
    formatline = '(3(1x,'//trim(compactformat)//'))'
    if (nr >= 1) write(rdown,fmt=formatline) face(j)%r(:,1)
    if (nr >= 2) write(rup,fmt=formatline) face(j)%r(:,1)
  end if
  nreflect_multiplier = 0
  if (allocated(face(j)%reflect_multiplier)) nreflect_multiplier = ubound(face(j)%reflect_multiplier,2)

  formatline = '(a,'//trim(indexformat)//',a,i1,a,i1,a,i2,a,'//trim(indexformat)// &
    ',a,i1,2(a'//repeat(',a,g12.5',totaldimensions)// &
    '),a,g12.5,a,g12.5'// &
    repeat(',a,i1,a'//repeat(',a,g12.5',totaldimensions),totaldimensions)// &
    ',a'//repeat(',a,'//trim(indexformat),nknode)// &
    ',a'//repeat(',a,'//trim(indexformat),nicell)// &
    ',a,l1'// &
    ',a'//repeat(',a,3(1x,'//trim(compactformat)//'),a',nr)// &
    ',a,l1'// &
    ',a'//repeat(',a,3(1x,i2),a',nreflect_multiplier)//')'

  write(print_face,fmt=formatline,iostat=error) 'j = ',j,'j: type = ',face(j)%type, &
    ': dimensions = ',face(j)%dimensions, &
    ': gtype = ',face(j)%gtype, &
    ': glue_jface = ',face(j)%glue_jface, &
    ': glue_reflect = ',face(j)%glue_reflect, &
    ': x =',(' ',face(j)%x(n),n=1,totaldimensions), &
    ': dx_unit =',(' ',face(j)%dx_unit(n),n=1,totaldimensions), &
    ': r(1),dx_down ='//trim(rdown)// &
    ': r(2),dx_up ='//trim(rup)// &
    ': area = ',face(j)%area,': dx = ',face(j)%dx, &
    (': norm(',l,') =',(' ',face(j)%norm(n,l),n=1,totaldimensions),l=1,totaldimensions), &
    ': knode =',(' ',face(j)%knode(n),n=1,nknode), &
    ': icell =',(' ',face(j)%icell(n),n=1,nicell), &
    ': glue_present = ',face(j)%glue_present, &
    ': r =',(' [',face(j)%r(:,n),']',n=1,nr), &
    ': reflect_present = ',face(j)%reflect_present, &
    ': reflect_multiplier =',(' [',face(j)%reflect_multiplier(:,n),']',n=1,nreflect_multiplier)

end if

if (error /= 0) print_face = 'ERROR: problem in print_face: formatline = '//trim(formatline)

end function print_face

!----------------------------------------------------------------------------

function print_cell(i,compact)

integer :: i ! icell index
logical, optional :: compact
logical :: compact_l
character(len=10000) :: print_cell
integer :: n, njface, error, nknode, nicell, nr, nreflect_multiplier
character(len=10000) :: formatline

compact_l = .false.
if (present(compact)) compact_l = compact

if (compact_l) then

  formatline = '(a,'//trim(indexformat)//',a,i1,a,i1,a,i2,a,'//repeat(',a,g12.5',totaldimensions)//')'
  write(print_cell,fmt=formatline,iostat=error) 'i = ',i,'i: type = ',cell(i)%type, &
    ': dimensions = ',cell(i)%dimensions, &
    ': gtype = ',cell(i)%gtype, &
    ': x =',(' ',cell(i)%x(n),n=1,totaldimensions)

else

  nknode = allocatable_integer_size(cell(i)%knode)
  njface = allocatable_integer_size(cell(i)%jface)
  nicell = allocatable_integer_size(cell(i)%icell)
  nr = 0
  if (allocated(cell(i)%r)) nr = ubound(cell(i)%r,2)
  nreflect_multiplier = 0
  if (allocated(cell(i)%reflect_multiplier)) nreflect_multiplier = ubound(cell(i)%reflect_multiplier,2)

  formatline = '(a,'//trim(indexformat)//',a,i1,a,i1,a,i2,a'//repeat(',a,g12.5',totaldimensions)// &
    ',a,g12.5,a,i1'// &
    ',a'//repeat(',a,'//trim(indexformat),nknode)// &
    ',a'//repeat(',a,'//trim(indexformat),njface)// &
    ',a'//repeat(',a,'//trim(indexformat),nicell)// &
    ',a,l1'// &
    ',a'//repeat(',a,3(1x,'//trim(compactformat)//'),a',nr)// &
    ',a,l1'// &
    ',a'//repeat(',a,3(1x,i2),a',nreflect_multiplier)//')'

  write(print_cell,fmt=formatline,iostat=error) 'i = ',i,'i: type = ',cell(i)%type, &
    ': dimensions = ',cell(i)%dimensions, &
    ': gtype = ',cell(i)%gtype, &
    ': x =',(' ',cell(i)%x(n),n=1,totaldimensions), &
    ': vol = ', cell(i)%vol, ': njface = ',njface, &
    ': knode =',(' ',cell(i)%knode(n),n=1,nknode), &
    ': jface =',(' ',cell(i)%jface(n),n=1,njface), &
    ': icell =',(' ',cell(i)%icell(n),n=1,nicell), &
    ': glue_present = ',cell(i)%glue_present, &
    ': r =',(' [',cell(i)%r(:,n),']',n=1,nr), &
    ': reflect_present = ',cell(i)%reflect_present, &
    ': reflect_multiplier =',(' [',cell(i)%reflect_multiplier(:,n),']',n=1,nreflect_multiplier)

end if

if (error /= 0) print_cell = 'ERROR: problem in print_cell: formatline = '//trim(formatline)

end function print_cell

!----------------------------------------------------------------------------

subroutine add_to_dv(thread,funke,derivative_multiplier,funka)

! derivative_multiplierative info from dv and pp are added to general variable container
! now elements are placed in ascending pp order with no duplicates

double precision :: derivative_multiplier ! factor by which dv data is multiplied as it is added
type(funk_type) :: funke ! existing funk container that will have funka dv data added to it
type(funk_type) :: funka ! container that has dv data to add to the funke
!type(funk_type) :: funkt ! funk container which is used to assemble combined funk
! funkt is now defined globally, one for each thread
integer na, ne, n, thread
logical, parameter :: debug = .false.

if (debug) write(90,'(80(1h+)/a)') 'function add_to_dv'

if (debug) write(90,*) 'thread = ',thread
if (debug) write(90,*) 'derivative_multiplier = ',derivative_multiplier
if (derivative_multiplier == 0.d0) return
if (debug) write(90,*) 'funka%ndv = ',funka%ndv
if (funka%ndv == 0) return

! make sure funkt(thread) is large enough - base size on no duplication of elements
if (.not.allocated(funkt(thread)%pp)) then
  if (debug) write(90,*) 'allocating funkt(thread) with size ',funka%ndv+funke%ndv
  allocate(funkt(thread)%pp(funka%ndv+funke%ndv),funkt(thread)%dv(funka%ndv+funke%ndv))
else if (ubound(funkt(thread)%pp,1) < funka%ndv+funke%ndv) then
  deallocate(funkt(thread)%pp,funkt(thread)%dv)
  if (debug) write(90,*) 'reallocating funkt(thread) with size ',funka%ndv+funke%ndv
  allocate(funkt(thread)%pp(funka%ndv+funke%ndv),funkt(thread)%dv(funka%ndv+funke%ndv))
end if

if (debug) then
  write(90,*) 'adding funka: funka%ndv = ',funka%ndv
  if (funka%ndv > 0) then
    write(90,'(100(a,i8))') (' ',funka%pp(n),n=1,funka%ndv)
    write(90,'(100(a,g9.2))') (' ',derivative_multiplier*funka%dv(n),n=1,funka%ndv)
  end if
  write(90,*) 'existing funke: funke%ndv = ',funke%ndv
  if (funke%ndv > 0) then
    write(90,'(100(a,i8))') (' ',funke%pp(n),n=1,funke%ndv)
    write(90,'(100(a,g9.2))') (' ',funke%dv(n),n=1,funke%ndv)
  end if
! if (allocated(funke%pp)) then
!   write(90,'(100(a,i8))') (' ',funke%pp(n),n=1,funke%ndv)
!   write(90,'(100(a,g9.2))') (' ',funke%dv(n),n=1,funke%ndv)
! else
!   write(90,*) 'existing funke not allocated'
! end if
end if

! cycle through elements of both funke and funka, adding them to funkt(thread) in ascending order

ne = 1 ! next read position in funke
na = 1 ! next read position in funka
funkt(thread)%ndv = 0 ! last write position in funkt(thread)

do
  if (na > funka%ndv) then ! we've come to the end of funka - add on remaining funke elements
    funkt(thread)%pp(funkt(thread)%ndv+1:funkt(thread)%ndv+1+funke%ndv-ne) = funke%pp(ne:funke%ndv)
    funkt(thread)%dv(funkt(thread)%ndv+1:funkt(thread)%ndv+1+funke%ndv-ne) = funke%dv(ne:funke%ndv)
    funkt(thread)%ndv = funkt(thread)%ndv+1+funke%ndv-ne
!   ne = funke%ndv + 1
    if (debug) write(90,*) 'adding on final funke elements: na = ',na
    exit
  else if (ne > funke%ndv) then ! we've come to the end of funke - add on remaining funka elements
    funkt(thread)%pp(funkt(thread)%ndv+1:funkt(thread)%ndv+1+funka%ndv-na) = funka%pp(na:funka%ndv)
    funkt(thread)%dv(funkt(thread)%ndv+1:funkt(thread)%ndv+1+funka%ndv-na) = derivative_multiplier*funka%dv(na:funka%ndv)
    funkt(thread)%ndv = funkt(thread)%ndv+1+funka%ndv-na
!   na = funka%ndv + 1
    if (debug) write(90,*) 'adding on final funka elements: ne = ',ne
    exit
  else if (funka%pp(na) < funke%pp(ne)) then ! funka comes next
    funkt(thread)%ndv = funkt(thread)%ndv + 1
    funkt(thread)%pp(funkt(thread)%ndv:funkt(thread)%ndv) = funka%pp(na)
    funkt(thread)%dv(funkt(thread)%ndv:funkt(thread)%ndv) = derivative_multiplier*funka%dv(na)
    na = na + 1
  else if (funke%pp(ne) < funka%pp(na)) then ! funke comes next
    funkt(thread)%ndv = funkt(thread)%ndv + 1
    funkt(thread)%pp(funkt(thread)%ndv:funkt(thread)%ndv) = funke%pp(ne)
    funkt(thread)%dv(funkt(thread)%ndv:funkt(thread)%ndv) = funke%dv(ne)
    ne = ne + 1
  else ! funke and funka have same pp element next
    funkt(thread)%ndv = funkt(thread)%ndv + 1
    funkt(thread)%pp(funkt(thread)%ndv:funkt(thread)%ndv) = funke%pp(ne)
    funkt(thread)%dv(funkt(thread)%ndv:funkt(thread)%ndv) = funke%dv(ne) + derivative_multiplier*funka%dv(na)
    na = na + 1
    ne = ne + 1
    if (debug) write(90,*) 'duplicate elements: ne = ',ne,': na = ',na
  end if
end do
    
if (debug) write(90,'(3(a,i10))') 'thread = ',thread,': ne = ',ne,': na =',na
if (debug) write(90,*) 'all elements added to funkt(thread): funkt(thread)%ndv = ',funkt(thread)%ndv

! now copy funtk back to funke
if (.not.allocated(funke%pp)) then
  if (debug) write(90,*) 'allocating funke with size ',funkt(thread)%ndv
  allocate(funke%pp(funkt(thread)%ndv),funke%dv(funkt(thread)%ndv))
else if (ubound(funke%pp,1) < funkt(thread)%ndv) then
  deallocate(funke%pp,funke%dv)
  if (debug) write(90,*) 'reallocating funke with size ',funkt(thread)%ndv
  allocate(funke%pp(funkt(thread)%ndv),funke%dv(funkt(thread)%ndv))
end if

if (.false.) then ! copy over wholeus boleus
  funke%ndv = funkt(thread)%ndv 
  funke%pp(1:funke%ndv) = funkt(thread)%pp(1:funke%ndv)
  funke%dv(1:funke%ndv) = funkt(thread)%dv(1:funke%ndv)
else ! copy over elements whose magnitude is larger than eps_dv
  funke%ndv = 0
  do n = 1, funkt(thread)%ndv
    if (abs(funkt(thread)%dv(n)) > eps_dv) then
      funke%ndv = funke%ndv + 1
      funke%pp(funke%ndv) = funkt(thread)%pp(n)
      funke%dv(funke%ndv) = funkt(thread)%dv(n)
    end if
  end do
end if

if (debug) then
  write(90,*) 'final funke: funke%ndv = ',funke%ndv
  write(90,'(3(a,i10))') 'thread = ',thread,': ne = ',ne,': na =',na
  write(90,'(100(a,i8))') (' ',funke%pp(n),n=1,funke%ndv)
  write(90,'(100(a,g9.2))') (' ',funke%dv(n),n=1,funke%ndv)
end if

if (debug) write(90,'(a/80(1h-))') 'function add_to_dv'

end subroutine add_to_dv

!----------------------------------------------------------------------------

subroutine multiply_dv(funke,derivative_multiplier)

! derivative_multiplieratives dv in funke are multiplied by scalar derivative_multiplier

double precision :: derivative_multiplier ! factor by which dv data is multiplied
type(funk_type) :: funke ! existing funk container
integer :: n
logical, parameter :: debug = .false.

if (debug) write(*,'(80(1h+)/a)') 'function multiply_dv'

if (debug) then
  write(*,*) 'initial funke: funke%ndv = ',funke%ndv
  write(*,'(100(a,i8))') (' ',funke%pp(n),n=1,funke%ndv)
  write(*,'(100(a,g9.2))') (' ',funke%dv(n),n=1,funke%ndv)
  write(*,*) 'derivative_multiplier = ',derivative_multiplier
end if

if (allocated(funke%dv).and.funke%ndv > 0) funke%dv(1:funke%ndv) = funke%dv(1:funke%ndv)*derivative_multiplier

if (debug) then
  write(*,*) 'final funke: funke%ndv = ',funke%ndv
  write(*,'(100(a,i8))') (' ',funke%pp(n),n=1,funke%ndv)
  write(*,'(100(a,g9.2))') (' ',funke%dv(n),n=1,funke%ndv)
end if

if (debug) write(*,'(a/80(1h-))') 'function multiply_dv'

end subroutine multiply_dv

!----------------------------------------------------------------------------

subroutine reset_funk(funkl)

! info in funk structure is reset

type(funk_type) :: funkl

funkl%v = 0.d0
funkl%ndv = 0
!if (allocated(funkl%dv)) funkl%dv(:) = 0.d0 ! not strictly necessary
!if (allocated(funkl%pp)) funkl%pp(:) = 0 ! not strictly necessary

end subroutine reset_funk

!----------------------------------------------------------------------------

subroutine clear_funk_derivatives(funkl)

! derivatives within funk are cleared

type(funk_type) :: funkl

funkl%ndv = 0

end subroutine clear_funk_derivatives

!----------------------------------------------------------------------------

function print_funk(funkl)

character(len=1000) :: print_funk, print_funk_saved
type(funk_type) :: funkl ! local funk variable
integer :: error, nn
character(len=1000) :: formatline

formatline = '(a,g12.5,a,i2)'
write(print_funk,fmt=formatline,iostat=error) 'v = ',funkl%v,': ndv = ',funkl%ndv
if (error /= 0) print_funk = print_funk//'ERROR: 1: problem in print_funk: formatline = '//trim(formatline)

print_funk_saved = print_funk
if (allocated(funkl%pp)) then
  formatline = '(a'//repeat(',a,i8',ubound(funkl%pp,1))//')'
  write(print_funk,fmt=formatline,iostat=error) trim(print_funk_saved)//': pp =',(' ',funkl%pp(nn),nn=1,ubound(funkl%pp,1))
  if (error /= 0) print_funk = trim(print_funk_saved)//'ERROR: 2: problem in print_funk: formatline = '//trim(formatline)
else
  formatline = '(a)'
  write(print_funk,fmt=formatline,iostat=error) trim(print_funk_saved)//': pp not allocated'
  if (error /= 0) print_funk = trim(print_funk_saved)//'ERROR: 3: problem in print_funk: formatline = '//trim(formatline)
end if

print_funk_saved = print_funk
if (allocated(funkl%dv)) then
  formatline = '(a'//repeat(',a,g12.5',ubound(funkl%dv,1))//')'
  write(print_funk,fmt=formatline,iostat=error) trim(print_funk_saved)//': dv =',(' ',funkl%dv(nn),nn=1,ubound(funkl%dv,1))
  if (error /= 0) print_funk = trim(print_funk_saved)//'ERROR: 4: problem in print_funk: formatline = '//trim(formatline)
else
  formatline = '(a)'
  write(print_funk,fmt=formatline,iostat=error) trim(print_funk_saved)//': dv not allocated'
  if (error /= 0) print_funk = trim(print_funk_saved)//'ERROR: 5: problem in print_funk: formatline = '//trim(formatline)
end if

end function print_funk

!----------------------------------------------------------------------------

function get_const(name)

character(len=*) :: name
integer :: m
double precision :: get_const

m = var_number_from_name(name)
if (m == 0.or.trim(var(m)%type) /= "constant") then
  write(*,*) 'ERROR: a constant variable with name '//trim(name)//' was not found in get_const'
  stop
end if
if (trim(var(m)%centring) /= "none") then
  write(*,*) 'ERROR: the constant variable '//trim(name)//' does not have none centring in get_const'
  stop
end if
get_const = var(m)%funk(1)%v

end function get_const

!----------------------------------------------------------------------------

function distance(x1,x2)

! this function finds distance between two points

!integer :: n
double precision, dimension(:) :: x1, x2
double precision :: distance

! write(*,*) 'in distance with: x1 = ',x1,': x2 = ',x2
! distance = 0.d0
! do n = 1, totaldimensions
!   distance = distance + (x1(n)-x2(n))**2
!   write(*,*) 'n = ',n,': distance = ',distance,' x1(n) = ',x1(n),': x2(n) = ',x2(n)
! end do
! distance = sqrt(distance)

distance = vector_magnitude(x1-x2) ! use fortran intrinsic

end function distance

!----------------------------------------------------------------------------

function cross_product(x1,x2)

! this function finds cross product between two vectors

double precision, dimension(:), intent(in) :: x1, x2
double precision, dimension(totaldimensions) :: cross_product

if (ubound(x1,1) /= totaldimensions .or. ubound(x2,1) /= totaldimensions) stop &
  'ERROR: x1 or x2 dimensions inappropriate in function cross_product'
cross_product(1) = x1(2) * x2(3) - x1(3) * x2(2)
cross_product(2) = x1(3) * x2(1) - x1(1) * x2(3)
cross_product(3) = x1(1) * x2(2) - x1(2) * x2(1)

end function cross_product

!----------------------------------------------------------------------------

pure function vector_magnitude(x)

! this function finds magnitude of a vector

double precision, dimension(:), intent(in) :: x
double precision :: vector_magnitude

vector_magnitude = sqrt(dot_product(x,x))

end function vector_magnitude

!----------------------------------------------------------------------------

subroutine normalise_vector(vector,error)

! this function normalises the length of vector

double precision, dimension(:) :: vector
logical, optional, intent(out) :: error
double precision :: length

length = vector_magnitude(vector)
if (length > tinyish) then
  vector = vector/length
  if (present(error)) error = .false.
else
  vector = 0.d0
  if (present(error)) then
    error = .true.
  else
    write(*,*) 'WARNING: vector of zero length found in normalise_vector'
  end if
end if
  
end subroutine normalise_vector

!-----------------------------------------------------------------

subroutine set_face(face_to_set,new_value)

! set the face in face_to_set to the new_value, including shape
type(face_type), intent(in) :: new_value
type(face_type), intent(inout) :: face_to_set
integer :: l

! make shape of allocatables in face_to_set equal to shape of new_value
! icell
call resize_integer_array(keep_data=.false.,array=face_to_set%icell,new_size=allocatable_size(new_value%icell))
! knode
call resize_integer_array(keep_data=.false.,array=face_to_set%knode,new_size=allocatable_size(new_value%knode))

! copy kernels, including shape
do l = lbound(new_value%kernel,1), ubound(new_value%kernel,1)
  call copy_kernel(original=new_value%kernel(l),copy=face_to_set%kernel(l))
end do

! now set values (kernel values get set twice)
face_to_set = new_value

end subroutine set_face

!-----------------------------------------------------------------

subroutine set_cell(cell_to_set,new_value)

! set the cell in cell_to_set to the new_value, including shape
type(cell_type), intent(in) :: new_value
type(cell_type), intent(inout) :: cell_to_set
integer :: l

! make shape of allocatables in cell_to_set equal to shape of new_value
! knode
call resize_integer_array(keep_data=.false.,array=cell_to_set%knode,new_size=allocatable_size(new_value%knode))
! jface
call resize_integer_array(keep_data=.false.,array=cell_to_set%jface,new_size=allocatable_size(new_value%jface))
! icell
call resize_integer_array(keep_data=.false.,array=cell_to_set%icell,new_size=allocatable_size(new_value%icell))

! copy kernels, including shape
do l = lbound(new_value%kernel,1), ubound(new_value%kernel,1)
  call copy_kernel(original=new_value%kernel(l),copy=cell_to_set%kernel(l))
end do

! now set values (kernel values get set twice)
cell_to_set = new_value

end subroutine set_cell

!-----------------------------------------------------------------

function jface_from_knode_list(node_list)

integer :: j, jj, k, kk, jc, jjc
integer, dimension(:), allocatable :: node_list
integer :: jface_from_knode_list
logical, parameter :: debug = .false.

if (debug) write(*,'(80(1h+)/a)') 'function jface_from_knode_list'

if (debug) write(*,*) 'node_list = ',node_list

jface_from_knode_list = 0

if (.not.allocated(node(node_list(1))%jface)) return

face_loop: do jj = 1, ubound(node(node_list(1))%jface,1) ! loop through all faces that are associated with the first node
  j = node(node_list(1))%jface(jj)

  node_loop: do kk = 2, ubound(node_list,1)
    k = node_list(kk)

    if (.not.allocated(node(k)%jface)) return ! if these indices aren't even allocated, face must be new

    compare_face_loop: do jjc = 1, ubound(node(k)%jface,1)
      jc = node(k)%jface(jjc)

      if (jc == j) cycle node_loop ! face appears in both node lists so check next node

    end do compare_face_loop

    cycle face_loop ! here the original face was not found so move on to next one

  end do node_loop

  jface_from_knode_list = j ! j has been found in every node in the list - it must be the face

end do face_loop
  


! !   if (debug) write(*,*) 'kk = ',kk,': k = ',k
!     node_list_loop: do kkl = 1, ubound(node_list,1)
!       kl = node_list(kkl)
! !     if (debug) write(*,*) 'kkl = ',kkl,': kl = ',kl
!       if (k == kl) cycle node_loop ! if we find the node, move onto next node for face
! !     if (k == kl) then
! !       if (debug) write(*,*) 'matched face(j)%node (k) to node_list (kl): kk = ',kk,': k = ',k,': kkl = ',kkl,': kl = ',kl,': j = ',j
! !       cycle node_loop ! if we find the node, move onto next node for face
! !     end if
!     end do node_list_loop
! !   if (debug) write(*,*) 'did not find match for face%node node: kk = ',kk,': k = ',k,': j = ',j
!     cycle face_loop ! if we are here then the node is not present in this face, so move on to next face
!     stop 'ERROR: should not be here'
!   end do node_loop
!   jface_from_knode_list = j ! if we are here then all nodes matched - so face matches
!   exit face_loop
! end do face_loop

if (debug) then
  if (jface_from_knode_list == 0) then
    write(*,*) 'did not find face: jface_from_knode_list = ',jface_from_knode_list
  else
    write(*,*) 'found face: jface_from_knode_list = ',jface_from_knode_list,': face%knode = ',face(jface_from_knode_list)%knode
  end if
end if

if (debug) write(*,'(a/80(1h+))') 'function jface_from_knode_list'

end function jface_from_knode_list

! !-----------------------------------------------------------------

! function icell_from_jface_list(face_list)

! integer :: i, ii, k, kk, ic, iic
! integer, dimension(:), allocatable :: face_list
! integer :: icell_from_jface_list
! logical, parameter :: debug = .false.

! if (debug) write(*,'(80(1h+)/a)') 'function icell_from_jface_list'

! if (debug) write(*,*) 'face_list = ',face_list

! icell_from_jface_list = 0

! if (.not.allocated(face_list)) return

! cell_loop: do ii = 1, 2 ! loop through all cells that are associated with the first face
!   i = face(face_list(1))%icell(ii)
!   if (i == 0) cycle cell_loop

!   face_loop: do kk = 2, ubound(face_list,1)
!     k = face_list(kk)

!     compare_cell_loop: do iic = 1, 2
!       ic = face(k)%icell(iic)
!       
!       if (ic == i) cycle face_loop ! cell appears in both face lists so check next face

!     end do compare_cell_loop

!     cycle cell_loop ! here the original cell was not found so move on to next one

!   end do face_loop

!   icell_from_jface_list = i ! i has been found in every face in the list - it must be the cell

! end do cell_loop

! ! cell_loop: do i = 1, itotal
! ! 	face_loop: do jj = 1, ubound(cell(i)%jface,1)
! ! 		j = cell(i)%jface(jj)
! ! 		face_list_loop: do jjl = 1, ubound(face_list,1)
! ! 			jl = face_list(jjl)
! ! 			if (j == jl) cycle face_loop ! if we find the face, move onto next face for cell
! ! 		end do face_list_loop
! ! 		cycle cell_loop ! if we are here then the face is not present in this cell, so move on to next cell
! ! 	end do face_loop
! ! 	icell_from_jface_list = i ! if we are here then all faces matched - so cell matches
! ! 	exit cell_loop
! ! end do cell_loop

! if (debug) then
!   if (icell_from_jface_list == 0) then
!     write(*,*) 'did not find cell: icell_from_jface_list = ',icell_from_jface_list
!   else
!     write(*,*) 'found cell: icell_from_jface_list = ',icell_from_jface_list,': cell%jface = ',cell(icell_from_jface_list)%jface
!   end if
! end if

! if (debug) write(*,'(a/80(1h+))') 'function icell_from_jface_list'

! end function icell_from_jface_list

!-----------------------------------------------------------------

function icell_from_knode_list(node_list)

integer :: i, ii, k, kk, ic, iic
integer, dimension(:), allocatable :: node_list
integer :: icell_from_knode_list
logical, parameter :: debug = .false.

if (debug) write(*,'(80(1h+)/a)') 'function icell_from_knode_list'

if (debug) write(*,*) 'node_list = ',node_list

icell_from_knode_list = 0

! temp &&&
!return

if (.not.allocated(node(node_list(1))%icell)) return

cell_loop: do ii = 1, ubound(node(node_list(1))%icell,1) ! loop through all cells that are associated with the first node
  i = node(node_list(1))%icell(ii)

  node_loop: do kk = 2, ubound(node_list,1)
    k = node_list(kk)

    if (.not.allocated(node(k)%icell)) return ! if these indices aren't even allocated, cell must be new

    compare_cell_loop: do iic = 1, ubound(node(k)%icell,1)
      ic = node(k)%icell(iic)

      if (ic == i) cycle node_loop ! cell appears in both node lists so check next node

    end do compare_cell_loop

    cycle cell_loop ! here the original cell was not found so move on to next one

  end do node_loop

  icell_from_knode_list = i ! i has been found in every node in the list - it must be the cell

end do cell_loop

if (debug) then
  if (icell_from_knode_list == 0) then
    write(*,*) 'did not find cell: icell_from_knode_list = ',icell_from_knode_list
  else
    write(*,*) 'found cell: icell_from_knode_list = ',icell_from_knode_list,': cell%knode = ',cell(icell_from_knode_list)%knode
  end if
end if

if (debug) write(*,'(a/80(1h+))') 'function icell_from_knode_list'

end function icell_from_knode_list

!-----------------------------------------------------------------

subroutine find_2d_geometry(knode,area,norm,centre,error_angle,error)

! find geometry info about a 2d convex and flat polygon
! convex means that any line drawn from one edge to another intersects only those two edges
! flat means that all points lie on the one plane (checked within this routine)
! norm is a unit normal to the surface ( normalised( (node(2)-node(1)) X (node(3)-node(2)) ) )
! area and centre are calculated by splitting surface into triangles, each with a vertex on the first node
! centre is the centroid, defined for our purposes as the point which is gives the same value
!  for a function as the average value of that function, assuming a linear dependence on space

integer, dimension(:), intent(in) :: knode ! ordered list of node indices that surround geometry
double precision, intent(out), optional :: area ! area of geometry
double precision, dimension(totaldimensions), intent(out), optional :: centre ! vectors specifying centre of surface
double precision, dimension(totaldimensions,totaldimensions), intent(out), optional :: norm ! vectors specifying normal to surface
double precision, intent(out), optional :: error_angle ! this is the maximum difference between adjacent normals on the geometry if it is curved
logical, intent(out), optional :: error ! if an error occurred when calculating this geometry
double precision, dimension(totaldimensions) :: norms, normtriangle, centretriangle, normtrianglelast
integer :: kk
double precision :: aa, areatriangle, error_angle_l, aa_sum
logical :: error_l, error_normalise
double precision, parameter :: smallaa = 1.d-30
logical, parameter :: debug = .false.

if (debug) write(31,'(80(1h-)/a)') 'subroutine find_2d_geometry'

if (debug) write(31,*) 'finding area of 2d_geometry: knode = ',knode,': norms = ',norms

! new method which breaks surface into multiple triangles, each anchored to the first vertex
! v0.50, now handles curved faces by splitting each face into a number of triangles and weighting each component by the area of that triangle

error_l = .false.
error_angle_l = 0.d0
if (present(error_angle)) error_angle = 0.d0
aa_sum = 0.d0
normtrianglelast = 0.d0
norms = 0.d0
if (present(centre)) centre = [ 0.d0, 0.d0, 0.d0 ]
do kk = 3, ubound(knode,1)
! so triangle has vertices knode(1), knode(kk-1) and knode(kk) in an anti-clockwise sense
  if (kk > 3) normtrianglelast = normtriangle ! save last normtriangle

! area of a triangle is half the area of a parallelgram formed between adjacent sides
  normtriangle = cross_product( node(knode(kk-1))%x-node(knode(1))%x , node(knode(kk))%x-node(knode(1))%x )
  areatriangle = vector_magnitude(normtriangle)/2.d0
  aa_sum = aa_sum + areatriangle
  call normalise_vector(vector=normtriangle,error=error_normalise)
  if (error_normalise) then
    write(31,*) 'ERROR: problem when trying to normalise normtriangle in find_2d_geometry'
    error_l = .true.
  end if
  norms = norms + normtriangle*areatriangle

! while we're at it, check that the surface is flat if it has more than 3 vertices
  if (kk > 3) then
    if (debug.or.present(error_angle)) then
      error_angle_l = acos(min(max(dot_product(normtriangle,normtrianglelast),-1.d0),1.d0))
      if (debug) then
        if (error_angle_l > 1.d-8) then
          write(31,*) 'ERROR: a 2d geometry is not flat'
          write(31,*) 'normtriangle = ',normtriangle
          write(31,*) 'normtrianglelast = ',normtrianglelast
          write(31,*) 'error_angle_l = ',error_angle_l
          write(31,*) 'knode = ',knode
        end if
      end if
      if (present(error_angle)) error_angle = max(error_angle,error_angle_l)
    end if
  end if

! now centroid
  if (present(centre)) then
! centroid of any simplex (point, line, triangle, tetrahedron) given by sum of vertices divided by number of vertices (http://en.wikipedia.org/wiki/Centroid)
    centretriangle = (node(knode(1))%x + node(knode(kk-1))%x + node(knode(kk))%x)/3.d0
! centroid of compound form given by area weighted sum of centroid components (http://en.wikipedia.org/wiki/Centroid)
    centre = centre + areatriangle*centretriangle
  end if

end do

if (aa_sum.lt.smallaa) then
  write(31,*) 'ERROR: zero or negative area sum in find_2d_geometry'
  error_l = .true.
  aa_sum = smallaa
end if
! call error_stop('zero or negative area in find_2d_geometry')
! for curved faces, normalise the normals using magnitude of combined normal rather than area sum
aa = vector_magnitude(norms)
if (aa.lt.smallaa) then
  write(31,*) 'ERROR: zero or negative normal length (area) in find_2d_geometry'
  error_l = .true.
  aa = smallaa
end if
if (debug) then
  if (abs(aa-aa_sum)/max(aa,aa_sum) > 1.d-10) then
    write(31,*) 'relative difference between aa and aa_sum = ',abs(aa-aa_sum)/max(aa,aa_sum)
    write(31,*) 'aa = ',aa
    write(31,*) 'aa_sum = ',aa_sum
  end if
end if

if (present(centre)) centre = centre/aa_sum ! slightly more accurate to use aa_sum rather than aa here

if (present(area)) area = aa ! must be aa for divergence of constant to be correct around each cell

if (present(norm)) then
  norm(:,1) = norms/aa ! and finally normalise normal
  norm(:,2) = node(knode(2))%x-node(knode(1))%x ! first tangent, lying in plane of geometry from node 1->2, probably not a good representation for curved faces
  call normalise_vector(vector=norm(:,2),error=error_normalise)
  if (error_normalise) then
    write(31,*) 'ERROR: problem when trying to normalise norm(:,2) in find_2d_geometry'
    error_l = .true.
  end if
  norm(:,3) = cross_product( norm(:,1), norm(:,2) ) ! second tangent, lying in plane of geometry, poor for curved faces
  call normalise_vector(vector=norm(:,3),error=error_normalise)
  if (error_normalise) then
    write(31,*) 'ERROR: problem when trying to normalise norm(:,3) in find_2d_geometry'
    error_l = .true.
  end if
  norm(:,2) = cross_product( norm(:,3), norm(:,1) ) ! revisit second tangent, so that system is orthogonal
  call normalise_vector(vector=norm(:,2),error=error_normalise)
  if (error_normalise) then
    write(31,*) 'ERROR: problem when trying to normalise norm(:,2) after 2nd cross-product in find_2d_geometry'
    error_l = .true.
  end if
end if

if (present(error)) error=error_l

if (debug) write(31,'(a/80(1h-))') 'subroutine find_2d_geometry'

end subroutine find_2d_geometry

!-----------------------------------------------------------------

subroutine find_3d_geometry(jface,volume,centre)

! find geometry info about a 3d convex polyhedron
! it is assumed that each surface of the polyhedron is flat (not checked)
! convex means that any line drawn from one surface to another intersects only those two surfaces
! volume and centre are calculated by splitting surface into tetrahedra, each with a vertex on the first node of the first face
! centre is the centroid, defined for our purposes as the point which is gives the same value
!  for a function as the average value of that function, assuming a linear dependence on space

integer, dimension(:), intent(in) :: jface ! list of face indices that surround geometry
double precision, intent(out), optional :: volume ! volume of geometry
double precision, dimension(totaldimensions), intent(out), optional :: centre ! vector specifying centre of the polyhedron
double precision, dimension(totaldimensions) :: centretetrahedron
integer :: jj, kk
integer, dimension(4) :: knode
double precision :: vv, volumetetrahedron
logical, parameter :: debug = .false.

if (debug) write(*,'(80(1h-)/a)') 'subroutine find_3d_geometry'

if (debug) write(*,*) 'finding volume of 3d_geometry: jface = ',jface

! new method which breaks surface into multiple tetrahedra, each anchored to the first vertex of the first face

vv = 0.d0
if (present(centre)) centre = [ 0.d0, 0.d0, 0.d0 ]
knode(1) = face(jface(1))%knode(1) ! anchor point

jj_loop: do jj = 2, ubound(jface,1) ! cycle through faces that may possibly not have the anchor vertex as a member

! check that anchor node is not a member of this face
  do kk = 1, ubound(face(jface(jj))%knode,1)
    if (face(jface(jj))%knode(kk) == knode(1)) cycle jj_loop
  end do

! local anchor point on this face is the first node (a la find_2d_geometry)
  knode(2) = face(jface(jj))%knode(1)

! now cycle through the tetrahedron associated with this face
  do kk = 3, ubound(face(jface(jj))%knode,1)
! four nodes that define tetrahedron are now:
!   knode(1) = face(jface(1))%knode(1) - constant for all - set above
!   knode(2) = face(jface(jj))%knode(1) - constant for this face - set above
    knode(3) =  face(jface(jj))%knode(kk-1)
    knode(4) =  face(jface(jj))%knode(kk)
    
! according to http://en.wikipedia.org/wiki/Tetrahedron volume of this tet is given by
    volumetetrahedron = abs( dot_product( node(knode(2))%x-node(knode(1))%x , &
      cross_product( node(knode(3))%x-node(knode(1))%x , node(knode(4))%x-node(knode(1))%x ) ) )/6.d0
    vv = vv + volumetetrahedron

    if (present(centre)) then
! centroid of any simplex (point, line, triangle, tetrahedron) given by sum of vertices divided by number of vertices (http://en.wikipedia.org/wiki/Centroid)
      centretetrahedron = (node(knode(1))%x + node(knode(2))%x + node(knode(3))%x + node(knode(4))%x)/4.d0
! centroid of compound form given by volume weighted sum of centroid components (http://en.wikipedia.org/wiki/Centroid)
      centre = centre + volumetetrahedron*centretetrahedron
    end if

  end do

end do jj_loop

if (vv.lt.1.d-30) stop 'ERROR: zero or negative volume in find_3d_geometry'

if (present(centre)) centre = centre/vv

if (present(volume)) volume = vv

if (debug) write(*,'(a/80(1h-))') 'subroutine find_3d_geometry'

end subroutine find_3d_geometry

!----------------------------------------------------------------------------

subroutine add_jface_to_nodes(jface,knode)

! here we take a face (jface) and list of nodes (knode) and check that all the nodes have this face associated with them
integer, dimension(:), allocatable, intent(in) :: knode ! ordered list of node indices that surround geometry
integer, intent(in) :: jface
integer :: kk, k, j, jj

if (.not.allocated(knode)) return

kk_loop: do kk = 1, ubound(knode,1)
  k = knode(kk)
  if (allocated(node(k)%jface)) then
    do jj = 1, ubound(node(k)%jface,1)
      j = node(k)%jface(jj)
      if (j == jface) cycle kk_loop ! face is already in list, so move on to next node
    end do
  end if
  call push_integer_array(array=node(k)%jface,new_element=jface)
end do kk_loop

end subroutine add_jface_to_nodes

!-----------------------------------------------------------------

subroutine add_icell_to_nodes(icell,knode)

! here we take a cell (icell) and list of nodes (knode) and check that all the nodes have this cell associated with them
integer, dimension(:), allocatable, intent(in) :: knode ! ordered list of node indices that surround geometry
integer, intent(in) :: icell
integer :: kk, k, i, ii

if (.not.allocated(knode)) return

kk_loop: do kk = 1, ubound(knode,1)
  k = knode(kk)
  if (allocated(node(k)%icell)) then
    do ii = 1, ubound(node(k)%icell,1)
      i = node(k)%icell(ii)
      if (i == icell) cycle kk_loop ! cell is already in list, so move on to next node
    end do
  end if
  call push_integer_array(array=node(k)%icell,new_element=icell)
end do kk_loop

end subroutine add_icell_to_nodes

!-----------------------------------------------------------------

function fortran_float(n)

! this is just the dble intrinsic, renamed so that it can pass through maxima

integer :: n
double precision :: fortran_float

fortran_float = dble(n)

end function fortran_float

!-----------------------------------------------------------------

function heaviside(x)

! this is the heaviside function

double precision :: x, heaviside

if (x > 0) then
  heaviside = 1.d0
else if (x < 0) then
  heaviside = 0.d0
else
  heaviside = 0.5d0
end if

end function heaviside

!-----------------------------------------------------------------

function random()

! this generates a random number in the range 0 <= random < 1

double precision :: random

call random_number(random) ! this is an intrinsic function apparently

end function random

!-----------------------------------------------------------------

function signum(x)

! this is the sign function as used in maxima

double precision :: x, signum

signum = sign(1.d0,x)

end function signum

!-----------------------------------------------------------------
! ! this function returns 1 if the lastcell was the upcell, -1 if the lastcell was the downcell, and 0 otherwise
! ! accessed from the arb language using <lastcell>

! function lastcell(i,j)

! double precision :: lastcell
! integer :: i, j

! if (j == 0.or.j > ubound(face,1)) then
!   lastcell = 0.d0
! else if (face(j)%icell(2) == i) then
!   lastcell = 1.d0
! else if (face(j)%icell(1) == i) then
!   lastcell = -1.d0
! else
!   lastcell = 0.d0
! end if

! end function lastcell

!-----------------------------------------------------------------
! this function returns 1 if the cell we are in is the upcell of the lastface, or -1 if we are in the downcell of the lastface (or 0 otherwise)
! accessed from the arb language using <adjacentcellsignns>
! based on ns, so must be used in the context of cycling around adjacent cells to a face

function adjacentcellsignns(ns)

double precision :: adjacentcellsignns
integer :: ns

if (ns == 1) then
  adjacentcellsignns = -1.d0
else if (ns == 2) then
  adjacentcellsignns = 1.d0
else
  adjacentcellsignns = 0.d0
end if

end function adjacentcellsignns

!-----------------------------------------------------------------

function newtonupdate(m,ijk)

! this function returns the last newtonupdate (unbackstepped) for the unknown variable m

double precision :: newtonupdate
character(len=1000) :: error_string
integer :: m, ijk, ns

! sanity check on unknown variable identification
if (trim(var(m)%type) /= "unknown") call error_stop("newtonupdate has been called with an m index that does not refer to an unknown")
error_string="called from newtonupdate trying to reference unknown variable"//trim(var(m)%name)
ns = nsvar(m=m,ijk=ijk,error_string=error_string)
newtonupdate = delphi(var(m)%funk(ns)%pp(1))

end function newtonupdate

!-----------------------------------------------------------------

function magnitude(m)

! this function returns the magnitude for the unknown or equation variable m

double precision :: magnitude
integer :: m

! sanity check on unknown variable identification
if (trim(var(m)%type) /= "unknown" .and. trim(var(m)%type) /= "equation") &
  call error_stop("magnitude has been called with an m index that does not refer to an unknown or equation variable")
magnitude = max(var(m)%magnitude,0.d0)

end function magnitude

!-----------------------------------------------------------------

function arb_variable_from_fortran_logical(logical_variable)

! this function returns 0 for .false., and 1 for .true.

logical :: logical_variable
double precision :: arb_variable_from_fortran_logical

if (logical_variable) then
  arb_variable_from_fortran_logical = 1.d0
else
  arb_variable_from_fortran_logical = 0.d0
end if

end function arb_variable_from_fortran_logical

!-----------------------------------------------------------------
! these are some trig hyperbolic functions which are not fortran intrinsics

function sech(x)

double precision :: x, sech

sech = 1.d0/cosh(x)

end function sech

!-----------------------------------------------------------------

function csch(x)

double precision :: x, csch

csch = 1.d0/sinh(x)

end function csch

!-----------------------------------------------------------------

function coth(x)

double precision :: x, coth

coth = 1.d0/tanh(x)

end function coth

!-----------------------------------------------------------------

function divop(i,j)

! assemble coefficients required for divergence sum in domain equations

integer, intent(in) :: i,j
double precision :: divop
logical, parameter :: debug = .false.

if (debug) write(*,'(80(1h+)/a)') 'function divop'

if (cell(i)%type == 2) &
  call error_stop("function divop (which may be from <facedivop>) is being referenced within a boundary cell: "// &
  " only use this function in domain cells which have a non-zero area")
divop = face(j)%area/max(cell(i)%vol,tinyish)
if (face(j)%icell(1) /= i) divop = -divop ! reverse sign if dot product of nface and ncell isn't 1

if (debug) write(*,'(a/80(1h-))') 'function divop'

end function divop

!-----------------------------------------------------------------

subroutine time_process(description)

! this routine needs to be called, once before a process with description missing, and then
!  after the process again, with a description added

character(len=*), intent(in), optional :: description
real :: this_cpu_time, elapsed_wall_time
logical, parameter :: debug = .false.

if (debug) write(*,'(80(1h+)/a)') 'subroutine time_process'

if (.not.output_timings) then
  if (debug) write(*,*) 'logical output_timings set to .false. - not doing timings'
  return
end if

call cpu_time(this_cpu_time)
if (.not.present(description)) last_wall_time = secnds(0.e0)

if (present(description)) then
  elapsed_wall_time = secnds(last_wall_time)
  if (output_detailed_timings) write(*,'(a,2(g11.4,a))') 'TIMING: '//trim(description)// &
    ' routines took ',elapsed_wall_time,'s of wall time and ',this_cpu_time-last_cpu_time,'s of cpu time'
  total_wall_time = total_wall_time + elapsed_wall_time
  total_cpu_time = total_cpu_time + this_cpu_time - last_cpu_time
end if

last_cpu_time = this_cpu_time

if (debug) write(*,'(a/80(1h-))') 'subroutine time_process'

end subroutine time_process

!-----------------------------------------------------------------

subroutine time_variable_update(thread,calling_phase,m,region_l)

! this routine calculates the total time spent on updating a variable, and the number of times that the variable has been updated
! must be called by a particular processor in sequence

integer :: calling_phase ! 0 for initial, 1 for final (used an integer here as lookup needs to be fast)
integer :: m ! variable number
integer :: thread ! thread number
logical :: region_l ! we are timing a region update rather than a variable update
real :: this_cpu_time
logical, parameter :: debug = .false.

if (debug) write(*,'(80(1h+)/a)') 'subroutine time_variable_update'

if (calling_phase == 0) then
  call cpu_time(update_time_start(thread))
else if (calling_phase == 1) then
  call cpu_time(this_cpu_time)
  if (region_l) then
    region(m)%update_time = region(m)%update_time + this_cpu_time - update_time_start(thread)
    region(m)%update_number = region(m)%update_number + 1
  else
    var(m)%update_time = var(m)%update_time + this_cpu_time - update_time_start(thread)
    var(m)%update_number = var(m)%update_number + 1
  end if
end if

if (debug) write(*,'(a/80(1h-))') 'subroutine time_variable_update'

end subroutine time_variable_update

!-----------------------------------------------------------------

subroutine copy_kernel(original,copy)

type(kernel_type) :: original, copy
integer :: new_size
logical :: copy_reflect
logical, parameter :: debug = .false.

if (debug) write(*,'(80(1h+)/a)') 'subroutine copy_kernel'

copy%centring = original%centring
copy%reflect_present = original%reflect_present ! now just doing straight copy of this logical and the reflect_multiplier array, regardless of values

new_size = 0
if (allocated(original%ijk)) new_size = ubound(original%ijk,1)
if (allocated(original%reflect_multiplier)) then
  copy_reflect = .true.
else
  copy_reflect = .false.
end if

if (allocated(copy%ijk)) deallocate(copy%ijk)
if (allocated(copy%v)) deallocate(copy%v)
if (allocated(copy%reflect_multiplier)) deallocate(copy%reflect_multiplier)
if (new_size == 0) return

allocate(copy%ijk(new_size),copy%v(new_size))
copy%ijk = original%ijk
copy%v = original%v
if (copy_reflect) then
  allocate(copy%reflect_multiplier(totaldimensions,new_size))
  copy%reflect_multiplier = original%reflect_multiplier ! NB, these are the same size and dimension
end if
!copy = original ! this should also work...

if (debug) write(*,'(a/80(1h-))') 'subroutine copy_kernel'

end subroutine copy_kernel

!-----------------------------------------------------------------

function location_in_list(array,element)

integer, dimension(:), allocatable :: array
integer :: element
integer :: location_in_list
integer :: i
logical, parameter :: debug = .false.

if (debug) write(*,'(80(1h+)/a)') 'function location_in_list'

location_in_list = 0

if (.not.allocated(array)) return

do i = 1, ubound(array,1)
  if (element == array(i)) then
    location_in_list = i
    exit
  end if
end do

if (debug) write(*,'(a/80(1h-))') 'function location_in_list'

end function location_in_list

!-----------------------------------------------------------------

function location_in_list_dummy(array,element)

! same as location in list but instead accepts a non-allocatable array

integer, dimension(:) :: array
integer :: element
integer :: location_in_list_dummy
integer :: i
logical, parameter :: debug = .false.

if (debug) write(*,'(80(1h+)/a)') 'function location_in_list_dummy'

location_in_list_dummy = 0

do i = 1, ubound(array,1)
  if (element == array(i)) then
    location_in_list_dummy = i
    exit
  end if
end do

if (debug) write(*,'(a/80(1h-))') 'function location_in_list_dummy'

end function location_in_list_dummy

!-----------------------------------------------------------------

function has_elements_in_common(array1,array2)

! returns true if the two allocatable arrays at least one element in common
! NB, allows that either not be allocated (returning false)

integer, dimension(:), allocatable :: array1, array2
logical :: has_elements_in_common
integer :: i1, i2
logical, parameter :: debug = .false.

if (debug) write(*,'(80(1h+)/a)') 'function has_elements_in_common'

has_elements_in_common = .false.

if (.not.allocated(array1)) return
if (.not.allocated(array2)) return

outer_loop: do i1 = 1, ubound(array1,1)
  do i2 = 1, ubound(array2,1)
    if (array1(i1) == array2(i2)) then
      has_elements_in_common = .true.
      exit outer_loop
    end if
  end do
end do outer_loop

if (debug) write(*,'(a/80(1h-))') 'function has_elements_in_common'

end function has_elements_in_common

!-----------------------------------------------------------------

function region_delta(ijk,region_number)

double precision :: region_delta
integer :: ijk, region_number
logical, parameter :: debug = .false.

if (debug) write(*,'(80(1h+)/a)') 'function region_delta'

if (region(region_number)%ns(ijk) == 0) then
  region_delta = 0.d0
else
  region_delta = 1.d0
end if

if (debug) write(*,'(a/80(1h-))') 'function region_delta'

end function region_delta

!-----------------------------------------------------------------

function var_list_number(type,centring,include_regions)

! little function to return number of var_list that corresponds to type and centring
! now also option to include dynamic regions in the list too
character(len=*) :: centring ! whether region is cell or face centred
character(len=*) :: type ! type of var variable
logical, optional :: include_regions ! if true and present, include relevant dynamic regions in the lists, otherwise don't (default if not present)
integer :: var_list_number
integer :: ntype, ncentring
integer, parameter :: ntypemax = ubound(var_types,1) + 1 ! can do this as var_types is a parameter
integer, parameter :: ntypecentringmax = ntypemax*5
logical, parameter :: debug = .false.

if (debug) write(*,'(80(1h+)/a)') 'function var_list_number'

if (trim(type) == "all") then
  ntype = ntypemax
else
  ntype = 1
  do while (ntype <= ubound(var_types,1))
    if (trim(type) == trim(var_types(ntype))) exit
    ntype = ntype + 1
  end do
  if (ntype > ubound(var_types,1)) call error_stop('unknown type '//trim(type)//' in var_list_number')
end if

if (trim(centring) == "cell") then
  ncentring = 1
else if (trim(centring) == "face") then
  ncentring = 2
else if (trim(centring) == "node") then
  ncentring = 3
else if (trim(centring) == "none") then
  ncentring = 4
else if (trim(centring) == "all") then
  ncentring = 5
else
  stop "ERROR: unknown centring in var_list_number"
end if

var_list_number = ntype + (ncentring-1)*(ntypemax)

if (present(include_regions)) then
  if (include_regions) var_list_number = var_list_number+ntypecentringmax
end if

if (debug) write(*,'(a/80(1h-))') 'function var_list_number'

end function var_list_number

!-----------------------------------------------------------------

function nsvar(m,ijk,error_string,noerror)

! little function to lookup ns data number corresponding to location ijk for var(m)
integer, intent(in) :: m
integer, intent(in), optional :: ijk
logical, intent(in), optional :: noerror ! do not report an error even if out of range detected
logical :: noerror_l
character(len=*), intent(in), optional :: error_string ! if present, this will be printed out when an error is detected
character(len=1100) :: error_stringl ! to optimise speed, error_stringl only defined when needed
integer :: nsvar

if (m > ubound(var,1) .or. m < 1) then
  if (present(error_string)) then
    error_stringl = '  Info from calling routine: '//trim(error_string)
  else
    error_stringl = ''
  end if
  call error_stop('variable index m out of range in nsvar.'//trim(error_stringl))
end if

if (var(m)%centring == "none") then
  nsvar = 1
else
  if (.not.present(ijk)) then
    if (present(error_string)) then
      error_stringl = '  Info from calling routine: '//trim(error_string)
    else
      error_stringl = ''
    end if
    call error_stop('ijk is not present for a cell or face centred var in function nsvar: This '// &
    'means that an attempt is being made to reference variable '//trim(var(m)%name)//' at an erroreous location. '// &
    'Look at the use of this variable in the equations and check that its region and centring context are consistent.'// &
     trim(error_stringl))
  end if
  if (ijk == 0) then
    if (present(error_string)) then
      error_stringl = '  Info from calling routine: '//trim(error_string)
    else
      error_stringl = ''
    end if
    call error_stop('ijk is equal to 0 in function nsvar: This means that an attempt is being made '// &
    'to reference variable '//trim(var(m)%name)//' at an erroreous location. '// &
    'Look at the use of this variable in the equations and check that its region and centring context are consistent.'// &
    trim(error_stringl))
  end if
  if (ijk > allocatable_integer_size(region(var(m)%region_number)%ns)) then
    if (present(error_string)) then
      error_stringl = '  Info from calling routine: '//trim(error_string)
    else
      error_stringl = ''
    end if
    call error_stop('ijk is greater than size of ns array in function nsvar: This means that an attempt is being made '// &
    'to reference variable '//trim(var(m)%name)//' at an erroreous location. '// &
    'Look at the use of this variable in the equations and check that its region and centring context are consistent.'// &
    trim(error_stringl))
  end if
  nsvar = region(var(m)%region_number)%ns(ijk)
  if (nsvar == 0) then
    noerror_l = .false.
    if (present(noerror)) noerror_l = noerror
    if (noerror_l) then
      nsvar = 0
    else
      if (present(error_string)) then
        error_stringl = '  Info from calling routine: '//trim(error_string)
      else
        error_stringl = ''
      end if
      call error_stop('nsvar is equal to 0 in nsvar: This means that an attempt is being made '// &
        'to reference '//trim(var(m)%centring)//' centred variable '//trim(var(m)%name)//' outside of the region '// &
        trim(var(m)%region)//' in which it is defined. '// &
        'Look at the use of this variable in the equations and check that its region and centring context are consistent.'// &
        trim(error_stringl))
    end if
  end if
end if

end function nsvar

!-----------------------------------------------------------------

function ijkvar(m,ns)

! little function to lookup ijk index corresponding to data number ns for var(m)
integer :: m
integer :: ns
integer :: ijkvar

if (m > ubound(var,1) .or. m < 1) stop 'ERROR: m out of range in function ijkvar'

if (var(m)%centring == "none") then
  ijkvar = 1
else
  ijkvar = region(var(m)%region_number)%ijk(ns)
end if

end function ijkvar

!-----------------------------------------------------------------

subroutine reset_face(default_face)

! resets a face element
type(face_type) :: default_face
integer :: n
logical, parameter :: debug = .false.

if (debug) write(*,'(80(1h+)/a)') 'subroutine reset_face'

default_face%type = 0
default_face%x = 0.d0
default_face%area = 0.d0
default_face%dx = 0.d0
default_face%dx_unit = 0.d0
default_face%dx_kernel = 0.d0
default_face%norm = 0.d0
if (allocated(default_face%icell)) deallocate(default_face%icell)
allocate(default_face%icell(2))
if (allocated(default_face%knode)) deallocate(default_face%knode)
default_face%dimensions = 0
default_face%gtype = 0
!do n = 0, 2*totaldimensions
do n = lbound(default_face%kernel,1), ubound(default_face%kernel,1)
  default_face%kernel(n)%centring = ''
  if (allocated(default_face%kernel(n)%ijk)) deallocate(default_face%kernel(n)%ijk)
  if (allocated(default_face%kernel(n)%v)) deallocate(default_face%kernel(n)%v)
end do
default_face%glue_jface = 0
default_face%glue_reflect = 0

if (debug) write(*,'(a/80(1h-))') 'subroutine reset_face'

end subroutine reset_face

!-----------------------------------------------------------------

subroutine reset_cell(default_cell)

! resets a cell element
type(cell_type) :: default_cell
integer :: n
logical, parameter :: debug = .false.

if (debug) write(*,'(80(1h+)/a)') 'subroutine reset_cell'

default_cell%type = 0
default_cell%x = 0.d0
default_cell%vol = 0.d0
default_cell%dx_kernel = 0.d0
default_cell%dx_max = 0.d0
default_cell%dx_min = 0.d0
if (allocated(default_cell%knode)) deallocate(default_cell%knode)
if (allocated(default_cell%jface)) deallocate(default_cell%jface)
if (allocated(default_cell%icell)) deallocate(default_cell%icell)
default_cell%dimensions = 0
default_cell%gtype = 0
!do n = 0, totaldimensions
do n = lbound(default_cell%kernel,1), ubound(default_cell%kernel,1)
  default_cell%kernel(n)%centring = ''
  if (allocated(default_cell%kernel(n)%ijk)) deallocate(default_cell%kernel(n)%ijk)
  if (allocated(default_cell%kernel(n)%v)) deallocate(default_cell%kernel(n)%v)
end do
default_cell%cross_kernel = 0.d0
default_cell%central_kernel = 0.d0
default_cell%kernel_sum = 0.d0

if (debug) write(*,'(a/80(1h-))') 'subroutine reset_cell'

end subroutine reset_cell

!----------------------------------------------------------------------------

subroutine copy_integer_array(original,copy)

! copy allocatable original array to copy array

integer, dimension(:), allocatable :: original, copy

call resize_integer_array(keep_data=.false.,array=copy,new_size=allocatable_integer_size(original))
if (allocated(original)) copy = original

end subroutine copy_integer_array

!-----------------------------------------------------------------

subroutine copy_double_precision_array(original,copy)

! copy allocatable original array to copy array

double precision, dimension(:), allocatable :: original, copy

call resize_double_precision_array(keep_data=.false.,array=copy,new_size=allocatable_double_precision_size(original))
if (allocated(original)) copy = original

end subroutine copy_double_precision_array

!-----------------------------------------------------------------

subroutine copy_character_array(original,copy)

! copy allocatable original array to copy array

character(len=*), dimension(:), allocatable :: original
character(len=len(original)), dimension(:), allocatable :: copy

call resize_character_array(keep_data=.false.,array=copy,new_size=allocatable_character_size(original))
if (allocated(original)) copy = original

end subroutine copy_character_array

!-----------------------------------------------------------------

subroutine copy_logical_array(original,copy)

! copy allocatable original array to copy array

logical, dimension(:), allocatable :: original
logical, dimension(:), allocatable :: copy

call resize_logical_array(keep_data=.false.,array=copy,new_size=allocatable_logical_size(original))
if (allocated(original)) copy = original

end subroutine copy_logical_array

!-----------------------------------------------------------------

subroutine copy_real_array(original,copy)

! copy allocatable original array to copy array

real, dimension(:), allocatable :: original, copy

call resize_real_array(keep_data=.false.,array=copy,new_size=allocatable_real_size(original))
if (allocated(original)) copy = original

end subroutine copy_real_array

!-----------------------------------------------------------------

function elements_in_common(array1,array2)

! this returns the number of common elements between 2 integer arrays
! could do fancier search by reordering/reducing array2 but would
! require the array to be copied

integer, dimension(:), allocatable :: array1, array2
integer :: elements_in_common
integer :: n, n2

elements_in_common = 0

if (.not.allocated(array1).or..not.allocated(array2)) return

do n = 1, ubound(array1,1)
  n2 = location_in_list(array=array2,element=array1(n))
  if (n2 /= 0) elements_in_common = elements_in_common + 1
end do

end function elements_in_common

!-----------------------------------------------------------------

subroutine memory_manage_dvs(type,action)

! here we deallocate or reallocate the dv arrays in the var%funks

character(len=*) :: type ! variable type this is applied to
character(len=*) :: action ! deallocate or reallocate
integer :: n, m, ns
logical, parameter :: debug = .false.

if (debug) write(*,'(80(1h+)/a)') 'subroutine memory_manage_dvs'

if (debug) write(*,*) 'doing action = '//trim(action)//' for type '//trim(type)

!if (debug) pause

if (trim(action) == 'deallocate') then
  do n = 1, allocatable_size(var_list(var_list_number(centring="all",type=type))%list)
    m = var_list(var_list_number(centring="all",type=type))%list(n)
    if (debug) write(*,*) 'deallocating dv for variable '//trim(var(m)%name)
    do ns = 1, ubound(var(m)%funk,1)
      var(m)%funk(ns)%ndv = -abs(var(m)%funk(ns)%ndv) ! set ndv to be negative of previous value as a marker
      if (allocated(var(m)%funk(ns)%pp)) deallocate(var(m)%funk(ns)%pp)
      if (allocated(var(m)%funk(ns)%dv)) deallocate(var(m)%funk(ns)%dv)
    end do
  end do
else if (trim(action) == 'reallocate') then
  do n = 1, allocatable_size(var_list(var_list_number(centring="all",type=type))%list)
    m = var_list(var_list_number(centring="all",type=type))%list(n)
    if (debug) write(*,*) 'reallocating dv for variable '//trim(var(m)%name)
    do ns = 1, ubound(var(m)%funk,1)
      if (var(m)%funk(ns)%ndv < 0) then
        if (allocated(var(m)%funk(ns)%pp)) deallocate(var(m)%funk(ns)%pp) ! should not need this
        if (allocated(var(m)%funk(ns)%dv)) deallocate(var(m)%funk(ns)%dv) ! should not need this
        allocate(var(m)%funk(ns)%pp(-var(m)%funk(ns)%ndv))
        allocate(var(m)%funk(ns)%dv(-var(m)%funk(ns)%ndv))
        var(m)%funk(ns)%ndv = 0
      end if
    end do
  end do
else
  stop 'ERROR: unknown action in memory_manage_dvs'
end if

!if (debug) pause

if (debug) write(*,'(a/80(1h-))') 'subroutine memory_manage_dvs'

end subroutine memory_manage_dvs

!-----------------------------------------------------------------

function scanstring(string,substring)

! this routine searches string for substring, and if found, returns
!  the index of the starting character

character(len=*) :: string
character(len=*) :: substring
integer :: scanstring
integer :: l, lstring, lsubstring

scanstring = 0
lstring = len(string)
lsubstring = len(substring)

do l = 1, lstring-lsubstring+1
  if (string(l:l+lsubstring-1) == substring) then
    scanstring = l
    return
  end if
end do

end function scanstring

!-----------------------------------------------------------------

! fortran 2003 requires gfortran 4.6
!function number_is_valid(scalar)
!
!! check that a number is neither a NaN or infinite
!
!use ieee_arithmetic
!
!double precision, intent(in) :: scalar
!logical :: number_is_valid
!
!number_is_valid = .false.
!if (ieee_is_nan(scalar)) return
!if (.not.ieee_is_finite(scalar)) return
!number_is_valid = .true.
!      
!end function number_is_valid

function number_is_valid(scalar)

! check that a number is neither a NaN or infinite

double precision, intent(in) :: scalar
logical :: number_is_valid

number_is_valid = .false.
if (scalar /= scalar) return
if (abs(scalar) > huge(scalar)) return
number_is_valid = .true.
      
end function number_is_valid

!-----------------------------------------------------------------

function variable_location_string(m,ns)

! little routine to create a formated string describing a variable location

integer :: m, ns, i, j
character(len=1000) :: variable_location_string
character(len=1000) :: formatline

if (var(m)%centring == 'cell') then
  i = region(var(m)%region_number)%ijk(ns)
  formatline = '(a,'//trim(dindexformat(i))//',a,3(1x,g11.3),a)'
  write(variable_location_string,fmt=formatline) 'cell i = ',i,' and x =',cell(i)%x, &
    ' ('//trim(var(m)%type)//' '//trim(var(m)%name)//')'
else if (var(m)%centring == 'face') then
  j = region(var(m)%region_number)%ijk(ns)
  formatline = '(a,'//trim(dindexformat(j))//',a,3(1x,g11.3),a)'
  write(variable_location_string,fmt=formatline) 'face j = ',j,' and x =',face(j)%x, &
    ' ('//trim(var(m)%type)//' '//trim(var(m)%name)//')'
else
  write(variable_location_string,'(a)') 'nowhere ('//trim(var(m)%type)//' '//trim(var(m)%name)//' has none centring)'
end if

end function variable_location_string

!----------------------------------------------------------------------------

function dindexformat(i)

! this function gives the minimum integer format for a given number
! d is for dynamic

character(len=3) :: dindexformat
integer :: i, ipositive, ilength, icompare

if (i < 0) then
  ilength = 2
  ipositive = -i
else
  ilength = 1
  ipositive = i
end if

icompare = 10
do while (ipositive >= icompare)
  ilength = ilength + 1
  icompare = icompare*10
end do

write(dindexformat,'(i2)') ilength
dindexformat = 'i'//adjustl(dindexformat)

end function dindexformat

!-----------------------------------------------------------------

function basename(fullname)

! this function extracts a file's basename from a file's fullname
! basename has the path stripped off, and trailing extension

character(len=1000) :: basename
character(len=*) :: fullname
integer :: cutr, cutl

cutl=scan(fullname,'/',.true.) ! find rightmost occurance of directory separator
cutr=scan(fullname,'.',.true.) ! find rightmost occurance of . indicating extension
if (cutr <= cutl) cutr = len(fullname)+1
if (cutr - 1 - (cutl + 1) > 1000) stop "ERROR: file name too long in basename"
basename = trim(adjustl(fullname(cutl+1:cutr-1)))

end function basename

!-----------------------------------------------------------------

function dirname(fullname)

! this function extracts a file's path from a file's fullname
! if the path is found, it includes the trailing /

character(len=1000) :: dirname
character(len=*) :: fullname
integer :: cutr, cutl

cutl=scan(fullname,'/',.true.) ! find rightmost occurance of directory separator
if (cutl == 0) then
  dirname = '' ! there is no leading directory, so return an empty string
else
! use this to strip off the basename
  dirname = trim(fullname(1:cutl))
end if

end function dirname

!-----------------------------------------------------------------

subroutine error_stop(comment)

! little subroutine to stop simulation and printout comment

character(len=*), optional :: comment
logical :: fconverge_opened, foutputstep_opened

inquire(unit=fconverge,opened=fconverge_opened)
inquire(unit=foutputstep,opened=foutputstep_opened)

if (present(comment)) then
  write(*,'(a)') 'ERROR: '//trim(comment)
  if (fconverge_opened) write(fconverge,'(a)') 'ERROR: '//trim(comment)
end if

if (fconverge_opened) close(fconverge)
if (foutputstep_opened) close(foutputstep)

call exit(1) ! exit while setting error exit status

end subroutine error_stop

!-----------------------------------------------------------------

function check_option(array,possibilities)

! now gives highest priority to the rightmost options
character(len=*), dimension(:), allocatable, intent(in) :: array
character(len=*), dimension(:), intent(in) :: possibilities
character(len=100) :: check_option
integer :: n,m

check_option = ""

if (.not.allocated(array)) return

!do n = 1, ubound(array,1) ! forward lookup
do n = ubound(array,1), 1, -1 ! reverse lookup
  do m = 1, ubound(possibilities,1)
    if (trim(array(n)) == trim(possibilities(m))) then
      check_option = trim(array(n))
      return
    end if
  end do
end do

end function check_option

!-----------------------------------------------------------------

function check_stopfile(action)

! little function to check on the existence of a stop file
! only three actions are accepted: stopback, stopnewt and stoptime
! multiple file names are checked for each action

character(len=*), intent(in) :: action
logical :: check_stopfile
integer :: n, listlength
! depending on whether this is a transient simulation or not, stop may either be included or not in the stopfilelist
! character(len=8), dimension(7), parameter :: stopfilelist = [ "kill   ", "stopback", "stopiter", "stopnewt", "stop    ", "stoptime", "halt    " ]

if (trim(action) == 'kill') then
  listlength = 1 ! kill causes the fastest stop, interupting at all checkpoints and not saving the last current solution estimate
else if (trim(action) == 'stopback') then
  listlength = 2 ! stopback is almost the same as kill, however kill also acts on the iterative solvers
else if (trim(action) == 'stopiter') then
  listlength = 3 ! stopiter will halt the iterative solvers but allows the backstepping operation to complete, so that the last current solution is saved
else if (trim(action) == 'stopnewt'.and.transient_simulation) then
  listlength = 4 ! for a transient sim, the above plus stopnewt will interupt the newton loop
else if (trim(action) == 'stopnewt') then
  listlength = 5 ! for a steady-state sim, the above plus stop will interupt the newton loop
else
  listlength = 7 ! for a transient sim the above plus stop, stoptime and halt will halt the time loop
end if

check_stopfile = .false.
do n = 1, listlength
  inquire(file=trim(stopfilelist(n)),exist=check_stopfile)
  if (check_stopfile) return
end do

end function check_stopfile

!-----------------------------------------------------------------

function check_dumpfile(action)

! little function to check on the existence of a dump file
! only two actions are accepted: dumpnewt and dumptime
! multiple file names are checked for each action

character(len=*), intent(in) :: action
logical :: check_dumpfile
integer :: n, listlength
! depending on whether this is a transient simulation or not, dump may either be included or not in the dumpfilelist
! character(len=8), dimension(3), parameter :: dumpfilelist = [ "dumpnewt", "dump    ", "dumptime" ]

if (trim(action) == 'dumpnewt'.and.transient_simulation) then
  listlength = 1
else if (trim(action) == 'dumpnewt') then
  listlength = 2
else
  listlength = 3
end if

check_dumpfile = .false.
do n = 1, listlength
  inquire(file=trim(dumpfilelist(n)),exist=check_dumpfile)
  if (check_dumpfile) then
    call unlink(trim(dumpfilelist(n)))
    return
  end if
end do

end function check_dumpfile

!-----------------------------------------------------------------

subroutine ring_bell

! little subroutine to ring the bell
! NB: this string is entered via cntrl-v cntrl-g in vi
write(*,*) ""

end subroutine ring_bell

!-----------------------------------------------------------------

function trunk_dble(input)

! little function that truncates a double precision variable so that it's exponent
!  magnitude fits in atmost 2 character spaces
! giving ourselves a bit of leeway here on the exponent just in case different compilers/formats handle things differently

double precision :: trunk_dble
double precision, intent(in) :: input

trunk_dble = input
! deal with large magnitudes
if (abs(trunk_dble) >= 1.0d+99) trunk_dble = 2.d0*sign(huge(1.d0),trunk_dble) ! should produce infinity with the same sign as trunk_dble
! deal with small magnitudes
if (abs(trunk_dble) <= 1.0d-99) trunk_dble = 0.d0

end function trunk_dble

!-----------------------------------------------------------------

function changecase(tocase,string)

! little function to convert the case of a string
! inspired by http://www.star.le.ac.uk/~cgp/fortran.html 
character(len=1), intent(in) :: tocase ! case to change to (U = upper, L = lower, C = change)
character(len=*), intent(in) :: string
character(len=len(string)) :: changecase
integer :: n

changecase = string
do n = 1, len(string)
  if ((tocase == 'U'.or.tocase == 'C') .and. changecase(n:n) >= 'a' .and. changecase(n:n) <= 'z') then
    changecase(n:n) = achar(iachar(string(n:n)) - 32) 
  else if ((tocase == 'L'.or.tocase == 'C') .and. changecase(n:n) >= 'A' .and. changecase(n:n) <= 'Z') then
    changecase(n:n) = achar(iachar(string(n:n)) + 32) 
  end if
end do

end function changecase

!-----------------------------------------------------------------

function removespaces(string)

! little function to replace spaces in a string with underscores
character(len=*), intent(in) :: string
character(len=len(string)) :: removespaces
integer :: n

removespaces = string
do n = 1, len(string)
  if (string(n:n) == ' ') removespaces(n:n) = '_'
end do

end function removespaces

!-----------------------------------------------------------------

function extract_next_string(textline,error,empty,delimiter)

! function to remove first string from textline
! delimiter is a list of delimiters that the string is allowed to have (4 types)
! empty = .false. and error = .false. -> string has nonwhitespace characters and has matched delimiters, that are valid (if delimiter is given)
! empty = .true. and error = .false. and delimiters specified -> string is empty but has matching, valid delimiters
! empty = .true. and error = .false. and delimiters not specified -> string is empty but has matching delimiters
! empty = .true. and error = .true. and delimiters specified -> string is empty, and valid delimiters were never found
! empty = .true. and error = .true. and delimiters not specified -> not possible
! empty = .false. and error = .true. and delimiters specified -> matching delimiters not found
! empty = .false. and error = .true. and delimiters not specified -> matching delimiters not found
! to pass both " and ' in the one delimiter string, use repeats: ie "'"""

character(len=*) :: textline
character(len=len(textline)) :: extract_next_string
logical :: error
logical, optional :: empty ! true if returned string is empty
integer :: cut
character(len=*), optional :: delimiter ! if specified, error is flagged if delimiter is not of a type listed in this string: either ' ' (no delimiter),'"',"'", or '<' (region or variable name which keeps its delimiters)
character(len=1) :: delimiter_l ! local version of delimiter
logical, parameter :: debug=.false.

error = .true.
if (present(empty)) empty = .false.
extract_next_string = ''
delimiter_l = ' '

if (debug) write(*,'(a)') "textline = "//trim(textline)
if (debug) write(*,'(a)') "delimiter = |"//delimiter//"|"

! if (len_trim(textline) == 0) then
!   error = .false. ! an empty string no longer flags an error
!   if (present(empty)) empty = .true.
!   return ! length of string with trailing whitespace removed
! end if

textline = adjustl(textline) ! remove leading spaces so that textline either starts with the string or the delimiter

! find delimiter (if there is one)
if (textline(1:1) == '"' .or. textline(1:1) == "'" .or. textline(1:1) == "<") delimiter_l = textline(1:1)

if (debug) write(*,'(a)') "delimiter_l = "//delimiter_l

! check that delimiter is consistent with requested, if requested
if (present(delimiter)) then
  cut = scan(delimiter,delimiter_l) ! if the delimiter_l is found in the list, then all is OK
  if (cut == 0) then
    if (present(empty)) empty = .true. ! if string doesn't start with requested delimiter, then string is empty (if it does start with the requested delimiter but there is no matching delimiter, empty will be false)
    return
  end if
end if

! we have chosen a delimiter, so now extract string
if (delimiter_l == "<") then
  cut=scan(textline(2:len(textline)),">")+1
  if (cut == 1) return ! matching delimiter not found, so exit with error on and empty off
  extract_next_string = textline(1:cut) ! include delimiters
else if (delimiter_l == " ") then
  cut=scan(textline(1:len(textline))," ")
  if (cut == 0) cut = len(textline)+1 ! specific case of textline containing no spaces
  extract_next_string = textline(1:cut-1) ! this may be empty, but does not flag an error
else
  cut=scan(textline(2:len(textline)),delimiter_l)+1
  if (cut == 1) return ! matching delimiter not found, so exit with error on and empty off
  extract_next_string = textline(2:cut-1) ! exclude delimiters
end if

textline=adjustl(textline(cut+1:len(textline)))
error = .false.
if (debug) write(*,*) "cut = ",cut
if (present(empty).and.len_trim(extract_next_string) == 0) empty = .true.
if (debug) write(*,'(a)') "extract_next_string = "//trim(extract_next_string)
if (debug) write(*,'(a)') "textline = "//trim(textline)

end function extract_next_string

!-----------------------------------------------------------------

subroutine extract_options(textline,options)

! extract a list of options from textline, dumping in the allocatable character array options
! array will be ordered the same way that it appeared in the textline now, ie, from left to right
character(len=*) :: textline
character(len=100), dimension(:), allocatable :: options ! option characters always have a length of 100 througout code
character(len=100) :: option
integer :: cut

do
  cut=scan(textline,',')
  if (cut == 1) then
    textline = trim(adjustl(textline(2:len(textline))))
  else if (cut > 1) then
    option = trim(adjustl(textline(1:cut-1)))
    if (option /= "") call push_character_array(array=options,new_element=option)
    textline = trim(adjustl(textline(cut+1:len(textline))))
  else
    option = trim(adjustl(textline))
    if (option /= "") call push_character_array(array=options,new_element=option)
    exit
  end if
end do

end subroutine extract_options

!-----------------------------------------------------------------

function extract_option_name(option,error)

! function to extract option_name from full option listing
! note, the options should already be left justified from the extract_options subroutine

character(len=*) :: option ! will have length 100
character(len=len(option)) :: extract_option_name
logical :: error
integer :: cut

error = .false.
extract_option_name = ''
cut=scan(option,'=') ! look for the equals sign
if (cut < 1) cut=scan(option,' ') ! if the equals sign is not present (indicated by zero cut) then scan instead for first space

if (cut <= 1) then
  error = .true.
else
  extract_option_name = trim(option(1:cut-1))
end if

end function extract_option_name

!-----------------------------------------------------------------

subroutine set_option_integer(option,option_name,option_variable,option_type)

! subroutine that sets the option_variable whose name is option_name to the integer in the option string

character(len=*) :: option ! will have length 100
character(len=*) :: option_name
character(len=*), optional :: option_type ! ie, general, kernel, solver etc
character(len=20) :: option_type_l ! 
integer :: option_variable
character(len=1000) :: formatline
logical :: error

option_type_l = ''
if (present(option_type)) option_type_l = " "//option_type
  
error = .false.
option_variable = extract_option_integer(option,error)
if (error) call error_stop("could not determine the required "//trim(option_name)//" from the"//trim(option_type_l)// &
  " option "//trim(option))
formatline = '(a,'//trim(dindexformat(option_variable))//')'
write(*,fmt=formatline) "INFO: setting"//trim(option_type_l)//" option "//trim(option_name)//" = ",option_variable

end subroutine set_option_integer

!-----------------------------------------------------------------

function extract_option_integer(option,error)

! function to extract an integer value from a full option listing

character(len=*) :: option ! will have length 100
integer :: extract_option_integer
logical :: error
integer :: cut, ierror

error = .false.
extract_option_integer = 0
cut=scan(option,'=') ! look for the equals sign

if (cut < 1) then
  error = .true.
else
  read(option(cut+1:len(option)),*,iostat=ierror) extract_option_integer
  if (ierror /= 0) error = .true.
end if

end function extract_option_integer

!-----------------------------------------------------------------

subroutine set_option_string(option,option_name,option_variable,option_type)

! subroutine that sets the option_variable whose name is option_name to the string in the option string

character(len=*) :: option ! will have length 100
character(len=*) :: option_name
character(len=*), optional :: option_type ! ie, general, kernel, solver etc
character(len=20) :: option_type_l ! 
character(len=*) :: option_variable
character(len=1000) :: formatline
logical :: error

option_type_l = ''
if (present(option_type)) option_type_l = " "//option_type
  
error = .false.
option_variable = extract_option_string(option,error)
if (error) call error_stop("could not determine the required "//trim(option_name)//" from the"//trim(option_type_l)// &
  " option "//trim(option))
write(*,'(a)') "INFO: setting"//trim(option_type_l)//" option "//trim(option_name)//" = "//trim(option_variable)

end subroutine set_option_string

!-----------------------------------------------------------------

function extract_option_string(option,error)

! function to extract a string value from a full option listing

character(len=*) :: option ! will have length 100
character(len=len(option)) :: extract_option_string
logical :: error
integer :: cut, ierror

error = .false.
extract_option_string = ''
cut=scan(option,'=') ! look for the equals sign

if (cut < 1) then
  error = .true.
else
  read(option(cut+1:len(option)),*,iostat=ierror) extract_option_string
  if (ierror /= 0) then
    error = .true.
  else
    extract_option_string = trim(adjustl(extract_option_string)) ! remove all preceding white space
  end if
end if

end function extract_option_string

!-----------------------------------------------------------------

subroutine set_option_double_precision(option,option_name,option_variable,option_type)

! subroutine that sets the option_variable whose name is option_name to the double_precision in the option string

character(len=*) :: option ! will have length 100
character(len=*) :: option_name
character(len=*), optional :: option_type ! ie, general, kernel, solver etc
character(len=20) :: option_type_l ! 
double precision :: option_variable
character(len=1000) :: formatline
logical :: error

option_type_l = ''
if (present(option_type)) option_type_l = " "//option_type
  
error = .false.
option_variable = extract_option_double_precision(option,error)
if (error) call error_stop("could not determine the required "//trim(option_name)//" from the"//trim(option_type_l)// &
  " option "//trim(option))
formatline = '(a,'//trim(floatformat)//')'
write(*,fmt=formatline) "INFO: setting"//trim(option_type_l)//" option "//trim(option_name)//" = ",option_variable

end subroutine set_option_double_precision

!-----------------------------------------------------------------

function extract_option_double_precision(option,error)

! function to extract an double precision (or real) value from a full option listing

character(len=*) :: option ! will have length 100
double precision :: extract_option_double_precision
logical :: error
integer :: cut, ierror

error = .false.
extract_option_double_precision = 0.d0
cut=scan(option,'=') ! look for the equals sign

if (cut < 1) then
  error = .true.
else
  read(option(cut+1:len(option)),*,iostat=ierror) extract_option_double_precision
  if (ierror /= 0) error = .true.
end if

end function extract_option_double_precision

!-----------------------------------------------------------------

subroutine set_option_logical(option,option_name,option_description,option_variable,option_type)

! subroutine that sets the option_variable whose name is option_name to the logical in the option string

character(len=*) :: option ! full option string, will have length 100
character(len=*) :: option_name ! the referenced name of the option (eg, outputtimings)
character(len=*) :: option_description ! this is a short description of the variables, used only in messages
logical :: option_variable ! this is the variable to set
character(len=*), optional :: option_type ! ie, general, kernel, solver etc
character(len=20) :: option_type_l ! local version of option_type (eg, general, kernel, solver etc)
character(len=1000) :: formatline
logical :: error

option_type_l = ''
if (present(option_type)) option_type_l = " "//option_type
  
error = .false.
option_variable = extract_option_logical(option,error)
if (error) then
! no option for a logical means, set to true, unless name is negated as in nooutputtimings
  if (trim(extract_option_name(option,error)) == "no"//trim(option_name)) then
    option_variable = .false.
  else
    option_variable = .true.
  end if
  if (error) &
    call error_stop("could not determine the required "//trim(option_name)//" ("//trim(option_description)//") from the"// &
      trim(option_type_l)//" option "//trim(option))
else
  if (trim(extract_option_name(option,error)) == "no"//trim(option_name)) &
    call error_stop("could not determine the required "//trim(option_name)//" ("//trim(option_description)//") from the"// &
      trim(option_type_l)//" option "//trim(option))
end if
formatline = '(a,l1)'
write(*,fmt=formatline) "INFO: setting"//trim(option_type_l)//" option "//trim(option_name)//" ("//trim(option_description)// &
  ") = ",option_variable

end subroutine set_option_logical

!-----------------------------------------------------------------

function extract_option_logical(option,error)

! function to extract a logical value from a full option listing

character(len=*) :: option ! will have length 100
logical :: extract_option_logical
logical :: error
integer :: cut, ierror

error = .false.
extract_option_logical = .false.
cut=scan(option,'=') ! look for the equals sign

if (cut < 1) then
  error = .true.
else
  read(option(cut+1:len(option)),*,iostat=ierror) extract_option_logical
  if (ierror /= 0) error = .true.
end if

end function extract_option_logical

!-----------------------------------------------------------------

function ijkstring(centring)

! returns a single character which is appropriate for the type of centring
character(len=1) :: ijkstring
character(len=*) :: centring

if (trim(centring) == 'cell') then
  ijkstring = 'i'
else if (trim(centring) == 'face') then
  ijkstring = 'j'
else if (trim(centring) == 'node') then
  ijkstring = 'k'
else ! could be none centred
  ijkstring = 'n'
end if

end function ijkstring

!----------------------------------------------------------------------------

subroutine push_glue_face(region,option_line)

! pushes another glue_face entry onto the array
! this must happen before options are split from a single line of text

character(len=1000), dimension(2) :: region ! the two regions to be glued together
character(len=*) :: option_line ! a text line of options, taken from the input file
type(glue_face_type), dimension(:), allocatable :: old_glue_face
integer :: old_glue_face_size

old_glue_face_size = 0
if (allocated(glue_face)) then
  old_glue_face_size = ubound(glue_face,1)
  allocate(old_glue_face(old_glue_face_size))
  old_glue_face = glue_face
  deallocate(glue_face)
end if

allocate(glue_face(old_glue_face_size+1))

if (old_glue_face_size > 0) then
  glue_face(1:old_glue_face_size) = old_glue_face
  deallocate(old_glue_face)
end if

glue_face(old_glue_face_size+1)%region=region
glue_face(old_glue_face_size+1)%option_line=option_line

end subroutine push_glue_face

!----------------------------------------------------------------------------

subroutine add_to_scalar_list(list,new_element,check_length)

type(scalar_list_type) :: list
double precision :: new_element
logical, optional :: check_length ! if this is true check the length of the list before adding element - default is off to save time
logical :: check_lengthl ! local version of check_length
integer :: change

change = 0

list%length = list%length + 1
check_lengthl = .false.
if (present(check_length)) check_lengthl = check_length

if (check_lengthl) then
  if (.not.allocated(list%elements)) then
    change = list%length
  else
    change = list%length - ubound(list%elements,1)
  end if

  if (change > 0) call resize_double_precision_array(keep_data=.true.,array=list%elements,change=change)
end if

list%elements(list%length) = new_element

end subroutine add_to_scalar_list

!----------------------------------------------------------------------------

subroutine add_to_integer_list(list,new_element,check_length)

type(integer_list_type) :: list
integer :: new_element
logical, optional :: check_length ! if this is true check the length of the list before adding element - default is off to save time
logical :: check_lengthl ! local version of check_length
integer :: change

change = 0

list%length = list%length + 1
check_lengthl = .false.
if (present(check_length)) check_lengthl = check_length

if (check_lengthl) then
  if (.not.allocated(list%elements)) then
    change = list%length
  else
    change = list%length - ubound(list%elements,1)
  end if

  if (change > 0) call resize_integer_array(keep_data=.true.,array=list%elements,change=change)
end if

list%elements(list%length) = new_element

end subroutine add_to_integer_list

!----------------------------------------------------------------------------

subroutine sort_scalar_list(list,direction)

! eg, http://rosettacode.org/wiki/Sorting_algorithms/Insertion_sort#Fortran
type(scalar_list_type) :: list
double precision :: direction
double precision :: value
integer :: i, j

do i = 2, list%length
  j = i - 1
  value = list%elements(i)
  do while ( j >= 1 .and. direction*list%elements(j) > value*direction)
    list%elements(j+1) = list%elements(j)
    j = j - 1
  end do
  list%elements(j+1) = value
end do

end subroutine sort_scalar_list

!----------------------------------------------------------------------------

subroutine sort_scalar_index_list(index_list,value_list,direction)

! same as sort_scalar_list, but here value_list positions are unchanged, and instead index_list is returned as the ordered
!  indices of the value_list array
! so ordered list is: value_list%elements(index_list%elements(n)),n=1,index_list%length)
type(integer_list_type) :: index_list
type(scalar_list_type) :: value_list
double precision :: direction
double precision :: current
integer :: i, j, icurrent

do i = 2, index_list%length
  j = i - 1
  icurrent = index_list%elements(i)
  current = value_list%elements(icurrent)
  do while ( direction*value_list%elements(index_list%elements(j)) > current*direction)
    index_list%elements(j+1) = index_list%elements(j)
    j = j - 1
    if (j == 0) exit
  end do
  index_list%elements(j+1) = icurrent
end do

end subroutine sort_scalar_index_list

!----------------------------------------------------------------------------

subroutine add_to_vector_list(list,new_element,check_length)

type(vector_list_type) :: list
double precision, dimension(totaldimensions) :: new_element
logical, optional :: check_length ! if this is true check the length of the list before adding element - default is off to save time
logical :: check_lengthl ! local version of check_length
integer, dimension(2) :: change_2d

change_2d = 0
check_lengthl = .false.
if (present(check_length)) check_lengthl = check_length

list%length = list%length + 1

if (check_lengthl) then
  if (.not.allocated(list%elements)) then
    change_2d(1) = totaldimensions
    change_2d(2) = list%length
  else
    change_2d(2) = list%length - ubound(list%elements,2)
  end if

  if (change_2d(2) > 0) call resize_double_precision_2d_array(keep_data=.true.,array=list%elements,change=change_2d)
end if

list%elements(:,list%length) = new_element

end subroutine add_to_vector_list

!----------------------------------------------------------------------------

subroutine resize_double_precision_2d_array(keep_data,array,change,new_size,default_value)

! here we change the size of an array (by change) while maintaining its data
! this is for a two dimensional array, with new_size introduced as a 2d vector
! change values are >= 0
! if a new_size index is negative then this index is taken to be the old size

double precision, dimension(:,:), allocatable :: array, array_store
double precision, intent(in), optional :: default_value
integer, dimension(2), intent(in), optional :: change, new_size
integer, dimension(2) :: change_l, old_size, new_size_l, min_size
integer :: n, m
logical, optional :: keep_data

change_l = 0 ! default here is for no change
if (present(change)) change_l = change

old_size = 0
if (allocated(array)) then
  do n = 1, 2
    old_size(n) = ubound(array,n)
  end do
end if

if (present(new_size)) then
  if (present(change)) stop 'ERROR: both change and new_size specified in resize_double_precision_2d_array'
  new_size_l = new_size
  do n = 1, 2
    if (new_size_l(n) < 0) new_size_l(n) = old_size(n) ! if a new_size index is negative, make this dimension the same as the old array's size
  end do
else
  new_size_l = old_size + change_l
end if

do n = 1, 2
  min_size(n) = min(old_size(n),new_size_l(n))
end do

! if not keeping data (default is to keep data) then...
if (present(keep_data)) then
  if (.not.keep_data) min_size = 0
end if

! if any data is to be required in the future then save it now
if (maxval(min_size)>0) then
  allocate(array_store(min_size(1),min_size(2)))
  do n = 1, min_size(1)
    do m = 1, min_size(2)
      array_store(n,m) = array(n,m) ! explicit loops, so that indices are correct across unequally sized arrays
    end do
  end do
end if

!if (old_size>0) deallocate(array)
if (allocated(array)) deallocate(array)

if (maxval(new_size_l)>0) then
  allocate(array(new_size_l(1),new_size_l(2)))
  if (present(default_value)) then
    array = default_value
  else
    array = 0.d0 ! zero all values as a default
  end if
  if (maxval(min_size)>0) then
    do n = 1, min_size(1)
      do m = 1, min_size(2)
        array(n,m) = array_store(n,m) ! explicit loops, so that indices are correct across unequally sized arrays
      end do
    end do
    deallocate(array_store)
  end if
end if

end subroutine resize_double_precision_2d_array

!-----------------------------------------------------------------

subroutine resize_integer_2d_array(keep_data,array,change,new_size,default_value)

! here we change the size of an array (by change) while maintaining its data
! this is for a two dimensional array, with new_size introduced as a 2d vector
! change values are >= 0
! if a new_size index is negative then this index is taken to be the old size

integer, dimension(:,:), allocatable :: array, array_store
integer, intent(in), optional :: default_value
integer, dimension(2), intent(in), optional :: change, new_size
integer, dimension(2) :: change_l, old_size, new_size_l, min_size
integer :: n, m
logical, optional :: keep_data

change_l = 0 ! default here is for no change
if (present(change)) change_l = change

old_size = 0
if (allocated(array)) then
  do n = 1, 2
    old_size(n) = ubound(array,n)
  end do
end if

if (present(new_size)) then
  if (present(change)) stop 'ERROR: both change and new_size specified in resize_integer_2d_array'
  new_size_l = new_size
  do n = 1, 2
    if (new_size_l(n) < 0) new_size_l(n) = old_size(n) ! if a new_size index is negative, make this dimension the same as the old array's size
  end do
else
  new_size_l = old_size + change_l
end if

do n = 1, 2
  min_size(n) = min(old_size(n),new_size_l(n))
end do

! if not keeping data (default is to keep data) then...
if (present(keep_data)) then
  if (.not.keep_data) min_size = 0
end if

! if any data is to be required in the future then save it now
if (maxval(min_size)>0) then
  allocate(array_store(min_size(1),min_size(2)))
  do n = 1, min_size(1)
    do m = 1, min_size(2)
      array_store(n,m) = array(n,m) ! explicit loops, so that indices are correct across unequally sized arrays
    end do
  end do
end if

!if (old_size>0) deallocate(array)
if (allocated(array)) deallocate(array)

if (maxval(new_size_l)>0) then
  allocate(array(new_size_l(1),new_size_l(2)))
  if (present(default_value)) then
    array = default_value
  else
    array = 0 ! zero all values as a default
  end if
  if (maxval(min_size)>0) then
    do n = 1, min_size(1)
      do m = 1, min_size(2)
        array(n,m) = array_store(n,m) ! explicit loops, so that indices are correct across unequally sized arrays
      end do
    end do
    deallocate(array_store)
  end if
end if

end subroutine resize_integer_2d_array

!----------------------------------------------------------------------------

function print_kernel_reflect(kernel)

type(kernel_type) :: kernel
character(len=10000) :: print_kernel_reflect
character(len=12) :: temp
integer :: n

write(print_kernel_reflect,'(a,l1)') ': reflect_present = ',kernel%reflect_present

if (kernel%reflect_present) then
  print_kernel_reflect = trim(print_kernel_reflect)//': reflect_multiplier ='
  if (allocated(kernel%reflect_multiplier)) then
    do n = 1, ubound(kernel%reflect_multiplier,2)
      write(temp,'(a,3(i3),a)') ' (',kernel%reflect_multiplier(:,n),')'
      print_kernel_reflect = trim(print_kernel_reflect)//temp
    end do
  else
    print_kernel_reflect = trim(print_kernel_reflect)//' not allocated, whereas reflect_present is true, indicating an error'
  end if
end if

end function print_kernel_reflect

!-----------------------------------------------------------------

function same_entity(ijk,reflect_multiplier,r,dx)

! tests whether an element (cell, node) are the same based upon their indices (ijk), reflect_multiplier and location (r)
! locations are considered coincident based on maximum difference in location components right now (more efficient than true distance)

logical :: same_entity
integer, dimension(2) :: ijk
integer, dimension(totaldimensions,2) :: reflect_multiplier
double precision, dimension(totaldimensions,2) :: r
double precision :: dx ! lengthscale of position vectors
double precision, parameter :: eps_dx = 1.d-8 ! relative distance above which elements are considered to be not coincident
integer :: n

same_entity = .false.
if (ijk(1) /= ijk(2)) return
do n = 1, totaldimensions
  if (reflect_multiplier(n,1) /= reflect_multiplier(n,2)) return
end do
do n = 1, totaldimensions
  if (abs(r(n,2) - r(n,1)) > dx*eps_dx) return ! test each component - this is suffice for estabilishing coincidence
end do

same_entity = .true.

end function same_entity

!-----------------------------------------------------------------

function cell_shares_a_node(icentre,i,reflect_multiplier,r,dx,nodes)

! tests whether a cell shares a node with the icentre cell
! NB: the icentre cell is in its native position for the purposes of this test, with the reflect_multiplier and r vectors relative to icentre
! locations are considered coincident based on maximum difference in location components right now (more efficient than true distance)

logical :: cell_shares_a_node
integer :: icentre ! cell around which we are looking
integer :: i ! cell to test whether it or its sister has a common node with icentre
integer, dimension(totaldimensions) :: reflect_multiplier
double precision, dimension(totaldimensions) :: r
double precision, dimension(2) :: xnode
double precision :: dx ! lengthscale of position vectors
integer, optional :: nodes ! number of common nodes to be found for a positive result
double precision, parameter :: eps_dx = 1.d-8 ! relative distance above which elements are considered to be not coincident
integer :: n, k, kk, k2, kk2, nodes_l

nodes_l = 1
if (present(nodes)) nodes_l = nodes

cell_shares_a_node = .false.
cell_node_loop: do kk = 1, ubound(cell(icentre)%knode,1)
  if (ubound(cell(icentre)%knode,1)-kk+1 < nodes_l) return ! the number of cell nodes left to test is less than that required for a positive result
  k = cell(icentre)%knode(kk)
  inner_loop: do kk2 = 1, ubound(cell(i)%knode,1)
    k2 = cell(i)%knode(kk2)
    if (k == k2.or.location_in_list(array=node(k2)%glue_knode,element=k) /= 0) then
! nodes are contained in both cells with the same or coincident glued knode indices
! now test to see whether they are actually in the same location
      do n = 1, totaldimensions
! find location of each node, respecting any reflections, and per component (faster)
        xnode(1) =  node(k)%x(n) - cell(icentre)%x(n)
        xnode(2) = r(n) + dble(reflect_multiplier(n))*( node(k2)%x(n) - cell(i)%x(n) )
        if (abs(xnode(2) - xnode(1)) > dx*eps_dx) cycle inner_loop ! test each component - this is suffice for estabilishing coincidence
      end do
      if (nodes_l == 1) then
        cell_shares_a_node = .true.
        return
      else
        nodes_l = nodes_l - 1
        cycle cell_node_loop
      end if
    end if
  end do inner_loop
end do cell_node_loop

end function cell_shares_a_node

!-----------------------------------------------------------------

function face_shares_a_node(jcentre,i,reflect_multiplier,r,dx)

! tests whether a cell shares a node with the jcentre face
! same as cell_shares_a_node but for a face

logical :: face_shares_a_node
integer :: jcentre ! face around which we are looking
integer :: i ! cell to test whether it or its sister has a common node with icentre
integer, dimension(totaldimensions) :: reflect_multiplier
double precision, dimension(totaldimensions) :: r
double precision, dimension(2) :: xnode
double precision :: dx ! lengthscale of position vectors
double precision, parameter :: eps_dx = 1.d-8 ! relative distance above which elements are considered to be not coincident
integer :: n, k, kk, k2, kk2
logical, parameter :: debug = .false.

face_shares_a_node = .false.
do kk = 1, ubound(face(jcentre)%knode,1)
  k = face(jcentre)%knode(kk)
  inner_loop: do kk2 = 1, ubound(cell(i)%knode,1)
    k2 = cell(i)%knode(kk2)
    if (debug) write(83,*) 'in face_shares_a_node: k = ',k,': k2 = ',k2
    if (k == k2.or.location_in_list(array=node(k2)%glue_knode,element=k) /= 0) then
      if (debug) write(83,*) 'in face_shares_a_node: k and k2 are coincident: checking on locations'
! nodes are contained in both cells with the same or coincident glued knode indices
! now test to see whether they are actually in the same location
      do n = 1, totaldimensions
! find location of each node, respecting any reflections, and per component (faster)
        xnode(1) =  node(k)%x(n) - face(jcentre)%x(n)
        xnode(2) = r(n) + dble(reflect_multiplier(n))*( node(k2)%x(n) - cell(i)%x(n) )
        if (debug) write(83,*) 'in face_shares_a_node: dimension n = ',n,': xnode(1) = ',xnode(1),': xnode(2) = ',xnode(2)
        if (abs(xnode(2) - xnode(1)) > dx*eps_dx) cycle inner_loop ! test each component - this is suffice for estabilishing coincidence
      end do
      face_shares_a_node = .true.
      if (debug) write(83,*) 'in face_shares_a_node: found a shared node'
      return
    end if
  end do inner_loop
end do

if (debug) write(83,*) 'in face_shares_a_node: did not find a shared node'

end function face_shares_a_node

!-----------------------------------------------------------------

subroutine expand_mask(icentre,jcentre,have_icell,limit_mask_to_shared_nodes,include_adjacent_boundary_cells,maximum_separation, &
  imask,separation_index,separation_array,reflect_multiplier,r,dx)

! this routine originally taken from expand_kernel_mask in kernel_module, but now generalised
! idea is to expand a mask (a kernel mask if called from setup_kernels or an icell mask if called from setup_mesh) by moving from one
!  base set of cells through ajoining faces to a larger set of surrounding cells

integer, optional :: icentre, jcentre ! if either is present, this is the element around which the kernel is based
logical :: have_icell ! if true then cell%icell values have already been calculated (ie, we are now calculating kernels) and so can be used here
logical :: limit_mask_to_shared_nodes ! if on then cells in mask must share a node with the base entity (icentre or jcentre)
logical :: include_adjacent_boundary_cells ! cells are also considered neighbours if they are both boundary cells and share at least (dimension-1) nodes
integer, optional :: maximum_separation ! this is the maximum separation that we wish to include.  If not present, then number of elements in the mask is not limited by separation.  On output contains the maximum separation used.
integer, dimension(:), allocatable :: imask ! this is the list of cells that is passed out, with a changed size
integer, dimension(:), allocatable :: separation_index ! this is the list of separation indices is passed out, with a changed size
integer, dimension(:), allocatable, optional :: separation_array ! this the separation indices per element passed out, with a changed size, optional
integer, dimension(:,:), allocatable :: reflect_multiplier ! this the reflect_multiplier for each cell: first index is the dimension (1:3), second is kernel element number
double precision, dimension(:,:), allocatable :: r ! location of cell centres relative to centre of kernel: first index is the dimension (1:3), second is the kernel element number
double precision :: dx ! order of magnitude of element dimensions
integer, dimension(totaldimensions) :: reflect_multiplier2
double precision, dimension(totaldimensions) :: r2
integer :: separation, i, ii, i2, ii2, i3, ii3, number_added, this_separation_start, last_separation_start, &
  sign_upcell, n, maximum_separation_l, start_index, end_index, j, jj, ii2max
character(len=1000) :: formatline
integer, dimension(2), save :: change_2d = [0,1], ijk_2d
double precision, dimension(totaldimensions,2), save :: r_2d
integer, dimension(totaldimensions,2), save :: reflect_multiplier_2d
logical, parameter :: debug = .false.

if (debug) write(83,'(80(1h+)/a)') 'subroutine expand_mask'

separation = ubound(separation_index,1) ! this is the previously highest defined separation
if (present(maximum_separation)) then
  maximum_separation_l = maximum_separation
else
  maximum_separation_l = 1000 ! set this to a large number if mask is not to be limited by the separation of elements
end if

if (ubound(imask,1) /= separation_index(separation)) call error_stop( &
  'imask size does not match with final separation_index in calling arguments to expand_mask')
if (.not.present(maximum_separation).and..not.limit_mask_to_shared_nodes) call error_stop( &
  'incompatible options in expand_mask: maximum_separation is not present and limit_mask_to_shared_nodes off')
if (limit_mask_to_shared_nodes.and..not.(present(icentre).or.present(jcentre))) call error_stop( &
  'limit_mask_to_shared_nodes is on in expand_mask, but neither icentre or jcentre are passed in')
if (include_adjacent_boundary_cells.and..not.have_icell) call error_stop( &
  'include_adjacent_boundary_cells is on in expand_mask but have_icell is not') 

if (debug) then
  write(83,*) 'calling parameters: maximum_separation_l = ',maximum_separation_l
  write(83,*) 'calling parameters: separation_index = ',separation_index
  write(83,*) 'calling parameters: dx = ',dx
  if (present(icentre)) write(83,'(a,i10,a)') 'calling parameters: icentre = ',icentre,'i'
  if (present(jcentre)) write(83,'(a,i10,a)') 'calling parameters: jcentre = ',jcentre,'j'
  write(83,'(3(a,l1))') 'calling parameters: have_icell = ',have_icell,': limit_mask_to_shared_nodes = ', &
    limit_mask_to_shared_nodes,': include_adjacent_boundary_cells = ',include_adjacent_boundary_cells 
  write(83,'(a,1000(1x,i11))')      'calling parameters:              imask = ',imask
  if (present(separation_array)) write(83,'(a,1000(1x,i11))')      'calling parameters:   separation_array = ',separation_array
  write(83,'(a,1000(1x,3(i2),5x))') 'calling parameters: reflect_multiplier = ',(reflect_multiplier(:,n),n=1,ubound(reflect_multiplier,2))
  write(83,'(a,1000(1x,g10.2,1x))') 'calling parameters:                 r1 = ',r(1,:)
  write(83,'(a,1000(1x,g10.2,1x))') 'calling parameters:                 r2 = ',r(2,:)
  write(83,'(a,1000(1x,g10.2,1x))') 'calling parameters:                 r3 = ',r(3,:)
end if
  
number_added = 1 ! set the number of cells added to nonzero for first loop

! loop through separations until we reach the requested maximum_separation
separation_loop: do while (separation < maximum_separation_l.and.number_added > 0)

  number_added = 0

! define indices of imask that start this separation and the last
  this_separation_start = 1
  last_separation_start = 1
  if (separation > 1) this_separation_start = separation_index(separation-1) + 1
  if (separation > 2) last_separation_start = separation_index(separation-2) + 1

! if (debug) then
!   write(83,'(3(a,i3))') 'separation = ',separation,': start_index = ',start_index,': end_index = ',end_index
!   write(83,*) 'previous separation (separation = ',separation-1,') list = ',imask(start_index:end_index)
! end if

! loop through all cells which have this separation looking for ones that are adjcaent and not this, the next or the last separation already
  this_separation_loop: do ii = this_separation_start, ubound(imask,1) ! assuming that in fortran this index is not updated dynamically
    i = imask(ii)

! loop through neighbours of this cell that share a face element
! or, if include_adjacent_boundary_cells is on, also include boundary cells that share at least dimension-1 nodes with other boundary cells

    if (include_adjacent_boundary_cells.and.cell(i)%type == 2) then
      ii2max = ubound(cell(i)%icell,1) ! use icell if including adjacent boundary cells and we are in a boundary cell
    else
      ii2max = ubound(cell(i)%jface,1)+1 ! default is to just include the surrounding adjacent cells, using jface to locate them
    end if

!   neighbour_loop: do ii2 = 2, ubound(cell(i)%jface,1)+1
!     i2 = cell(i)%icell(ii2)
!   neighbour_loop: do jj = 1, ubound(cell(i)%jface,1)
!     j = cell(i)%jface(jj)

    neighbour_loop: do ii2 = 2, ii2max

! find the index i2, location r2 and reflect_multiplier2 for the examined cell
!------------------
! face adjacent cells
      if (ii2 <= ubound(cell(i)%jface,1)+1) then
        jj = ii2 - 1
        j = cell(i)%jface(jj)
        i2 = face(j)%icell(1)
        if (i2 == i) then
          i2 = face(j)%icell(2)
          sign_upcell = 1 ! this is positive if the face normal points away from cell i towards cell i2
        else
          sign_upcell = -1
        end if
! find location of examined cell
! note, dx_up and dx_down already take into account any local glued face effects
!       r2 = r(:,ii) + ( face(j)%dx_up - face(j)%dx_down )*dble(reflect_multiplier(:,ii)*sign_upcell)
        r2 = r(:,ii) + ( face(j)%r(:,2) - face(j)%r(:,1) )*dble(reflect_multiplier(:,ii)*sign_upcell)
! also find reflect_multiplier for the examined cell
        reflect_multiplier2 = reflect_multiplier(:,ii)
        if (face(j)%glue_reflect /= 0) reflect_multiplier2(face(j)%glue_reflect) = -reflect_multiplier2(face(j)%glue_reflect) 

      else
!------------------
! boundary cells adjacent to other boundary cells

! if we are here then location will be based on pre-calculated icell info which includes only those cells that share a node with i
        i2 = cell(i)%icell(ii2)
! however only relevant if the examined cell is a boundary cell
        if (cell(i2)%type /= 2) cycle neighbour_loop
! find location based on saved r
        r2 = r(:,ii) + cell(i)%r(:,ii2)*dble(reflect_multiplier(:,ii)) ! vector components will be multiplied
! and reflect_multiplier also based on saved reflect_multiplier
        reflect_multiplier2 = cell(i)%reflect_multiplier(:,ii2)*reflect_multiplier(:,ii)

! need to check number of nodes in common here for dim=2, only accepting cells if they share 2 (or more?) nodes
! the matching here is done with cell i as the centre
        if (cell(i)%dimensions == 2) then
          if (.not.cell_shares_a_node(icentre=i,i=i2,reflect_multiplier=cell(i)%reflect_multiplier(:,ii2), &
            r=cell(i)%r(:,ii2),dx=dx,nodes=2)) cycle neighbour_loop
        end if
      end if
!------------------

! check whether cell shares a common node with the central element, and if not (and the kernel is limited to limit_mask_to_shared_nodes) then skip
      if (limit_mask_to_shared_nodes) then
        if (present(icentre)) then
          if (.not.cell_shares_a_node(icentre=icentre,i=i2,reflect_multiplier=reflect_multiplier2,r=r2,dx=dx)) &
            cycle neighbour_loop
        else if (present(jcentre)) then
          if (debug) write(83,'(a,i3,a,3(i2),a,3(g10.2))') &
            'checking if face_shares_a_node: i2 = ',i2,': reflect_multiplier2 = ',reflect_multiplier2,': r2 = ',r2
          if (.not.face_shares_a_node(jcentre=jcentre,i=i2,reflect_multiplier=reflect_multiplier2,r=r2,dx=dx)) &
            cycle neighbour_loop
          if (debug) write(83,'(a)') '  it does'
        end if
      end if

! loop through cells in the current separation and those that have been added in the new separation level testing to see whether
!  this cell has been added before
      ijk_2d(2) = i2
      reflect_multiplier_2d(:,2) = reflect_multiplier2
      r_2d(:,2) = r2
      do ii3 = last_separation_start, ubound(imask,1)
        i3 = imask(ii3)
        ijk_2d(1) = i3
        reflect_multiplier_2d(:,1) = reflect_multiplier(:,ii3)
        r_2d(:,1) = r(:,ii3)
        if (same_entity(ijk=ijk_2d,reflect_multiplier=reflect_multiplier_2d,r=r_2d,dx=dx)) &
          cycle neighbour_loop
      end do

! if we are here then cell i2 is not in the last, current or new separation levels so should be added to the list
      if (debug) then
        formatline = '(a,'//trim(indexformat)//',a,i2,a,3(1x,i2),a,3(1x,g10.2))'
        write(83,fmt=formatline) 'found new imask element = ',i2,' having separation = ',separation+1,': reflect_multiplier2 =', &
          reflect_multiplier2,': r2 =',r2
        if (ii2 > ubound(cell(i)%jface,1)+1) write(83,'(2(a,i1))') '  adjacent boundary cell: cell(i)%type = ',cell(i)%type, &
          ': cell(i2)%type = ',cell(i2)%type
      end if
      call push_integer_array(array=imask,new_element=i2)
      call resize_integer_2d_array(array=reflect_multiplier,change=change_2d,keep_data=.true.)
      reflect_multiplier(:,ubound(reflect_multiplier,2)) = reflect_multiplier2
      call resize_double_precision_2d_array(array=r,change=change_2d,keep_data=.true.)
      r(:,ubound(r,2)) = r2
      number_added = number_added + 1
    end do neighbour_loop

! TODO: call subroutine recursively here to find all surrounding cells, and pick out any boundary cells
! no, store this info for all cells in icell_reflect and icell_r arrays, possibly deallocating after being used, or possibly only for boundary cell%type = 2 cells and their boundary counterparts

! ! include boundary cells that share a node with other boundary cells
!     if (boundary_node_separations.and.cell(i)%type == 2) then
!       boundary_neighbour_loop: do ii2 = ubound(cell(i)%jface,1)+2, ubound(cell(i)%icell,1)
!         i2 = cell(i)%icell(ii2)
!         if (location_in_list(array=imask,element=i2) /= 0) cycle boundary_neighbour_loop ! cell is already in list
!         if (limit_mask_to_shared_nodes.and.location_in_list(array=iarray,element=i2) == 0) cycle boundary_neighbour_loop ! cell is not in overall iarray list
!         if (.not.cell(i2)%type == 2) cycle boundary_neighbour_loop ! cell is also a boundary cell
! ! if the boundary cell has a dimension of 2, we need the cells to share atleast two nodes to be considered adjacent
!         if (cell(i)%dimensions == 2) then
!           common_nodes = 0
!           do kk = 1, ubound(cell(i)%knode,1)
!             k = cell(i)%knode(kk)
!             if (location_in_list(array=cell(i2)%knode,element=k) /= 0) common_nodes = common_nodes + 1
!             if (common_nodes == 2) exit
!           end do
!           if (common_nodes < 2) cycle boundary_neighbour_loop
!         end if
! ! if we are here then cell i2 has the current separation and is an adjacect boundary cell to a boundary cell
!         if (debug) write(83,*) 'found new boundary to boundary imask element = ',i2,' having separation = ',separation
!         call push_integer_array(array=imask,new_element=i2)
!         separation_index(separation) = ubound(imask,1)
!         number_added = number_added + 1
!       end do boundary_neighbour_loop
!     end if

  end do this_separation_loop

  if (number_added > 0) then
    separation = separation + 1 ! update to the current maximum_separation
    call push_integer_array(array=separation_index,new_element=ubound(imask,1))
  end if
    
! if (debug) write(83,*) 'calculated separation (separation = ',separation,') list = ', &
!   imask(separation_index(separation-1)+1:separation_index(separation))
  
end do separation_loop

if (present(maximum_separation)) maximum_separation = separation ! update maximum_separation to reflect the maximum separation present in the kernel

! update separation_array if passed in
if (present(separation_array)) then
  call resize_integer_array(keep_data=.false.,array=separation_array,new_size=separation_index(ubound(separation_index,1)))
  end_index = 0
  do separation = 1, ubound(separation_index,1)
    start_index = end_index + 1
    end_index = separation_index(separation)
    separation_array(start_index:end_index) = separation
  end do
end if

if (debug) then
  write(83,*) 'final parameters: separation_index = ',separation_index
  if (present(maximum_separation)) write(83,*) 'final parameters: maximum_separation = ',maximum_separation
  write(83,*) 'final parameters: ubound(imask,1) = ',ubound(imask,1)
  write(83,'(a,1000(1x,i11))')      'final parameters:              imask = ',imask
  if (present(separation_array)) write(83,'(a,1000(1x,i11))')      'final parameters:   separation_array = ',separation_array
  write(83,'(a,1000(1x,3(i2),5x))') 'final parameters: reflect_multiplier = ',(reflect_multiplier(:,n),n=1,ubound(reflect_multiplier,2))
  write(83,'(a,1000(1x,g10.2,1x))') 'final parameters:                 r1 = ',r(1,:)
  write(83,'(a,1000(1x,g10.2,1x))') 'final parameters:                 r2 = ',r(2,:)
  write(83,'(a,1000(1x,g10.2,1x))') 'final parameters:                 r3 = ',r(3,:)
end if
  
if (debug) write(83,'(a/80(1h-))') 'subroutine expand_mask'

end subroutine expand_mask

! !-----------------------------------------------------------------

! function cell_shares_a_single_node(icentre,i,reflect_multiplier,r,dx)

! ! tests whether a cell shares a node with the icentre cell
! ! NB: the icentre cell is in its native position for the purposes of this test, with the reflect_multiplier and r vectors relative to icentre
! ! locations are considered coincident based on maximum difference in location components right now (more efficient than true distance)

! logical :: cell_shares_a_single_node
! integer :: icentre ! cell around which we are looking
! integer :: i ! cell to test whether it or its sister has a common node with icentre
! integer, dimension(totaldimensions) :: reflect_multiplier
! double precision, dimension(totaldimensions) :: r
! double precision, dimension(2) :: xnode
! double precision :: dx ! lengthscale of position vectors
! double precision, parameter :: eps_dx = 1.d-8 ! relative distance above which elements are considered to be not coincident
! integer :: n, k, kk, k2, kk2

! cell_shares_a_single_node = .false.
! do kk = 1, ubound(cell(icentre)%knode,1)
!   k = cell(icentre)%knode(kk)
!   inner_loop: do kk2 = 1, ubound(cell(i)%knode,1)
!     k2 = cell(i)%knode(kk2)
!     if (k == k2.or.location_in_list(array=node(k2)%glue_knode,element=k) /= 0) then
! ! nodes are contained in both cells with the same or coincident glued knode indices
! ! now test to see whether they are actually in the same location
!       do n = 1, totaldimensions
! ! find location of each node, respecting any reflections, and per component (faster)
!         xnode(1) =  node(k)%x(n) - cell(icentre)%x(n)
!         xnode(2) = r(n) + dble(reflect_multiplier(n))*( node(k2)%x(n) - cell(i)%x(n) )
!         if (abs(xnode(2) - xnode(1)) > dx*eps_dx) cycle inner_loop ! test each component - this is suffice for estabilishing coincidence
!       end do
!       cell_shares_a_single_node = .true.
!       return
!     end if
!   end do inner_loop
! end do

! end function cell_shares_a_single_node

!---------------------------------------------------------------------------

subroutine add_to_separation_list(local_list,iadd,iiadd,ifrom,iifrom)

! here we add a cell to the separation list if it isn't in the list previously, possibly taking account of glued faces
! note, the separation levels in the arb files start at 0, whereas the separation_index array starts at 1 - ie, there is an offset of 1
!  between the array and use, hence the +1's in this subroutine

type(separation_list_type) :: local_list ! reference to the separation_list(thread) that should be passed in
integer, optional :: iadd ! cell we are trying to add to the list
integer, optional :: iiadd ! icell reference (from cell(ifrom)%icell) to the cell that we are trying to add to the list
integer, optional :: ifrom ! cell that iadd was referenced from (ie, shares a node with)
integer, optional :: iifrom ! local_list reference (local_list%icell(iifrom)) to the cell that iadd was referenced from (ie, shares a node with)
integer :: nseparation, this_index, next_index, last_start_index, ii
integer, dimension(2), parameter :: change_2d = [0,1]
integer, dimension(2) :: ijk_2d
double precision, dimension(totaldimensions,2) :: r_2d
integer, dimension(totaldimensions,2) :: reflect_multiplier_2d
logical, parameter :: debug = .false.

! this is the current separation level
! cells are being added in the next separation level
nseparation = local_list%nseparation

if (debug) write(91,*) '+++++++ nseparation = ',nseparation,': present(iadd) = ',present(iadd)

if (.not.present(iadd)) then
! the purpose here is merely to ensure that the separation_index array is large enough

! otherwise we are looking to add a cell to the list
  this_index = local_list%separation_index(nseparation+1) ! this is the index of the last cell in the current separation

! if the new separation level is not defined or allocated, increase it's size by one and set it to the nseparation_index of the current separation
  if (allocatable_integer_size(local_list%separation_index) < nseparation+2) then
    call push_integer_array(array=local_list%separation_index,new_element=this_index)
  else if (local_list%separation_index(nseparation+2) == 0) then
    local_list%separation_index(nseparation+2) = this_index ! or do the same if not yet defined
  end if ! otherwise there is already at least one cell in the new separation level

! if the new separation level is not defined or allocated, increase it's size by one and set it to the nseparation_index of the current separation
! if (allocatable_integer_size(local_list%separation_index) < nseparation+2) &
!   call push_integer_array(array=local_list%separation_index,new_element=0)

else if (nseparation == -1) then
! special case this first one, noting that each of the four elements already has a minimum of one element (done in setup_module)

  if (debug) write(91,*) 'iadd = ',iadd

  local_list%icell = 0 ! not strictly necessary
  local_list%icell(1) = iadd
  local_list%separation_index = 0
  local_list%separation_index(1) = 1
  local_list%reflect_multiplier = 1
  local_list%r = 0.d0

else
! otherwise we are looking to add a cell to the list
! this_index = local_list%separation_index(nseparation+1) ! this is the index of the last cell in the current separation

! if the new separation level is not defined or allocated, increase it's size by one and set it to the nseparation_index of the current separation
! if (allocatable_integer_size(local_list%separation_index) < nseparation+2) then
!   call push_integer_array(array=local_list%separation_index,new_element=this_index)
! else if (local_list%separation_index(nseparation+2) == 0) then
!   local_list%separation_index(nseparation+2) = this_index ! or do the same if not yet defined
! end if ! otherwise there is already at least one cell in the new separation level

! this code now replacing the above as call with no iadd now performed
! if (local_list%separation_index(nseparation+2) == 0) local_list%separation_index(nseparation+2) = this_index ! or do the same if not yet defined

  if (debug) write(91,*) 'iadd = ',iadd,': ifrom = ',ifrom

  next_index = local_list%separation_index(nseparation+2)
  if (nseparation > 1) then
    last_start_index = local_list%separation_index(nseparation-1)+1
  else
    last_start_index = 1
  end if

  if (debug) then
    write(91,*) 'last_start, next_index = ',last_start_index,next_index
    write(91,*) 'separation_index = ',local_list%separation_index
    write(91,*) 'icell = ',local_list%icell
    write(91,*) 'reflect_multiplier = ',local_list%reflect_multiplier
    write(91,*) 'r = ',local_list%r
  end if

! find trial r and reflect_multiplier functions for the cell to be added, and add this and the index into the second component of three comparison arrays
  ijk_2d(2) = iadd
! requires iifrom (based on local_list), ifrom and iiadd (based on cell(ifrom)$icell)
! r_2d(:,2) = local_list%r(:,iifrom) + cell(ifrom)%r(:,iiadd)*dble(local_list%reflect_multiplier(:,iifrom)) ! vector components will be multiplied
! now uses celltoicellr wrapper that does not require r to be allocated
  r_2d(:,2) = local_list%r(:,iifrom) + celltoicellr(i=ifrom,ns=iiadd)*dble(local_list%reflect_multiplier(:,iifrom)) ! vector components will be multiplied
! and reflect_multiplier also based on saved reflect_multiplier
! reflect_multiplier_2d(:,2) = cell(ifrom)%reflect_multiplier(:,iiadd)*local_list%reflect_multiplier(:,iifrom)
  reflect_multiplier_2d(:,2) = celltoicellreflect_multiplier(i=ifrom,ns=iiadd)*local_list%reflect_multiplier(:,iifrom)

  if (debug) then
    write(91,*) 'before loop: iadd defined by:'
    write(91,*) 'ijk_2d(:,2) = ',ijk_2d(2)
    write(91,*) 'reflect_multiplier_2d(:,2) = ',reflect_multiplier_2d(:,2)
    write(91,*) 'r_2d(:,2) = ',r_2d(:,2)
  end if

! now loop through cells, in the new, last and before that separations (in reverse), seeing whether this cell has been previously included
  do ii = next_index, last_start_index, -1
    ijk_2d(1) = local_list%icell(ii)
    r_2d(:,1) = local_list%r(:,ii)
    reflect_multiplier_2d(:,1) = local_list%reflect_multiplier(:,ii)
    if (debug) then
      write(91,*) 'within loop: itest defined by:'
      write(91,*) 'ijk_2d(:,1) = ',ijk_2d(1)
      write(91,*) 'reflect_multiplier_2d(:,1) = ',reflect_multiplier_2d(:,1)
      write(91,*) 'r_2d(:,1) = ',r_2d(:,1)
    end if
    if (same_entity(ijk=ijk_2d,reflect_multiplier=reflect_multiplier_2d,r=r_2d,dx=max(cell(ifrom)%dx_max,cell(iadd)%dx_max))) then
      if (debug) write(91,*) 'found iadd = ',iadd
      return ! cell has been found, so jump outa here
    end if
  end do

  if (debug) write(91,*) 'iadd not found: adding iadd = ',iadd

! otherwise add the cell
  next_index = next_index + 1
  local_list%separation_index(nseparation+2) = next_index

! if the arrays aren't large enough, add some space here
  if (allocatable_integer_size(local_list%icell) < next_index) then
    call resize_integer_array(array=local_list%icell,change=1,keep_data=.true.)
    call resize_integer_2d_array(array=local_list%reflect_multiplier,change=change_2d,keep_data=.true.)
    call resize_double_precision_2d_array(array=local_list%r,change=change_2d,keep_data=.true.)
  end if

! and now set the values
  local_list%icell(next_index) = iadd
  local_list%reflect_multiplier(:,next_index) = reflect_multiplier_2d(:,2)
  local_list%r(:,next_index) = r_2d(:,2)

  if (debug) then
    write(91,*) 'last_start, next_index = ',last_start_index,next_index
    write(91,*) 'separation_index = ',local_list%separation_index
    write(91,*) 'icell = ',local_list%icell
    write(91,*) 'reflect_multiplier = ',local_list%reflect_multiplier
    write(91,*) 'r = ',local_list%r
  end if

end if

end subroutine add_to_separation_list

!-----------------------------------------------------------------

function faces_surrounding_node_are_glued(k)

! little function to determine whether this is true or not

logical :: faces_surrounding_node_are_glued
integer :: k, j, jj

faces_surrounding_node_are_glued = .false.

do jj = 1, ubound(node(k)%jface,1)
  j = node(k)%jface(jj)
  if (face(j)%glue_jface /= 0) then
    faces_surrounding_node_are_glued = .true.
    return
  end if
end do

end function faces_surrounding_node_are_glued

!-----------------------------------------------------------------

function facetoicellr(j,l,ns,error_string)

! same as celltoicellr, but for icells surrounding a face

double precision, dimension(totaldimensions) :: facetoicellr
integer :: j, ns
integer, optional :: l
character(len=1000) :: error_stringl ! to optimise speed, error_stringl only defined when needed
character(len=1000), optional :: error_string

! could also check for j validity

if (ns <= allocatable_integer_size(face(j)%icell)) then

  if (present(l)) then ! scalar lth component
    if (ns <= 2.or.face(j)%glue_present) then ! is stored
      facetoicellr(l) = face(j)%r(l,ns)
    else ! is not
      facetoicellr(l) = cell(face(j)%icell(ns))%x(l) - face(j)%x(l)
    end if
  else ! vector
    if (ns <= 2.or.face(j)%glue_present) then ! is stored
      facetoicellr = face(j)%r(:,ns)
    else ! is not
      facetoicellr = cell(face(j)%icell(ns))%x - face(j)%x
    end if
  end if

else

  if (present(error_string)) then
    error_stringl = '  Info from calling routine: '//trim(error_string)
  else
    error_stringl = ''
  end if
  call error_stop('Face index ns is out of range in facetoicellr.  There is something wrong with the context in which this '// &
    'variable is being used: '//trim(error_stringl))

end if

end function facetoicellr

!-----------------------------------------------------------------

function facetoicellreflect_multiplier(j,l,ns,error_string)

! same as celltoicellreflect_multiplier, but for icells surrounding a face

integer, dimension(totaldimensions) :: facetoicellreflect_multiplier
integer :: j, ns
integer, optional :: l
character(len=1000) :: error_stringl ! to optimise speed, error_stringl only defined when needed
character(len=1000), optional :: error_string

! could also check for j validity

if (ns <= allocatable_integer_size(face(j)%icell)) then

  if (present(l)) then ! scalar lth component
    if (ns <= 2.or.face(j)%reflect_present) then ! is stored
      facetoicellreflect_multiplier(l) = face(j)%reflect_multiplier(l,ns)
    else ! is not
      facetoicellreflect_multiplier(l) = 1
    end if
  else ! vector
    if (ns <= 2.or.face(j)%reflect_present) then ! is stored
      facetoicellreflect_multiplier = face(j)%reflect_multiplier(:,ns)
    else ! is not
      facetoicellreflect_multiplier = 1
    end if
  end if

else

  if (present(error_string)) then
    error_stringl = '  Info from calling routine: '//trim(error_string)
  else
    error_stringl = ''
  end if
  call error_stop('Face index ns is out of range in facetoicellreflect_multiplier.  There is something wrong with the context in which this '// &
    'variable is being used: '//trim(error_stringl))

end if

end function facetoicellreflect_multiplier

!-----------------------------------------------------------------

function celltoicellr(i,l,ns,error_string)

! little function to give an r position vector from cell centre to cell(i)%icell(ns) centre,
!  irrespective of whether cell(i)%r it is allocated or not
! this function will provide only the lth component if l is specified (within a dimension(3) array), or the whole vector if
!  l is not given

! this really needs to be fixed to properly reference icentre, rather than ilast (which will break with any nested someloops)

double precision, dimension(totaldimensions) :: celltoicellr
integer :: i, ns
integer, optional :: l
character(len=1000) :: error_stringl ! to optimise speed, error_stringl only defined when needed
character(len=1000), optional :: error_string

if (ns <= allocatable_integer_size(cell(i)%icell)) then

  if (present(l)) then ! scalar lth component
    if (cell(i)%glue_present) then ! is stored
      celltoicellr(l) = cell(i)%r(l,ns)
    else ! is not
      celltoicellr(l) = cell(cell(i)%icell(ns))%x(l) - cell(i)%x(l)
    end if
  else ! vector
    if (cell(i)%glue_present) then ! is stored
      celltoicellr = cell(i)%r(:,ns)
    else ! is not
      celltoicellr = cell(cell(i)%icell(ns))%x - cell(i)%x
    end if
  end if

else

  if (present(error_string)) then
    error_stringl = '  Info from calling routine: '//trim(error_string)
  else
    error_stringl = ''
  end if
  call error_stop('Cell index ns is out of range in celltoicellr.  There is something wrong with the context '// &
    'in which this variable is being used: '//trim(error_stringl))

end if

end function celltoicellr

!-----------------------------------------------------------------

function celltoicellreflect_multiplier(i,l,ns,error_string)

! little function to give a reflect_multiplier vector from cell centre to cell(i)%icell(ns) centre,
!  irrespective of whether cell(i)%reflect_multiplier is allocated or not
! this function will provide only the lth component if l is specified (within a dimension(3) array), or the whole vector if
!  l is not given

integer, dimension(totaldimensions) :: celltoicellreflect_multiplier
integer :: i, ns
integer, optional :: l
character(len=1000) :: error_stringl ! to optimise speed, error_stringl only defined when needed
character(len=1000), optional :: error_string

if (ns <= allocatable_integer_size(cell(i)%icell)) then

  if (present(l)) then ! scalar lth component
    if (cell(i)%reflect_present) then ! is stored
      celltoicellreflect_multiplier(l) = cell(i)%reflect_multiplier(l,ns)
    else ! is not
      celltoicellreflect_multiplier(l) = 1
    end if
  else ! vector
    if (cell(i)%reflect_present) then ! is stored
      celltoicellreflect_multiplier = cell(i)%reflect_multiplier(:,ns)
    else ! is not
      celltoicellreflect_multiplier = 1
    end if
  end if

else

  if (present(error_string)) then
    error_stringl = '  Info from calling routine: '//trim(error_string)
  else
    error_stringl = ''
  end if
  call error_stop('Cell index ns is out of range in celltoicellreflect_multiplier.  There is something wrong with '// &
    'the context in which this variable is being used: '//trim(error_stringl))

end if

end function celltoicellreflect_multiplier

!-----------------------------------------------------------------
! TODO: rewrite so takes relative separation loop number as option, as per sepcentreN option

function celltoseparationicellr(thread,icurrent,l,error_string)

! little function to give an r position vector from the centre of the thread separation_list cell centre to cell icurrent
! this function will provide only the lth component if l is specified (within a dimension(3) array), or the whole vector if
!  l is not given
! checks that iicurrent (stored in someloop(thread)%separation_list(someloop(thread)%current_separation_list(1))) and icurrent are consistent

double precision, dimension(totaldimensions) :: celltoseparationicellr
integer :: thread, icurrent, iicurrent, mcurrent
integer, optional :: l
character(len=1000) :: error_stringl ! to optimise speed, error_stringl only defined when needed
character(len=1000), optional :: error_string

mcurrent = someloop(thread)%current_separation_list(1)
if (mcurrent == 0) then
  if (present(error_string)) then
    error_stringl = '  Info from calling routine: '//trim(error_string)
  else
    error_stringl = ''
  end if
  call error_stop('no current separation_list is specified in celltoseparationicellr.  There is something wrong with the way '// &
    'in which this separation_list is being used (possibly there are nested separation_lists or a sum or other averaging '// &
    'operation taking place within a separation_list?): '//trim(error_stringl))
end if

iicurrent = someloop(thread)%separation_list(mcurrent)%iicurrent ! pick up latest iicurrent from separation_list

! rewrite to remove this check, instead just looking at latest separation loop
if (icurrent == someloop(thread)%separation_list(mcurrent)%icell(iicurrent)) then ! check that we are referencing the correct cell

  if (present(l)) then ! scalar lth component
    celltoseparationicellr(l) = someloop(thread)%separation_list(mcurrent)%r(l,iicurrent)
  else ! vector
    celltoseparationicellr = someloop(thread)%separation_list(mcurrent)%r(:,iicurrent)
  end if

else

  if (present(error_string)) then
    error_stringl = '  Info from calling routine: '//trim(error_string)
  else
    error_stringl = ''
  end if
  call error_stop('icurrent does not match iicurrent in celltoseparationicellr.  There is something wrong with the way '// &
    'in which this separation_list is being used (possibly there are nested separation_lists or a sum or other averaging '// &
    'operation taking place within a separation_list?): '//trim(error_stringl))

end if

end function celltoseparationicellr

!-----------------------------------------------------------------

subroutine print_simulation_info(fileunit,comment)

character(len=1), optional :: comment
character(len=1) :: comment_l
integer :: fileunit

comment_l = ''
if (present(comment)) comment_l = comment
if (simulation_info%title /= '') write(fileunit,'(a)') comment_l//' TITLE = '//trim(simulation_info%title)
if (simulation_info%author /= '') write(fileunit,'(a)') comment_l//' AUTHOR = '//trim(simulation_info%author)
if (simulation_info%date /= '') write(fileunit,'(a)') comment_l//' DATE = '//trim(simulation_info%date)
if (simulation_info%version /= '') write(fileunit,'(a)') comment_l//' VERSION = '//trim(simulation_info%version)
if (simulation_info%description /= '') write(fileunit,'(a)') comment_l//' DESCRIPTION = '//trim(simulation_info%description)
if (simulation_info%filename /= '') write(fileunit,'(a)') comment_l//' FILENAME = '//trim(simulation_info%filename)
if (simulation_info%absfilename /= '') write(fileunit,'(a)') comment_l//' ABSFILENAME = '//trim(simulation_info%absfilename)
if (simulation_info%rundate /= '') write(fileunit,'(a)') comment_l//' RUNDATE = '//trim(simulation_info%rundate)
if (simulation_info%runversion /= '') write(fileunit,'(a)') comment_l//' RUNVERSION = '//trim(simulation_info%runversion)
if (simulation_info%runhost /= '') write(fileunit,'(a)') comment_l//' RUNHOST = '//trim(simulation_info%runhost)

end subroutine print_simulation_info

!-----------------------------------------------------------------

subroutine identity_matrix(matrix)

! little function to form an identity matrix, without doing bound checks

double precision, dimension(:,:) :: matrix
integer :: n

matrix = 0.d0
do n = 1, ubound(matrix,1)
  matrix(n,n) = 1.d0
end do

end subroutine identity_matrix

!-----------------------------------------------------------------

function rotation_matrix(axis,angle)

! here we form matrix that when matmul by x will be rotated about the line specified by vector axis (normalised) by angle (radian)
! http://inside.mines.edu/fs_home/gmurray/ArbitraryAxisRotation/

double precision, dimension(totaldimensions,totaldimensions) :: rotation_matrix
double precision, dimension(totaldimensions), intent(in) :: axis
double precision, intent(in) :: angle
integer :: n1, n2, n3
double precision :: adding

do n1 = 1, 3
  do n2 = 1, 3
    if (n1 == n2) then
      rotation_matrix(n1,n1) = axis(n1)**2 + (1.d0-axis(n1)**2)*cos(angle)
    else
      n3 = 6 - n1 - n2 ! this is the other dimension
      adding = 1.d0
      if (n1 == 1.and.n2 == 2) adding = -1.d0
      if (n1 == 2.and.n2 == 3) adding = -1.d0
      if (n1 == 3.and.n2 == 1) adding = -1.d0
      rotation_matrix(n1,n2) = axis(n1)*axis(n2)*(1.d0-cos(angle)) + adding*axis(n3)*sin(angle)
    end if
  end do
end do

end function rotation_matrix

!-----------------------------------------------------------------

function separationcentreicell(thread,n,error_string)

! find the icell number for the cell at the centre of the separation centre n

integer, intent(in) :: thread
integer, intent(in) :: n
integer :: separation_list_number ! number of the separation list that we are finding the centre of - each separation loop corresponds to a particular separation loop in the input file
integer :: separationcentreicell
character(len=1000) :: error_string ! compulsory here!

if (n > ubound(someloop(thread)%current_separation_list,1) .or. n < 1) &
  call error_stop('n out of array range in separationcentreicell, indicated major problem: '//trim(error_string))
separation_list_number = someloop(thread)%current_separation_list(n)
if (separation_list_number == 0) call error_stop( &
  'an attempt is being made made to reference a separation list centre incorrectly in separationcentreicell: '//trim(error_string))
separationcentreicell = someloop(thread)%separation_list(separation_list_number)%icell(1)
if (separationcentreicell == 0)  call error_stop( &
  'an attempt is being made made to reference a separation list centre that is not currently used in separationcentreicell: '// &
  trim(error_string))

end function separationcentreicell

!-----------------------------------------------------------------

function celltoseparationicellreflect_multiplier(thread,n,l,error_string)

! find whether during the nth latest separation loop a reflection has occured in the lth direction

integer, intent(in) :: thread
integer, intent(in) :: n
integer, intent(in) :: l ! dimension
integer :: iicurrent
integer :: separation_list_number ! number of the separation list that we are finding the centre of - each separation loop corresponds to a particular separation loop in the input file
double precision :: celltoseparationicellreflect_multiplier
character(len=1000) :: error_string ! compulsory here!

if (n > ubound(someloop(thread)%current_separation_list,1) .or. n < 1) &
  call error_stop('n out of array range in celltoseparationicellreflect_multiplier, indicated major problem: '//trim(error_string))
separation_list_number = someloop(thread)%current_separation_list(n)
if (separation_list_number == 0) call error_stop( &
  'an attempt is being made made to reference a separation list centre incorrectly in '// &
  'celltoseparationicellreflect_multiplier: '//trim(error_string))
if (l < 1 .or. l > 3) call error_stop('dimension incorrect in celltoseparationicellreflect_multiplier: '//trim(error_string))

iicurrent = someloop(thread)%separation_list(separation_list_number)%iicurrent
celltoseparationicellreflect_multiplier = &
  dble(someloop(thread)%separation_list(separation_list_number)%reflect_multiplier(l,iicurrent))

end function celltoseparationicellreflect_multiplier

!-----------------------------------------------------------------

function celltoseparationicellrsquared(thread,n,error_string)

! find r^2 between the nth latest separation cell centre and the current cell in this loop

integer, intent(in) :: thread
integer, intent(in) :: n
integer :: iicurrent
integer :: l
integer :: separation_list_number ! number of the separation list that we are finding the centre of - each separation loop corresponds to a particular separation loop in the input file
double precision :: celltoseparationicellrsquared
character(len=1000) :: error_string ! compulsory here!

if (n > ubound(someloop(thread)%current_separation_list,1) .or. n < 1) &
  call error_stop('n out of array range in celltoseparationicellreflect_multiplier, indicated major problem: '//trim(error_string))
separation_list_number = someloop(thread)%current_separation_list(n)
if (separation_list_number == 0) call error_stop( &
  'an attempt is being made made to reference a separation list centre incorrectly in '// &
  'celltoseparationicellreflect_multiplier: '//trim(error_string))

iicurrent = someloop(thread)%separation_list(separation_list_number)%iicurrent

celltoseparationicellrsquared = 0.d0
do l = 1, 3
  celltoseparationicellrsquared = celltoseparationicellrsquared + &
    someloop(thread)%separation_list(separation_list_number)%r(l,iicurrent)**2
end do

end function celltoseparationicellrsquared

!-----------------------------------------------------------------

function facereflect(j,l,error_string)

! arb variable <facereflect[l=:]>, returning -1 is face is reflected in that direction, or 1 if not
! now also returns whether face is reflected at all, using l=0 index

integer :: j ! face index
integer :: l ! dimension
character(len=1000) :: error_string ! compulsory here!
double precision :: facereflect

if (l < 0 .or. l > 3 .or. j < 0 .or. j > jtotal) call error_stop('problem with facereflect: '//trim(error_string))
if (l == 0) then
  if (face(j)%glue_reflect > 0) then
    facereflect = -1.d0 ! face is reflected
  else
    facereflect = 1.d0 ! face is not
  end if
else if (l == face(j)%glue_reflect) then
  facereflect = -1.d0
else
  facereflect = 1.d0
end if

end function facereflect

!-----------------------------------------------------------------

function facefromcelldirection(i,j)

! this is positive if the normal points outwards from the last cell
! this function does not check for out-of-bounds, and technically only checks whether cell i is the downcell for face j

integer :: i,j
double precision :: facefromcelldirection

if (face(j)%icell(1) == i) then
  facefromcelldirection = 1.d0
else
  facefromcelldirection = -1.d0
end if

end function facefromcelldirection

!-----------------------------------------------------------------

subroutine initialise_random_number

! call the random_seed subroutine with a set of integers based on the time

integer :: clock, seed_size, n ! for random seed
integer, dimension(:), allocatable :: seed

call random_seed(size=seed_size)
allocate(seed(seed_size))
do n = 1, seed_size
  call system_clock(clock)
  seed(n)=clock
end do
call random_seed(put=seed) ! for gnu compiler need to generate unique seed, put needs to be random itself...
deallocate(seed)

end subroutine initialise_random_number

!-----------------------------------------------------------------

subroutine copy_integer_2d_array(original,copy)

! copy 2d allocatable original array to copy array

integer, dimension(:,:), allocatable :: original, copy
integer, dimension(2) :: new_size

if (allocated(original)) then ! silently ignore if original isn't already allocated
  new_size = [ ubound(original,1), ubound(original,2) ]
  call resize_integer_2d_array(keep_data=.false.,array=copy,new_size=new_size)
  copy = original
end if

end subroutine copy_integer_2d_array

!-----------------------------------------------------------------

subroutine copy_double_precision_2d_array(original,copy)

! copy 2d allocatable original array to copy array

double precision, dimension(:,:), allocatable :: original, copy
integer, dimension(2) :: new_size

if (allocated(original)) then ! silently ignore if original isn't already allocated
  new_size = [ ubound(original,1), ubound(original,2) ]
  call resize_double_precision_2d_array(keep_data=.false.,array=copy,new_size=new_size)
  copy = original
end if

end subroutine copy_double_precision_2d_array

!-----------------------------------------------------------------

end module general_module

!----------------------------------------------------------------------------
