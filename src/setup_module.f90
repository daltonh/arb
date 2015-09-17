! file src/setup_module.f90
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
module setup_module

implicit none

private
public setup ! only subroutine accessible from outside the module

! various setup related options

!-----------------------------------------------------------------
contains

!-----------------------------------------------------------------

subroutine setup

! this handles everything prior to a simulation starting

use general_module
use equation_module
use gmesh_module
use kernel_module
use output_module
use solver_module
use region_module
logical, parameter :: debug = .false.

if (debug) write(*,'(80(1h+)/a)') 'subroutine setup'

if (convergence_details_file) then

  if (debug) write(*,*) 'calling setup_convergence_file'

  call setup_convergence_file ! setup convergence file

end if

if (debug) write(*,*) 'calling setup_gmesh'

call setup_gmesh_elements ! create reference gtype_list array which has details of the gmsh element types

if (debug) write(*,*) 'calling allocate_meta_arrays'

call allocate_meta_arrays ! allocate arrays that have meta data about variables

if (debug) write(*,*) 'calling read_input_file'

call read_input_file ! read arb input file for numerical data, some region and mesh info

if (debug) write(*,*) 'calling setup_mesh'

call setup_mesh ! read in meshes

if (debug) write(*,*) 'calling setup_regions'

call setup_regions ! assign ijk indices to each region not setup by mesh

if (debug) write(*,*) 'calling setup_solver'

call setup_solver ! process the solver options and possibly choose solver type

if (debug) write(*,*) 'calling setup_kernels'

call setup_kernels ! create averaging and differencing kernels

if (debug) write(*,*) 'calling setup_external_functions'

call setup_external_functions ! hook to setup any external functions

if (debug) write(*,*) 'calling setup_vars'

call setup_vars ! do setup for each var variable

if (debug) write(*,*) 'calling finalise_gmesh'

call finalise_gmesh ! finalise the setup of all gmeshes

call output_step(action="setup") ! setup the output_step file

if (debug) write(*,'(a/80(1h-))') 'subroutine setup'

end subroutine setup

!-----------------------------------------------------------------

subroutine setup_convergence_file

use general_module
character(len=100) :: filename
integer :: ierror

! open convergence output file if requested
filename = "output/convergence_details.txt"
open(fconverge,file=trim(filename),access='append',iostat=ierror)
if (ierror /= 0) call error_stop('problem opening file '//trim(filename))

end subroutine setup_convergence_file

!-----------------------------------------------------------------

subroutine read_input_file

! here we read in all sorts of simulation data from the constants.in file, but
! not the actual constants values (which is done in read_constants)

use general_module
use gmesh_module
integer :: ierror, n, m, gmesh_number, array_size
character(len=5000) :: textline, otextline
character(len=4000) :: name
character(len=1000) :: keyword, formatline, options, filename
character(len=1000), dimension(2) :: glue_region
character(len=100) :: option_name
real :: versiontmp
logical :: error, empty
logical, parameter :: debug = .false.
                  
if (debug) write(*,'(80(1h+)/a)') 'subroutine read_input_file'

! push default mesh (index 0) which will include everything
call push_gmesh(filename='output/output.msh')

write(*,'(a)') "INFO: reading simulation information from arb input file "//trim(input_file)
open(unit=finput,file=trim(input_file),status='old',iostat=ierror)
if (ierror /= 0) call error_stop('problem opening arb input file '//trim(input_file))

fileloop: do
  read(finput,'(a)',iostat=ierror) textline
  if (ierror /= 0) exit fileloop ! reached end of file
  otextline = textline ! save original textline for error outputing
  call remove_comments(textline) ! remove comments from line
  keyword=extract_next_string(textline,error,empty=empty,delimiter=' ')
  if (empty) cycle

! if (debug) write(*,'(a)') 'keyword = '//trim(keyword)//': otextline = '//trim(otextline)//''
  if (debug) write(*,'(a)') 'keyword = '//trim(keyword)//': textline = '//trim(textline)
  if (error) call error_stop('problem reading the keyword on on line:'//trim(otextline))

!---------------
! glue_faces

  if (trim(keyword) == 'GLUE_FACES') then ! read in face regions to be glued together and any options
! glue_region(1)
    glue_region(1) = extract_next_string(textline,error,empty=empty,delimiter="<")
    if (error.or.empty) call error_stop('first region name in '//trim(keyword)// &
      ' definition incorrectly specified on line:'//trim(otextline))
! glue_region(2)
    glue_region(2) = extract_next_string(textline,error,empty=empty,delimiter="<")
    if (empty) then
      glue_region(2) = glue_region(1) ! assume that the region names are the same
    else if (error) then
      call error_stop('second region name in '//trim(keyword)// &
      ' definition incorrectly specified on line:'//trim(otextline))
    end if
    call push_glue_face(region=glue_region,option_line=textline) ! add new glue_face onto back of existing array, saving rest of the line as options
    write(*,'(a)') 'INFO: read glueing directive between faces '//trim(glue_region(1))//' and '//trim(glue_region(2))// &
      ' using options '//trim(textline)
  end if

!---------------
! mesh info

  if (trim(keyword) == 'MSH_FILE') then
! extract gmsh filename, either delimited by or not
    filename = extract_next_string(textline,error,empty=empty,delimiter="'"" ")
    if (error.or.empty) call error_stop('problem reading in name of gmsh mesh file from line '//otextline) ! if there is space at first character or string is empty, then filename not found
    call push_gmesh(filename,gmesh_number)
! extract any options and add to the end of the options array
    call extract_options(textline,gmesh(gmesh_number)%options)
! assemble a list of the gmesh options
    if (allocated(gmesh(gmesh_number)%options)) then
      write(options,'(100(a))') (' '//trim(gmesh(gmesh_number)%options(n)),n=1,ubound(gmesh(gmesh_number)%options,1))
    else
      options = " (none)"
    end if
! find the input and output_scale variables
    do n = 1, allocatable_character_size(gmesh(gmesh_number)%options)
      option_name = extract_option_name(gmesh(gmesh_number)%options(n),error)
      if (.not.error.and.(trim(option_name) == "inputscale".or.trim(option_name) == "inputinversescale")) then
        gmesh(gmesh_number)%input_scale = extract_option_double_precision(gmesh(gmesh_number)%options(n),error)
        if (error) call error_stop("could not determine inputscale from the msh file option "// &
          trim(gmesh(gmesh_number)%options(n)))
        if (trim(option_name) == "inputinversescale") gmesh(gmesh_number)%input_scale = 1.d0/gmesh(gmesh_number)%input_scale
      else if (.not.error.and.(trim(option_name) == "outputscale".or.trim(option_name) == "outputinversescale")) then
        gmesh(gmesh_number)%output_scale = extract_option_double_precision(gmesh(gmesh_number)%options(n),error)
        if (error) call error_stop("could not determine outputscale from the msh file option "// &
          trim(gmesh(gmesh_number)%options(n)))
        if (trim(option_name) == "outputinversescale") gmesh(gmesh_number)%output_scale = 1.d0/gmesh(gmesh_number)%output_scale
      end if
    end do
    formatline = '(a,'//trim(dindexformat(gmesh_number))//',a)'
    write(*,fmt=formatline) 'INFO: gmesh created from arb input file: gmesh_number = ',gmesh_number,': fullname = '// &
      trim(gmesh(gmesh_number)%filename)//': basename = '//trim(gmesh(gmesh_number)%basename)// &
      ': current (prioritised) user-set options ='//trim(options)
  end if

!---------------
! newtrestol

  if (trim(keyword) == 'NEWTRESTOL') then
    read(textline,*,iostat=ierror) newtrestol
    if (ierror /= 0) call error_stop('problem reading in newton loop tolerance from line '//otextline)
    write(*,'(a,g14.6)') 'INFO: newtrestol = ',newtrestol
  end if

!---------------
! newtstepmax

  if (trim(keyword) == 'NEWTSTEPMAX') then
    read(textline,*,iostat=ierror) newtstepmax
    if (ierror /= 0) call error_stop('problem reading in maximum number of newton steps from line '//otextline)
    formatline = '(a,'//trim(dindexformat(newtstepmax))//')'
    write(*,fmt=formatline) 'INFO: newtstepmax = ',newtstepmax
  end if

!---------------
! newtstepmin

  if (trim(keyword) == 'NEWTSTEPMIN') then
    read(textline,*,iostat=ierror) newtstepmin
    if (ierror /= 0) call error_stop('problem reading in minimum number of newton steps from line '//otextline)
    formatline = '(a,'//trim(dindexformat(newtstepmin))//')'
    write(*,fmt=formatline) 'INFO: newtstepmin = ',newtstepmin
  end if

!---------------
! timestepstart

  if (trim(keyword) == 'TIMESTEPSTART') then
    read(textline,*,iostat=ierror) timestep
    if (ierror /= 0) call error_stop('problem reading in starting timestep from line '//otextline)
    formatline = '(a,'//trim(dindexformat(timestep))//')'
    write(*,fmt=formatline) 'INFO: initial timestep = ',timestep
    if (transient_simulation) ignore_gmesh_step = .true. ! signal that step from gmesh file is not to be overwritten by values in file
  end if

!---------------
! timestepadditional

  if (trim(keyword) == 'TIMESTEPADDITIONAL') then
    read(textline,*,iostat=ierror) timestepadditional
    if (ierror /= 0) call error_stop('problem reading in starting timestepadditional from line '//otextline)
    formatline = '(a,'//trim(dindexformat(timestepadditional))//')'
    write(*,fmt=formatline) 'INFO: timestepadditional = ',timestepadditional
  end if

!---------------
! newtstepstart

  if (trim(keyword) == 'NEWTSTEPSTART') then
    read(textline,*,iostat=ierror) newtstep
    if (ierror /= 0) call error_stop('problem reading in starting newtstep from line '//otextline)
    formatline = '(a,'//trim(dindexformat(newtstep))//')'
    write(*,fmt=formatline) 'INFO: initial newtstep = ',newtstep
    if (.not.transient_simulation) ignore_gmesh_step = .true. ! signal that step from gmesh file is not to be overwritten by values in file
  end if

!---------------
! newtstepdebugout

  if (trim(keyword) == 'NEWTSTEPDEBUGOUT') then
    read(textline,*,iostat=ierror) newtstepdebugout
    if (ierror /= 0) call error_stop('problem reading in maximum number of newton steps before debugging from line '//otextline)
    formatline = '(a,'//trim(dindexformat(newtstepdebugout))//')'
    write(*,fmt=formatline) 'INFO: newtstepdebugout = ',newtstepdebugout
  end if

!---------------
! timestepmax

  if (trim(keyword) == 'TIMESTEPMAX') then
    read(textline,*,iostat=ierror) timestepmax
    if (ierror /= 0) call error_stop('problem reading in maximum number of time steps from line '//otextline)
    formatline = '(a,'//trim(dindexformat(timestepmax))//')'
    write(*,fmt=formatline) 'INFO: timestepmax = ',timestepmax
  end if

!---------------
! timestepmin

  if (trim(keyword) == 'TIMESTEPMIN') then
    read(textline,*,iostat=ierror) timestepmin
    if (ierror /= 0) call error_stop('problem reading in minimum number of time steps from line '//otextline)
    formatline = '(a,'//trim(dindexformat(timestepmin))//')'
    write(*,fmt=formatline) 'INFO: timestepmin = ',timestepmin
  end if

!---------------
! timestepout

  if (trim(keyword) == 'TIMESTEPOUT') then
    read(textline,*,iostat=ierror) timestepout
    if (ierror /= 0) call error_stop('problem reading in number of time steps between output from line '//otextline)
    formatline = '(a,'//trim(dindexformat(timestepout))//')'
    write(*,fmt=formatline) 'INFO: timestepout = ',timestepout
  end if

!---------------
! newtstepout

  if (trim(keyword) == 'NEWTSTEPOUT') then
    read(textline,*,iostat=ierror) newtstepout
    if (ierror /= 0) call error_stop('problem reading in number of newt steps between output from line '//otextline)
    formatline = '(a,'//trim(dindexformat(newtstepout))//')'
    write(*,fmt=formatline) 'INFO: newtstepout = ',newtstepout
  end if

!---------------
! version

  if (trim(keyword) == 'VERSION') then
    read(textline,*,iostat=ierror) versiontmp
    if (ierror /= 0) call error_stop('problem reading in version number from line '//otextline)
    if (abs(version - versiontmp) > 2.*tiny(1.e0)) then
      formatline = '(a,f4.2,a,f4.2,a)'
      if (versiontmp < minimum_version) then
        write(*,fmt=formatline) 'ERROR: unsafe version mismatch between the input file (',versiontmp,') and the arb code (', &
          version,'): the language syntax has changed significantly between these versions and the input file needs updating'
        stop
      else
        write(*,fmt=formatline) 'WARNING: (safe) version mismatch between the input file (',versiontmp,') and the arb code (', &
          version,'): additional language features are now available'
      end if
    end if
  end if

!---------------
! kernel options

  if (trim(keyword) == 'KERNEL'.or.trim(keyword) == 'KERNELS'.or.trim(keyword) == 'KERNEL_OPTIONS') then
! extract any options and add to the start of the options array
    array_size = allocatable_character_size(kernel_options)
    call extract_options(textline,kernel_options)
    if (allocatable_character_size(kernel_options) - array_size == 0) then ! the options array was the same size as before, which is an error really
      write(*,'(a)') 'WARNING: the '//trim(keyword)//' keyword was found but no options were read in'
    else if (allocated(kernel_options)) then
      write(*,'(101(a))') 'INFO: the following (prioritised) user-set kernel options have been read in:', &
        (' '//trim(kernel_options(n)),n=1,ubound(kernel_options,1))
    end if
  end if

!---------------
! solver options

  if (trim(keyword) == 'SOLVER'.or.trim(keyword) == 'SOLVERS'.or.trim(keyword) == 'SOLVER_OPTIONS') then
! extract any options and add to the start of the options array
    array_size = allocatable_character_size(solver_options)
    call extract_options(textline,solver_options)
    if (allocatable_character_size(solver_options) - array_size == 0) then ! the options array was the same size as before, which is an error really
      write(*,'(a)') 'WARNING: the '//trim(keyword)//' keyword was found but no options were read in'
    else if (allocated(solver_options)) then
      write(*,'(101(a))') 'INFO: the following (prioritised) user-set solver options have been read in:', &
        (' '//trim(solver_options(n)),n=1,ubound(solver_options,1))
    end if
  end if

!---------------
! variable options

  if (trim(keyword) == 'VARIABLE_OPTIONS') then
    name = extract_next_string(textline,error,empty=empty,delimiter="<")
    if (error.or.empty) call error_stop('variable name in '//trim(keyword)//' in input file incorrect on line:'//trim(otextline))
! find number for this name
    m = var_number_from_name(name)
    if (m == 0) call error_stop('variable '//trim(name)//' specified in arb input file was not found')
! extract any options and add to the start of the options array
    array_size = allocatable_character_size(var(m)%options)
    call extract_options(textline,var(m)%options)
    if (allocatable_character_size(var(m)%options) - array_size == 0) then ! the options array was the same size as before, which is an error really
      write(*,'(a)') 'WARNING: the '//trim(keyword)//' keyword was found but no options were read in for variable '//trim(name)
    else if (allocated(var(m)%options)) then
      write(*,'(101(a))') 'INFO: the following (prioritised) user-set options for variable '//trim(name)//' have been read in:', &
        (' '//trim(var(m)%options(n)),n=1,ubound(var(m)%options,1))
    end if
  end if

!---------------
! compound options

  if (trim(keyword) == 'COMPOUND_OPTIONS') then
    name = extract_next_string(textline,error,empty=empty,delimiter="<")
    if (error.or.empty) call error_stop('compound name in '//trim(keyword)//' in input file incorrect on line:'//trim(otextline))
! find number for this name
    m = compound_number_from_name(name)
    if (m == 0) call error_stop('compound '//trim(name)//' specified in arb input file was not found')
! extract any options and add to the start of the options array
    array_size = allocatable_character_size(compound(m)%options)
    call extract_options(textline,compound(m)%options)
    if (allocatable_character_size(compound(m)%options) - array_size == 0) then ! the options array was the same size as before, which is an error really
      write(*,'(a)') 'WARNING: the '//trim(keyword)//' keyword was found but no options were read in for compound '//trim(name)
    else if (allocated(compound(m)%options)) then
      write(*,'(101(a))') 'INFO: the following (prioritised) user-set options for compound '//trim(name)//' have been read in:', &
        (' '//trim(compound(m)%options(n)),n=1,ubound(compound(m)%options,1))
    end if
  end if

!---------------
! simulation info

  if (keyword(1:5) == 'INFO_') then
    keyword = keyword(6:len_trim(keyword)) ! remove INFO_
    name = extract_next_string(textline,error,empty=empty,delimiter="'"" ")
    if (error) call error_stop('simulation info '//trim(keyword)//' in input file incorrect on line:'//trim(otextline))
    if (trim(keyword) == 'TITLE') then
      simulation_info%title = name(1:200)
    else if (trim(keyword) == 'DESCRIPTION') then
      simulation_info%description = name(1:4000) ! actually name and the description field are the same size, of 4000
    else if (trim(keyword) == 'AUTHOR') then
      simulation_info%author = name(1:200)
    else if (trim(keyword) == 'VERSION') then
      simulation_info%version = name(1:200)
    else if (trim(keyword) == 'DATE') then
      simulation_info%date = name(1:200)
    else if (trim(keyword) == 'RUNVERSION') then
      simulation_info%runversion = name(1:200)
    else if (trim(keyword) == 'RUNDATE') then
      simulation_info%rundate = name(1:200)
    else if (trim(keyword) == 'RUNHOST') then
      simulation_info%runhost = name(1:200)
    else if (trim(keyword) == 'FILENAME') then
      simulation_info%filename = name(1:200)
    else
      call error_stop('simulation info keyword '//trim(keyword)//' in input file incorrect on line:'//trim(otextline))
    end if
    if (trim(keyword) /= 'DESCRIPTION') write(*,'(a)') 'SIMULATION INFO: '//trim(keyword)//' = '//trim(name)
  end if

!---------------

end do fileloop

close(finput)

if (debug) write(*,'(a/80(1h-))') 'subroutine read_input_file'

end subroutine read_input_file

!-----------------------------------------------------------------

subroutine setup_mesh

! here we do all sorts of mesh setting
! set types for all cells, faces and nodes
! check on icell associations for faces
! set i,j,k boundary and domain and numbers
! set x for faces and nodes

use general_module
use gmesh_module
integer :: i, ii, j, jj, k, kk, ncell, gmesh_number, n, ierror, i2, ii2, kk2, k2, jbase, jglue, l
type(cell_type) :: default_cell
double precision, dimension(totaldimensions) :: tangc, normc
double precision :: maximum_error_angle, error_angle ! a parameter that indicates face curvature (in radians) if any curved 2d geometries are found
integer, dimension(:), allocatable :: separation_index
character(len=1000) :: formatline, filename
logical :: error, push_this_cell
integer, dimension(2) :: new_size_2d
logical, parameter :: debug = .false.

if (debug) write(*,'(80(1h+)/a)') 'subroutine setup_mesh'

!-------------------------------------
! set initial empty element numbers
ktotal = 0
jtotal = 0
itotal = 0

!-------------------------------------
! read in the gmesh data
if (debug) write(*,*) 'reading in gmesh data'

call read_gmesh(contents='mesh')

!-------------------------------------
if (debug) write(*,*) 'doing generic mesh setup'

! run through cells checking icells and possibly setting icell(2) indices
! also does some sanity checks on the faces, checking that they are connected to at least one cell, and not more than that
do i = 1, itotal
  cell(i)%type = 1 ! domain cell
  jj_loop: do jj = 1, ubound(cell(i)%jface,1)
    j = cell(i)%jface(jj)
    if (face(j)%icell(1) == 0) &
      call error_stop('a face that is adjacent to a cell has no icell = 1 attached: face = '//trim(print_face(j))) ! this would probably be a bug with the mesh reader, as opposed to a bug with the mesh itself
    if (ubound(face(j)%icell,1) < 2) call push_integer_array(array=face(j)%icell,new_element=0) ! allocate the space for another icell entry to allow check
    if (face(j)%icell(1) == i.or.face(j)%icell(2) == i) cycle jj_loop
    if (face(j)%icell(2) /= 0) call error_stop('a face has more than 2 cells attached to it: face = '//trim(print_face(j)))
    face(j)%icell(2) = i ! if we are here then icell(1) was not i, and icell(2) was empty, so set it to i
  end do jj_loop
end do
      
!-------------------------------------
! run through nodes setting type to default
do k = 1, ktotal
  node(k)%type = 1
end do

!-------------------------------------
! now do any mesh manipulation, assign face type and create any boundary cells
idomain = itotal
jboundary = 0
jdomain = 0

if (debug) write(*,*) 'checking for orphaned faces'

do j = 1, jtotal
! check for orphaned faces
  if (face(j)%icell(1) == 0) then
    write(*,'(a)') 'ERROR: a face has no primary cell (icell=1) attached to it: face = '//trim(print_face(j))
    write(*,*) 'A possibility is that there is a problem with the dimensions in a .msh file or in a region definition.'
    write(*,*) 'Check that the dimensions in each msh file (physical entities) are consistent, and that the maximum dimension '// &
      'of each domain is listed (near the top of the msh file, first column, a couple of lines under $PhysicalNames). '// &
      'Remember that each domain region must be given a physical entity name in gmsh.'
    write(*,*) 'Also check that any cell domains used in each msh file that have a dimension less than that of the maximum '// &
      'domain have their centrings explicity set in constants.in'
    write(*,*) 'Another possibility is that a region (physical entity in gmsh) has been defined that has no relation to '// &
      'any other region.  For example, a surface/line/point may have been defined (containing faces) which does not bound any '// &
      'volume/surface/line (which contains the cells).  This would leave the faces as orphans and produce this error.'
    write(*,*) 'If you''re really stuck, set debug = .true. in gmesh_module.f90, subroutine read_gmesh_file, and look at how '// &
      'element is created as it is read in from the msh file.  This info will be contained in the file fort.81.'
    stop
  end if
! if (ubound(face(j)%icell,1) < 2) call push_integer_array(array=face(j)%icell,new_element=0)
end do

if (debug) write(*,*) 'assembling the glue_faces'

call glue_faces

if (debug) write(*,*) 'creating boundary cells and glueing any faces together'

do j = 1, jtotal

  face(j)%type = 1 ! domain face is the default

  if (face(j)%glue_jface /= 0) then
! the faces can only be glued together if the icell(2) position of each is spare
    if (face(j)%icell(2) == 0.and.face(face(j)%glue_jface)%icell(2) == 0) then
      face(j)%icell(2) = face(face(j)%glue_jface)%icell(1)
      face(face(j)%glue_jface)%icell(2) = face(j)%icell(1)
! anything other than the correct glueing is a terminal error
    else if (face(j)%icell(2) /= face(face(j)%glue_jface)%icell(1).or.face(face(j)%glue_jface)%icell(2) /= face(j)%icell(1)) then
      call error_stop("a problem has occurred when trying to glue two faces together: face1 = "// &
        trim(print_face(j))//": face2 = "//trim(print_face(face(j)%glue_jface)))
    end if

  else if (face(j)%icell(2) == 0) then
    face(j)%type = 2 ! boundary face

! create new boundary cell, storing in default_cell
    call reset_cell(default_cell)
    default_cell%type = 2
    default_cell%dimensions = face(j)%dimensions
    default_cell%gtype = face(j)%gtype ! adopt gmsh element type - must be set
    call resize_integer_array(keep_data=.false.,array=default_cell%jface,new_size=1)
    default_cell%jface(1) = j
    call copy_integer_array(original=face(j)%knode,copy=default_cell%knode)

    itotal = itotal + 1
    if (ubound(cell,1) < itotal) call resize_cell_array(change=int(ktotal/10+1))
    call set_cell(cell_to_set=cell(itotal),new_value=default_cell)
    face(j)%icell(2) = itotal

!   if (debug) write(*,*) 'creating new boundary cell: i = ',itotal,': jface = ',default_cell%jface

! set node icell references for the new cell
    call add_icell_to_nodes(itotal,default_cell%knode)

! nodes associated with boundary faces are also boundary nodes
    do kk = 1, ubound(face(j)%knode,1)
      k = face(j)%knode(kk)
      node(k)%type = 2
    end do

  end if

  if (face(j)%type == 2) then
    jboundary = jboundary + 1
  else
    jdomain = jdomain + 1
  end if

end do

iboundary = itotal - idomain

! resize cell arrays
call resize_cell_array(new_size=itotal)

! run through gmeshes checking that any boundary faces or cells have a corresponding boundary cell or face
do gmesh_number = 0, ubound(gmesh,1)
  if (allocated(gmesh(gmesh_number)%gelement)) then
    do n = 1, ubound(gmesh(gmesh_number)%gelement,1)
      i = gmesh(gmesh_number)%gelement(n)%icell
      j = gmesh(gmesh_number)%gelement(n)%jface
      if (j /= 0.and.i == 0) then
        if (face(j)%type == 2) gmesh(gmesh_number)%gelement(n)%icell = face(j)%icell(2)
      end if
      if (i /= 0.and.j == 0) then
        if (cell(i)%type == 2) gmesh(gmesh_number)%gelement(n)%jface = cell(i)%jface(1)
      end if
    end do
  end if
end do

!-------------------------------------
! run through nodes again counting types
! also set glue_present, which signifies whether any faces attached to the node are glued

kboundary = 0
do k = 1, ktotal
  if (node(k)%type == 2) kboundary = kboundary + 1
  node(k)%glue_present = faces_surrounding_node_are_glued(k)
end do
kdomain = ktotal - kboundary

!-------------------------------------
! run through cells calculating vol, x (centre) and dx_max/min (maximum/minimum node to node distance)
! for this and the next we only need the index arrays: face%icell(1) and face%icell(2), face%knode, face%glue_jface, cell%knode and cell%jface

if (debug) write(*,*) 'calculating mesh cell geometries'

maximum_error_angle = 0.d0
do i = 1, itotal
  if (cell(i)%dimensions == 3) then
    call find_3d_geometry(jface=cell(i)%jface,volume=cell(i)%vol,centre=cell(i)%x)
  else if (cell(i)%dimensions == 2) then
    call find_2d_geometry(knode=cell(i)%knode,area=cell(i)%vol,centre=cell(i)%x,error_angle=error_angle,error=error)
    if (error) then
      write(*,'(a)') 'WARNING: problem calculating the geometry a cell: see fort.31 for more details'
      write(31,'(a)') 'WARNING: problem calculating the geometry a cell: see fort.31 for more details'
      write(31,*) ' i = ',i
      write(31,*) ' cell(i)%knode = ',cell(i)%knode
    end if
    maximum_error_angle = max(error_angle,maximum_error_angle)
  else if (cell(i)%dimensions == 1) then
    cell(i)%vol = distance( node(cell(i)%knode(1))%x , node(cell(i)%knode(2))%x )
    cell(i)%x = ( node(cell(i)%knode(1))%x + node(cell(i)%knode(2))%x )/2.d0
  else if (cell(i)%dimensions == 0) then
    cell(i)%vol = 1.d0
    cell(i)%x = node(cell(i)%knode(1))%x
  end if
  cell(i)%dx_max = 0.d0
  cell(i)%dx = 0.d0 ! this is a vector of the maximum node to node distance components
! do kk = 1, ubound(cell(i)%knode,1)
!   k = cell(i)%knode(kk)
!   cell(i)%dx_max = max(cell(i)%dx_max,distance( cell(i)%x, node(k)%x ))
! end do
! now equals maximum node to node distance around this cell
  do kk = 1, ubound(cell(i)%knode,1)
    k = cell(i)%knode(kk)
    do kk2 = kk + 1, ubound(cell(i)%knode,1)
      k2 = cell(i)%knode(kk2)
      cell(i)%dx_max = max(cell(i)%dx_max,distance( node(k)%x, node(k2)%x ))
      do l = 1,3
        cell(i)%dx(l) = max(cell(i)%dx(l),abs( node(k)%x(l)-node(k2)%x(l) ))
      end do
    end do
  end do
! equals minimum node to node distance around this cell - must be <= dx_max
  cell(i)%dx_min = cell(i)%dx_max
  do kk = 1, ubound(cell(i)%knode,1)
    k = cell(i)%knode(kk)
    do kk2 = kk + 1, ubound(cell(i)%knode,1)
      k2 = cell(i)%knode(kk2)
      cell(i)%dx_min = min(cell(i)%dx_min,distance( node(k)%x, node(k2)%x ))
    end do
  end do
! now also zero cell's volume if it is a boundary cell, v0.42
  if (cell(i)%type == 2) cell(i)%vol = 0.d0
end do

if (maximum_error_angle > 1.d-20) write(*,'(a,g11.3,a)') &
  'WARNING: at least one two dimensional cell is curved, with a maximum error angle of ', &
  maximum_error_angle*180.d0/pi,' degrees'
!-------------------------------------
! run through faces calculating area, x (centre) and norm
! also maximum_faceknodes

if (debug) write(*,*) 'calculating mesh face geometries'

new_size_2d = [totaldimensions,2]
maximum_error_angle = 0.d0
do j = 1, jtotal
! now calculate the rest
  if (face(j)%dimensions == 2) then
    call find_2d_geometry(knode=face(j)%knode,area=face(j)%area,norm=face(j)%norm,centre=face(j)%x,error_angle=error_angle,error=error)
    if (error) then
      write(*,'(a)') 'WARNING: problem calculating the geometry of a face: see fort.31 for more details'
      write(31,'(a)') 'WARNING: problem calculating the geometry of a face: see fort.31 for more details'
      write(31,*) ' j = ',j
      write(31,*) ' face(j)%knode = ',face(j)%knode
    end if
    maximum_error_angle = max(error_angle,maximum_error_angle)
  else if (face(j)%dimensions == 1) then
    face(j)%area = distance( node(face(j)%knode(1))%x , node(face(j)%knode(2))%x )
    face(j)%x = ( node(face(j)%knode(1))%x + node(face(j)%knode(2))%x )/2.d0
    tangc = face(j)%x - cell(face(j)%icell(1))%x ! NB icell(1) is neither a glued cell or a boundary cell, hence this one-sided definition
    normc = cross_product( tangc , node(face(j)%knode(2))%x - node(face(j)%knode(1))%x )
    face(j)%norm(:,1) = cross_product( node(face(j)%knode(2))%x - node(face(j)%knode(1))%x , normc )
    call normalise_vector(face(j)%norm(:,1))
    face(j)%norm(:,2) = node(face(j)%knode(2))%x - node(face(j)%knode(1))%x ! vector that is tangent to line
    call normalise_vector(face(j)%norm(:,2))
    face(j)%norm(:,3) = cross_product( face(j)%norm(:,1) , face(j)%norm(:,2) ) ! third vector which points out of solution plane
    call normalise_vector(face(j)%norm(:,3))
  else if (face(j)%dimensions == 0) then
    face(j)%area = 1.d0
    face(j)%x = node(face(j)%knode(1))%x
! previously facenorm was based on the vector to the downcell
!   face(j)%norm(:,1) = face(j)%x - cell(face(j)%icell(1))%x ! NB icell(1) is neither a glued cell or a boundary cell, hence this one-sided definition
! now we set the orientation based on both cells that surround the point, including periodic and reflect glued boundaries
    if (face(j)%glue_jface /= 0) then
      if (face(j)%glue_reflect /= 0) then
! a glued reflect face has a normal in the direction of the reflection
        face(j)%norm(:,1) = 0
        face(j)%norm(face(j)%glue_reflect,1) = face(j)%x(face(j)%glue_reflect) - cell(face(j)%icell(1))%x(face(j)%glue_reflect)
      else
! a periodic glued face has a normal based on both adjacent cells, noting that glued cell has a normal facing towards the glued face too
        jglue = face(j)%glue_jface
        face(j)%norm(:,1) = cell(face(jglue)%icell(1))%x - face(jglue)%x + face(j)%x - cell(face(j)%icell(1))%x
      end if
    else
! under normal circumstances normal points from downcell to upcell
      face(j)%norm(:,1) = cell(face(j)%icell(2))%x - cell(face(j)%icell(1))%x
    end if
    call normalise_vector(face(j)%norm(:,1))
! from orthogonal vectors select one which has smallest dot product with first
    face(j)%norm(:,2) = [1.d0, 0.d0, 0.d0]
    face(j)%norm(:,3) = [0.d0, 1.d0, 0.d0]
    if (abs(dot_product(face(j)%norm(:,2),face(j)%norm(:,1))) < abs(dot_product(face(j)%norm(:,3),face(j)%norm(:,1))) ) then
      face(j)%norm(:,3) = cross_product( face(j)%norm(:,1) , face(j)%norm(:,2) )
      face(j)%norm(:,2) = cross_product( face(j)%norm(:,1) , face(j)%norm(:,3) )
    else
      face(j)%norm(:,2) = cross_product( face(j)%norm(:,1) , face(j)%norm(:,3) )
      face(j)%norm(:,3) = cross_product( face(j)%norm(:,1) , face(j)%norm(:,2) )
    end if
    call normalise_vector(face(j)%norm(:,2))
    call normalise_vector(face(j)%norm(:,3))
  end if
! calculate some adjacent face vectors/distances, possibly taking into account any glued faces
! now these are stored in r, which will be increased in size, and then possibly decreased again after the kernels have been constructed
  call resize_double_precision_2d_array(array=face(j)%r,new_size=new_size_2d,keep_data=.false.,default_value=0.d0)
  if (face(j)%glue_jface /= 0) then
    jglue = face(j)%glue_jface
!   face(j)%r(:,2) = cell(face(j)%icell(2))%x - face(jglue)%x ! if the face is glued, then the upcell vector becomes the downcell vector from the glued face
    face(j)%r(:,2) = cell(face(jglue)%icell(1))%x - face(jglue)%x ! if the face is glued, then the upcell vector becomes the downcell vector from the glued face
    if (face(j)%glue_reflect /= 0) face(j)%r(face(j)%glue_reflect,2) = -face(j)%r(face(j)%glue_reflect,2) ! if the face is reflected, need to change the upcell dx accordingly
  else
    face(j)%r(:,2) = cell(face(j)%icell(2))%x - face(j)%x ! vector from face centre to adjacent upcell
  end if
  face(j)%r(:,1) = cell(face(j)%icell(1))%x - face(j)%x ! vector from face centre to adjacent downcell, which cannot be glued (only upcells could be glued)
  face(j)%dx = distance( face(j)%r(:,2) , face(j)%r(:,1) ) ! distance between adjacent cell centres
  face(j)%dx_unit = (face(j)%r(:,2) - face(j)%r(:,1))/face(j)%dx ! unit normal in direction of adjacent cell centres

  maximum_faceknodes = max(maximum_faceknodes,allocatable_integer_size(face(j)%knode))
end do

if (maximum_error_angle > 1.d-20) write(*,'(a,g11.3,a)') &
  'WARNING: at least one two dimensional face is curved, with a maximum error angle of ', &
  maximum_error_angle*180.d0/pi,' degrees'
!-------------------------------------
! run through faces increasing icell, and at the same time, calculating face%r and face%reflect_multiplier
! this list includes all cells that share a common node with the face, included in the specific order of:
!   first two elements are the immediate cell neighbours, including repeats if a face is glued to itself (reflection) - already defined
!   the second elements are from the surrounding nodes, including repeats for any self-glued cells

if (.true.) then

  new_size_2d = [totaldimensions,2]
  do j = 1, jtotal
! use expand_mask to find all surrounding cells (including phantom ones that have been glued here)
! calculate and store reflect_multiplier and r at the same time, possibly reducing in size after the kernels have been allocated
    call resize_integer_array(array=separation_index,new_size=1,keep_data=.false.,default_value=2)
    call resize_integer_2d_array(array=face(j)%reflect_multiplier,new_size=new_size_2d,keep_data=.false.,default_value=1)
    if (face(j)%glue_reflect /= 0) face(j)%reflect_multiplier(face(j)%glue_reflect,2) = -1

    call expand_mask(jcentre=j,have_icell=.false.,limit_mask_to_shared_nodes=.true.,include_adjacent_boundary_cells=.false., &
      imask=face(j)%icell,separation_index=separation_index,reflect_multiplier=face(j)%reflect_multiplier,r=face(j)%r, &
      dx=face(j)%dx)

! set logicals related to glue present within the icell domain
! if any reflect_multipliers aren't 1 then a reflect is present within the icell domain
    face(j)%reflect_present = .false.
    if (minval(face(j)%reflect_multiplier) /= 1) face(j)%reflect_present = .true.
! also loop through all faces attached to all cells (via the nodes) within the domain checking whether any glue operations (reflect or translate) have occurred within the icell domain
    face(j)%glue_present = face(j)%reflect_present
    if (.not.face(j)%glue_present) then
      do kk = 1, ubound(face(j)%knode,1)
        face(j)%glue_present = node(face(j)%knode(kk))%glue_present
        if (face(j)%glue_present) exit
      end do
    end if

  end do

else
  do jbase = 1, jtotal
    do n = 1, 2 ! loop through nodes attached to the considered face plus any glued faces
      if (n == 1) then
        j = jbase
      else if (face(jbase)%glue_jface /= 0) then ! if we are looking at the glued face (n=2) and it is defined then repeat loop
        j = face(jbase)%glue_jface
      else
        cycle
      end if
      kk_loop: do kk = 1, ubound(face(j)%knode,1)
        k = face(j)%knode(kk)
        ii_loop: do ii = 1, ubound(node(k)%icell,1)
          if (location_in_list(array=face(jbase)%icell,element=node(k)%icell(ii)) == 0) &
            call push_integer_array(array=face(jbase)%icell,new_element=node(k)%icell(ii))
        end do ii_loop
      end do kk_loop
    end do

  end do
end if

!-------------------------------------
! run through cells setting icell (surrounding cells), and at the same time, calculating cell%r and cell%reflect_multiplier
! this list includes all cells that share a common node with the central cell, included in the specific order of:
!   1 = central cell
!   2:ubound(cell%jface,1)+1 = cells sharing a face, including repeats of the central cell if it is glued to the current cell.  Order that adjacent cells are added is the same as that of the adjacent faces
!   ubound(cell%jface,1)+1:ubound(cell%icell,1) rest are the cells that only share a node, including repeats if necessary for glued faces

new_size_2d = [totaldimensions,1]
do i = 1, itotal

! first element is central cell
  call resize_integer_array(keep_data=.false.,array=cell(i)%icell,new_size=1,default_value=i)

  if (.true.) then

! use expand_mask to find all surrounding cells (including phantom ones that have been glued here)
! calculate and store reflect_multiplier and r at the same time, possibly reducing in size after the kernels have been allocated
    call resize_integer_array(array=separation_index,new_size=1,keep_data=.false.,default_value=1)
    call resize_integer_2d_array(array=cell(i)%reflect_multiplier,new_size=new_size_2d,keep_data=.false.,default_value=1)
    call resize_double_precision_2d_array(array=cell(i)%r,new_size=new_size_2d,keep_data=.false.,default_value=0.d0)

    call expand_mask(icentre=i,have_icell=.false.,limit_mask_to_shared_nodes=.true.,include_adjacent_boundary_cells=.false., &
      imask=cell(i)%icell,separation_index=separation_index,reflect_multiplier=cell(i)%reflect_multiplier,r=cell(i)%r, &
      dx=cell(i)%dx_max)

! set logicals related to glue present within the icell domain
! if any reflect_multipliers aren't 1 then a reflect is present within the icell domain
    cell(i)%reflect_present = .false.
    if (minval(cell(i)%reflect_multiplier) /= 1) cell(i)%reflect_present = .true.
! also loop through all faces attached to all cells (via the nodes) within the domain checking whether any glue operations (reflect or translate) have occurred within the icell domain
    cell(i)%glue_present = cell(i)%reflect_present
    if (.not.cell(i)%glue_present) then
      do kk = 1, ubound(cell(i)%knode,1)
        cell(i)%glue_present = node(cell(i)%knode(kk))%glue_present
        if (cell(i)%glue_present) exit
      end do
    end if

  else
    ! second elements are from the surrounding faces
    ! now with repeats for possibly cells glued onto themselves
      do jj = 1, ubound(cell(i)%jface,1)
        j = cell(i)%jface(jj)
        do ii = 1, 2
          if ((ii == 2.and.face(j)%glue_jface /= 0).or.face(j)%icell(ii) /= i) & ! the first means that the cell is glued to this cell (may be itself), the second covers all the rest
            call push_integer_array(array=cell(i)%icell,new_element=face(j)%icell(ii))
        end do
    !   do ii = 1, 2
    !     if (face(j)%icell(ii) /= i) call push_integer_array(array=cell(i)%icell,new_element=face(j)%icell(ii))
    !   end do
    !   if (face(j)%icell(1) == face(j)%icell(2)) call push_integer_array(array=cell(i)%icell,new_element=face(j)%icell(1)) ! face is glued onto itself, so add an entry representing an adjacent cell, otherwise many things will break!
      end do

    ! third elements are from the surrounding nodes, but now found via cells attached to surrounding faces so accounting for the glued faces
    ! no repeats for glued cells right now, but could include this
      do jj = 1, ubound(cell(i)%jface,1)
        j = cell(i)%jface(jj)
        do ii2 = 3, ubound(face(j)%icell,1)
          i2 = face(j)%icell(ii2)
          if (location_in_list(array=cell(i)%icell,element=i2) == 0) call push_integer_array(array=cell(i)%icell,new_element=i2)
        end do
      end do

    ! third elements are from the surrounding nodes
    ! do kk = 1, ubound(cell(i)%knode,1)
    !   k = cell(i)%knode(kk)
    !   ii2_loop: do ii = 1, ubound(node(k)%icell,1)
    !     if (location_in_list(array=cell(i)%icell,element=node(k)%icell(ii)) == 0) &
    !       call push_integer_array(array=cell(i)%icell,new_element=node(k)%icell(ii))
    !   end do ii2_loop
    ! end do
  end if

  maximum_cellknodes = max(maximum_cellknodes,allocatable_integer_size(cell(i)%knode))
  maximum_celljfaces = max(maximum_celljfaces,allocatable_integer_size(cell(i)%jface))

end do

if (allocated(separation_index)) deallocate(separation_index)

!-------------------------------------
! run through nodes (re)setting icell (surrounding cells), node%r and node%reflect_multiplier for glued_nodes
! node(k)%icell (and r and reflect_multiplier) needs to have a single entry for each real and glued cell (even to itself)
! before this loop, glue_faces will have removed duplicate entries for node(k)%icells that are glued (reflected) to themselves through this node, so
! if the node is glued, base the calculation on an adjacent face%icell that would already (above) have been correctly calculated via subroutine expand_mask

do k = 1, ktotal

  if (.not.node(k)%glue_present) then ! this is the simplest situation - use one allocation based on node(k)%icell size
    node(k)%reflect_present = .false.
    ncell = allocatable_integer_size(node(k)%icell)
    new_size_2d = [totaldimensions,ncell]
    call resize_integer_2d_array(array=node(k)%reflect_multiplier,new_size=new_size_2d,keep_data=.false.,default_value=1)
    call resize_double_precision_2d_array(array=node(k)%r,new_size=new_size_2d,keep_data=.false.,default_value=0.d0)
    do ii = 1, ncell
      i = node(k)%icell(ii)
      node(k)%r(:,ii) = cell(i)%x - node(k)%x ! location of cell centres relative to centre of kernel: first index is the dimension (1:3), second is the kernel element number
    end do
  else
! remove icell, and use 'incrementation' to form icell, r and reflect_multiplier, as this will only be for a relatively small number of nodes
! node(k)%jface only contains faces that are directly attached to node k, so pick any face (the first) on which to base the new node(k)%icell
! the reflect multipliers from this face will directly translate to the node
! the r vectors from this face will need to be adjusted for the change from face to node centre (nb, r is vector from node to cell centre here)
    j = node(k)%jface(1)
    deallocate(node(k)%icell)
    do ii = 1, allocatable_integer_size(face(j)%icell)
      i = face(j)%icell(ii)
      push_this_cell = .false.
! see if this cell has the relevant node, either the actual node, or its glued sister
! do this in two separate statements to avoid second being evaluated if first is already true
      if (location_in_list(array=cell(i)%knode,element=k) /= 0) then
        push_this_cell = .true.
      else if (has_elements_in_common(array1=node(k)%glue_knode,array2=cell(i)%knode)) then
        push_this_cell = .true.
      end if
! cell is attached to node, so add to node(k)%icell array and copy over r (origin shifted) and reflect_multiplier from face(j)
      if (push_this_cell) then
        call push_integer_array(array=node(k)%icell,new_element=i)
        ii2 = allocatable_integer_size(node(k)%icell)
        new_size_2d = [totaldimensions,ii2]
        call resize_integer_2d_array(array=node(k)%reflect_multiplier,new_size=new_size_2d,keep_data=.true.)
        call resize_double_precision_2d_array(array=node(k)%r,new_size=new_size_2d,keep_data=.true.)
        node(k)%reflect_multiplier(:,ii2) = face(j)%reflect_multiplier(:,ii)
        node(k)%r(:,ii2) = face(j)%x - node(k)%x + face(j)%r(:,ii)
      end if
    end do
    if (minval(node(k)%reflect_multiplier) /= 1) node(k)%reflect_present = .true.
  end if
end do

!-------------------------------------
! run through faces and cells checking that number of components is consistent with dimensions

if (debug) write(*,*) 'checking mesh consistency'

do i = 1, itotal
  if (cell(i)%type == 2) then
    if (ubound(cell(i)%jface,1) /= 1 ) &
      call error_stop('boundary cell does not have 1 face: cell = '//trim(print_cell(i)))
    if (cell(i)%dimensions == 2) then
      if (ubound(cell(i)%knode,1) < 3 ) &
        call error_stop('2d boundary cell has less than 3 nodes: cell = '//trim(print_cell(i)))
    else if (cell(i)%dimensions == 1) then
      if (ubound(cell(i)%knode,1) /= 2 ) &
        call error_stop('1d boundary cell does not have 2 nodes: cell = '//trim(print_cell(i)))
    else if (cell(i)%dimensions == 0) then
      if (ubound(cell(i)%knode,1) /= 1 ) &
        call error_stop('0d boundary cell does not have 1 node: cell = '//trim(print_cell(i)))
    else
      call error_stop('domain cell has incorrect dimensions/nodes: cell = '//trim(print_cell(i)))
    end if
  else
    if (cell(i)%dimensions == 3) then
      if (ubound(cell(i)%jface,1) < 4 .or. ubound(cell(i)%knode,1) < 4) &
        call error_stop('3d domain cell has less than 4 faces/nodes: cell = '//trim(print_cell(i)))
    else if (cell(i)%dimensions == 2) then
      if (ubound(cell(i)%jface,1) < 3 .or. ubound(cell(i)%knode,1) < 3) &
        call error_stop('2d domain cell has less than 3 faces/nodes: cell = '//trim(print_cell(i)))
    else if (cell(i)%dimensions == 1) then
      if (ubound(cell(i)%jface,1) /= 2  .or. ubound(cell(i)%knode,1) /= 2) &
        call error_stop('1d domain cell does not have 2 faces/nodes: cell = '//trim(print_cell(i)))
    else
      call error_stop('domain cell has incorrect dimensions: cell = '//trim(print_cell(i)))
    end if
    do jj = 1, ubound(cell(i)%jface,1)
      j = cell(i)%jface(jj)
      if (face(j)%dimensions /= cell(i)%dimensions - 1) &
        call error_stop('face has dimensions which are inconsistent with adjacent cell: cell = '//trim(print_cell(i))// &
        ': face = '//trim(print_face(j)))
    end do
  end if
end do

do j = 1, jtotal
  if (face(j)%dimensions == 2) then
    if (ubound(face(j)%knode,1) < 3 ) &
      call error_stop('2d face has less than 3 nodes: face = '//trim(print_face(j)))
  else if (face(j)%dimensions == 1) then
    if (ubound(face(j)%knode,1) /= 2 ) &
      call error_stop('1d face does not have 2 nodes: face = '//trim(print_face(j)))
  else if (face(j)%dimensions == 0) then
    if (ubound(face(j)%knode,1) /= 1 ) &
      call error_stop('0d face does not have 1 node: face = '//trim(print_face(j)))
  else
    call error_stop('face has incorrect dimensions/nodes: face = '//trim(print_face(j)))
  end if
end do

!------------------------------------------
! write out mesh details if requested

write(*,'(a)') 'INFO: number of mesh elements::'
formatline = '(3(a,'//trim(indexformat)//'))'
write(*,fmt=formatline) ' NODES: ktotal = ',ktotal,': kdomain = ',kdomain,': kboundary = ',kboundary
formatline = '(3(a,'//trim(indexformat)//'))'
write(*,fmt=formatline) ' FACES: jtotal = ',jtotal,': jdomain = ',jdomain,': jboundary = ',jboundary
formatline = '(3(a,'//trim(indexformat)//'))'
write(*,fmt=formatline) ' CELLS: itotal = ',itotal,': idomain = ',idomain,': iboundary = ',iboundary

if (mesh_details_file) then
  if (debug) write(*,*) 'writing mesh details to mesh_details.txt file'

  filename = "output/mesh_details.txt"
  open(fdetail,file=trim(filename),status='replace',iostat=ierror)
  if (ierror /= 0) call error_stop('problem opening file '//trim(filename))

  write(fdetail,'(a)') 'MESH DETAILS:'
  formatline = '(3(a,'//trim(indexformat)//'))'
  write(fdetail,fmt=formatline) 'NODES: ktotal = ',ktotal,': kdomain = ',kdomain,': kboundary = ',kboundary
  do k = 1,ktotal
    write(fdetail,'(a)') 'node: '//trim(print_node(k))
  end do
  formatline = '(3(a,'//trim(indexformat)//'))'
  write(fdetail,fmt=formatline) 'FACES: jtotal = ',jtotal,': jdomain = ',jdomain,': jboundary = ',jboundary
  do j = 1,jtotal
    write(fdetail,'(a)') 'face: '//trim(print_face(j))
  end do
  formatline = '(3(a,'//trim(indexformat)//'))'
  write(fdetail,fmt=formatline) 'CELLS: itotal = ',itotal,': idomain = ',idomain,': iboundary = ',iboundary
  do i = 1,itotal
    write(fdetail,'(a)') 'cell: '//trim(print_cell(i))
  end do

  close(fdetail)
end if

if (debug) write(*,'(a/80(1h-))') 'subroutine setup_mesh'

end subroutine setup_mesh

!-----------------------------------------------------------------

subroutine read_constants

! here we read in numerical values for the constants given in the input file

use general_module
integer :: ierror, cut, m, region_number, nn, ns, ijk
character(len=1000) :: textline, keyword, name, formatline, region_name, error_string
double precision :: value
logical, parameter :: debug = .false.
                  
if (debug) write(*,'(80(1h+)/a)') 'subroutine read_constants'

write(*,'(a)') "INFO: reading numerical constant values from file "//trim(input_file)
open(unit=finput,file=trim(input_file),status='old',iostat=ierror)
if (ierror /= 0) call error_stop('problem opening arb input file '//trim(input_file))

fileloop: do
  read(finput,'(a)',iostat=ierror) textline
  if (ierror /= 0) exit fileloop ! reached end of file
  call remove_comments(textline) ! remove comments from line
  cut=scan(textline,' ') ! split input line at first space
  if (cut<=1) cycle fileloop ! if there is space at first character or string is empty, cycle
  keyword=textline(1:cut-1)
  textline=adjustl(textline(cut:len_trim(textline)))
  if (keyword(1:8) /= "CONSTANT") cycle ! this is the only keyword we process in this subroutine

! get name and find constant var number
  cut=scan(textline,'>') ! find end of name string
  if (textline(1:1) /= '<'.or.cut<2) &
    call error_stop('the name in '//trim(keyword)//' in '//trim(input_file)//' is incorrectly specified on line:'//trim(textline))
  name = textline(1:cut) ! name includes <>
  textline=adjustl(textline(cut+1:len_trim(textline)))
  m = var_number_from_name(name)
  if (m == 0.or.trim(var(m)%type) /= "constant") &
    call error_stop('the constant '//trim(name)//' in '//trim(input_file)//' is incorrectly specified on line:'//trim(textline))

! read in value, multiplying by multiplier
  read(textline,*,iostat=ierror) value
  if (ierror /= 0) call error_stop('problem reading in the numerical value of constant '//trim(name)//' in file '//trim(input_file))
  value = value*var(m)%multiplier ! convert to specified units

! read in region_name if required
! read this in regardless of centring, to allow a sanity check
  cut=scanstring(textline,'ON ') ! find location sequence
! upper case will be produced by setup_equations.pl so we don't have to worry about case sensitivity
  if (cut > 0) textline=adjustl(textline(cut+2:len_trim(textline)))
  cut=scan(textline,'>') ! find end of region name
  if (textline(1:1) == '<'.and.cut>1) then
    region_name = textline(1:cut)
    if (var(m)%centring == 'none') &
      call error_stop('the constant '//trim(name)//' in '//trim(input_file)//' is has the region '//trim(region_name)//' specified')
  else if (var(m)%centring /= 'none') then
    call error_stop('the constant '//trim(name)//' in '//trim(input_file)//' has no region specified, yet it is '// &
      trim(var(m)%centring)//' centred')
  end if

! find region and check that the centrings of the region and variable match
  if (var(m)%centring /= 'none') then
    region_number = region_number_from_name(name=region_name)
    if (region_number == 0) call error_stop('the region '//trim(region_name)// &
      ' for which we are trying to read in some numerical constant values is not known')
    if (var(m)%centring /= region(region_number)%centring) &
      call error_stop('the region '//trim(region_name)//' for which we are trying to read in some numerical constant values '// &
        'has a different centring to '// &
        'the corresponding variable '//trim(name)//'.  Change the definition statements for this constant to make the centrings '// &
        'consistent')
  end if

! so we are now ready to write the values to the elements contained within this region
  if (trim(var(m)%centring) == "none") then
    var(m)%funk(1)%v = value
  else
    do nn = 1, ubound(region(region_number)%ijk,1)
      ijk = region(region_number)%ijk(nn)
! right now if the requested region is outside of the region we are reading in values for, the read will fail
! could change this behaviour, but may mean that user doesn't know what they are doing???
      error_string='Error occurred while reading numerical constant '//trim(var(m)%name)// &
        'from file '//trim(input_file)
      ns = nsvar(m=m,ijk=ijk,error_string=error_string)
      var(m)%funk(ns)%v = value
    end do
  end if

! if (debug) then
!   formatline = '(a,'//trim(floatformat)//')'
    formatline = '(a,'//trim(compactformat)//')'
    if (var(m)%centring == "none") then
      write(*,fmt=formatline) 'INFO: read numerical value for '//var(m)%centring//' '//trim(var(m)%type)//' '//trim(var(m)%name)// &
        ': value = ',value
    else
      write(*,fmt=formatline) 'INFO: read numerical value for '//var(m)%centring//' '//trim(var(m)%type)//' '//trim(var(m)%name)// &
        ' defined on region '//trim(region(var(m)%region_number)%name)//' to (only) elements contained within region ' &
        //trim(region_name)//': value = ',value
    end if
! end if
    
!---------------

end do fileloop

close(finput)

if (debug) write(*,'(a/80(1h-))') 'subroutine read_constants'

end subroutine read_constants

!----------------------------------------------------------------------------

subroutine setup_vars

! here we allocate array elements for the fields and functions and initialise fields

use general_module
use equation_module
use solver_module
integer :: m, ns, n, mc, o, pptotal, mtype, var_list_number_l, relstep
character(len=1000) :: formatline, component_list
character(len=100) :: option_name
logical :: first, error
logical, parameter :: debug = .false.
logical :: debug_sparse = .true.

if (debug) debug_sparse = .true.
                  
if (debug_sparse) write(*,'(80(1h+)/a)') 'subroutine setup_vars'

! allocate var funks, zero these funks and set region numbers
do m = 1, ubound(var,1)
  if (var(m)%centring /= "none") then
    if (var(m)%region_number == 0) call error_stop('there is a problem with region '//trim(var(m)%region)// &
      ' which is associated with '//trim(var(m)%type)//' '//trim(var(m)%name)//': something catastrophic')
! this check is redundant as only the update_region will be dynamic - the region is the static parent of the update_region if the latter is dynamic
    if (region(var(m)%region_number)%dynamic) call error_stop('the region '//trim(var(m)%region)// &
      ' which is associated with '//trim(var(m)%type)//' '//trim(var(m)%name)// &
      ' is dynamic: only static (gmsh, setup and system) regions'// &
      ' can be used to define variables')
! let a variable assigned to a region with no elements, and hence no values, work, with a warning
!   if (allocatable_size(region(var(m)%region_number)%ijk) == 0) call error_stop('there is a problem with region '// &
!     trim(var(m)%region)//' which is associated with '//trim(var(m)%type)//' '//trim(var(m)%name)// &
!     ': the region contains no elements')
    if (allocatable_size(region(var(m)%region_number)%ijk) == 0) write(*,'(a)') 'WARNING: there is a problem with region '// &
      trim(var(m)%region)//' which is associated with '//trim(var(m)%type)//' '//trim(var(m)%name)// &
      ': the region contains no elements'
    if (var(m)%type /= 'local') allocate(var(m)%funk(ubound(region(var(m)%region_number)%ijk,1)))
  else
    var(m)%region_number = 0 ! dummy region number for none centred variables
    if (var(m)%type /= 'local') allocate(var(m)%funk(1))
  end if
  if (allocated(var(m)%funk)) then
    do ns = 1, ubound(var(m)%funk,1)
      call reset_funk(var(m)%funk(ns))
    end do
  end if
end do

! allocate the funk container that will be used in all derivative calculations
allocate(funkt(nthreads))

! allocate a temporary storage array for timing variable updates (output_variable_update_time)
if (output_variable_update_times) allocate(update_time_start(nthreads))

! allocate someloops, and funk and separation lists within these
allocate(someloop(nthreads))
do n = 1, nthreads
  if (msomeloop > 0) allocate(someloop(n)%funk(msomeloop))
! allocate the list of separation_lists, putting in atleast one element to avoid allocate checks
  allocate(someloop(n)%current_separation_list(max(1,mseparation_list))) 
  someloop(n)%current_separation_list = 0 ! zero means nothing is current
! if there are some separation loops used in the equations, allocate a unique separation_list for each to each someloop (thread)
  if (mseparation_list > 0) then
    allocate(someloop(n)%separation_list(mseparation_list))
    do m = 1, mseparation_list
! allocate one element to each of the separation lists contents to avoid time-consuming checks later
      allocate(someloop(n)%separation_list(m)%icell(1))
      allocate(someloop(n)%separation_list(m)%separation_index(1))
      allocate(someloop(n)%separation_list(m)%r(totaldimensions,1))
      allocate(someloop(n)%separation_list(m)%reflect_multiplier(totaldimensions,1))
    end do
  end if
end do

! now print var_lists
if (debug) then
  write(82,*) 'var_list details:'
  do m = 1, var_list_number(type="all",centring="all")
    write(82,*) 'var_list number = ',m,': allocatable_size = ',allocatable_size(var_list(m)%list)
    if (allocatable_size(var_list(m)%list) > 0) then
      do n = 1, allocatable_size(var_list(m)%list)
        write(82,*) '  contains '//trim(var(var_list(m)%list(n))%name)//' of centring = '// &
          trim(var(var_list(m)%list(n))%centring)//' and type = '//trim(var(var_list(m)%list(n))%type)
      end do
    end if
  end do
end if

! run through unknowns setting derivatives and calculating ptotal
ptotal = 0 ! this is the number of unknown variables (should equal pptotal)
do n = 1, allocatable_size(var_list(var_list_number_unknown)%list)
  m = var_list(var_list_number_unknown)%list(n)
  do ns = 1, ubound(var(m)%funk,1)
    ptotal = ptotal + 1
    call push_array(var(m)%funk(ns)%pp,ptotal)
    call push_array(var(m)%funk(ns)%dv,1.d0)
    var(m)%funk(ns)%ndv = 1
  end do
end do

! allocate newton solver working variables
if (.not.allocated(delphiold)) allocate(delphiold(ptotal))
if (.not.allocated(delphi)) allocate(delphi(ptotal))
delphi = 0.d0
delphiold = 0.d0

! initialise and place values in the fast lookup array unknown_var_from_pp
allocate(unknown_var_from_pp(ptotal))
do n = 1, allocatable_size(var_list(var_list_number_unknown)%list)
  m = var_list(var_list_number_unknown)%list(n)
  unknown_var_from_pp(var(m)%funk(1)%pp(1):var(m)%funk(ubound(var(m)%funk,1))%pp(1)) = m
end do
  
! run through equations calculating pptotal
pptotal = 0 ! this is the number of equations (should equal ptotal)
do n = 1, allocatable_size(var_list(var_list_number_equation)%list)
  m = var_list(var_list_number_equation)%list(n)
  pptotal = pptotal + ubound(var(m)%funk,1)
end do

! count maximum relstep in each of the transients/newtients, now for both variables and dynamic regions
transient_relstepmax = 0
var_list_number_l = var_list_number(centring="all",type="transient",include_regions=.true.)
do n = 1, allocatable_size(var_list(var_list_number_l)%list)
  if (var_list(var_list_number_l)%region(n)) then
    relstep = region(var_list(var_list_number_l)%list(n))%relstep
  else
    relstep = var(var_list(var_list_number_l)%list(n))%relstep
  end if
  transient_relstepmax = max(transient_relstepmax,relstep)
end do
newtient_relstepmax = 0
var_list_number_l = var_list_number(centring="all",type="newtient",include_regions=.true.)
do n = 1, allocatable_size(var_list(var_list_number_l)%list)
  if (var_list(var_list_number_l)%region(n)) then
    relstep = region(var_list(var_list_number_l)%list(n))%relstep
  else
    relstep = var(var_list(var_list_number_l)%list(n))%relstep
  end if
  newtient_relstepmax = max(newtient_relstepmax,relstep)
end do

! find user-set magnitude and whether dynamically adjusted magnitudes are set
!formatline = '(a,'//trim(floatformat)//')'
formatline = '(a,'//trim(compactformat)//')'
do m = 1, ubound(var,1)
  if (var(m)%type /= 'unknown'.and.var(m)%type /= 'equation') cycle
! set defaults
  if (var(m)%type == 'equation') then
    var(m)%dynamic_magnitude = .true.
    var(m)%dynamic_magnitude_multiplier = 1.1d0
  else ! unknowns
    var(m)%dynamic_magnitude = .false.
    var(m)%dynamic_magnitude_multiplier = 2.d0
  end if
  if (trim(check_option(var(m)%options,magnitude_options)) == 'dynamicmagnitude') then
    var(m)%dynamic_magnitude = .true.
    write(*,'(a)') 'INFO: setting dynamically adjusted magnitudes for '//trim(var(m)%type)//' '//trim(var(m)%name)
  else if (trim(check_option(var(m)%options,magnitude_options)) == 'staticmagnitude') then
    var(m)%dynamic_magnitude = .false.
    write(*,'(a)') 'INFO: setting static magnitudes for '//trim(var(m)%type)//' '//trim(var(m)%name)
  end if
  do n = 1, allocatable_size(var(m)%options) ! read through list from left to right, so that rightmost setting takes precedence
    option_name = extract_option_name(var(m)%options(n),error)
    if (error) cycle
    if (trim(option_name) == 'magnitude') then
      if (var(m)%magnitude_constant /= 0) cycle ! skip setting this magnitude if it will be set by a magnitude_constant
      var(m)%magnitude = extract_option_double_precision(var(m)%options(n),error)
      if (error) call error_stop("could not determine the user-set magnitude for variable "//trim(var(m)%name)// &
        " from the option "//trim(var(m)%options(n)))
      write(*,fmt=formatline) 'INFO: setting the initial magitude for '//trim(var(m)%type)//' '//trim(var(m)%name)//' to ', &
        var(m)%magnitude
    else if (trim(option_name) == 'dynamicmagnitudemultiplier') then
      var(m)%dynamic_magnitude_multiplier = extract_option_double_precision(var(m)%options(n),error)
      if (error) call error_stop("could not determine the user-set dynamicmagnitudemultiplier for variable "//trim(var(m)%name)// &
        " from the option "//trim(var(m)%options(n)))
      write(*,fmt=formatline) 'INFO: setting the dynamicmagitudemultiplier for '//trim(var(m)%type)//' '//trim(var(m)%name)// &
        ' to ',var(m)%dynamic_magnitude_multiplier
      if (var(m)%dynamic_magnitude_multiplier < 1.d0) call error_stop("each dynamicmagnitudemultiplier must be >= 1.d0")
    end if
  end do
end do

! add default output options - these come before user-set options so only take effect if user has not already set them
! COMPONENTS
! output is off by default
! stepoutput is off by default
! input is off by default (values will be read in via compounds for unknowns by default)
! elementdata used by default
do n = 1, ubound(var,1)
  call push_character_array(array=var(n)%options,new_element='nooutput',reverse=.true.)
  call push_character_array(array=var(n)%options,new_element='nostepoutput',reverse=.true.)
  call push_character_array(array=var(n)%options,new_element='noinput',reverse=.true.)
  call push_character_array(array=var(n)%options,new_element='elementdata',reverse=.true.)
end do
! COMPOUNDS
! output - if var is an unknown or an output or a cell centred derived then on by default
!   for a transient simulation the last relstep data does not need to be output for restart
! stepoutput - on by default for none centred unknown, derived, transients and outputs for current relstep
!   note output variables that should only be updated when the msh files are written should have
!   stepoutputnoupdate or nosetupoutput set as options
! input is on for all unknowns
! elementdata used by default unless cell centred scalar, then elementnodelimiteddata
! elementnodedata cannot be used for face centred quantities (or none centred for that matter)
do n = 1, ubound(compound,1)
! ref: default output
! outputs are not always output
  if (compound(n)%type == 'output'.or.((compound(n)%type == 'unknown'.or. &
    (compound(n)%type == 'derived'.and.compound(n)%centring == 'cell').or. &
    compound(n)%type == 'transient').and.compound(n)%relstep < max(transient_relstepmax,1))) then
    call push_character_array(array=compound(n)%options,new_element='output',reverse=.true.)
  else
    call push_character_array(array=compound(n)%options,new_element='nooutput',reverse=.true.)
  end if
! ref: default stepoutput
! none centred outputs now also always stepoutputted
  if ((compound(n)%type == 'output'.or.((compound(n)%type == 'unknown'.or. &
    compound(n)%type == 'transient'.or.compound(n)%type == 'output') &
    .and.compound(n)%relstep == 0)).and.compound(n)%centring == 'none') then
    call push_character_array(array=compound(n)%options,new_element='stepoutput',reverse=.true.)
  else
    call push_character_array(array=compound(n)%options,new_element='nostepoutput',reverse=.true.)
  end if
! v0.42, constants are now not input by default
! if (compound(n)%type == 'unknown'.or.compound(n)%type == 'transient'.or. &
!     compound(n)%type == 'constant') then
! ref: default input
  if (compound(n)%type == 'unknown'.or.compound(n)%type == 'transient') then
    call push_character_array(array=compound(n)%options,new_element='input',reverse=.true.)
  else
    call push_character_array(array=compound(n)%options,new_element='noinput',reverse=.true.)
  end if
  if (compound(n)%centring /= 'none') then
    if (compound(n)%rank == 'scalar'.and.compound(n)%centring == 'cell') then
      call push_character_array(array=compound(n)%options,new_element='elementnodelimiteddata',reverse=.true.)
    else
      call push_character_array(array=compound(n)%options,new_element='elementdata',reverse=.true.)
    end if
  end if
end do

! run through constants reading in file values and then setting any equation determined ones
call read_constants
call update_and_check_constants
call read_initial_outputs

! run through unknowns setting initial values
call update_and_check_unknowns(initial=.true.)

! run through transients setting initial values
if (transient_simulation) call update_and_check_initial_transients

! run through newtients setting initial values
if (newtient_simulation) call update_and_check_initial_newtients

! run through setting initial derived and equation variables
call update_and_check_derived_and_equations(setup=.true.)

! spray out some info about all the var variables
if (debug_sparse) then
  if (debug) then
    write(*,'(a)') '---------------------------------------------------------------'
    write(*,'(a)') 'INFO: component variables:'
    do mtype = 1, ubound(var_types,1)
      write(*,'(a/a)') '------------------------------------',trim(var_types(mtype))
      do n = 1, allocatable_size(var_list(var_list_number(centring="all",type=trim(var_types(mtype))))%list)
        m = var_list(var_list_number(centring="all",type=trim(var_types(mtype))))%list(n)
        if (var(m)%centring == 'none') then
          formatline = '(a,'// &
            trim(dindexformat(m))//',a,'// &
            trim(dindexformat(var(m)%relstep))//',a,'// &
            trim(dindexformat(var(m)%someloop))//',a'// &
            repeat(',a',allocatable_size(var(m)%options))//')'
          write(*,fmt=formatline) ' variable_number = ',m, &
            ': name = '//trim(var(m)%name)// &
            ': type = '//trim(var(m)%type)// &
            ': centring = '//var(m)%centring// &
            ': rank = '//trim(var(m)%rank)// &
            ': relstep = ',var(m)%relstep, &
            ': someloop = ',var(m)%someloop, &
            ': (prioritised) options =',(' '//trim(var(m)%options(o)),o=1,allocatable_size(var(m)%options))
        else
          formatline = '(a,'// &
            trim(dindexformat(m))//',a,'// &
            trim(dindexformat(var(m)%region_number))//',a,'// &
            trim(dindexformat(var(m)%relstep))//',a,'// &
            trim(dindexformat(var(m)%someloop))//',a,'// &
            trim(dindexformat(region(var(m)%region_number)%ijk(1)))//',a,'// &
            trim(dindexformat(ubound(region(var(m)%region_number)%ijk,1)))//',a,'// &
            trim(dindexformat(region(var(m)%region_number)%ijk(ubound(region(var(m)%region_number)%ijk,1))))// &
            ',a'//repeat(',a',allocatable_size(var(m)%options))//')'
          write(*,fmt=formatline) ' variable_number = ',m, &
            ': name = '//trim(var(m)%name)// &
            ': type = '//trim(var(m)%type)// &
            ': centring = '//var(m)%centring// &
            ': rank = '//trim(var(m)%rank)// &
            ': relstep = ',var(m)%relstep, &
            ': someloop = ',var(m)%someloop, &
            ': region = '//trim(var(m)%region)// &
            ': region_number = ',var(m)%region_number, &
            ': ijk(1) = ',region(var(m)%region_number)%ijk(1), &
            ': ijk(',ubound(region(var(m)%region_number)%ijk,1),') = ', &
            region(var(m)%region_number)%ijk(ubound(region(var(m)%region_number)%ijk,1)), &
            ': (prioritised) options =',(' '//trim(var(m)%options(o)),o=1,allocatable_size(var(m)%options))
        end if
      end do
    end do
  end if

  write(*,'(a)') '---------------------------------------------------------------'
  write(*,'(a)') 'INFO: compound variables:'
  do mtype = 1, ubound(var_types,1)
    write(*,'(a/a)') '------------------------------------',trim(var_types(mtype))
    do m = 1, ubound(compound,1)
      if (trim(compound(m)%type) /= trim(var_types(mtype))) cycle
      component_list = "["
      do o = 1, ubound(compound(m)%component,1)
        if (compound(m)%component(o) == 0) then
          component_list = trim(component_list)//' (empty)'
        else
          component_list = trim(component_list)//' '//trim(var(compound(m)%component(o))%name)
        end if
      end do
      component_list = trim(component_list)//' ]'
      if (compound(m)%centring == 'none') then
        formatline = '(a,'// &
          trim(dindexformat(m))//',a,'// &
          trim(dindexformat(compound(m)%relstep))//',a'// &
          repeat(',a',allocatable_size(compound(m)%options))//')'
        write(*,fmt=formatline) ' compound_number = ',m, &
          ': name = '//trim(compound(m)%name)// &
          ': type = '//trim(compound(m)%type)// &
          ': centring = '//compound(m)%centring// &
          ': rank = '//trim(compound(m)%rank)// &
          ': relstep = ',compound(m)%relstep, &
          ': component_list = '//trim(component_list)// &
          ': (prioritised) options =',(' '//trim(compound(m)%options(o)),o=1,allocatable_size(compound(m)%options))
      else
        formatline = '(a,'// &
          trim(dindexformat(m))//',a,'// &
          trim(dindexformat(compound(m)%relstep))//',a,'// &
          trim(dindexformat(compound(m)%region_number))// &
          ',a'//repeat(',a',allocatable_size(compound(m)%options))//')'
        write(*,fmt=formatline) ' compound_number = ',m, &
          ': name = '//trim(compound(m)%name)// &
          ': type = '//trim(compound(m)%type)// &
          ': centring = '//compound(m)%centring// &
          ': rank = '//trim(compound(m)%rank)// &
          ': relstep = ',compound(m)%relstep, &
          ': region = '//trim(compound(m)%region)// &
          ': region_number = ',compound(m)%region_number, &
          ': component_list = '//trim(component_list)// &
          ': (prioritised) options =',(' '//trim(compound(m)%options(o)),o=1,allocatable_size(compound(m)%options))
      end if
    end do
  end do
  write(*,'(a)') '---------------------------------------------------------------'
end if

if (pptotal /= ptotal) then
  write(*,*) 'ERROR: the total number of equations does not match the total number of unknown variables'
  write(*,*) '  pptotal (number of equations) = ',pptotal
  write(*,*) '  ptotal (number of unknown variables) = ',ptotal
  write(*,*) '  ptotal-pptotal (additional number of unknown variables) = ',ptotal-pptotal
  stop
else if (pptotal == 0) then
  write(*,'(a)') 'WARNING: no equations/unknowns are being solved for'
end if

if (debug_sparse) write(*,'(a/80(1h-))') 'subroutine setup_vars'

end subroutine setup_vars

!-----------------------------------------------------------------

subroutine glue_faces

! ref: glue faces
! here we glue any faces together

use general_module
integer :: n, m, jj, j, jjmax, kk, k, kkmax, m1, m2, jj1, jj2, j1, j2, norphans, jglue, kk2, k2, kk3, k3, ksister
character(len=100) :: option_name, formatline
double precision, dimension(totaldimensions) :: centre, targetx, rel_x
double precision :: maxdist, maxdist2, dist2, dist
logical :: error, translate
logical, parameter :: debug = .false.

if (.not.allocated(glue_face)) return

if (debug) write(*,'(80(1h+)/a)') 'subroutine glue_faces'

! run through each glue face command
do n = 1, ubound(glue_face,1)

  if (debug) write(80,*) 'GLUE_FACE ',n

! find region numbers
  do m = 1, 2
    glue_face(n)%region_number(m) = region_number_from_name(name=glue_face(n)%region(m))
! check that region exists
    if (glue_face(n)%region_number(m) == 0) &
      call error_stop("the region "//trim(glue_face(n)%region(m))//" which is being referenced in a GLUE_FACE "// &
      "command is not known:  Note, regions used in this command must be defined directly in a gmesh file")
! and that it is of type gmsh, which means that it is defined by reading in from a gmsh file and so is also static
    if (region(glue_face(n)%region_number(m))%type /= 'gmsh') &
      call error_stop("the region "//trim(glue_face(n)%region(m))//" which is being referenced in a GLUE_FACE "// &
      "command is not defined primarily as a GMSH region:  Note, regions used in this command must be defined directly in "// &
      "the gmesh file rather than be defined via user commands or generated by the fortran (ie, system regions).  Note also "// &
      "that input file region definitions override gmesh file region definitions, so if the region is defined in a gmesh "// &
      "file and you are seeing this error, you may need to remove an overriding input file definition.")
! check that centring is consistent
    if (trim(region(glue_face(n)%region_number(m))%centring) /= 'face') &
      call error_stop("the region "//trim(glue_face(n)%region(m))//" which is being referenced in a GLUE_FACE "// &
      "command is "//trim(region(glue_face(n)%region_number(m))%centring)//" centred: it should be face centred.")
    if (debug) write(80,*) 'glue_face(n=',n,')%region_number(m=',m,') = ',glue_face(n)%region_number(m),' for region '// &
      trim(glue_face(n)%region(m))
  end do

! create an array of the options
  call extract_options(textline=glue_face(n)%option_line,options=glue_face(n)%options)

! see if there is any translate option and any reflection specification
  translate = .true. ! default is for translate to be on
  glue_face(n)%reflect = 0 ! default is no reflection, suitable for periodic boundaries, unless both regions are the same
  if (glue_face(n)%region_number(1) == glue_face(n)%region_number(2)) glue_face(n)%reflect = -1
  do m = 1,allocatable_size(glue_face(n)%options)
    option_name = extract_option_name(glue_face(n)%options(m),error)
    if (error) call error_stop("problem extracting a GLUE_FACE option name from "//trim(glue_face(n)%options(m)))
    if (option_name == 'translate') translate = .true.
    if (option_name == 'notranslate') translate = .false.
    if (option_name == 'reflect') then
      glue_face(n)%reflect = extract_option_integer(glue_face(n)%options(m),error)
      if (error) glue_face(n)%reflect = -1 ! if no reflection direction is specified then make it negative so that it will be set by the first face
    end if
    if (option_name == 'noreflect') glue_face(n)%reflect = 0 ! turns reflect off again
  end do
  if (debug) write(80,*) 'after options have been read in: translate = ',translate,': reflect = ',glue_face(n)%reflect

! construct arrays of facex locations based on surrounding node positions
! store these in face(j)%x arrays
! at the same time calculate translate if needed, which is a vector from region 1 to region 2
  glue_face(n)%translate = 0.d0 
  do m = 1, 2
    jjmax = allocatable_size(region(glue_face(n)%region_number(m))%ijk)
    if (jjmax == 0) call error_stop("the region "//trim(glue_face(n)%region(m))//" used in a GLUE_FACE command contains no "// &
      "elements")
    centre = 0.d0
    do jj = 1, jjmax
      j = region(glue_face(n)%region_number(m))%ijk(jj)
      kkmax = allocatable_size(face(j)%knode)
      if (kkmax == 0) call error_stop("a face in the region "//trim(glue_face(n)%region(m))//" used in a GLUE_FACE command "// &
        "is connected to no nodes: face = "//print_face(j))
      face(j)%x = 0.d0
      do kk = 1, kkmax
        k = face(j)%knode(kk)
        face(j)%x = face(j)%x + node(k)%x
      end do
      face(j)%x = face(j)%x/dble(kkmax)
      centre = centre + face(j)%x ! average of all centre locations used if translate is needed
    end do
    centre = centre/dble(jjmax)
    if (translate.and.m == 1) glue_face(n)%translate = centre ! at end of first loop translate is the centre of region 1
  end do
  if (translate) glue_face(n)%translate = centre - glue_face(n)%translate ! now centre is the centre of region 2, and the translate vector is defined as centre(2) - centre(1)
  if (debug.and.translate) write(80,*) 'translate = ',glue_face(n)%translate

! based on first face in first region try to determine reflection direction if required
  if (glue_face(n)%reflect < 0) then
    j = region(glue_face(n)%region_number(1))%ijk(1)
    call error_stop("detection of reflection direction not implemented yet in subroutine glue_faces: specify reflect=1|2|3 "// &
      "explicitly as an option for the GLUE_FACE command")
  end if

! if (glue_face(n)%reflect < 0) &
!   call error_stop("detection of reflection direction not implemented yet in subroutine glue_faces: specify reflect=1|2|3 "// &
!     "explicitly as an option for the GLUE_FACE command")

! now loop through each region finding the closest faces
  do m1 = 1, 2
    m2 = 3 - m1
    do jj1 = 1, allocatable_size(region(glue_face(n)%region_number(m1))%ijk)
      j1 = region(glue_face(n)%region_number(m1))%ijk(jj1)
      if (face(j1)%glue_jface /= 0) cycle
      maxdist2 = huge(1.d0) ! use dist and dist2 (the square) concurrently now
      maxdist = sqrt(maxdist2)
      targetx = face(j1)%x + dble((-1)**(m1-1))*glue_face(n)%translate ! taking account of the translation, this is the spot where the other glued face should be
      do jj2 = 1, allocatable_size(region(glue_face(n)%region_number(m2))%ijk)
        j2 = region(glue_face(n)%region_number(m2))%ijk(jj2)
        rel_x = targetx - face(j2)%x
! in the interests of speed, compare each component against maxdist separately as a coarse filter
        if (abs(rel_x(1)) > maxdist) cycle
        if (abs(rel_x(2)) > maxdist) cycle
        if (abs(rel_x(3)) > maxdist) cycle
! now calculate the distance squared and compared this against the stored value
        dist2 = dot_product(rel_x,rel_x)
        if (dist2 < maxdist2) then
          jglue = j2
! save both the squared distance and actual distance
          maxdist2 = dist2
          maxdist = sqrt(max(dist2,0.d0))
        end if
      end do
      face(j1)%glue_jface = jglue
      face(j1)%glue_reflect = glue_face(n)%reflect
      if (debug) write(80,*) 'from search found glue pairs: m1 = ',m1,': j1 = ',j1,': jglue = ',jglue,': maxdist = ',maxdist, &
        ': face(j1)%x = ',face(j1)%x,': face(j2)%x = ',face(j2)%x,': targetx = ',targetx
    end do
  end do

! now run through these again checking that the relationship is reciprocal, and if not, remove the glue
  do m = 1, 2
    norphans = 0
    do jj = 1, allocatable_size(region(glue_face(n)%region_number(m))%ijk)
      j = region(glue_face(n)%region_number(m))%ijk(jj)
      if (face(face(j)%glue_jface)%glue_jface /= j) then
        if (debug) write(80,*) 'an orphaned jface was found: m = ',m,': j = ',j,': glue_jface = ',face(j)%glue_jface, &
          'face(glue_jface)%glue_jface = ',face(face(j)%glue_jface)%glue_jface
        face(j)%glue_jface = 0
        norphans = norphans + 1
      end if
    end do
    if (norphans > 0) then
      formatline = '(a,'//trim(dindexformat(norphans))//',a)'
      write(*,fmt=formatline) 'WARNING: during the glueing of '//trim(glue_face(n)%region(1))// &
        ' to '//trim(glue_face(n)%region(2))//' ',norphans,' faces in region '//trim(glue_face(n)%region(m))// &
        ' remain unglued.  If there is supposed to be a one-to-one correspondance between these regions then this '// &
        'indicates an error'
    end if
  end do

  formatline = '(a,i1,a,3(1x,'//trim(compactformat)//'))'
  write(*,fmt=formatline) 'INFO: completed glueing operation between face regions '//trim(glue_face(n)%region(1))//' and '// &
    trim(glue_face(n)%region(2))//': reflection coordinate = ',glue_face(n)%reflect,': translate vector =',glue_face(n)%translate

end do

! now define node%knode for any nodes that are coincident with this one after any glueing operations - the sister nodes
if (debug) write(80,*) 'Finding sister nodes:'
do j = 1, jtotal
  jglue = face(j)%glue_jface
  if (jglue == 0.or.jglue == j) cycle ! this node will not be coincident with others if there is no glue or it is glued to itself
! find closest other nodes based on distance
  do kk = 1, allocatable_size(face(j)%knode)
    k = face(j)%knode(kk)
    maxdist = huge(1.d0)
    ksister = 0
    do kk2 = 1, allocatable_size(face(jglue)%knode)
      k2 = face(jglue)%knode(kk2)
      dist = distance(node(k)%x,node(k2)%x)
      if (dist < maxdist) then
        ksister = k2
        maxdist = dist
      end if
    end do
    if (ksister > 0.and.ksister /= k) then ! for no reflect faces I don't see how k == ksister, but guard against anyway
      if (debug.and.ksister > 0) write(80,*) 'found coincident node: k = ',k,': ksister = ',ksister
      if (location_in_list(array=node(k)%glue_knode,element=ksister) == 0) &
        call push_integer_array(array=node(k)%glue_knode,new_element=ksister)
    end if
  end do
end do

! now populate this list so that all nodes know of all other nodes that are coincident with it
if (debug) write(80,*) 'Listing all coincident nodes:'
do k = 1, ktotal
  kk2 = 1
  do while (kk2 <= allocatable_size(node(k)%glue_knode))
    k2 = node(k)%glue_knode(kk2)
    do kk3 = 1, allocatable_size(node(k2)%glue_knode)
      k3 = node(k2)%glue_knode(kk3)
      if (location_in_list(array=node(k)%glue_knode,element=k3) == 0.and.k3 /= k) &
        call push_integer_array(array=node(k)%glue_knode,new_element=k3)
    end do
    kk2 = kk2 + 1
  end do
  if (debug.and.allocatable_size(node(k)%glue_knode) > 0) write(80,*) 'coincident nodes: k = ',k,': glue_knode = ', &
    node(k)%glue_knode
end do

if (debug) write(*,'(a/80(1h-))') 'subroutine glue_faces'

end subroutine glue_faces

!-----------------------------------------------------------------

end module setup_module

!-----------------------------------------------------------------
