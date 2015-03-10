! file src/region_module.f90
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
module region_module

implicit none

private
public setup_regions, update_region, setup_region_link ! only subroutine accessible from outside the module

! various setup related options

!-----------------------------------------------------------------
contains

!-----------------------------------------------------------------

subroutine setup_regions

! here we setup the regions by finding the i or j indices for each

use general_module
integer :: m, i, j, k, ns, ierror
character(len=1000) :: formatline, filename, textline, textline2
logical :: debug_sparse = .true.
logical, parameter :: debug = .false.

if (debug) debug_sparse = .true.
                  
if (debug) write(82,'(80(1h+)/a)') 'subroutine setup_regions'

! loop through all regions finding static ijk indicies and initialising ns arrays for all

do m = 1, ubound(region,1)

! allocate ns arrays for all regions - these stay allocated and always have these sizes

  if (region(m)%centring == "cell") then
    allocate(region(m)%ns(itotal))
  else if (region(m)%centring == "face") then
    allocate(region(m)%ns(jtotal))
  else if (region(m)%centring == "node") then
    allocate(region(m)%ns(ktotal))
  else
    call error_stop(trim(region(m)%type)//'region '//trim(region(m)%name)//" having description '"// &
      trim(region(m)%location%description)//"' has neither cell, face or node centring")
  end if

  if (.not.(region(m)%type == 'gmsh' .or. region(m)%type == 'system')) then
    if (region(region(m)%part_of)%centring /= region(m)%centring) &
      call error_stop("the "//trim(region(m)%centring)//" of region "//trim(region(m)%name)//" does not match the " &
      //trim(region(region(m)%part_of)%centring)//" centring of its part_of region "//trim(region(region(m)%part_of)%name))
    if (region(region(m)%parent)%centring /= region(m)%centring) &
      call error_stop("the "//trim(region(m)%centring)//" of region "//trim(region(m)%name)//" does not match the " &
      //trim(region(region(m)%parent)%centring)//" centring of its parent region "//trim(region(region(m)%parent)%name))
  end if

! and now perform the setup update of all of the system and gmsh (static) regions, calculating and allocating ijk, and from that, ns
! all gmsh regions must be done before the user regions as the user regions may refer to gmsh ns and ijk indicies
  if (trim(region(m)%type) == "system" .or. trim(region(m)%type) == "gmsh") call update_region(m=m,initial=.false.)

end do

! and now update the static user regions separately
do m = 1, ubound(region,1)
  if (trim(region(m)%type) == "setup") call update_region(m=m,initial=.false.)
end do

! dynamic regions get updated with their corresponding variable

!---------------------
! calculate dimensions for each region, noting that dynamic regions are based on parent static regions

! first do static regions - that is system, setup and gmsh
do m = 1, ubound(region,1)
  if (region(m)%dynamic) cycle
! for gmsh regions the dimensions should already have been set during the file read in
! a negative dimensions means that the region has not been found within a msh file, so indicates an error
  if (trim(region(m)%type) == "gmsh") then
    if (region(m)%dimensions < 0) call error_stop("the gmsh region "//trim(region(m)%name)//" having description '"// &
      trim(region(m)%location%description)//"' has not been found in a msh file, but is needed for the simulation")
  else
    region(m)%dimensions = 0
! check that for user regions part_of and parent centrings are consistent
    if (region(m)%centring == 'cell') then
      do ns = 1, allocatable_size(region(m)%ijk)
        region(m)%dimensions = max(region(m)%dimensions,cell(region(m)%ijk(ns))%dimensions)
      end do
    else if (region(m)%centring == 'face') then
      do ns = 1, allocatable_size(region(m)%ijk)
        region(m)%dimensions = max(region(m)%dimensions,face(region(m)%ijk(ns))%dimensions)
      end do
    else if (region(m)%centring == 'node') then
      region(m)%dimensions = 0
    end if
  end if
  maximum_dimensions = max(maximum_dimensions,region(m)%dimensions) ! set maximum number of dimensions of any region used in the simulation
end do

! now do dynamic based on just-calculated static regions
do m = 1, ubound(region,1)
  if (region(m)%dynamic) region(m)%dimensions = region(region(m)%parent)%dimensions
end do
    
if (debug_sparse) write(*,'(a,i1)') 'INFO: the maximum number of dimensions of any region is ',maximum_dimensions

!---------------------
! write out summary info about the regions
if (debug_sparse) then
  write(*,'(a)') 'INFO: regions:'
  do m=1,ubound(region,1)
    if (region(m)%dynamic) then
      formatline = '(a,'//trim(dindexformat(m))//',a,'//trim(dindexformat(region(m)%dimensions))//',a)'
      write(*,fmt=formatline) ' region_number = ',m,': name = '//trim(region(m)%name)//': dynamic: type = '// &
        trim(region(m)%type)//': centring = '//region(m)%centring//': dimensions = ',region(m)%dimensions, &
        ': location = '//trim(region(m)%location%description)//': initial_location = '// &
        trim(region(m)%initial_location%description)
    else if (allocatable_size(region(m)%ijk) == 0) then
      formatline = '(a,'//trim(dindexformat(m))//',a,'//trim(dindexformat(region(m)%dimensions))//',a)'
      write(*,fmt=formatline) ' region_number = ',m,': name = '//trim(region(m)%name)//': static: type = '// &
        trim(region(m)%type)//': centring = '//region(m)%centring//': dimensions = ',region(m)%dimensions, &
        ': location = '//trim(region(m)%location%description)//': contains no elements'
    else
      formatline = '(a,'//trim(dindexformat(m))//',a,'//trim(dindexformat(region(m)%dimensions))// &
        ',a,'//trim(dindexformat(region(m)%ijk(1)))// &
        ',a,'//trim(dindexformat(allocatable_size(region(m)%ijk)))// &
        ',a,'//trim(dindexformat(region(m)%ijk(allocatable_size(region(m)%ijk))))//')'
      write(*,fmt=formatline) ' region_number = ',m,': name = '//trim(region(m)%name)//': static: type = '// &
        trim(region(m)%type)//': centring = '//region(m)%centring//': dimensions = ',region(m)%dimensions, &
        ': location = '//trim(region(m)%location%description)// &
        ': ijk(1) = ',region(m)%ijk(1),': ijk(',allocatable_size(region(m)%ijk),') = ', &
        region(m)%ijk(allocatable_size(region(m)%ijk))
    end if
  end do
end if
! and some warnings all the time if a region contains no elements
do m=1,ubound(region,1)
  if (region(m)%dynamic) cycle
  if (allocatable_size(region(m)%ijk) == 0) write(*,'(a)') 'WARNING: static region '//trim(region(m)%name)//' contains no elements'
end do

!---------------------
! setup any region links
if (allocated(region_link)) then
  if (debug_sparse) write(*,'(a)') 'INFO: region_links:'
  do m = 1, ubound(region_link,1)
    call setup_region_link(m,debug_sparse)
  end do
end if

!---------------------
if (region_details_file) then
  if (debug_sparse) write(*,*) 'INFO: writing region details to region_details.txt file'

  filename = "output/region_details.txt"
  open(fdetail,file=trim(filename),status='replace',iostat=ierror)
  if (ierror /= 0) call error_stop('problem opening file '//trim(filename))

  write(fdetail,'(a)') 'REGION DETAILS:'
  formatline = '(3(a,'//trim(indexformat)//'))'
  write(fdetail,fmt=formatline) 'NODES: ktotal = ',ktotal,': kdomain = ',kdomain,': kboundary = ',kboundary
  do k = 1,ktotal
    textline = ''
    do m = 1, ubound(region,1)
      if (region(m)%dynamic) cycle
      if (region(m)%centring /= 'node') cycle
      if (region(m)%ns(k) /= 0) then
        formatline = '(a,'//trim(dindexformat(m))//',a)'
        write(textline2,formatline) trim(region(m)%name)//" ",m,"r:"
        textline = trim(textline)//" "//trim(textline2)
      end if
    end do
    write(fdetail,'(a)') 'node: '//trim(print_node(k,compact=.true.))//': regions ='//trim(textline)
  end do
  formatline = '(3(a,'//trim(indexformat)//'))'
  write(fdetail,fmt=formatline) 'FACES: jtotal = ',jtotal,': jdomain = ',jdomain,': jboundary = ',jboundary
  do j = 1,jtotal
    textline = ''
    do m = 1, ubound(region,1)
      if (region(m)%dynamic) cycle
      if (region(m)%centring /= 'face') cycle
      if (region(m)%ns(j) /= 0) then
        formatline = '(a,'//trim(dindexformat(m))//',a)'
        write(textline2,formatline) trim(region(m)%name)//" ",m,"r:"
        textline = trim(textline)//" "//trim(textline2)
      end if
    end do
    write(fdetail,'(a)') 'face: '//trim(print_face(j,compact=.true.))//': regions ='//trim(textline)
  end do
  formatline = '(3(a,'//trim(indexformat)//'))'
  write(fdetail,fmt=formatline) 'CELLS: itotal = ',itotal,': idomain = ',idomain,': iboundary = ',iboundary
  do i = 1,itotal
    textline = ''
    do m = 1, ubound(region,1)
      if (region(m)%dynamic) cycle
      if (region(m)%centring /= 'cell') cycle
      if (region(m)%ns(i) /= 0) then
        formatline = '(a,'//trim(dindexformat(m))//',a)'
        write(textline2,formatline) trim(region(m)%name)//" ",m,"r:"
        textline = trim(textline)//" "//trim(textline2)
      end if
    end do
    write(fdetail,'(a)') 'cell: '//trim(print_cell(i,compact=.true.))//': regions ='//trim(textline)
  end do

  close(fdetail)
end if
!---------------------

if (debug) write(82,'(a/80(1h-))') 'subroutine setup_regions'

end subroutine setup_regions

!-----------------------------------------------------------------

subroutine update_region(m,initial)

! here we setup the regions by finding the i or j indices for each

use general_module
integer :: m ! region number to be updated
logical :: initial ! whether the the initial or normal location string is to be used
type(region_location_type) :: local_location ! set to either initial or normal location
integer :: ierror, i, j, k, n, cut, nregion, ijkregion, nsregion, ns, nscompound, ii, jj, kk, ijk, l, ijktotal
double precision :: tmp, tmpmax
double precision, dimension(totaldimensions) :: x, xmin, xmax ! a single location
character(len=1000) :: name, aregion, region_list, formatline
character(len=100) :: type ! this the local_location type that we are dealing with
character(len=4) :: centring
character(len=1) :: rsign
integer, dimension(:), allocatable :: nregion_list
logical :: existing, in_common, compoundtype, compoundadd
logical, dimension(:), allocatable :: elementisin
logical :: debug_sparse = .true.
logical, parameter :: debug = .false.

if (debug) debug_sparse = .true.
                  
if (debug) write(82,'(80(1h+)/a)') 'subroutine update_region'

if (debug) write(82,*) 'Processing region m = ',m,': name = '//trim(region(m)%name)// &
  ': centring = '//trim(region(m)%centring)//': initial = ',initial,': part_of = ', &
  region(m)%part_of,': parent = ',region(m)%parent

!-----------------------------------------------------------------
if (trim(region(m)%type) == 'system') then

  if (trim(region(m)%name) == '<all cells>') then
    allocate(region(m)%ijk(itotal))
    do n = 1, itotal
      region(m)%ijk(n) = n
    end do
  else if (trim(region(m)%name) == '<domain>') then
    allocate(region(m)%ijk(idomain))
    n = 0
    do i = 1, itotal
      if (cell(i)%type == 1) then
        n = n + 1
        region(m)%ijk(n) = i
      end if
    end do
  else if (trim(region(m)%name) == '<boundary cells>') then
    allocate(region(m)%ijk(iboundary))
    n = 0
    do i = 1, itotal
      if (cell(i)%type == 2) then
        n = n + 1
        region(m)%ijk(n) = i
      end if
    end do
  else if (trim(region(m)%name) == '<all faces>') then
    allocate(region(m)%ijk(jtotal))
    do n = 1, jtotal
      region(m)%ijk(n) = n
    end do
  else if (trim(region(m)%name) == '<domain faces>') then
    allocate(region(m)%ijk(jdomain))
    n = 0
    do j = 1, jtotal
      if (face(j)%type == 1) then
        n = n + 1
        region(m)%ijk(n) = j
      end if
    end do
  else if (trim(region(m)%name) == '<boundaries>') then
    allocate(region(m)%ijk(jboundary))
    n = 0
    do j = 1, jtotal
      if (face(j)%type == 2) then
        n = n + 1
        region(m)%ijk(n) = j
      end if
    end do
  else if (trim(region(m)%name) == '<all nodes>') then
    allocate(region(m)%ijk(ktotal))
    do n = 1, ktotal
      region(m)%ijk(n) = n
    end do
  else if (trim(region(m)%name) == '<domain nodes>') then
    allocate(region(m)%ijk(kdomain))
    n = 0
    do k = 1, ktotal
      if (node(k)%type == 1) then
        n = n + 1
        region(m)%ijk(n) = k
      end if
    end do
  else if (trim(region(m)%name) == '<boundary nodes>') then
    allocate(region(m)%ijk(kboundary))
    n = 0
    do k = 1, ktotal
      if (node(k)%type == 2) then
        n = n + 1
        region(m)%ijk(n) = k
      end if
    end do
  else
    call error_stop("subroutine update_system_region called with incorrect region name "//trim(region(m)%name))
  end if

!-----------------------------------------------------------------
else if (trim(region(m)%type) /= 'gmsh') then
! now deal with all user regions which have locations and/or initial locations to be processed

! note: this may mean that the region is already defined but defining twice won't hurt if the definition is the same

  if (initial) then
    local_location = region(m)%initial_location
    if (.not.local_location%active) call error_stop("region "//trim(region(m)%name)// &
      " is trying to be updated but its initial location it isn't active")
  else
    local_location = region(m)%location
    if (.not.local_location%active) call error_stop("region "//trim(region(m)%name)// &
      " is trying to be updated but its location it isn't active")
  end if

  if (debug) then
    write(82,*) "now processing local_location"
    write(82,*) "type = "//trim(local_location%type)
    write(82,*) "description = "//trim(local_location%description)
    if (allocated(local_location%floats)) write(82,*) "floats = ",local_location%floats
    if (allocated(local_location%integers)) write(82,*) "integers = ",local_location%integers
    if (allocated(local_location%regions)) write(82,*) "regions = ",local_location%regions
  end if

! TODO:
! VARIABLE region
! SEPARATION region

! deallocate any allocated regions, starting afresh each time the region is calculated
! will have to think about this more for separation regions etc
  if (allocated(region(m)%ijk)) then
    if (.not.region(m)%dynamic) write(*,'(a/a)') "NOTE: an "//trim(local_location%type)// &
      " region operator is acting on region "//trim(region(m)%name)// &
      " that already contains elements: the previous element will be overwritten with the new"
! only deallocate specific region types
! NB: a none region will be deallocated here and hence be empty
    if (.not.(trim(local_location%type) == "at")) deallocate(region(m)%ijk)
  end if

! think about sizing of ijk for dynamic elements - maybe allocate once based on parent size? - and then deal with zero ijk elements when used?

! set ijktotal as it is used for most of these location types
  if (region(m)%centring == 'cell') then
    ijktotal = itotal
  else if (region(m)%centring == 'face') then
    ijktotal = jtotal
  else
    ijktotal = ktotal
  end if

!---------------------
! a user defined region from the arb input file that is a single point
! ref: at region

  if (trim(local_location%type) == "at") then

    if (.not.(allocated(region(m)%ijk))) allocate(region(m)%ijk(1))
    region(m)%ijk(1) = 0
    x = local_location%floats ! the perl has ensured that this has exactly 3 elements

    tmpmax = 1.d+20
    do ns=1,allocatable_integer_size(region(region(m)%part_of)%ijk)
      ijk = region(region(m)%part_of)%ijk(ns)
      if (ijk == 0) cycle ! allow for zero elements in ijk
      if (region(m)%centring == "cell") then
        tmp = distance(x,cell(ijk)%x)
      else if (region(m)%centring == "face") then
        tmp = distance(x,face(ijk)%x)
      else
        tmp = distance(x,node(ijk)%x)
      end if
      if (tmp < tmpmax) then
        region(m)%ijk(1) = ijk
        tmpmax = tmp
      end if
    end do

    if (region(m)%ijk(1) == 0) deallocate(region(m)%ijk) ! now insist that region is not allocated if it is empty

!---------------------
! a user defined region from the arb input file that is any elements within a box
! TODO: deal with other geometries

! ref: within box region ref: within region
  else if (trim(local_location%type) == "within box") then

! check that points are in min and max order and otherwise reorder

    xmin = local_location%floats(1:3) ! the perl has ensured that this has exactly 3 elements
    xmax = local_location%floats(4:6) ! the perl has ensured that this has exactly 3 elements
    do l = 1, 3
      if (xmin(l) > xmax(l)) then
        write(*,'(a,i1,a)') 'WARNING: dimension ',l,' of the points that define the BOX geometry in region '// &
          trim(region(m)%name)//' were incorrectly ordered: they should be in the order of the minimum coordinate values '// &
          'in each dimension, followed by the maximum coordinate values in each dimension.'
        write(*,'(2(a,i1,a,g14.6))') ' xmin(',l,') = ',xmin(l),' xmax(',l,') = ',xmax(l)
        write(*,*) 'These values will be swapped.'
        tmp = xmax(l)
        xmax(l) = xmin(l)
        xmin(l) = tmp
      end if
    end do

    allocate(elementisin(ijktotal)) 
    elementisin = .false.

    ijk_loop: do ns = 1, allocatable_integer_size(region(region(m)%part_of)%ijk) ! just set the elements within the part_of region to be true
      ijk = region(region(m)%part_of)%ijk(ns)
      if (ijk == 0) cycle ! allow for zero elements in ijk
      if (region(m)%centring == "cell") then
        x = cell(ijk)%x
      else if (region(m)%centring == "face") then
        x = face(ijk)%x
      else
        x = node(ijk)%x
      end if
      do l = 1, 3
        if ((x(l)-xmin(l))*(xmax(l)-x(l)) < 0.d0) cycle ijk_loop
      end do
      elementisin(ijk) = .true. ! converting this to the region(m)%ijk array is handled later
    end do ijk_loop

!---------------------
! a new region composed of the boundary to another region, or similar type of related domain

  else if (trim(local_location%type) == "boundary of".or.trim(local_location%type) == "domain of".or.trim(local_location%type) == "associated with".or. &
    trim(local_location%type) == "surrounds") then

! check centring of requested related region
    nregion = local_location%regions(1) ! perl only allows one region to be defined
    if (region(nregion)%centring == 'none') &
      call error_stop('incorrect centring for '//trim(local_location%type)//' consitutent region '//trim(region(nregion)%name))

    allocate(elementisin(ijktotal)) 
    elementisin = .false.

    do nsregion = 1,allocatable_integer_size(region(nregion)%ijk)  ! loop through all ijk indices in constituent region
      ijkregion = region(nregion)%ijk(nsregion) ! NB, ijkregion and region(m)%ijk may actually be i or j values depending on centring
      if (ijkregion == 0) cycle ! in case this is empty due to a dynamic region
      if (region(nregion)%centring == 'cell') then
        if (region(m)%centring == 'cell') then
! create cell region from cell region
          do ii = 1, ubound(cell(ijkregion)%jface,1)+1 ! loop around cells that border cells, and itself
            i = cell(ijkregion)%icell(ii)
            if (region(region(m)%part_of)%ns(i) == 0) cycle ! check that element is a member of the part_of region
            if (trim(local_location%type) == "boundary of".and.cell(i)%type /= 2) cycle
            if (trim(local_location%type) == "domain of".and.cell(i)%type /= 1) cycle
!           if (trim(local_location%type) == "surrounds".and.location_in_list(array=region(nregion)%ijk,element=i) /= 0) cycle ! unfortunately ns array has not yet been defined for all other elements
            if (trim(local_location%type) == "surrounds".and.region(nregion)%ns(i) /= 0) cycle ! unfortunately ns array has not yet been defined for all other elements
            if (.not.elementisin(i)) elementisin(i) = .true.
          end do
        else if (region(m)%centring == 'face') then
! create face region from cell region
          do jj = 1, ubound(cell(ijkregion)%jface,1)
            j = cell(ijkregion)%jface(jj)
            if (region(region(m)%part_of)%ns(j) == 0) cycle ! check that element is a member of the part_of region
            if (trim(local_location%type) == "boundary of".and.face(j)%type /= 2) cycle
            if (trim(local_location%type) == "domain of".and.face(j)%type /= 1) cycle
            if (trim(local_location%type) == "surrounds") then
!             if (ijkregion == face(j)%icell(2).and.location_in_list(array=region(nregion)%ijk,element=face(j)%icell(1)) /= 0) cycle
!             if (ijkregion == face(j)%icell(1).and.location_in_list(array=region(nregion)%ijk,element=face(j)%icell(2)) /= 0) cycle
              if (ijkregion == face(j)%icell(2).and.region(nregion)%ns(face(j)%icell(1)) /= 0) cycle
              if (ijkregion == face(j)%icell(1).and.region(nregion)%ns(face(j)%icell(2)) /= 0) cycle
            end if
            if (.not.elementisin(j)) elementisin(j) = .true.
          end do
        else
! create node region from cell region
! surrounds not implemented
          do kk = 1, ubound(cell(ijkregion)%knode,1)
            k = cell(ijkregion)%knode(kk)
            if (region(region(m)%part_of)%ns(k) == 0) cycle ! check that element is a member of the part_of region
            if (trim(local_location%type) == "boundary of".and.node(k)%type /= 2) cycle
            if (trim(local_location%type) == "domain of".and.node(k)%type /= 1) cycle
            if (trim(local_location%type) == "surrounds") &
              call error_stop("surrounds not implemented for constructing node from cell region "//trim(region(nregion)%name))
            if (.not.elementisin(k)) elementisin(k) = .true.
          end do
        end if
      else if (region(nregion)%centring == 'face') then
! create cell region from face region
! boundary of will pick out boundary cells coincident with boundary faces
! surrounds not implemented
        if (region(m)%centring == 'cell') then
          do ii = 1, 2
            i = face(ijkregion)%icell(ii)
            if (region(region(m)%part_of)%ns(i) == 0) cycle ! check that element is a member of the part_of region
            if (trim(local_location%type) == "boundary of".and.cell(i)%type /= 2) cycle
            if (trim(local_location%type) == "domain of".and.cell(i)%type /= 1) cycle
            if (trim(local_location%type) == "surrounds") &
              call error_stop("surrounds not implemented for constructing node from cell region "//trim(region(nregion)%name))
            if (.not.elementisin(i)) elementisin(i) = .true.
          end do
        else if (region(m)%centring == 'face') then
! create face region from face region
! boundary of will pick out faces that are on the boundary
! associated with is nonsense - will just copy region
          j = ijkregion
          if (region(region(m)%part_of)%ns(j) == 0) cycle ! check that element is a member of the part_of region
          if (trim(local_location%type) == "boundary of".and.face(j)%type /= 2) cycle
          if (trim(local_location%type) == "domain of".and.face(j)%type /= 1) cycle
          if (.not.elementisin(j)) elementisin(j) = .true.
        else
! create node region from face region
          do kk = 1, ubound(face(ijkregion)%knode,1)
            k = face(ijkregion)%knode(kk)
            if (region(region(m)%part_of)%ns(k) == 0) cycle ! check that element is a member of the part_of region
            if (trim(local_location%type) == "boundary of".and.node(k)%type /= 2) cycle
            if (trim(local_location%type) == "domain of".and.node(k)%type /= 1) cycle
            if (trim(local_location%type) == "surrounds") &
              call error_stop("surrounds not implemented for constructing node from face region "//trim(region(nregion)%name))
            if (.not.elementisin(k)) elementisin(k) = .true.
          end do
        end if
      else
! TODO: fix this node region stuff
        call error_stop(trim(local_location%type)//" not implemented for constructing node region "//trim(region(nregion)%name))
      end if
    end do

!---------------------
! a region composed of a compound list of other regions, or a region that has all of the listed regions in common

  else if (trim(local_location%type) == "compound" .or. trim(local_location%type) == "common") then

    allocate(elementisin(ijktotal)) 
    elementisin = .false.
    if (trim(local_location%type) == "compound") then
      compoundtype = .true. ! use this for fast lookup later
    else
      compoundtype = .false. 
      do ns = 1, allocatable_integer_size(region(region(m)%part_of)%ijk) ! just set the elements within the part_of region to be true
        ijk = region(region(m)%part_of)%ijk(ns)
        if (ijk == 0) cycle
        elementisin(ijk) = .true.
      end do
    end if

! loop through all regions identified in the list
    do n = 1, allocatable_integer_size(local_location%regions)
      nregion = local_location%regions(n)
      if (compoundtype) then
        compoundadd = .false.
        if (local_location%integers(n) == 1) compoundadd = .true.
      end if

      if (debug) write(82,*) '++ in compound loop processing region = '//trim(region(nregion)%name)//' with integer = ', &
        local_location%integers(n)

! check centring of requested related region is consistent with the parent
      if (region(nregion)%centring /= region(m)%centring) &
        call error_stop('incorrect centring for '//trim(local_location%type)//' consitutent region '//trim(region(nregion)%name)// &
        ' that is being used in region '//trim(region(m)%name))

! now loop through all elements within part_of region

      do ns = 1, allocatable_integer_size(region(region(m)%part_of)%ijk) ! just set the elements within the part_of region to be true
        ijk = region(region(m)%part_of)%ijk(ns)
        if (debug) write(82,*) 'before: ns = ',ns,': ijk = ',ijk,': elementisin(ijk) = ',elementisin(ijk)
        if (ijk == 0) cycle
        if (compoundtype) then
          if (region(nregion)%ns(ijk) == 0) cycle ! if this element is not in the location region then don't do anything - cycle
          if (compoundadd) then
            if (.not.elementisin(ijk)) elementisin(ijk) = .true. ! adding element
          else
            if (elementisin(ijk)) elementisin(ijk) = .false. ! subtracting element
          end if
        else
          if (elementisin(ijk)) then
            if (region(nregion)%ns(ijk) == 0) elementisin(ijk) = .false.
          end if
        end if
        if (debug) write(82,*) 'after: ns = ',ns,': ijk = ',ijk,': elementisin(ijk) = ',elementisin(ijk)
      end do

    end do

!---------------------
! a region composed of all elements

  else if (trim(local_location%type) == "all") then

    n = allocatable_integer_size(region(region(m)%part_of)%ijk)
    if (n > 0) then
      allocate(region(m)%ijk(n))
      region(m)%ijk = region(region(m)%part_of)%ijk
    end if

!---------------------
  else if (trim(local_location%type) /= "none") then ! anything but none is an error

    call error_stop('location type for region '//trim(region(m)%name)//' is not understood: type = '//trim(local_location%type))

  end if

!-----------------------------------------------------------------
end if

if (allocated(elementisin)) then
! now loop through the elementisin list creating a new ijk list from it
  n = 0 ! to avoid multiple calls to push_array allocate the array using needed size
  do ijk = 1, ijktotal
    if (elementisin(ijk)) n = n + 1
  end do
  allocate(region(m)%ijk(n))
  n = 0
  do ijk = 1, ijktotal
    if (elementisin(ijk)) then
      n = n + 1
      region(m)%ijk(n) = ijk
    end if
  end do
  deallocate(elementisin)
end if

! find ns indicies which give the data number corresponding to location i, j or k

region(m)%ns = 0 ! a zero indicates that this region does not include this element
do ns = 1, allocatable_size(region(m)%ijk)
  ijk = region(m)%ijk(ns)
  if (ijk == 0) cycle ! now handle zero ijk
  region(m)%ns(ijk) = ns
end do

if (debug_sparse) then
  if (region(m)%dynamic) then
    formatline = "(a,"//trim(dindexformat(allocatable_integer_size(region(m)%ijk)))//",a)"
    write(*,fmt=formatline) "INFO: updated dynamic "//trim(region(m)%centring)//" region "// &
      trim(region(m)%name)//" which now has ",allocatable_integer_size(region(m)%ijk)," elements"
  end if
end if

! check that each region contains some elements, and that it is allocated (even if zero length)
! ie, ensure that all region%ijk arrays are allocated

if (allocatable_integer_size(region(m)%ijk) == 0) then
  write(*,'(a)') 'WARNING: the region '//trim(region(m)%name)//' contains no elements (none allocated)'
!  if (.not.allocated(region(m)%ijk)) allocate(region(m)%ijk(0)) ! bad practice
!else if (maxval(region(m)%ijk) == 0) then
!  write(*,'(a)') 'WARNING: the region '//trim(region(m)%name)//' contains no elements (all zero actually)'
end if

    
if (debug) then
  write(82,*) '# region = '//trim(region(m)%name)//': centring = '//region(m)%centring
  write(82,*) '# '//ijkstring(region(m)%centring)//', ns'
  do ijk = 1, allocatable_size(region(m)%ns)
    write(82,*) ijk, region(m)%ns(ijk)
  end do
end if

!---------------------

if (debug) write(82,'(a/80(1h-))') 'subroutine update_region'

end subroutine update_region

!-----------------------------------------------------------------

subroutine setup_region_link(m,debug_sparse)

! little subroutine to setup the link between a from_region and to_region
use general_module
! using omp to do the element search
!$ use omp_lib
integer :: m, nsf, nst, n_to, n_from, to_ijk, from_ijk, ierror, from_region_number, to_region_number, &
  from_cell_centred, to_cell_centred
integer, dimension(:), allocatable :: from_ns ! temporary array of ns in from_region from ns in to_region
double precision :: maxdist, maxdist2, dist2
double precision, dimension(totaldimensions) :: from_x, to_x, rel_x ! single locations
character(len=1000) :: formatline, filename
logical :: existing, debug_sparse
logical, parameter :: debug = .false.
                  
if (debug) write(*,'(80(1h+)/a)') 'subroutine setup_region_link'

if (debug) write(*,'(a,i3)') ' finding region_link number ',m

! find to_region
region_link(m)%to_region_number = region_number_from_name(name=region_link(m)%to_region, &
  centring=region_link(m)%to_centring,existing=existing,creatable=.false.)
! check that region exists, that centring is consistent and that region is not dynamic
if (.not.existing) call error_stop("problem with "//trim(region_link(m)%to_centring)// &
  " region "//trim(region_link(m)%to_region)//" which is part of a region link function: "// &
  "region does not exist")
to_region_number = region_link(m)%to_region_number
if (to_region_number == 0) call error_stop("problem with "//trim(region_link(m)%to_centring)// &
  " region "//trim(region_link(m)%to_region)//" which is part of a region link function: "// &
  "region most likely has a centring which is inconsistent with its use")
if (region(to_region_number)%dynamic) call error_stop("problem with "//trim(region_link(m)%to_centring)// &
  " region "//trim(region_link(m)%to_region)//" which is part of a region link function: "// &
  "region is dynamic and this isn't allowed for a region link")
if (allocatable_integer_size(region(to_region_number)%ijk) == 0) call error_stop("problem with "// &
  trim(region_link(m)%to_centring)// &
  " region "//trim(region_link(m)%to_region)//" which is part of a region link function: "// &
  "region contains no elements")

! find from_region
region_link(m)%from_region_number = region_number_from_name(name=region_link(m)%from_region, &
  centring=region_link(m)%from_centring,existing=existing,creatable=.false.)
! check that region exists, that centring is consistent and that region is not dynamic
if (.not.existing) call error_stop("problem with "//trim(region_link(m)%from_centring)// &
  " region "//trim(region_link(m)%from_region)//" which is part of a region link function: "// &
  "region does not exist")
from_region_number = region_link(m)%from_region_number
if (from_region_number == 0) call error_stop("problem with "//trim(region_link(m)%from_centring)// &
  " region "//trim(region_link(m)%from_region)//" which is part of a region link function: "// &
  "region most likely has a centring which is inconsistent with its use")
if (region(region_link(m)%from_region_number)%dynamic) call error_stop("problem with "//trim(region_link(m)%from_centring)// &
  " region "//trim(region_link(m)%from_region)//" which is part of a region link function: "// &
  "region is dynamic and this isn't allowed for a region link")
if (allocatable_integer_size(region(from_region_number)%ijk) == 0) call error_stop("problem with "// &
  trim(region_link(m)%from_centring)// &
  " region "//trim(region_link(m)%from_region)//" which is part of a region link function: "// &
  "region contains no elements")

! now create links - ns index to ns index
if (allocated(region_link(m)%to_ns)) deallocate(region_link(m)%to_ns)
allocate(region_link(m)%to_ns(ubound(region(region_link(m)%from_region_number)%ijk,1)))
region_link(m)%to_ns = 0 ! default value if no link is found

! also create temporary storage of reverse indicies for checking purposes
allocate(from_ns(ubound(region(region_link(m)%to_region_number)%ijk,1)))
from_ns = 0
n_from = 0
n_to = 0

! speed up loop by predefining some stuff
if (region_link(m)%from_centring == 'cell') then
  from_cell_centred = 1
else if (region_link(m)%from_centring == 'face') then
  from_cell_centred = 0
else
  from_cell_centred = 2
end if
if (region_link(m)%to_centring == 'cell') then
  to_cell_centred = 1
else if (region_link(m)%to_centring == 'face') then
  to_cell_centred = 0
else
  to_cell_centred = 2
end if
! as this is an expensive lookup, do in parallel
!$omp parallel do private(nsf,maxdist2,maxdist,from_x,nst,to_x,rel_x,dist2)
do nsf = 1, ubound(region_link(m)%to_ns,1)
  maxdist2 = huge(1.d0) ! use dist and dist2 (the square) concurrently now
  maxdist = sqrt(maxdist2)
  if (from_cell_centred == 1) then
    from_x = cell(region(from_region_number)%ijk(nsf))%x
  else if (from_cell_centred == 0) then
    from_x = face(region(from_region_number)%ijk(nsf))%x
  else
    from_x = node(region(from_region_number)%ijk(nsf))%x
  end if
  do nst = 1, ubound(region(to_region_number)%ijk,1)
    if (to_cell_centred == 1) then
      to_x = cell(region(to_region_number)%ijk(nst))%x
    else if (to_cell_centred == 0) then
      to_x = face(region(to_region_number)%ijk(nst))%x
    else
      to_x = node(region(to_region_number)%ijk(nst))%x
    end if
    rel_x = to_x - from_x
! in the interests of speed, compare each component against maxdist separately as a coarse filter
    if (abs(rel_x(1)) > maxdist) cycle
    if (abs(rel_x(2)) > maxdist) cycle
    if (abs(rel_x(3)) > maxdist) cycle
! now calculate the distance squared and compared this against the stored value
    dist2 = dot_product(rel_x,rel_x)
    if (dist2 < maxdist2) then
      region_link(m)%to_ns(nsf) = nst
! save both the squared distance and actual distance
      maxdist2 = dist2
      maxdist = sqrt(max(dist2,0.d0))
    end if
  end do
end do
!$omp end parallel do

do nsf = 1, ubound(region_link(m)%to_ns,1)
  if (region_link(m)%to_ns(nsf) /= 0) then
    n_from = n_from + 1 ! number of from elements that have links defined
    if (from_ns(region_link(m)%to_ns(nsf)) == 0) n_to = n_to + 1 ! number of to elements that have links defined
    from_ns(region_link(m)%to_ns(nsf)) = nsf
  end if
end do

if (debug_sparse) then
  formatline = '(a,'//trim(dindexformat(m))//',a,'//trim(dindexformat(n_from))//',a,'//trim(dindexformat(n_to))//',a)'
  write(*,fmt=formatline) " region_link ",m," from "//trim(region_link(m)%from_centring)//" region "// &
    trim(region_link(m)%from_region)//" to "//trim(region_link(m)%to_centring)//" region "// &
    trim(region_link(m)%to_region)//" contains ",n_from," forward and ",n_to," reverse links"
end if

! run some checks on linking

! check that each from_region cell has a valid link
if (n_to < ubound(region(region_link(m)%to_region_number)%ijk,1)) then
  formatline = '(a,'//trim(dindexformat(ubound(region(region_link(m)%to_region_number)%ijk,1)-n_to))//',a)'
  write(*,fmt=formatline) '    INFO: ',ubound(region(region_link(m)%to_region_number)%ijk,1)-n_to,' elements in '// &
    trim(region_link(m)%to_centring)//' region '//trim(region_link(m)%to_region)//' do not have links from '// &
    trim(region_link(m)%from_centring)//' region '//trim(region_link(m)%from_region)
end if

! see if there is a unique to_region element for every from_region
if (n_from > n_to) then
  formatline = '(a,'//trim(dindexformat(n_from-n_to))//',a)'
  write(*,fmt=formatline) '    WARNING: there are ',n_from-n_to,' non-unique links to '//trim(region_link(m)%to_region)// &
    ' '//trim(region_link(m)%to_centring)//' elements coming from multiple '//trim(region_link(m)%from_region)// &
    ' '//trim(region_link(m)%from_centring)//' elements - i.e., there is not a ''one-to-one'''// &
    ' correspondance between the linked regions'
end if

! check that each from_region cell has a valid link
if (n_from < ubound(region_link(m)%to_ns,1)) then
  formatline = '(a,'//trim(dindexformat(ubound(region_link(m)%to_ns,1)-n_from))//',a)'
  write(*,fmt=formatline) '    WARNING: ',ubound(region_link(m)%to_ns,1)-n_from,' '//trim(region_link(m)%from_centring)// &
    ' elements in '//trim(region_link(m)%from_region)//' do not have links to '//trim(region_link(m)%to_centring)// &
    ' elements in '//trim(region_link(m)%to_region)
end if

if (link_details_file) then
  if (debug) write(*,*) 'writing region details to region_details.txt file'

  filename = "output/link_details.txt"
  open(fdetail,file=trim(filename),status='replace',iostat=ierror)
  if (ierror /= 0) call error_stop('problem opening file '//trim(filename))

  write(fdetail,'(a)') repeat('*',80)
  write(fdetail,'(a)') "region_link from "//trim(region_link(m)%from_region)//" to "//trim(region_link(m)%to_region)
  write(fdetail,'(/a)') 'FORWARD LINKS'
  do nsf = 1, ubound(region(region_link(m)%from_region_number)%ijk,1)
    from_ijk = region(region_link(m)%from_region_number)%ijk(nsf)
    if (region_link(m)%to_ns(nsf) /= 0) then
      to_ijk = region(region_link(m)%to_region_number)%ijk(region_link(m)%to_ns(nsf))
    else
      to_ijk = 0
    end if
    formatline = '(a,'//trim(dindexformat(nsf))//',a,'//trim(dindexformat(from_ijk))//',a,'//trim(dindexformat(to_ijk))//')'
    write(fdetail,fmt=formatline) '  region link ',nsf,' from ijk = ',from_ijk,' to ijk = ',to_ijk
  end do
  write(fdetail,'(/a)') 'REVERSE LINKS'
  do nst = 1, ubound(region(region_link(m)%to_region_number)%ijk,1)
    to_ijk = region(region_link(m)%to_region_number)%ijk(nst)
    if (from_ns(nst) /= 0) then
      from_ijk = region(region_link(m)%from_region_number)%ijk(from_ns(nst))
    else
      from_ijk = 0
    end if
    formatline = '(a,'//trim(dindexformat(nst))//',a,'//trim(dindexformat(to_ijk))//',a,'//trim(dindexformat(from_ijk))//')'
    write(fdetail,fmt=formatline) '  region link ',nst,' to ijk = ',to_ijk,' from ijk = ',from_ijk
  end do
  write(fdetail,'(/a)') repeat('*',80)

  close(fdetail)
end if

deallocate(from_ns)

if (debug) write(*,'(a/80(1h-))') 'subroutine setup_region_link'

end subroutine setup_region_link

!-----------------------------------------------------------------

end module region_module

!-----------------------------------------------------------------
