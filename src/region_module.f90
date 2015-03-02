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
public setup_regions, setup_region_link ! only subroutine accessible from outside the module

! various setup related options

!-----------------------------------------------------------------
contains

!-----------------------------------------------------------------

subroutine setup_regions

! here we setup the regions by finding the i or j indices for each

use general_module
integer :: ierror, m, i, j, k, n, cut, nregion, ijkregion, nsregion, ns, nscompound, ii, jj, kk, ijk, l, ijktotal, mparent
double precision :: tmp, tmpmax
double precision, dimension(totaldimensions) :: x, xmin, xmax ! a single location
character(len=1000) :: keyword, name, formatline, location, aregion, region_list, filename, geometry
character(len=100) :: type
character(len=4) :: centring
character(len=1) :: rsign
integer, dimension(:), allocatable :: nregion_list
logical :: existing, debug_sparse = .true., in_common
logical, dimension(:), allocatable :: elementisin
logical, parameter :: debug = .false.

if (debug) debug_sparse = .true.
                  
if (debug) write(82,'(80(1h+)/a)') 'subroutine setup_regions'

! loop through all regions finding static ijk indicies and initialising ns arrays for all

do m = 1, ubound(region,1)

! allocate ns arrays for all regions

  if (region(m)%centring == "cell") then
    allocate(region(m)%ns(itotal))
  else if (region(m)%centring == "face") then
    allocate(region(m)%ns(jtotal))
  else if (region(m)%centring == "node") then
    allocate(region(m)%ns(ktotal))
  else
    stop 'ERROR: a region has neither cell, face or node centring'
  end if

  call update_region(m=m,initial=.false.)

end do

!---------------------

! calculate dimensions for each region, noting that dynamic regions are based on parent static regions
do m = 1, ubound(region,1)

  if (region(m)%parent > 0) then ! set to 0 for non-dynamic regions in setup_equations.pl
    mparent = region(m)%parent
  else
    mparent = m
  end if

  region(mparent)%dimensions = 0
  if (region(mparent)%centring == 'cell') then
    do ns = 1, allocatable_size(region(mparent)%ijk)
      region(mparent)%dimensions = max(region(mparent)%dimensions,cell(region(mparent)%ijk(ns))%dimensions)
    end do
  else if (region(mparent)%centring == 'face') then
    do ns = 1, allocatable_size(region(mparent)%ijk)
      region(mparent)%dimensions = max(region(mparent)%dimensions,face(region(mparent)%ijk(ns))%dimensions)
    end do
  else if (region(mparent)%centring == 'node') then
    region(mparent)%dimensions = 0
  end if
  maximum_dimensions = max(maximum_dimensions,region(mparent)%dimensions) ! set maximum number of dimensions of any region used in the simulation

end do
    
if (debug_sparse) write(*,'(a,i1)') 'INFO: the maximum number of dimensions of any region is ',maximum_dimensions

!---------------------
! write out summary info about the regions
if (debug_sparse) write(*,'(a)') 'INFO: regions:'
do m=1,ubound(region,1)
  if (allocatable_size(region(m)%ijk) == 0) then
    formatline = '(a,'//trim(dindexformat(m))//',a)'
    if (debug_sparse) write(*,fmt=formatline) ' region_number = ',m,': name = '//trim(region(m)%name)//': location = '// &
      trim(region(m)%location)//': centring = '//region(m)%centring//': contains no elements'
  else
    formatline = '(a,'//trim(dindexformat(m))//',a,'//trim(dindexformat(region(m)%dimensions))// &
      ',a,'//trim(dindexformat(region(m)%ijk(1)))// &
      ',a,'//trim(dindexformat(allocatable_size(region(m)%ijk)))// &
      ',a,'//trim(dindexformat(region(m)%ijk(allocatable_size(region(m)%ijk))))//')'
    if (debug_sparse) write(*,fmt=formatline) ' region_number = ',m,': name = '//trim(region(m)%name)//': location = '// &
      trim(region(m)%location)//': centring = '//region(m)%centring//': dimensions = ',region(m)%dimensions, &
      ': ijk(1) = ',region(m)%ijk(1),': ijk(',allocatable_size(region(m)%ijk),') = ', &
      region(m)%ijk(allocatable_size(region(m)%ijk))
  end if
end do
! and some warnings all the time if a region contains no elements
do m=1,ubound(region,1)
  if (allocatable_size(region(m)%ijk) == 0) write(*,'(a)') 'WARNING: region '//trim(region(m)%name)//' contains no elements'
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
character(len=1000) :: keyword, name, formatline, location, aregion, region_list, filename, geometry
character(len=100) :: type
character(len=4) :: centring
character(len=1) :: rsign
integer, dimension(:), allocatable :: nregion_list
logical :: existing, debug_sparse = .true., in_common
logical, dimension(:), allocatable :: elementisin
logical, parameter :: debug = .false.

if (debug) debug_sparse = .true.
                  
if (debug) write(82,'(80(1h+)/a)') 'subroutine update_region'

if (debug) write(82,*) 'Processing region m = ',m,': name = '//trim(region(m)%name)// &
  ': centring = '//trim(region(m)%centring)//': initial = ',initial

!-----------------------------------------------------------------
if (trim(type) == 'system') then

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
else if (trim(type) == 'static') then

  if (initial) then
    local_location = region(m)%initial_location
    if (.not.local_location%active) call error_stop("region "//trim(region(m)%name)// &
      " is trying to be updated but its initial location it isn't active")
  else
    local_location = region(m)%location
    if (.not.local_location%active) call error_stop("region "//trim(region(m)%name)// &
      " is trying to be updated but its location it isn't active")
  end if
  location = local_location%description ! now work on this local version of the description

  ! define the keyword which in this context means the type of region creation method
  keyword = 'UNKNOWN'
  if (location(1:2) == "AT") then
    keyword = 'AT'
  else if (location(1:6) == "WITHIN") then
    keyword = 'WITHIN'
  else if (location(1:11) == "BOUNDARY OF") then
    keyword = 'BOUNDARY OF'
  else if (location(1:9) == "DOMAIN OF") then
    keyword = 'DOMAIN OF'
  else if (location(1:15) == "ASSOCIATED WITH") then
    keyword = 'ASSOCIATED WITH'
  else if (location(1:9) == "SURROUNDS") then
    keyword = 'SURROUNDS'
  else if (location(1:8) == "COMPOUND") then
    keyword = 'COMPOUND'
  else if (location(1:6) == "COMMON") then
    keyword = 'COMMON'
  else if (location(1:13) == "INTERSECTION") then
    keyword = 'INTERSECTION'
  else if (location(1:5) == "UNION") then
    keyword = 'UNION'
  end if

  !---------------------
  ! a user defined region from the arb input file that is a single point
  ! ref: AT region

  if (trim(keyword) == "AT") then

  ! if region is already defined then it is overwritten
    if (allocated(region(m)%ijk)) then
      write(*,'(a/a)') &
        "NOTE: an AT region operator is acting on region "//trim(region(m)%name)// &
        " that already contains an element:", &
        " the previous element will be overwritten with the new"
    else
      allocate(region(m)%ijk(1))
    end if
    region(m)%ijk(1) = 0

    read(location(3:1000),*,iostat=ierror) x
    if (ierror /= 0) call error_stop('AT location for region '//trim(region(m)%name)//' is not understood as a single point')

  ! now see whether definition is limited to another region via the PART OF statement
    n = scanstring(location,'PART OF')
    if (n > 0 .and. n < 993) then ! a PART OF region name should follow
      aregion = adjustl(location(n+7:1000)) ! assignment in fortran implies padding with blanks
      n = scan(aregion,'<')
      cut = scan(aregion,'>')
      if (n < 1.or.cut <= n) call error_stop('the PART OF region name for region '//trim(region(m)%name)// &
        ' could not be read from the following definition '//trim(location))
      aregion = aregion(n:cut)
    else if (trim(region(m)%centring) == 'cell') then
      aregion = '<all cells>'
    else if (trim(region(m)%centring) == 'face') then
      aregion = '<all faces>'
    else
      aregion = '<all nodes>'
    end if
    nregion = region_number_from_name(name=aregion,centring=region(m)%centring,existing=existing,creatable=.false.)
  ! check that region exists and that centring is consistent
    if (.not.existing) call error_stop("region "//trim(aregion)//" which is the PART OF part of an AT region "// &
      trim(region(m)%name)//" is not found")
    if (nregion == 0) call error_stop("problem with the PART OF region "//trim(aregion)//" in AT region "//trim(region(m)%name)// &
      " definition:- regions most likely the regions have different centrings")

    tmpmax = 1.d+20
    do ns=1,allocatable_integer_size(region(nregion)%ijk)
      ijk = region(nregion)%ijk(ns)
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

    if (region(m)%ijk(1) == 0) then
      write(*,'(a)') 'WARNING: no elements were found within the AT '//trim(geometry)// &
        ' region of '//trim(region(m)%name)
      deallocate(region(m)%ijk) ! allocate this as zero length later
    end if

  !---------------------
  ! a user defined region from the arb input file that is any elements within a geometry 
  ! TODO: integrate WITHIN and AT statements to give PART OF functionality to former

  ! ref: WITHIN region
  else if (trim(keyword) == "WITHIN") then

  ! if region is already defined then it is overwritten
    if (allocated(region(m)%ijk)) then
      write(*,'(a/a)') &
        "NOTE: a WITHIN region operator is acting on region "//trim(region(m)%name)// &
        " that already contains an element:", &
        " the previous element will be overwritten with the new"
      deallocate(region(m)%ijk)
    end if

  ! find the geometry type
    location = adjustl(location(8:1000))
    cut = scan(location,' ')
    read(location(1:cut-1),*,iostat=ierror) geometry
    if (ierror /= 0) call error_stop('WITHIN geometry type for region '//trim(region(m)%name)//' is not understood')
    location = adjustl(location(cut+1:1000))

    if (trim(geometry) == 'BOX') then
      if (debug) write(82,*) 'found WITHIN BOX geometry with location points: '//trim(location)

      read(location,*,iostat=ierror) xmin, xmax
      if (ierror /= 0) call error_stop('WITHIN BOX location for region '//trim(region(m)%name)// &
        ' is not understood as two three dimensional points')

  ! check that points are in min and max order and otherwise reorder
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

      if (region(m)%centring == "cell") then
        ijktotal = itotal
      else if (region(m)%centring == "face") then
        ijktotal = jtotal
      else
        ijktotal = ktotal
      end if

      ijk_loop: do ijk=1,ijktotal
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
        call push_integer_array(array=region(m)%ijk,new_element=ijk)
      end do ijk_loop

    else
      call error_stop('WITHIN geometry type of '//trim(geometry)//' for region '//trim(region(m)%name)//' is not understood')
    end if

  !   if (.not.allocated(region(m)%ijk)) write(*,'(a)') 'WARNING: no elements were found within the WITHIN '//trim(geometry)// &
  !     ' region of '//trim(region(m)%name)
    if (allocatable_integer_size(region(m)%ijk) == 0) write(*,'(a)') &
      'WARNING: no elements were found within the WITHIN '//trim(geometry)// &
      ' region of '//trim(region(m)%name)

  !---------------------
  ! a new region composed of the boundary to another region, or similar type of related domain

  else if (trim(keyword) == "BOUNDARY OF".or.trim(keyword) == "DOMAIN OF".or.trim(keyword) == "ASSOCIATED WITH".or. &
    trim(keyword) == "SURROUNDS") then

  ! note: this may mean that the region is already defined but defining twice won't hurt if the definition is the same
    if (allocated(region(m)%ijk)) then
      write(*,'(a/a)') &
        "NOTE: a "//trim(keyword)//" region operator is acting on region "//trim(region(m)%name)// &
        " that already contains some elements:", &
        " the previous elements will be overwritten with the new"
      deallocate(region(m)%ijk)
    end if

  ! check centring of requested region
    if (region(m)%centring /= 'face'.and.region(m)%centring /= 'cell'.and.region(m)%centring /= 'node') call &
      error_stop('incorrect centring for requested '//trim(keyword)//' region '//trim(region(m)%name))

  ! find constitutent region around which to find boundary
    aregion = adjustl(trim(location(len(trim(keyword))+1:1000)))
    
    nregion = region_number_from_name(name=aregion,existing=existing,creatable=.false.)
  ! check that region exists and that centring is consistent
    if (.not.existing) call error_stop("region "//trim(aregion)//" which is specified in "//trim(keyword)//" operator for "// &
      trim(region(m)%name)//" is not found")
    if (nregion == 0) call error_stop("problem with region "//trim(aregion)//" in "//trim(keyword)//" region "// &
      trim(region(m)%name))
    if (region(nregion)%centring /= 'face'.and.region(nregion)%centring /= 'cell'.and.region(nregion)%centring /= 'node') &
      call error_stop('incorrect centring for '//trim(keyword)//' consitutent region '//trim(region(nregion)%name))

  ! store whether ijk index is included in the new region in temporary array now for faster lookups
    if (region(m)%centring == 'cell') then
      ijktotal = itotal
    else if (region(m)%centring == 'face') then
      ijktotal = jtotal
    else
      ijktotal = ktotal
    end if
    allocate(elementisin(ijktotal)) 
    elementisin = .false.

    do nsregion = 1,allocatable_integer_size(region(nregion)%ijk)  ! loop through all ijk indices in constituent region
      ijkregion = region(nregion)%ijk(nsregion) ! NB, ijkregion and region(m)%ijk may actually be i or j values depending on centring
      if (region(nregion)%centring == 'cell') then
        if (region(m)%centring == 'cell') then
  ! create cell region from cell region
          do ii = 1, ubound(cell(ijkregion)%jface,1)+1 ! loop around cells that border cells, and itself
            i = cell(ijkregion)%icell(ii)
            if (trim(keyword) == "BOUNDARY OF".and.cell(i)%type /= 2) cycle
            if (trim(keyword) == "DOMAIN OF".and.cell(i)%type /= 1) cycle
            if (trim(keyword) == "SURROUNDS".and.location_in_list(array=region(nregion)%ijk,element=i) /= 0) cycle ! unfortunately ns array has not yet been defined for all other elements
            if (.not.elementisin(i)) elementisin(i) = .true.
          end do
        else if (region(m)%centring == 'face') then
  ! create face region from cell region
          do jj = 1, ubound(cell(ijkregion)%jface,1)
            j = cell(ijkregion)%jface(jj)
            if (trim(keyword) == "BOUNDARY OF".and.face(j)%type /= 2) cycle
            if (trim(keyword) == "DOMAIN OF".and.face(j)%type /= 1) cycle
            if (trim(keyword) == "SURROUNDS") then
              if (ijkregion == face(j)%icell(2).and.location_in_list(array=region(nregion)%ijk,element=face(j)%icell(1)) /= 0) cycle
              if (ijkregion == face(j)%icell(1).and.location_in_list(array=region(nregion)%ijk,element=face(j)%icell(2)) /= 0) cycle
            end if
            if (.not.elementisin(j)) elementisin(j) = .true.
          end do
        else
  ! create node region from cell region
  ! SURROUNDS not implemented
          do kk = 1, ubound(cell(ijkregion)%knode,1)
            k = cell(ijkregion)%knode(kk)
            if (trim(keyword) == "BOUNDARY OF".and.node(k)%type /= 2) cycle
            if (trim(keyword) == "DOMAIN OF".and.node(k)%type /= 1) cycle
            if (trim(keyword) == "SURROUNDS") &
              call error_stop("SURROUNDS not implemented for constructing node from cell region "//trim(region(nregion)%name))
            if (.not.elementisin(k)) elementisin(k) = .true.
          end do
        end if
      else if (region(nregion)%centring == 'face') then
  ! create cell region from face region
  ! BOUNDARY OF will pick out boundary cells coincident with boundary faces
  ! SURROUNDS not implemented
        if (region(m)%centring == 'cell') then
          do ii = 1, 2
            i = face(ijkregion)%icell(ii)
            if (trim(keyword) == "BOUNDARY OF".and.cell(i)%type /= 2) cycle
            if (trim(keyword) == "DOMAIN OF".and.cell(i)%type /= 1) cycle
            if (trim(keyword) == "SURROUNDS") &
              call error_stop("SURROUNDS not implemented for constructing node from cell region "//trim(region(nregion)%name))
            if (.not.elementisin(i)) elementisin(i) = .true.
          end do
        else if (region(m)%centring == 'face') then
  ! create face region from face region
  ! BOUNDARY OF will pick out faces that are on the boundary
  ! ASSOCIATED WITH is nonsense - will just copy region
          j = ijkregion
          if (trim(keyword) == "BOUNDARY OF".and.face(j)%type /= 2) cycle
          if (trim(keyword) == "DOMAIN OF".and.face(j)%type /= 1) cycle
          if (.not.elementisin(j)) elementisin(j) = .true.
        else
  ! create node region from face region
          do kk = 1, ubound(face(ijkregion)%knode,1)
            k = face(ijkregion)%knode(kk)
            if (trim(keyword) == "BOUNDARY OF".and.node(k)%type /= 2) cycle
            if (trim(keyword) == "DOMAIN OF".and.node(k)%type /= 1) cycle
            if (trim(keyword) == "SURROUNDS") &
              call error_stop("SURROUNDS not implemented for constructing node from face region "//trim(region(nregion)%name))
            if (.not.elementisin(k)) elementisin(k) = .true.
          end do
        end if
      else
        call error_stop(trim(keyword)//" not implemented for constructing node region "//trim(region(nregion)%name))
      end if
    end do

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

  !---------------------
  ! a new region composed of a compound list of other regions

  else if (trim(keyword) == "COMPOUND" .or. trim(keyword) == "UNION") then

  ! note: this may mean that the region is already defined but defining twice won't hurt if the definition is the same
    if (allocated(region(m)%ijk)) then
      write(*,'(a/a)') &
        "NOTE: a "//trim(keyword)//" region operator is acting on region "//trim(region(m)%name)// &
        " that already contains some elements:", &
        " the previous elements will be overwritten with the new"
      deallocate(region(m)%ijk)
    end if

    region_list = trim(location(9:1000))

  ! loop through all regions
    do while (len_trim(region_list) /= 0) 

      region_list = adjustl(region_list(1:len_trim(region_list))) ! remove leading spaces

      if (region_list(1:1) == "+" .or. region_list(1:1) == "-") then
        rsign = region_list(1:1)
        region_list = adjustl(region_list(2:len_trim(region_list)))
      else
        rsign = "+"
      end if

      cut = scan(region_list,">")
      if (region_list(1:1) /= "<" .or. cut <=1 ) call &
        error_stop("format for equation region incorrect in "//trim(keyword)//" operator list "//trim(region_list))

      aregion = region_list(1:cut)
      region_list = adjustl(region_list(cut+1:len_trim(region_list)))

      nregion = region_number_from_name(name=aregion,centring=region(m)%centring,existing=existing,creatable=.false.)
  ! check that region exists and that centring is consistent
      if (.not.existing) call error_stop("region "//trim(aregion)//" which is part of "//trim(keyword)//" region "// &
        trim(region(m)%name)//" is not found")
      if (nregion == 0) call error_stop("problem with region "//trim(aregion)//" in "//trim(keyword)//" region "// &
        trim(region(m)%name)//":- regions most likely have different centrings")
      if (nregion == m) call error_stop("problem with region "//trim(aregion)//" in "//trim(keyword)//" region "// &
        trim(region(m)%name)//":- cannot reference the region being created")

  ! loop through all existing ijk indices in region
  ! - if indice exists and we are adding, ignore, otherwise do
  ! - if indice exists and we are subtracting, do, otherwise ignore

  !     if (.not.allocated(region(nregion)%ijk)) call error_stop("region ijk indices not allocated in "//trim(keyword)//" operator for region "// &
  !       trim(region(nregion)%name))
      if (allocatable_integer_size(region(nregion)%ijk) == 0) then
        write(*,'(a)') "WARNING: region "//trim(region(nregion)%name)//" that is used in the "//trim(keyword)// &
        " region statement for region "//trim(region(m)%name)//" contains no elements"
        cycle
      end if
        
      do nsregion = 1,ubound(region(nregion)%ijk,1)  ! loop through all ijk indices in constituent region
        ijkregion = region(nregion)%ijk(nsregion) ! NB, ijkregion and region(m)%ijk may actually be i or j values depending on centring
        
        nscompound = 0 ! this is the position of iregion in the compound region's ijk indices
        do ns = 1,allocatable_size(region(m)%ijk)
          if (region(m)%ijk(ns) == ijkregion) then
            nscompound = ns ! region indice has been found in compound region's ijk indices
            exit
          end if
        end do

        if (rsign == "+" .and. nscompound == 0) then
          call push_integer_array(array=region(m)%ijk,new_element=ijkregion) ! add region location to equation ijk indices
        else if (rsign == "-" .and. nscompound /= 0) then
          if (nscompound /= ubound(region(m)%ijk,1)) then ! unless it is the last element of indicies
            region(m)%ijk(nscompound:ubound(region(m)%ijk,1)-1) = & ! shift indicies one space to the left to remove reference
              region(m)%ijk(nscompound+1:ubound(region(m)%ijk,1))
          end if
          call resize_integer_array(array=region(m)%ijk,change=-1) ! reduce ijk array by 1
        end if

      end do

    end do

  !---------------------
  ! a new region composed of the common elements from a list of space or comma separated regions

  else if (trim(keyword) == "COMMON" .or. trim(keyword) == "INTERSECTION") then

  ! note: this may mean that the region is already defined but defining twice won't hurt if the definition is the same
    if (allocated(region(m)%ijk)) then
      write(*,'(a/a)') &
        "NOTE: a "//trim(keyword)//" region operator is acting on region "//trim(region(m)%name)// &
        " that already contains some elements:", &
        " the previous elements will be overwritten with the new"
      deallocate(region(m)%ijk)
    end if

    region_list = trim(location(7:1000))
    if (debug) write(*,*) trim(keyword)//': initial region_list = |'//trim(region_list)//'|'

  ! loop through all regions creating a list of the regions that we are going to look for common elements in

    if (allocated(nregion_list)) deallocate(nregion_list)
    do while (len_trim(region_list) /= 0) 

      region_list = adjustl(region_list(1:len_trim(region_list))) ! remove leading spaces
      if (debug) write(*,*) trim(keyword)//': looping region_list = |'//trim(region_list)//'|'
      if (region_list(1:1) == "," .or. region_list(1:1) == '+') then
        region_list(1:1) = " " ! remove commas or plus signs silently by blanking them out - in the interests of stopping bugs, a minus sign will flag an error
        cycle
      end if

      cut = scan(region_list,">")
      if (debug) write(*,*) 'cut = ',cut,': region_list(1:cut) = '//trim(region_list(1:cut))
      if (region_list(1:1) /= "<" .or. cut <=1 ) call &
        error_stop("format for equation region incorrect in "//trim(keyword)//" operator list "//trim(region_list))

      aregion = region_list(1:cut)
      region_list = adjustl(region_list(cut+1:len_trim(region_list)))

      nregion = region_number_from_name(name=aregion,centring=region(m)%centring,existing=existing,creatable=.false.)
  ! check that region exists and that centring is consistent
      if (.not.existing) call error_stop("region "//trim(aregion)//" which is part of "//trim(keyword)//" region "// &
        trim(region(m)%name)//" is not found")
      if (nregion == 0) call error_stop("problem with region "//trim(aregion)//" in "//trim(keyword)//" region "// &
        trim(region(m)%name)//":- regions most likely have different centrings")
      if (nregion == m) call error_stop("problem with region "//trim(aregion)//" in "//trim(keyword)//" region "// &
        trim(region(m)%name)//":- cannot reference the region being created")

      call push_integer_array(array=nregion_list,new_element=nregion) ! add nregion to the list

    end do

  ! first check that all the regions have elements, otherwise they will have no elements in common

    do nregion = 1, allocatable_integer_size(nregion_list)
      if (allocatable_integer_size(region(nregion)%ijk) == 0) then
        write(*,'(a)') "WARNING: region "//trim(region(nregion)%name)//" that is used in the "//trim(keyword)// &
        " region statement for region "//trim(region(m)%name)//" contains no elements"
        deallocate(nregion_list) ! this will kill all further processing of this COMMON statements
        exit
      end if
    end do

  ! now run through each of the elements in the first region, looking for commonality with the elements of the other regions

    do nsregion = 1,ubound(region(nregion_list(1))%ijk,1)  ! loop through all ijk indices in the first region
      ijkregion = region(nregion_list(1))%ijk(nsregion) ! NB, ijkregion and region(m)%ijk may actually be i, j or k values depending on centring

      in_common = .true.
      do nregion = 2, allocatable_integer_size(nregion_list)
        if (location_in_list(array=region(nregion_list(nregion))%ijk,element=ijkregion) == 0) then
          in_common = .false.
          cycle
        end if
      end do

      if (in_common) call push_integer_array(array=region(m)%ijk,new_element=ijkregion) ! add region location to equation ijk indices

    end do

  else

    call error_stop('location for region '//trim(region(m)%name)//' is not understood: location = '//trim(location))

  end if

!-----------------------------------------------------------------
! gmsh and dynamic regions are not considered here
end if

! check that each region contains some elements, and that it is allocated (even if zero length)
! ie, ensure that all region%ijk arrays are allocated

if (allocatable_integer_size(region(m)%ijk) == 0) then
  write(*,'(a)') 'WARNING: the region '//trim(region(m)%name)//' contains no elements'
  if (.not.allocated(region(m)%ijk)) allocate(region(m)%ijk(0))
end if

! find ns indicies which give the data number corresponding to location i, j or k

region(m)%ns = 0 ! a zero indicates that this region does not include this element
do ns = 1, allocatable_size(region(m)%ijk)
  region(m)%ns(region(m)%ijk(ns)) = ns
end do
    
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
! check that region exists and that centring is consistent
if (.not.existing) then
  write(*,'(a)') "ERROR: "//trim(region_link(m)%to_centring)//" region "//trim(region_link(m)%to_region)// &
    " which is part of a region link function does not exist"
  stop
end if
if (region_link(m)%to_region_number == 0) then
  write(*,'(a)') "ERROR: problem with "//trim(region_link(m)%to_centring)//" region "//trim(region_link(m)%to_region)// &
    " which is part of a region link function:- region most likely has a centring which is inconsistent with its use"
  stop
end if

! find from_region
region_link(m)%from_region_number = region_number_from_name(name=region_link(m)%from_region, &
  centring=region_link(m)%from_centring,existing=existing,creatable=.false.)
! check that region exists and that centring is consistent
if (.not.existing) then
  write(*,'(a)') "ERROR: "//trim(region_link(m)%from_centring)//" region "//trim(region_link(m)%from_region)// &
    " which is part of a region link function does not exist"
  stop
end if
if (region_link(m)%from_region_number == 0) then
  write(*,'(a)') "ERROR: problem with "//trim(region_link(m)%from_centring)//" region "//trim(region_link(m)%to_region)// &
    " which is part of a region link function:- region most likely has a centring which is inconsistent with its use"
  stop
end if

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
from_region_number = region_link(m)%from_region_number
to_region_number = region_link(m)%to_region_number
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
