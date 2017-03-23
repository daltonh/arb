! file src/region_module.f90
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
use equation_module
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
  region(m)%ns = 0 ! default for all regions prior to being updated - specifically ensures that prior to being calculated, dynamic regions contain no elements (based on ns array)
  if (.not.allocated(region(m)%ijk)) allocate(region(m)%ijk(0)) ! allocate to zero size if array hasn't already been allocated, just to cover bases w.r.t. initial use of dynamic regions - note, code should be using allocatable_integer_size on ijk everywhere, but do this as insurance against wayward ubound calls

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

  filename = trim(output_dir)//"region_details.txt"
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
logical :: debug_sparse
logical, parameter :: debug = .false.
                  
if (debug) write(*,'(80(1h+)/a)') 'subroutine setup_region_link'

if (debug) write(*,'(a,i3)') ' finding region_link number ',m

! find to_region
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

  filename = trim(output_dir)//"link_details.txt"
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
