! file src/gmesh_module.f90
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
module gmesh_module

! within the context of arb, a gmesh is a mesh that is contained within one .msh file,
!  as written by either the program gmsh or a previous run of arb
! all mesh input and output is done within the context of these gmeshes
! this module contains routines that setup and read in these gmeshes

! statements specifying different data types and parameters
implicit none

! these are the only subroutines that are accessible from outside this module
private
public setup_gmesh_elements, finalise_gmesh, push_gmesh, read_gmesh

type node_list_type
  integer, dimension(:), allocatable :: node
end type node_list_type
  
! this data structure holds info about how elements, faces and nodes are defined in gmsh
type gtype_list_type
  character(len=100) :: name
  integer :: dimensions ! 0 for point, 1 for line, 2 for plane, 3 for volume
  integer :: nnodes ! number of surrounding nodes
  integer :: nfaces ! number of surrounding faces
  type(node_list_type), dimension(:), allocatable :: face_nodes ! lists the node indices of each of the nfaces faces
  logical :: supported ! suitable for arb?
  integer :: vtk_type ! vtk type of this element, or 0 for not supported
  integer, dimension(:), allocatable :: vtk_nodes ! a list of the nodes numbers of the element as they should be output for vtk
  integer, dimension(3,8) :: dat_nodes ! a list of the nodes numbers of the element as they should be output for dat - first index is dimension required, second is list of nodes
end type gtype_list_type

! reference list of all the gtypes
type(gtype_list_type), public, dimension(:), allocatable, save :: gtype_list

! info about each of the gelements
type gelement_type
  integer :: icell ! corresponding icell, 0 if none
  integer :: jface ! corresponding jface, 0 if none
  integer :: knode ! corresponding knode, 0 if none
  integer, dimension(:), allocatable :: gregions ! list of gregions that it is a member of, or failing that, one entry of 0
end type gelement_type

! info about each of the gregions
type gregion_type
  integer :: region_number
  character(len=4) :: centring
end type gregion_type

! type for each gmsh that is read in or output
type gmesh_type
  character(len=1000) :: filename
  character(len=1000) :: basename
  character(len=100), dimension(:), allocatable :: options ! array of options for this gmesh
  integer :: dimensions
  integer :: ngnodes ! number of gnodes
  integer, dimension(:), allocatable :: knode_from_gnode ! 0 if undefined
  integer, dimension(:), allocatable :: gnode_from_knode ! 0 if undefined
  integer :: ngelements ! number of gelements, including repeats for multiple regions
  integer :: ngelements_face ! number of face gelements, including repeats for multiple regions
  integer :: ngelements_cell ! number of cell gelements, including repeats for multiple regions
  integer :: ngelements_node ! number of node gelements, including repeats for multiple regions
  type(gelement_type), dimension(:), allocatable :: gelement ! list of gelements
  integer :: ngregions ! number of gregions
  type(gregion_type), dimension(:), allocatable :: gregion ! mapping from gregion number to arb region number, dimension gregion_max
  double precision :: input_scale=1.d0, output_scale=1.d0 ! scale factors used when reading in and writing out the mesh (nodes)
end type gmesh_type

! gmsh meshes
type(gmesh_type), public, dimension(:), allocatable, save :: gmesh

! some gmsh node parameters
integer, parameter, public :: node_gtype = 15 ! save the node gtype for safekeeping
integer, parameter, public :: line_gtype = 1 ! save the line gtype for safekeeping

!-----------------------------------------------------------------
contains

!-----------------------------------------------------------------

subroutine setup_gmesh_elements

! here we allocate data to the reference gtype_list array

integer :: n
logical, parameter :: debug = .false.

if (debug) write(*,'(80(1h+)/a)') 'subroutine setup_gmesh_elements'

allocate(gtype_list(31))

! default is that element is unsupported
do n=1,ubound(gtype_list,1)
  gtype_list(n)%name = "unknown"
  gtype_list(n)%supported = .false.
  gtype_list(n)%dat_nodes = 0
end do

! now set supported elements
! faces are definined in the context of the element having the same dimensions as the problem
! ie, as a cell
! if the element has a dimension that is less than the problem dimension, it is assumed to be a
! face (dimension-1)
! surface nodes are directed using the right-hand rule with a positive outward pointing normal

n = 15
gtype_list(n)%name = "1-node point"
gtype_list(n)%dimensions = 0
gtype_list(n)%nnodes = 1
gtype_list(n)%nfaces = 0
gtype_list(n)%supported = .true.
gtype_list(n)%vtk_type = 1
allocate(gtype_list(n)%vtk_nodes(gtype_list(n)%nnodes))
gtype_list(n)%vtk_nodes = [ 1 ]
gtype_list(n)%dat_nodes(1,:) = [ 1, 1, 0, 0, 0, 0, 0, 0 ]
gtype_list(n)%dat_nodes(2,:) = [ 1, 1, 1, 1, 0, 0, 0, 0 ]
gtype_list(n)%dat_nodes(3,:) = [ 1, 1, 1, 1, 1, 1, 1, 1 ]

n = 1
gtype_list(n)%name = "2-node line"
gtype_list(n)%dimensions = 1
gtype_list(n)%nnodes = 2
gtype_list(n)%nfaces = 2
allocate(gtype_list(n)%face_nodes(gtype_list(n)%nfaces))
allocate(gtype_list(n)%face_nodes(1)%node(1))
gtype_list(n)%face_nodes(1)%node = [ 1 ]
allocate(gtype_list(n)%face_nodes(2)%node(1))
gtype_list(n)%face_nodes(2)%node = [ 2 ]
gtype_list(n)%supported = .true.
gtype_list(n)%vtk_type = 3
allocate(gtype_list(n)%vtk_nodes(gtype_list(n)%nnodes))
gtype_list(n)%vtk_nodes = [ 1, 2 ]
gtype_list(n)%dat_nodes(1,:) = [ 1, 2, 0, 0, 0, 0, 0, 0 ]
gtype_list(n)%dat_nodes(2,:) = [ 1, 2, 2, 2, 0, 0, 0, 0 ]
gtype_list(n)%dat_nodes(3,:) = [ 1, 2, 2, 2, 2, 2, 2, 2 ]

n = 2
gtype_list(n)%name = "3-node triangle"
gtype_list(n)%dimensions = 2
gtype_list(n)%nnodes = 3
gtype_list(n)%nfaces = 3
allocate(gtype_list(n)%face_nodes(gtype_list(n)%nfaces))
allocate(gtype_list(n)%face_nodes(1)%node(2))
gtype_list(n)%face_nodes(1)%node = [ 1, 2 ]
allocate(gtype_list(n)%face_nodes(2)%node(2))
gtype_list(n)%face_nodes(2)%node = [ 2, 3 ]
allocate(gtype_list(n)%face_nodes(3)%node(2))
gtype_list(n)%face_nodes(3)%node = [ 3, 1 ]
gtype_list(n)%supported = .true.
gtype_list(n)%vtk_type = 5
allocate(gtype_list(n)%vtk_nodes(gtype_list(n)%nnodes))
gtype_list(n)%vtk_nodes = [ 1, 2, 3 ]
gtype_list(n)%dat_nodes(2,:) = [ 1, 2, 3, 3, 0, 0, 0, 0 ]
gtype_list(n)%dat_nodes(3,:) = [ 1, 2, 3, 3, 3, 3, 3, 3 ]

n = 3
gtype_list(n)%name = "4-node quadrangle"
gtype_list(n)%dimensions = 2
gtype_list(n)%nnodes = 4
gtype_list(n)%nfaces = 4
allocate(gtype_list(n)%face_nodes(gtype_list(n)%nfaces))
allocate(gtype_list(n)%face_nodes(1)%node(2))
gtype_list(n)%face_nodes(1)%node = [ 1, 2 ]
allocate(gtype_list(n)%face_nodes(2)%node(2))
gtype_list(n)%face_nodes(2)%node = [ 2, 3 ]
allocate(gtype_list(n)%face_nodes(3)%node(2))
gtype_list(n)%face_nodes(3)%node = [ 3, 4 ]
allocate(gtype_list(n)%face_nodes(4)%node(2))
gtype_list(n)%face_nodes(4)%node = [ 4, 1 ]
gtype_list(n)%supported = .true.
gtype_list(n)%vtk_type = 9
allocate(gtype_list(n)%vtk_nodes(gtype_list(n)%nnodes))
gtype_list(n)%vtk_nodes = [ 1, 2, 3, 4 ]
gtype_list(n)%dat_nodes(2,:) = [ 1, 2, 3, 4, 0, 0, 0, 0 ]
gtype_list(n)%dat_nodes(3,:) = [ 1, 2, 3, 4, 4, 4, 4, 4 ]

n = 4
gtype_list(n)%name = "4-node tetrahedron"
gtype_list(n)%dimensions = 3
gtype_list(n)%nnodes = 4
gtype_list(n)%nfaces = 4
allocate(gtype_list(n)%face_nodes(gtype_list(n)%nfaces))
allocate(gtype_list(n)%face_nodes(1)%node(3))
gtype_list(n)%face_nodes(1)%node = [ 1, 4, 3 ]
allocate(gtype_list(n)%face_nodes(2)%node(3))
gtype_list(n)%face_nodes(2)%node = [ 2, 3, 4 ]
allocate(gtype_list(n)%face_nodes(3)%node(3))
gtype_list(n)%face_nodes(3)%node = [ 1, 2, 4 ]
allocate(gtype_list(n)%face_nodes(4)%node(3))
gtype_list(n)%face_nodes(4)%node = [ 1, 3, 2 ]
gtype_list(n)%supported = .true.
gtype_list(n)%vtk_type = 10
allocate(gtype_list(n)%vtk_nodes(gtype_list(n)%nnodes))
gtype_list(n)%vtk_nodes = [ 1, 2, 3, 4 ]
gtype_list(n)%dat_nodes(3,:) = [ 1, 2, 3, 4, 4, 4, 4, 4 ]

n = 5
gtype_list(n)%name = "8-node hexahedron"
gtype_list(n)%dimensions = 3
gtype_list(n)%nnodes = 8
gtype_list(n)%nfaces = 6
allocate(gtype_list(n)%face_nodes(gtype_list(n)%nfaces))
allocate(gtype_list(n)%face_nodes(1)%node(4))
gtype_list(n)%face_nodes(1)%node = [ 1, 5, 8, 4 ]
allocate(gtype_list(n)%face_nodes(2)%node(4))
gtype_list(n)%face_nodes(2)%node = [ 5, 6, 7, 8 ]
allocate(gtype_list(n)%face_nodes(3)%node(4))
gtype_list(n)%face_nodes(3)%node = [ 2, 3, 7, 6 ]
allocate(gtype_list(n)%face_nodes(4)%node(4))
gtype_list(n)%face_nodes(4)%node = [ 1, 4, 3, 2 ]
allocate(gtype_list(n)%face_nodes(5)%node(4))
gtype_list(n)%face_nodes(5)%node = [ 1, 2, 6, 5 ]
allocate(gtype_list(n)%face_nodes(6)%node(4))
gtype_list(n)%face_nodes(6)%node = [ 3, 4, 8, 7 ]
gtype_list(n)%supported = .true.
gtype_list(n)%vtk_type = 12
allocate(gtype_list(n)%vtk_nodes(gtype_list(n)%nnodes))
gtype_list(n)%vtk_nodes = [ 1, 2, 3, 4, 5, 6, 7, 8 ]
gtype_list(n)%dat_nodes(3,:) = [ 1, 2, 3, 4, 5, 6, 7, 8 ]

n = 6
gtype_list(n)%name = "6-node prism"
gtype_list(n)%dimensions = 3
gtype_list(n)%nnodes = 6
gtype_list(n)%nfaces = 5
allocate(gtype_list(n)%face_nodes(gtype_list(n)%nfaces))
allocate(gtype_list(n)%face_nodes(1)%node(4))
gtype_list(n)%face_nodes(1)%node = [ 1, 2, 5, 4 ]
allocate(gtype_list(n)%face_nodes(2)%node(4))
gtype_list(n)%face_nodes(2)%node = [ 2, 3, 6, 5 ]
allocate(gtype_list(n)%face_nodes(3)%node(4))
gtype_list(n)%face_nodes(3)%node = [ 1, 4, 6, 3 ]
allocate(gtype_list(n)%face_nodes(4)%node(3))
gtype_list(n)%face_nodes(4)%node = [ 1, 3, 2 ]
allocate(gtype_list(n)%face_nodes(5)%node(3))
gtype_list(n)%face_nodes(5)%node = [ 4, 5, 6 ]
gtype_list(n)%supported = .true.
gtype_list(n)%vtk_type = 13
allocate(gtype_list(n)%vtk_nodes(gtype_list(n)%nnodes))
gtype_list(n)%vtk_nodes = [ 1, 3, 2, 4, 6, 5 ]
gtype_list(n)%dat_nodes(3,:) = [ 1, 3, 2, 2, 4, 6, 5, 5 ]

n = 7
gtype_list(n)%name = "5-node pyramid"
gtype_list(n)%dimensions = 3
gtype_list(n)%nnodes = 5
gtype_list(n)%nfaces = 5
allocate(gtype_list(n)%face_nodes(gtype_list(n)%nfaces))
allocate(gtype_list(n)%face_nodes(1)%node(3))
gtype_list(n)%face_nodes(1)%node = [ 1, 2, 5 ]
allocate(gtype_list(n)%face_nodes(2)%node(3))
gtype_list(n)%face_nodes(2)%node = [ 2, 3, 5 ]
allocate(gtype_list(n)%face_nodes(3)%node(3))
gtype_list(n)%face_nodes(3)%node = [ 3, 4, 5 ]
allocate(gtype_list(n)%face_nodes(4)%node(3))
gtype_list(n)%face_nodes(4)%node = [ 1, 5, 4 ]
allocate(gtype_list(n)%face_nodes(5)%node(4))
gtype_list(n)%face_nodes(5)%node = [ 1, 4, 3, 2 ]
gtype_list(n)%supported = .true.
gtype_list(n)%vtk_type = 14
allocate(gtype_list(n)%vtk_nodes(gtype_list(n)%nnodes))
gtype_list(n)%vtk_nodes = [ 1, 2, 3, 4, 5 ]
gtype_list(n)%dat_nodes(3,:) = [ 1, 2, 3, 4, 5, 5, 5, 5 ]

if (debug) write(*,'(a/80(1h+))') 'subroutine setup_gmesh_elements'

end subroutine setup_gmesh_elements

!-----------------------------------------------------------------

subroutine finalise_gmesh

! here we calculate some reverse lookups and count elements

use general_module
integer :: gmesh_number, region_number, i, j, k, n, gregion, gnode, knode, gelement, iadd, jadd, kadd, &
  new_size, old_size, jj, o, nregion, kextra, ijkadd
integer, dimension(:), allocatable :: face_in_gregion
type(gelement_type), dimension(:), allocatable :: gelement_old
character(len=1000) :: formatline
logical, parameter :: debug = .false.
logical :: debug_sparse = .true.
                  
if (debug) debug_sparse = .true.

if (debug_sparse) write(*,'(80(1h+)/a)') 'subroutine finalise_gmesh'

do gmesh_number = 0, ubound(gmesh,1)

  if (debug) write(*,*) 'gmesh_number = ',gmesh_number

!------------------------------
! setup output gmesh
  if (gmesh_number == 0) then

! setup output gmesh indices - completely overwrite any that were there previously
! knode and gnode have the same index
    if (allocated(gmesh(0)%knode_from_gnode)) deallocate(gmesh(0)%knode_from_gnode)
    if (ktotal > 0) then
      allocate(gmesh(0)%knode_from_gnode(ktotal))
      do k = 1, ktotal
        gmesh(0)%knode_from_gnode(k) = k
      end do
    end if

! gelements are arranged as: domain cells, boundary cells/faces, domain faces
    if (allocated(gmesh(0)%gelement)) deallocate(gmesh(0)%gelement)
! count number of faces that are zero dimensional, as these will be coincident with a node
! and subtract these from the total number of nodes, giving the extra number of elements that will be solely nodes
    kextra = ktotal
    do j = 1, jtotal
      if (face(j)%dimensions == 0) kextra = kextra - 1
    end do
    if (itotal+jdomain+kextra > 0) then
      allocate(gmesh(0)%gelement(itotal+jdomain+kextra))
      gmesh(0)%gelement(:)%icell = 0 ! does allocation like this work? - tested and does with gnu and intel compilers
      gmesh(0)%gelement(:)%jface = 0
      gmesh(0)%gelement(:)%knode = 0
      do i = 1, idomain
        gmesh(0)%gelement(i)%icell = i
      end do
      do i = idomain+1, itotal ! the same gmsh boundary element can refer to both the boundary cell and face
        gmesh(0)%gelement(i)%icell = i
        j = cell(i)%jface(1)
        gmesh(0)%gelement(i)%jface = j
        if (face(j)%dimensions == 0) gmesh(0)%gelement(i)%knode = face(j)%knode(1)
      end do
      n = itotal
      do j = 1, jtotal
        if (face(j)%type == 1) then
          n = n + 1
          if (n > itotal+jdomain) call error_stop("problem with setting up output gmesh in setup_mesh")
          gmesh(0)%gelement(n)%jface = j
          if (face(j)%dimensions == 0) gmesh(0)%gelement(n)%knode = face(j)%knode(1)
        end if
      end do
      do k = 1, ktotal
        if (face(node(k)%jface(1))%dimensions /= 0) then ! if the first face connected to the node isn't coincident, then the node is unique
          n = n + 1
          if (n > itotal+jdomain+kextra) call error_stop("problem with setting up output gmesh in setup_mesh")
          gmesh(0)%gelement(n)%knode = k
        end if
      end do
    end if

! set output gmesh dimensions
    if (allocated(cell)) then
      gmesh(0)%dimensions = maxval(cell(:)%dimensions)
    else
      gmesh(0)%dimensions = 0
    end if

! include all non-SYSTEM regions in output gmesh
    if (allocated(gmesh(0)%gregion)) deallocate(gmesh(0)%gregion)
    nregion  = 0
    if (allocated(region)) then
      do region_number = 1, ubound(region,1)
        if ((.not.region(region_number)%dynamic) .and. trim(region(region_number)%type) /= 'system') nregion = nregion + 1
      end do
      if (nregion > 0) then
        allocate(gmesh(0)%gregion(nregion))
        nregion  = 0
        do region_number = 1, ubound(region,1)
          if ((.not.region(region_number)%dynamic) .and. trim(region(region_number)%type) /= 'system') then
            nregion = nregion + 1
            gmesh(0)%gregion(nregion)%region_number = region_number
            gmesh(0)%gregion(nregion)%centring = region(region_number)%centring
          end if
        end do
      end if
    end if

  else
!------------------------------
! operations for gmsh input gmeshes

! also include any faces that were not included originally but that border cells that are within this gmesh
    if (jtotal > 0) then
      allocate(face_in_gregion(jtotal))
      face_in_gregion = 0 ! indicates face is not in region
! first mark any faces that are already given gelements as 2
      if (allocated(gmesh(gmesh_number)%gelement)) then
        old_size = ubound(gmesh(gmesh_number)%gelement,1)
        do gelement = 1, old_size
          if (gmesh(gmesh_number)%gelement(gelement)%jface /= 0) face_in_gregion(gmesh(gmesh_number)%gelement(gelement)%jface) = 2
        end do
      else
        old_size = 0
      end if
! now loop through all cells in region, identifying faces that border these cells
      new_size = old_size
      do gelement = 1, old_size
        i = gmesh(gmesh_number)%gelement(gelement)%icell
        if (i /= 0) then
          do jj = 1, ubound(cell(i)%jface,1)
            j = cell(i)%jface(jj)
            if (face_in_gregion(j) == 0) then
              face_in_gregion(j) = 1 ! face is in region but has not been previously defined, so define it
              new_size = new_size + 1
            end if
          end do
        end if
      end do
! now expand the gmesh%gelement array to include the new faces
      if (new_size /= old_size) then
        if (old_size > 0) then
          allocate(gelement_old(old_size))
          gelement_old = gmesh(gmesh_number)%gelement
        end if
        deallocate(gmesh(gmesh_number)%gelement)
        allocate(gmesh(gmesh_number)%gelement(new_size))
        gmesh(gmesh_number)%gelement(:)%icell = 0
        gmesh(gmesh_number)%gelement(:)%jface = 0
        gmesh(gmesh_number)%gelement(:)%knode = 0
        if (old_size > 0) then
          gmesh(gmesh_number)%gelement(1:old_size) = gelement_old ! allocate all the old data across
          deallocate(gelement_old)
        end if
        gelement = old_size
        do j = 1, jtotal
          if (face_in_gregion(j) == 1) then
            gelement = gelement + 1
            gmesh(gmesh_number)%gelement(gelement)%jface = j
          end if
        end do
        if (gelement /= new_size) stop "ERROR: problem in finalise_gmesh"
      end if
      deallocate(face_in_gregion)
    end if

  end if
!------------------------------
! operations for all gmeshes

! reverse lookup and counting of gnodes
  gmesh(gmesh_number)%ngnodes = 0
  if (ktotal > 0) then
    call resize_integer_array(array=gmesh(gmesh_number)%gnode_from_knode,new_size=ktotal)
    gmesh(gmesh_number)%gnode_from_knode = 0
    do gnode = 1, ubound(gmesh(gmesh_number)%knode_from_gnode,1)
      knode = gmesh(gmesh_number)%knode_from_gnode(gnode)
      if (knode /= 0) then
        gmesh(gmesh_number)%gnode_from_knode(knode) = gnode
        gmesh(gmesh_number)%ngnodes = gmesh(gmesh_number)%ngnodes + 1
      end if
    end do
  else if (allocated(gmesh(gmesh_number)%gnode_from_knode)) then
    deallocate(gmesh(gmesh_number)%gnode_from_knode)
  end if

! counting of gregions
  gmesh(gmesh_number)%ngregions = 0
  if (allocated(gmesh(gmesh_number)%gregion)) then
    do gregion = 1, ubound(gmesh(gmesh_number)%gregion,1)
      region_number = gmesh(gmesh_number)%gregion(gregion)%region_number
      if (region_number /= 0) gmesh(gmesh_number)%ngregions = gmesh(gmesh_number)%ngregions + 1
    end do
  end if

! counting of gelements and create arrays of gregions for each gelement
! the purpose of this loop is to 
! 1) calculate the number of gelement output lines will be output for each gmesh, noting that an element will be repeated for every region that it is in, except for boundary cells (which are not output for any cell regions)
! 2) form the gregions array for each element, which is used when writing each gelement
! note, that if a gelement is not in a region (or is only in one region but is a boundary cell) then it still is given an entry in the output file, but here is assigned a sole gregion array entry of 1
  gmesh(gmesh_number)%ngelements = 0 ! these have one entry per element, with repeats if an element is in multiple regions
  gmesh(gmesh_number)%ngelements_face = 0
  gmesh(gmesh_number)%ngelements_cell = 0
  gmesh(gmesh_number)%ngelements_node = 0
  if (allocated(gmesh(gmesh_number)%gelement)) then
    do gelement = 1, ubound(gmesh(gmesh_number)%gelement,1)
      if (allocated(gmesh(gmesh_number)%gelement(gelement)%gregions)) deallocate(gmesh(gmesh_number)%gelement(gelement)%gregions)
      i = gmesh(gmesh_number)%gelement(gelement)%icell
      iadd = 0
      if (i /= 0.and.allocated(gmesh(gmesh_number)%gregion)) then
        do gregion = 1, ubound(gmesh(gmesh_number)%gregion,1)
          if (gmesh(gmesh_number)%gregion(gregion)%centring.ne.'cell') cycle ! as per v0.53 the cell|face|node%region_list array has been removed, so instead check the region centring and then later that this element is in the region
  ! only include gelements that have the same dimension as the gregion (otherwise gmsh pickles)
          region_number = gmesh(gmesh_number)%gregion(gregion)%region_number
          if (region_number == 0) cycle
          if (cell(i)%dimensions /= region(region_number)%dimensions) cycle
  ! also only include non-boundary cells, otherwise there will be confusion when reading in file again
  ! NB: as per original gmsh file, boundary cells are created once the read in has taken place
  ! data can still be associated with them as there gelements for the boundary face elements are defined
          if (cell(i)%type /= 1) cycle
          if (region(region_number)%ns(i) /= 0) then
            iadd = iadd + 1
            call push_integer_array(gmesh(gmesh_number)%gelement(gelement)%gregions,new_element=gregion)
          end if
        end do
      end if
      j = gmesh(gmesh_number)%gelement(gelement)%jface
      jadd = 0
      if (j /= 0.and.allocated(gmesh(gmesh_number)%gregion)) then
        do gregion = 1, ubound(gmesh(gmesh_number)%gregion,1)
          if (gmesh(gmesh_number)%gregion(gregion)%centring.ne.'face') cycle
          region_number = gmesh(gmesh_number)%gregion(gregion)%region_number
          if (region_number == 0) cycle
          if (face(j)%dimensions /= region(region_number)%dimensions) cycle
          if (region(region_number)%ns(j) /= 0) then
            jadd = jadd + 1
            call push_integer_array(gmesh(gmesh_number)%gelement(gelement)%gregions,new_element=gregion)
          end if
        end do
      end if
      k = gmesh(gmesh_number)%gelement(gelement)%knode
      kadd = 0
      if (k /= 0.and.allocated(gmesh(gmesh_number)%gregion)) then
        do gregion = 1, ubound(gmesh(gmesh_number)%gregion,1)
          if (gmesh(gmesh_number)%gregion(gregion)%centring.ne.'node') cycle
          region_number = gmesh(gmesh_number)%gregion(gregion)%region_number
          if (region_number == 0) cycle
          if (0 /= region(region_number)%dimensions) cycle
          if (region(region_number)%ns(k) /= 0) then
            kadd = kadd + 1
            call push_integer_array(gmesh(gmesh_number)%gelement(gelement)%gregions,new_element=gregion)
          end if
        end do
      end if
! if element is not in a region, then add a dummy 0 index to its gregion list
      ijkadd = iadd+jadd+kadd
      if (ijkadd == 0) then
        call push_integer_array(gmesh(gmesh_number)%gelement(gelement)%gregions,new_element=0)
        ijkadd = 1
      end if
! count the number of number times it will be used, including repeats
      if (i /= 0) gmesh(gmesh_number)%ngelements_cell = gmesh(gmesh_number)%ngelements_cell + ijkadd
      if (j /= 0) gmesh(gmesh_number)%ngelements_face = gmesh(gmesh_number)%ngelements_face + ijkadd
      if (k /= 0) gmesh(gmesh_number)%ngelements_node = gmesh(gmesh_number)%ngelements_node + ijkadd
      if (i == 0 .and. j == 0 .and. k == 0) call error_stop("an element is not either a cell, face or node in finalise_gmesh")
      gmesh(gmesh_number)%ngelements = gmesh(gmesh_number)%ngelements + ijkadd
    end do
  end if

  if (debug) then
    write(87,*) 'GMESH: gmesh_number = ',gmesh_number,': basename = ',trim(gmesh(gmesh_number)%basename)
    write(87,*) 'ngelements = ',gmesh(gmesh_number)%ngelements
    write(87,*) 'ngelements_cell = ',gmesh(gmesh_number)%ngelements_cell
    write(87,*) 'ngelements_face = ',gmesh(gmesh_number)%ngelements_face
    write(87,*) 'ngelements_node = ',gmesh(gmesh_number)%ngelements_node
    write(87,*) 'gregions:'
    if (allocated(gmesh(gmesh_number)%gregion)) then
      do gregion = 1, ubound(gmesh(gmesh_number)%gregion,1)
        region_number = gmesh(gmesh_number)%gregion(gregion)%region_number
        write(87,*) 'gregion = ',gregion,': centring = '//trim(gmesh(gmesh_number)%gregion(gregion)%centring)// &
          ': region_number = ',region_number,': region centring = '//trim(region(region_number)%centring)//&
          ': region dimensions = ',region(region_number)%dimensions,': region name = '//trim(region(region_number)%name)
      end do
    end if
    write(87,*) 'gelements:'
    if (allocated(gmesh(gmesh_number)%gelement)) then
      do gelement = 1, ubound(gmesh(gmesh_number)%gelement,1)
        write(87,*) 'gelement = ',gelement,': icell = ',gmesh(gmesh_number)%gelement(gelement)%icell, &
        ': jface = ',gmesh(gmesh_number)%gelement(gelement)%jface, &
        ': knode = ',gmesh(gmesh_number)%gelement(gelement)%knode, &
        ': gregions =',(' ',gmesh(gmesh_number)%gelement(gelement)%gregions(gregion), &
        gregion=1,ubound(gmesh(gmesh_number)%gelement(gelement)%gregions,1))
      end do
    end if
  end if
    
end do

if (debug_sparse) then
  write(*,'(a)') 'INFO: gmeshes:'
  do gmesh_number = 0, ubound(gmesh,1) ! atleast mesh 0 must always be allocated
    formatline = '(a,'//trim(dindexformat(gmesh_number))// &
      ',a,'//trim(dindexformat(gmesh(gmesh_number)%ngnodes))// &
      ',a,'//trim(dindexformat(gmesh(gmesh_number)%ngelements))// &
      ',a,'//trim(dindexformat(gmesh(gmesh_number)%ngelements_cell))// &
      ',a,'//trim(dindexformat(gmesh(gmesh_number)%ngelements_face))// &
      ',a,'//trim(dindexformat(gmesh(gmesh_number)%ngelements_node))// &
      ',a,'//trim(dindexformat(gmesh(gmesh_number)%ngregions))// &
      ',a'//repeat(',a',ubound(gmesh(gmesh_number)%options,1))//')'
    write(*,fmt=formatline) ' gmesh_number = ',gmesh_number,': basename = '//trim(gmesh(gmesh_number)%basename)//': ngnodes = ', &
      gmesh(gmesh_number)%ngnodes, &
      ': ngelements = ',gmesh(gmesh_number)%ngelements,': ngelements_cell = ', &
      gmesh(gmesh_number)%ngelements_cell,': ngelements_face = ',gmesh(gmesh_number)%ngelements_face, &
      ': ngelements_node = ',gmesh(gmesh_number)%ngelements_node, &
      ': ngregions = ',gmesh(gmesh_number)%ngregions, &
      ': options (prioritised) =',(' '//trim(gmesh(gmesh_number)%options(o)),o=1,ubound(gmesh(gmesh_number)%options,1))
  end do
end if

if (debug_sparse) write(*,'(a/80(1h-))') 'subroutine finalise_gmesh'

end subroutine finalise_gmesh

!-----------------------------------------------------------------

subroutine push_gmesh(filename,gmesh_number)

! push new gmesh element onto gmesh array
! right now cannot handle allocated gregions_from_gelement

use general_module
character(len=*), intent(in) :: filename
integer, intent(out), optional :: gmesh_number
integer :: gmesh_number_local
type(gmesh_type), dimension(:), allocatable :: gmesh_tmp
integer :: n

if (allocated(gmesh)) then

! check if mesh is already defined
  do n = 0, ubound(gmesh,1)
    if (trim(gmesh(n)%basename) == trim(basename(filename))) then
      gmesh_number_local = n
      if (trim(gmesh(n)%filename) /= trim(filename)) then
        write(*,'(a)') 'ERROR: the same gmesh basename '//trim(gmesh(n)%basename)// &
          ' has been specified in multiple directories:'//'  this basename must be unique'
        stop
      end if
      if (present(gmesh_number)) gmesh_number = gmesh_number_local
      return
    end if
  end do

! if not already defined then increase gmesh array

  gmesh_number_local = ubound(gmesh,1)
  allocate(gmesh_tmp(0:gmesh_number_local))
  gmesh_tmp(0:gmesh_number_local) = gmesh(0:gmesh_number_local)
  deallocate(gmesh)
  gmesh_number_local = gmesh_number_local + 1
else
! output array should be the first one to be allocated, having index 0
  gmesh_number_local = 0
end if
allocate(gmesh(0:gmesh_number_local))
if (allocated(gmesh_tmp)) then
  gmesh(0:gmesh_number_local-1) = gmesh_tmp(0:gmesh_number_local-1)
  deallocate(gmesh_tmp)
end if

! set data for the new element
gmesh(gmesh_number_local)%filename = trim(filename)
gmesh(gmesh_number_local)%basename = trim(basename(filename))
! add default input and output options
if (gmesh_number_local == 0) then
  call push_character_array(array=gmesh(gmesh_number_local)%options,new_element='noinput')
  call push_character_array(array=gmesh(gmesh_number_local)%options,new_element='output')
else
  call push_character_array(array=gmesh(gmesh_number_local)%options,new_element='input')
  call push_character_array(array=gmesh(gmesh_number_local)%options,new_element='nooutput')
end if
call push_character_array(array=gmesh(gmesh_number_local)%options,new_element='novtkoutput') ! no vtk output by default for all gmeshes
call push_character_array(array=gmesh(gmesh_number_local)%options,new_element='nodatoutput') ! no dat output by default for all gmeshes
if (present(gmesh_number)) gmesh_number = gmesh_number_local ! return gmesh_number if requested

end subroutine push_gmesh

!-----------------------------------------------------------------

subroutine read_gmesh(contents,var_number)

use general_module
character(len=4) :: contents ! either mesh or data
integer, optional :: var_number ! if reading data then this is the var_number to read
integer :: gmesh_number
character(len=1000) :: formatline
character(len=100) :: input_option
logical :: success
logical, parameter :: debug = .false. ! this is not the variable you are looking for

if (debug) write(*,'(80(1h+)/a)') 'subroutine read_gmesh'

if (.not.allocated(gmesh)) stop 'ERROR: no gmsh meshes have been specified'

success = .false.

do gmesh_number = 0, ubound(gmesh,1)
  formatline = '(a,'//trim(dindexformat(gmesh_number))//',a)'
  input_option = check_option(gmesh(gmesh_number)%options,input_gmesh_options) ! get input option for gmesh
  if (trim(input_option) == "noinput" .or. &
    (contents == 'data'.and.(trim(input_option) == "centringmeshinput".or.trim(input_option) == "meshinput"))) then
    if (gmesh_number /= 0.or.debug) &
      write(*,fmt=formatline) 'INFO: skipping '//trim(contents)//' read for gmesh file (',gmesh_number,') having basename '// &
      trim(gmesh(gmesh_number)%basename)//' based on gmesh input option '//trim(input_option)
  else if (contents == 'mesh') then
    write(*,fmt=formatline) 'INFO: performing mesh read for gmesh file (',gmesh_number,') having basename '// &
      trim(gmesh(gmesh_number)%basename)//' based on gmesh input option '//trim(input_option)
    success = .true.
    if (trim(input_option) == "centringinput" .or. trim(input_option) == "centringmeshinput") then
      write(*,'(a)') 'INFO: reading in cell element mesh:'
      call read_gmesh_file(gmesh_number=gmesh_number,file_centring='cell',contents=contents) ! read in cell mesh for gmesh_number
      write(*,'(a)') 'INFO: reading in face element mesh:'
      call read_gmesh_file(gmesh_number=gmesh_number,file_centring='face',contents=contents) ! read in face mesh for gmesh_number
      write(*,'(a)') 'INFO: reading in node element mesh:'
      call read_gmesh_file(gmesh_number=gmesh_number,file_centring='node',contents=contents) ! read in node mesh for gmesh_number
    else
      call read_gmesh_file(gmesh_number=gmesh_number,file_centring='all ',contents=contents) ! read in combined mesh for gmesh_number
    end if
  else if (contents == 'data') then
    if (.not.present(var_number)) call error_stop("read_gmesh called with data contents but no var_number")
    if (trim(check_option(var(var_number)%options,input_options)) == "noinput".and. &
      trim(check_option(compound(var(var_number)%compound_number)%options,input_options)) == "noinput") then
      if (debug) write(*,fmt=formatline) 'INFO: skipping data read for variable '//trim(var(var_number)%name)// &
        ' for gmesh file (',gmesh_number,') having basename '//trim(gmesh(gmesh_number)%basename)//' based on component option '// &
        trim(check_option(var(var_number)%options,input_options))//' and compound option '// &
        trim(check_option(compound(var(var_number)%compound_number)%options,input_options)) 
    else
      if (debug) write(*,fmt=formatline) 'INFO: initiating data read for variable '//trim(var(var_number)%name)// &
        ' for gmesh file (',gmesh_number,') having basename '//trim(gmesh(gmesh_number)%basename)// &
        ' based on gmesh input option '//trim(input_option)//', component option '// &
        trim(check_option(var(var_number)%options,input_options))//' and compound option '// &
        trim(check_option(compound(var(var_number)%compound_number)%options,input_options)) 
      if (trim(input_option) == "centringinput") then
        if (debug) write(*,'(a)') 'INFO: reading in '//var(var_number)%centring//' element data:'
        call read_gmesh_file(gmesh_number=gmesh_number,file_centring=var(var_number)%centring,contents=contents, &
          var_number=var_number) ! read in centred data for gmesh_number
      else
        call read_gmesh_file(gmesh_number=gmesh_number,file_centring='all ',contents=contents,var_number=var_number) ! read in combined data for gmesh_number
      end if
    end if
  end if
end do

if (contents == 'mesh'.and..not.success) write(*,'(a)') "WARNING: no meshes have been read in"

if (debug) write(*,'(a/80(1h-))') 'subroutine read_gmesh'

end subroutine read_gmesh

!-----------------------------------------------------------------

subroutine read_gmesh_file(gmesh_number,file_centring,contents,var_number)

! this subroutine reads in a gmsh file, for contents = mesh or data
! specifically for contents=mesh it:
! - defines any regions not already defined (name, centring and location)
! - defines nodes (x, jface) and sets ktotal
! - defines cells (dimensions, jface, knode, gtype), adds to relevant region list and sets itotal
! - defines faces (dimensions, icell, knode, gtype), adds to relevant region list and sets jtotal

use general_module
integer :: gmesh_number, gmsh_bin, n, error
integer, optional :: var_number
real :: gmsh_version
character(len=1000) :: textline, filename
character(len=4) :: file_centring, contents
logical, parameter :: debug = .false. ! this is passed to called reading subroutines too - this is the variable you are looking for (ref: star wars)

if (debug) write(*,'(80(1h+)/a)') 'subroutine read_gmesh_file'

if (debug) write(*,*) 'file_centring = |'//trim(file_centring)//'|'
filename = trim(gmesh(gmesh_number)%filename)
! for centringinput alter filename to suit either cell or face
if (file_centring == "cell" .or. file_centring == "face" .or. file_centring == "node" .or. file_centring == "none") then
  n = scan(filename,'.',.true.)
  if (n == 0) call error_stop("problem constructing centring filename in subroutine read_gmesh_file: "//trim(filename))
  filename = filename(1:n)//trim(file_centring)//"."//filename(n+1:len(filename))
end if
if (debug) write(*,*) ' reading in filename = '//trim(filename)
open(unit=fgmsh,file=trim(filename),status='old',iostat=error)
if (error /= 0) call error_stop('problem opening mesh file '//trim(filename))

!---------------------------------------------
! check version and format of file
do 
  read(fgmsh,'(a)',iostat=error) textline
  if (error /= 0) call error_stop('gmsh file version not found for '//trim(filename))
  if (trim(textline) == "$MeshFormat") exit
end do
read(fgmsh,*,iostat=error) gmsh_version, gmsh_bin
if (error /= 0) call error_stop('problem reading in gmsh file version in '//trim(filename))
if (gmsh_version < 2.1) then
  write(*,'(a,f4.1,a)') 'ERROR: the gmsh file version (',gmsh_version,') is too old and is not supported (update to v2.1) in '// &
    trim(filename)
  stop
else if (gmsh_version > 2.2) then
  if (contents == 'mesh') write(*,'(a,f4.1,a)') 'WARNING: the gmsh mesh file version (',gmsh_version, &
    ') is not fully suported yet (but may work) in '//trim(filename)
end if
if (gmsh_bin /= 0) call error_stop('gmsh binary file type not supported in '//trim(filename))
if (debug) write(*,*) 'gmsh_version = ',gmsh_version
      
if (contents == 'mesh') then
!---------------------------------------------
! read in physical names and possibly create any required regions
! if reading the face or node file check that each physical region is defined
! NB: none centring is not used for mesh reading

  if (file_centring == "cell" .or. file_centring(1:3) == "all") then ! here all means cell+face+node
    call read_gmesh_regions(check=.false.,gmesh_number=gmesh_number,filename=filename,debug=debug) ! read in
    if (debug) write(*,*) 'gregions read in'
  else
    call read_gmesh_regions(check=.true.,gmesh_number=gmesh_number,filename=filename,debug=debug) ! check for consistency
    if (debug) write(*,*) 'gregions checked'
  end if

!---------------------------------------------
! read in nodes, as per regions

  if (file_centring == "cell" .or. file_centring(1:3) == "all") then ! here all means cell+face+node
    call read_gmesh_nodes(check=.false.,gmesh_number=gmesh_number,filename=filename,debug=debug) ! read in
    if (debug) write(*,*) 'nodes read in'
  else
    call read_gmesh_nodes(check=.true.,gmesh_number=gmesh_number,filename=filename,debug=debug) ! check for consistency
    if (debug) write(*,*) 'nodes checked'
  end if

!---------------------------------------------
! read in elements, creating cells and faces as we go, if nodes have been read in previously
  if (allocated(gmesh(gmesh_number)%knode_from_gnode)) then
    call read_gmesh_elements(gmesh_number=gmesh_number,filename=filename,debug=debug)
    if (debug) write(*,*) 'elements read in'
  end if

!---------------------------------------------
else if (contents == 'data') then
! read in data
  if (.not.present(var_number)) call error_stop("read_gmesh_file called with data contents but no var_number")
  call read_gmesh_data(gmesh_number=gmesh_number,filename=filename,var_number=var_number,debug=debug)
  if (debug) write(*,*) 'data read in'

end if
!---------------------------------------------

close(fgmsh)

if (debug) write(*,'(a/80(1h-))') 'subroutine read_gmesh_file'

end subroutine read_gmesh_file

!-----------------------------------------------------------------

subroutine read_gmesh_data(gmesh_number,filename,var_number,debug)

use general_module
integer :: error, gmesh_number, ntags, var_number, compound_number, nrank, nelements, gelement, nelement, n, k, ijk, ns, &
  region_number, nnodes, gmesh_step
character(len=1000) :: textline, filename, formatline, name
character(len=100) :: data_type
character(len=4) :: centring
double precision, dimension(:,:), allocatable :: data_array
logical :: compoundl
logical :: debug

if (debug) write(*,'(a)') 'INFO: reading in data for variable '//trim(var(var_number)%name)//' from file '//trim(filename)//':'

rewind(fgmsh)

data_type = ''

main_loop: do 
  read(fgmsh,'(a)',iostat=error) textline
  if (error /= 0) exit
  if (trim(textline) == '') cycle
  if (data_type /= '') then
    if (trim(textline) == '$End'//trim(data_type)) data_type = ''
  else if (trim(textline) == "$Data" .or. trim(textline) == "$ElementData" .or. trim(textline) == "$ElementNodeData") then
    data_type = trim(textline(2:len(textline)))

! if no elements are defined for spatial data then skip (really this is a file inconsistency though)
    if (.not.allocated(gmesh(gmesh_number)%gelement).and.trim(data_type) /= 'Data') then
      write(*,'(a)') 'WARNING: skipping orphaned '//trim(data_type)//' data in gmsh file '//trim(filename)// &
        ' for which there are no elements defined'
      cycle main_loop
    end if

! character data: ie, variable name
    read(fgmsh,*,iostat=error) ntags ! ntags is the number of character variables, the first of which should be the variable name
    if (error /= 0.or.ntags < 1) then
      write(*,*) 'WARNING: problem reading file for data type '//trim(data_type)//' in gmsh file '//trim(filename)
      cycle main_loop
    end if
    do n = 1, ntags
      read(fgmsh,*,iostat=error) textline
      if (error /= 0) then
        write(*,*) 'WARNING: problem reading file for data type '//trim(data_type)//' in gmsh file '//trim(filename)
        write(*,'(a)') '  Offending textline was '//trim(textline)
        cycle main_loop
      end if
      if (textline(1:1) /= "<") textline = "<"//trim(textline)//">" ! add arb delimiters to any data names that don't have them already
      if (n == 1) then
        name = var(var_number)%name
        centring = var(var_number)%centring
        region_number = var(var_number)%region_number
        if (trim(textline) == trim(var(var_number)%name)) then
          compoundl = .false.
          compound_number = 0
        else if (trim(textline) == trim(compound(var(var_number)%compound_number)%name)) then
          compoundl = .true.
          compound_number = var(var_number)%compound_number
        else
          cycle main_loop ! this data field does not contain the requested var so move on
        end if
!     else
! check on info here in the future
!       write(*,*) 'WARNING: ignoring character string in '//trim(filename)//' in gmsh file'//trim(textline)
      end if
    end do
    if (debug) then
      if (compoundl) then
        write(*,*) 'found compound variable '//trim(name)//' for var '//trim(var(var_number)%name)//' in gmsh file '//trim(filename)
      else
        write(*,*) 'found var variable '//trim(name)//' in gmsh file '//trim(filename)
      end if
    end if

! perform some sanity checks on data_type
    if ((trim(data_type) == 'Data'.and.centring /= 'none').or.(trim(data_type) /= 'Data'.and.centring == 'none').or. &
      (trim(data_type) == 'ElementNodeData'.and.centring /= 'cell')) then
      write(*,*) 'WARNING: inconsistent variable centring for variable '//trim(name)//' in gmsh file '//trim(filename)
      cycle main_loop
    end if
  
! real data: empty
    read(fgmsh,*,iostat=error) ntags ! ntags is the number of real variables, should be 0
    if (error /= 0) then
      write(*,*) 'WARNING: problem reading file for variable '//trim(name)//' in gmsh file '//trim(filename)
      cycle main_loop
    end if
    do n = 1, ntags
      read(fgmsh,*,iostat=error) textline
    end do

! integer data: timestep, number of entries per elements, number of elements
    read(fgmsh,*,iostat=error) ntags ! ntags is the number of real variables, should be 3
    if (error /= 0.or.ntags < 3) then
      write(*,*) 'WARNING: problem reading file for variable '//trim(name)//' in gmsh file '//trim(filename)
      cycle main_loop
    end if
    read(fgmsh,*,iostat=error) gmesh_step
    if (error /= 0) then
      write(*,*) 'WARNING: problem reading file for variable '//trim(name)//' in gmsh file '//trim(filename)
      cycle main_loop
    end if
! set and sanity check on newtstep/timestep
    if (.not.ignore_gmesh_step) then
      if (transient_simulation) then
        if (timestep /= 0.and.gmesh_step /= timestep) then
          write(*,'(a)') 'WARNING: timestep for variable '//trim(name)//' in gmsh file '//trim(filename)//' is '// &
            'inconsistent with the timestep for a previously read-in variable - using previous value'
        else if (gmesh_step /= timestep) then
          formatline = '(a,'//trim(dindexformat(gmesh_step))//',a)'
          write(*,fmt=formatline) 'INFO: setting timestep to ',gmesh_step,' based on variable '//trim(name)//' in gmsh file '//trim(filename)
          timestep = gmesh_step
        end if
      else
        if (newtstep /= 0.and.gmesh_step /= newtstep) then
          write(*,'(a)') 'INFO: newtstep for variable '//trim(name)//' in gmsh file '//trim(filename)//' is '// &
            'inconsistent with the newtstep for a previously read-in variable - using previous value'
        else if (gmesh_step /= newtstep) then
          formatline = '(a,'//trim(dindexformat(gmesh_step))//',a)'
          write(*,fmt=formatline) 'INFO: setting newtstep to ',gmesh_step,' based on variable '//trim(name)//' in gmsh file '//trim(filename)
          newtstep = gmesh_step
        end if
      end if
    end if
    read(fgmsh,*,iostat=error) nrank
    if (error /= 0) then
      write(*,*) 'WARNING: problem reading file for variable '//trim(name)//' in gmsh file '//trim(filename)
      cycle main_loop
    end if
    read(fgmsh,*,iostat=error) nelements
    if (error /= 0) then
      write(*,*) 'WARNING: problem reading file for variable '//trim(name)//' in gmsh file '//trim(filename)
      cycle main_loop
    end if

! perform a sanity check on variable rank
    if (compoundl) then
      if (nrank /= ubound(compound(compound_number)%component,1)) then
        write(*,*) 'WARNING: rank of compound '//trim(name)//' does not match that in gmsh file '//trim(filename)
        cycle main_loop
      end if
    else
      if (nrank /= 1) then
        write(*,*) 'WARNING: rank of component variable '//trim(name)//' does not match that in gmsh file '//trim(filename)
        cycle main_loop
      end if
    end if

    if (debug) write(*,*) ' about to read in data for variable = '//trim(name)//': compoundl = ',compoundl,': var_number = ', &
      var_number,': compound_number = ',compound_number,': nrank = ',nrank,': nelements = ',nelements,': data_type = '// &
      trim(data_type)

! do a temporary allocation of data_array, assuming one element
    allocate(data_array(nrank,1))
    ns = 1

! now to read in actual data
! errors found from here onwards indicate a real problem, so terminate if any found
    do nelement = 1, nelements
      read(fgmsh,'(a)',iostat=error) textline
      if (error /= 0) call error_stop ('problem reading file for variable '//trim(name)//' in gmsh file '//trim(filename))
      if (trim(data_type) == 'ElementNodeData') then
        read(textline,*,iostat=error) gelement, ntags
        if (ntags < 1) call error_stop('ntags incorrect for variable '//trim(name)//' in gmsh file '//trim(filename))
      else
        read(textline,*,iostat=error) gelement
        ntags = 1
      end if
      if (error /= 0) call error_stop ('problem reading file for variable '//trim(name)//' in gmsh file '//trim(filename))

! find cell or face if it is not none centred and check that it is within variable's region
      if (centring /= 'none') then
        if (gelement < 1.or.gelement > ubound(gmesh(gmesh_number)%gelement,1)) &
          call error_stop('gelement out of range for variable '//trim(name)//' in gmsh file '//trim(filename))
        ijk = 0
        if (centring == 'cell') then
          ijk = gmesh(gmesh_number)%gelement(gelement)%icell
        else if  (centring == 'face') then
          ijk = gmesh(gmesh_number)%gelement(gelement)%jface
        else if  (centring == 'node') then
          ijk = gmesh(gmesh_number)%gelement(gelement)%knode
        end if
        if (ijk == 0.and.debug) write(*,*) 'ijk = ',ijk,': gelement = ',gelement,': textline = ',trim(textline)
!       if (ijk == 0) call error_stop(trim(centring)//' centred data references an invalid element for variable '//trim(name)// &
!         ' in gmsh file '//trim(filename))
! now silently ignore data that is given on an element that has a centring that is inconsistent with that of the data
! this flexibility need (eg) when dealing with data that is neglected after a face has been glued, so that a boundary cell has disappeared, but the data remains (and should be ignored) on what the code sees as a face
        if (ijk == 0) cycle ! not only is this data not in the region, the centring is wrong too
        ns = region(region_number)%ns(ijk)
        if (ns == 0) cycle ! also silently skip any data that is not within the variable's region
      else
        if (gelement /= 1) &
          call error_stop('gelement should be 1 for none centred variable '//trim(name)//' in gmsh file '//trim(filename))
      end if

! find nnodes depending on data_type and for ElementNodeData reallocate data_array to match
      nnodes = 1 ! do not have to keep reallocating data_array
      if (trim(data_type) == 'ElementNodeData') then ! NB, this also implies cell centring
        nnodes = ubound(cell(ijk)%knode,1)
        if (ntags /= nnodes) call error_stop('mismatch between number of nodes that element has and what file specifies for '// &
          'variable '//trim(name)//' in gmsh file '//trim(filename))
        if (ubound(data_array,2) /= nnodes) then
          deallocate(data_array)
          allocate(data_array(nrank,nnodes))
        end if
      end if

! now read in data
      if (trim(data_type) == 'ElementNodeData') then
        read(textline,*,iostat=error) gelement, ntags, ((data_array(n,k),n=1,nrank),k=1,nnodes)
      else
        read(textline,*,iostat=error) gelement, ((data_array(n,k),n=1,nrank),k=1,nnodes)
      end if
      if (error /= 0)  call error_stop('problem reading in data line for variable '//trim(name)// &
          ' in gmsh file '//trim(filename))

! finally use data to set variable value
      if (compoundl) then
        n = location_in_list(compound(compound_number)%component,var_number)
      else
        n = 1
      end if
      if (trim(data_type) == 'Data'.or.trim(data_type) == 'ElementData') then
        var(var_number)%funk(ns)%v = data_array(n,1)
      else
! have to use cell kernel (4) to average node data to cells for elementnodedata
        var(var_number)%funk(ns)%v = 0.d0
        do k = 1, nnodes
          var(var_number)%funk(ns)%v = var(var_number)%funk(ns)%v + cell(ijk)%kernel(4)%v(k)*data_array(n,k)
        end do
      end if

    end do
    deallocate(data_array)
    if (compoundl) then
      formatline = '(a,'//trim(dindexformat(nelements))//',a,'//trim(dindexformat(nrank))//',a)'
      write(*,fmt=formatline) 'INFO: read data for variable '//trim(var(var_number)%name)//' from compound variable '// &
        trim(compound(compound_number)%name)//': centring = '//trim(centring)//': data_type = '//trim(data_type)// &
        ': nelements = ',nelements,': nrank = ',nrank,': gmsh file = '//trim(filename)
    else
      formatline = '(a,'//trim(dindexformat(nelements))//',a)'
      write(*,fmt=formatline) 'INFO: read data for variable '//trim(var(var_number)%name)// &
        ' from component: centring = '//trim(centring)//': data_type = '//trim(data_type)//': nelements = ',nelements, &
        ': gmsh file = '//trim(filename)
    end if

  end if
end do main_loop

end subroutine read_gmesh_data

!-----------------------------------------------------------------

subroutine read_gmesh_elements(gmesh_number,filename,debug)

use general_module
integer :: error, gmesh_number
integer :: i, j, k, n, m, jj, nelements, nelements_tenth, gelement, gtype, ntags, region_number, nnode, nface, nfaces, &
  gelement_max, change, gelement_previous
integer, dimension(:), allocatable :: tags, local_gnodes, local_knodes
integer, dimension(:), allocatable :: icell_from_gelement, jface_from_gelement, knode_from_gelement
type(cell_type) :: default_cell
type(face_type) :: default_face
character(len=4) :: centring
character(len=1000) :: textline, filename
logical :: debug

rewind(fgmsh)

allocate(default_face%icell(2)) ! allocate spaces for the two surrounding faces

do 
  read(fgmsh,'(a)',iostat=error) textline
! if (error /= 0) call error_stop('list of elements not found in gmsh file '//trim(filename))
  if (error /= 0) then
    write(*,'(a)') "WARNING: list of elements not found in gmsh file "//trim(filename)
    return
  end if
  if (trim(textline) == '') cycle
  if (trim(textline) == "$Elements") exit
end do
read(fgmsh,*,iostat=error) nelements
if (error /= 0) call error_stop('problem reading in number of elements in gmsh file '//trim(filename))
if (debug) write(*,*) 'number of elements: nelements = ',nelements

! allocate temporary gelement lookup arrays based on estimate of required size
allocate(icell_from_gelement(nelements))
allocate(jface_from_gelement(nelements))
allocate(knode_from_gelement(nelements))
icell_from_gelement = 0
jface_from_gelement = 0
knode_from_gelement = 0
gelement_max = 0 ! this records the highest gelement number

nelements_tenth = max(int(float(nelements)/10.),1)
if (mod(nelements,nelements_tenth) /= 0) nelements_tenth = nelements_tenth + 1

do n = 1, nelements

  if (mod(n,nelements_tenth) == 0 .or. n == nelements) write(*,'(a,i3,a)') &
    'INFO: reading in mesh:',min(int(float(n)*100./float(nelements)),100),'%' ! a real representing progress

  read(fgmsh,'(a)',iostat=error) textline
  if (error /= 0) call error_stop('problem reading elements in gmsh file '//trim(filename))
  if (trim(textline) == '') cycle
  read(textline,*,iostat=error) gelement, gtype, ntags
  if (error /= 0) call error_stop('problem reading initial element data in gmsh file '//trim(filename))
  if (gtype > ubound(gtype_list,1) .or. .not.gtype_list(gtype)%supported) stop &
    'ERROR: element type not supported in gmsh file'

! get tags and local_nodes arrays to the correct size ready for reading
  call resize_integer_array(array=tags,new_size=ntags)
  if (ntags == 0) allocate(tags(0)) ! fortran allows this now, so reallocate for future convienience even if the element has no associated tags
  call resize_integer_array(array=local_gnodes,new_size=gtype_list(gtype)%nnodes)

! re-read line now knowing number of tags and number of nodes
  read(textline,*,iostat=error) gelement, gtype, ntags, tags, local_gnodes
  if (error /= 0) call error_stop('problem reading element tags/nodes in gmsh file '//trim(filename))

! due to gmsh_output_node_elements_as_lines (now deprecated), node elements may be stored as degenerate line elements in gmsh
! these are identified as lines that have the two duplicate gnodes
! simply overwrite gtype, and as local_knodes is based on gtype only first node will be used (local_gnode array isn't used further)
  if (gtype == line_gtype) then
    if (local_gnodes(1) == local_gnodes(2)) then
      gtype = node_gtype
    end if
  end if

! convert local gnode list to knode list
  call resize_integer_array(array=local_knodes,new_size=gtype_list(gtype)%nnodes)
  do k = 1, ubound(local_knodes,1)
    local_knodes(k) = gmesh(gmesh_number)%knode_from_gnode(local_gnodes(k))
  end do

  if (ubound(icell_from_gelement,1) < gelement) then
    change = max(gelement-ubound(icell_from_gelement,1),int(nelements/10)+1)
    call resize_integer_array(keep_data=.true.,array=icell_from_gelement,change=change)
    call resize_integer_array(keep_data=.true.,array=jface_from_gelement,change=change)
    call resize_integer_array(keep_data=.true.,array=knode_from_gelement,change=change)
  end if
  gelement_max = max(gelement_max,gelement)

! determine based on region and dimension info whether element is a cell, face or node
! default is a cell if the element has the maximum dimensions, otherwise a face
! note, cells should always be a part of a gmsh physical entity originally, and hence always be associated with a region,
!  so if an element is not associated with a region then it must be a face
  if (gtype_list(gtype)%dimensions >= gmesh(gmesh_number)%dimensions) then
    centring = 'cell'
    if (gtype_list(gtype)%dimensions > gmesh(gmesh_number)%dimensions) then
      write(*,'(a,i1,a,i1,a)') "WARNING: an element was found in gmsh file "//trim(gmesh(gmesh_number)%basename)// &
        " that has dimensions higher than those listed in the physical names: increasing gmesh dimensions from ", &
        gmesh(gmesh_number)%dimensions," to ",gtype_list(gtype)%dimensions," as a result, but this may cause problems, "// &
        "namely that any physical regions (names) included within the file may have the wrong centring. "// &
        "It is recommend to include a cell centred physical region within the gmsh file having the maximum dimensions "// &
        "of the mesh. Failing that, define all region centrings explicitly within the arb input file so that none are "// &
        "guessed based on the mesh dimensions."
      gmesh(gmesh_number)%dimensions = gtype_list(gtype)%dimensions
    end if
  else if (gtype_list(gtype)%dimensions == 0 .and. gmesh(gmesh_number)%dimensions /= 1) then
    centring = 'node'
  else
    centring = 'face'
  end if
! now check on the parent region, overwriting above defaults if necessary
  region_number = 0
  if (allocatable_integer_size(tags) >= 1) then
    if (tags(1) /= 0.and.tags(1) <= ubound(gmesh(gmesh_number)%gregion,1)) then
      region_number = gmesh(gmesh_number)%gregion(tags(1))%region_number
      centring = gmesh(gmesh_number)%gregion(tags(1))%centring
    end if
  end if

  if (debug) write(81,*) 'NEW ELEMENT: gelement = ',gelement,': gtype = ',gtype,': name = ',trim(gtype_list(gtype)%name), &
    ': ntags = ',ntags,': tags = ',tags,': local_gnodes = ',local_gnodes,': local_knodes = ',local_knodes, &
    ': gtype_dimensions = ',gtype_list(gtype)%dimensions,': gmesh_dimensions = ',gmesh(gmesh_number)%dimensions, &
    ': centring = ',centring

!------------------
! import as a cell

  if (centring == 'cell') then

    if (debug) write(81,*) 'importing element as a cell'

    nfaces = gtype_list(gtype)%nfaces
    if (nfaces == 0) call error_stop('ERROR: a cell has no faces in the gmsh file '//trim(filename))
    call reset_cell(default_cell)
    call resize_integer_array(array=default_cell%jface,new_size=nfaces)

! loop through face node lists
    do nface = 1, nfaces

      call reset_face(default_face)
      call resize_integer_array(array=default_face%knode,new_size=ubound(gtype_list(gtype)%face_nodes(nface)%node,1))

! assemble face_node list
      do nnode = 1, ubound(default_face%knode,1)
        default_face%knode(nnode) = local_knodes(gtype_list(gtype)%face_nodes(nface)%node(nnode))
      end do

      if (debug) write(81,*) '  nface = ',nface,': default_face%knode = ',default_face%knode

! find face number if this face has been previously defined
      j = jface_from_knode_list(default_face%knode)

! if face is not found then create new one
      if (j == 0) then
        jtotal = jtotal + 1
        if (ubound(face,1) < jtotal) call resize_face_array(change=int(ktotal/10)+1)
        j = jtotal
! find gtype for face that has correct dimension and number of nodes
        default_face%gtype = 0
        do m = 1, ubound(gtype_list,1)
          if (gtype_list(m)%dimensions == gtype_list(gtype)%dimensions - 1.and. &
            gtype_list(m)%nnodes == ubound(default_face%knode,1) ) then
            default_face%gtype = m
            exit
          end if
        end do
        if (default_face%gtype == 0) call error_stop('gtype not identified for generated face in gmsh file '//trim(filename))

! set node jface references for the new face
        call add_jface_to_nodes(j,default_face%knode)
          
        if (debug) write(81,*) 'creating new face: j = ',j,'j: knode = ',default_face%knode,': gtype = ',default_face%gtype
      else
        default_face%gtype = face(j)%gtype
        if (debug) write(81,*) 'overwriting existing face: j = ',j,'j: knode = ',default_face%knode
      end if

      default_face%icell = [ 0, 0 ] ! reset icell(1) later, scrap icell(2)

! set or overwrite the face data to ensure that this cell is the icell=1 one
      default_face%dimensions = gtype_list(default_face%gtype)%dimensions
      call set_face(face_to_set=face(j),new_value=default_face)

      default_cell%jface(nface) = j

    end do

! now check that cell doesn't exist already from knowledge of nodes
    i = icell_from_knode_list(local_knodes)

    if (i == 0) then
      default_cell%gtype = gtype
      default_cell%dimensions = gtype_list(gtype)%dimensions
      call copy_integer_array(original=local_knodes,copy=default_cell%knode)
      itotal = itotal + 1
      if (ubound(cell,1) < itotal) call resize_cell_array(change=int(ktotal/5)+1)
      i = itotal
      call set_cell(cell_to_set=cell(i),new_value=default_cell)
! set node icell references for the new cell
      call add_icell_to_nodes(i,default_cell%knode)
      if (debug) write(81,*) 'creating new cell: i = ',i,': jface = ',default_cell%jface
    end if

! add gmesh lookup reference
    icell_from_gelement(gelement)=i

! set icell(1) so that the surrounding face normals point in the direction of the right-hand rule for each set of face nodes
    do jj = 1, ubound(cell(i)%jface,1)
      j = cell(i)%jface(jj)
      face(j)%icell(1) = i
    end do

! add cell number to region list if it isn't there already
    if (region_number /= 0) then
      if (debug) write(81,*) 'cell i = ',i,': is being added to region = ',trim(region(region_number)%name), &
        ': cell dimensions = ',cell(i)%dimensions,': region dimensions = ',region(region_number)%dimensions
      if (region(region_number)%dimensions /= cell(i)%dimensions) call error_stop('a cell is being added to region '// &
        trim(region(region_number)%name)//' however the dimensions are inconsistent')
      if (region(region_number)%centring /= 'cell') call error_stop('a cell is being added to region '// &
        trim(region(region_number)%name)//' however the centring is inconsistent')
      if (location_in_list(array=region(region_number)%ijk,element=i) == 0) &
        call push_integer_array(array=region(region_number)%ijk,new_element=i)
    end if

!------------------
! import as a face

  else if (centring == 'face') then

    if (debug) write(81,*) 'importing element as a face'

! find face number if this face has been previously defined
    j = jface_from_knode_list(local_knodes)

! if face is not found then create new one
    if (j == 0) then
      call reset_face(default_face)
      call copy_integer_array(original=local_knodes,copy=default_face%knode)
      default_face%gtype = gtype
      default_face%dimensions = gtype_list(gtype)%dimensions
      default_face%icell = [ 0, 0 ]
      jtotal = jtotal + 1
      if (ubound(face,1) < jtotal) call resize_face_array(change=int(ktotal/5)+1)
      j = jtotal
      call set_face(face_to_set=face(j),new_value=default_face)
      if (debug) write(81,*) 'creating new face: j = ',j,'j: knode = ',default_face%knode
! set node jface references for the new face
      call add_jface_to_nodes(j,default_face%knode)
    end if

! add gmesh lookup reference
    jface_from_gelement(gelement)=j

! add face number to region list if it isn't there already
    if (region_number /= 0) then
      if (debug) write(81,*) 'face j = ',j,'j: is being added to region = ',trim(region(region_number)%name), &
        ': face dimensions = ',face(j)%dimensions,': region dimensions = ',region(region_number)%dimensions
      if (region(region_number)%dimensions /= face(j)%dimensions) call error_stop('a face is being added to region '// &
        trim(region(region_number)%name)//' however the dimensions are inconsistent')
      if (region(region_number)%centring /= 'face') call error_stop('a face is being added to region '// &
        trim(region(region_number)%name)//' however the centring is inconsistent')
      if (location_in_list(array=region(region_number)%ijk,element=j) == 0) &
        call push_integer_array(array=region(region_number)%ijk,new_element=j)
    end if

!------------------
! import as a node
! this action is less drastic as nodes have already been listed
  else if (centring == 'node') then

    if (debug) write(81,*) 'importing element as a node'

! sanity check
    if (gtype /= node_gtype) call error_stop("an element was identified as a node but has the wrong gtype in "// &
      trim(region(region_number)%name))
! gelement number needs to be identified and placed in gmesh()%gelement()%knode for lookup
    k = local_knodes(1)
    knode_from_gelement(gelement) = k ! local_knodes is already a stored list (here 1) of the k indices

! add node number to region list if it isn't there already
    if (region_number /= 0) then
      if (debug) write(81,*) 'node k = ',k,'k: is being added to region = ',trim(region(region_number)%name), &
        ': region dimensions = ',region(region_number)%dimensions
      if (region(region_number)%dimensions /= 0) call error_stop('a node is being added to region '// &
        trim(region(region_number)%name)//' however the dimensions are inconsistent')
      if (region(region_number)%centring /= 'node') call error_stop('a node is being added to region '// &
        trim(region(region_number)%name)//' however the centring is inconsistent')
      if (location_in_list(array=region(region_number)%ijk,element=k) == 0) &
        call push_integer_array(array=region(region_number)%ijk,new_element=k)
    end if


!------------------
  end if

end do

! minimise cell and face arrays sizes
if (ubound(cell,1) /= itotal) call resize_cell_array(new_size=itotal)
if (ubound(face,1) /= jtotal) call resize_face_array(new_size=jtotal)

! allocate real gelement lookup arrays
! first copy info already in gelement array (from previous cell mesh reading for instance) into local variables
if (allocated(gmesh(gmesh_number)%gelement)) then
  gelement_previous = ubound(gmesh(gmesh_number)%gelement,1)
  if (gelement_previous > gelement_max) then
    call resize_integer_array(keep_data=.true.,array=icell_from_gelement,new_size=gelement_previous)
    call resize_integer_array(keep_data=.true.,array=jface_from_gelement,new_size=gelement_previous)
    call resize_integer_array(keep_data=.true.,array=knode_from_gelement,new_size=gelement_previous)
  end if
  do gelement = 1, gelement_previous
    if (gmesh(gmesh_number)%gelement(gelement)%icell /= 0) then
      if (icell_from_gelement(gelement) == 0) then
        icell_from_gelement(gelement) = gmesh(gmesh_number)%gelement(gelement)%icell
      else if (icell_from_gelement(gelement) /= gmesh(gmesh_number)%gelement(gelement)%icell) then
        call error_stop("cell element mismatch between elements from centringinput for "//trim(filename))
      end if
    end if
    if (gmesh(gmesh_number)%gelement(gelement)%jface /= 0) then
      if (jface_from_gelement(gelement) == 0) then
        jface_from_gelement(gelement) = gmesh(gmesh_number)%gelement(gelement)%jface
      else if (jface_from_gelement(gelement) /= gmesh(gmesh_number)%gelement(gelement)%jface) then
        call error_stop("face element mismatch between elements from centringinput for "//trim(filename))
      end if
    end if
    if (gmesh(gmesh_number)%gelement(gelement)%knode /= 0) then
      if (knode_from_gelement(gelement) == 0) then
        knode_from_gelement(gelement) = gmesh(gmesh_number)%gelement(gelement)%knode
      else if (knode_from_gelement(gelement) /= gmesh(gmesh_number)%gelement(gelement)%knode) then
        call error_stop("node element mismatch between elements from centringinput for "//trim(filename))
      end if
    end if
  end do
  gelement_max = max(gelement_max,gelement_previous)
  deallocate(gmesh(gmesh_number)%gelement)
end if

! now copy local arrays to global ones
allocate(gmesh(gmesh_number)%gelement(gelement_max))
do gelement = 1, gelement_max
  gmesh(gmesh_number)%gelement(gelement)%icell = icell_from_gelement(gelement)
  gmesh(gmesh_number)%gelement(gelement)%jface = jface_from_gelement(gelement)
  gmesh(gmesh_number)%gelement(gelement)%knode = knode_from_gelement(gelement)
end do
deallocate(icell_from_gelement,jface_from_gelement,knode_from_gelement)

end subroutine read_gmesh_elements

!-----------------------------------------------------------------

subroutine read_gmesh_nodes(check,gmesh_number,filename,debug)

use general_module
integer :: error, k, nnodes, gnode_check, gmesh_number, gnode_max
integer, dimension(:), allocatable :: gnode
character(len=1000) :: textline, filename
double precision, dimension(totaldimensions) :: x_check
logical :: check, debug

rewind(fgmsh)
do 
  read(fgmsh,'(a)',iostat=error) textline
! if (error /= 0) call error_stop('list of nodes not found in gmsh file '//trim(filename))
  if (error /= 0) then
    write(*,'(a)') "WARNING: list of nodes not found in gmsh file "//trim(filename)
    return
  end if
  if (trim(textline) == '') cycle
  if (trim(textline) == "$Nodes") exit
end do
read(fgmsh,*,iostat=error) nnodes
if (error /= 0) call error_stop('problem reading in number of nodes in gmsh file '//trim(filename))
if (debug) write(*,*) 'number of nodes: nnodes = ',nnodes
if (.not.check) then
  call resize_node_array(change=nnodes)
  allocate(gnode(nnodes))
  do k = 1, nnodes
    read(fgmsh,*,iostat=error) gnode(k),node(k+ktotal)%x
    if (error /= 0) call error_stop('problem reading nodes in gmsh file '//trim(filename))
    node(k+ktotal)%x = transform_coordinates(node(k+ktotal)%x)*gmesh(gmesh_number)%input_scale
  end do
  ! now create lookup array
  allocate(gmesh(gmesh_number)%knode_from_gnode(maxval(gnode)))
  gmesh(gmesh_number)%knode_from_gnode = 0
  do k = 1, nnodes
    gmesh(gmesh_number)%knode_from_gnode(gnode(k)) = k + ktotal
  end do
  deallocate(gnode)
  ktotal = ktotal + nnodes
else
  gnode_max = 0
  do k = 1, nnodes
    read(fgmsh,*,iostat=error) gnode_check,x_check
    x_check = transform_coordinates(x_check*gmesh(gmesh_number)%input_scale)
    if (error /= 0) call error_stop('problem reading nodes in gmsh file '//trim(filename))
    if (distance(x_check,node(gmesh(gmesh_number)%knode_from_gnode(gnode_check))%x) > 1.d-10) call error_stop( &
      "node mismatch between msh files: cell and face msh files must come from the same simulation")
    gnode_max = max(gnode_max,gnode_check)
  end do
  if (ubound(gmesh(gmesh_number)%knode_from_gnode,1) /= gnode_max) call error_stop("number of nodes mismatched between msh "// &
    "files:  cell and face msh files must come from the same simulation")
end if

end subroutine read_gmesh_nodes

!-----------------------------------------------------------------

function transform_coordinates(x)

! function to apply some type of transformation to the gmsh-generated coordinates

use general_module
! both three dimensional arrays, having components in the x/y/z direction
double precision, dimension(totaldimensions) :: transform_coordinates
double precision, dimension(totaldimensions), intent(in) :: x

transform_coordinates = x ! the default is no transformation (leave this in there)

! apply any transformations below this

!transform_coordinates(1) = 2.d0*transform_coordinates(1) + 2.d0 ! eg, translation and scaling of the x coordinate
!transform_coordinates = x*1.d-9 ! uniform scaling can now be handled directly by inputscale option (listed on MSH_FILE line)

end function transform_coordinates

!-----------------------------------------------------------------

subroutine read_gmesh_regions(check,gmesh_number,filename,debug)

use general_module
integer :: error, ngregions, n, gmesh_number, region_number
integer, dimension(:), allocatable :: gregion_dimensions, gregion_number
character(len=1000), dimension(:), allocatable :: gregion_name
character(len=1000) :: formatline, textline, filename, location
character(len=4) :: centring
type(region_type) :: default_element ! this is the element that will be added to region
logical :: check, debug, guess_centring

rewind(fgmsh)
do 
  read(fgmsh,'(a)',iostat=error) textline
! if (error /= 0) call error_stop('list of physical names not found in gmsh file '//trim(filename))
  if (error /= 0) then
    write(*,'(a)') "WARNING: list of physical names not found in gmsh file "//trim(filename)
    write(*,'(a)') "WARNING: assuming mesh is 3 dimensional given the lack of physical regions"
    gmesh(gmesh_number)%dimensions = 3
    return
  end if
  if (trim(textline) == '') cycle
  if (trim(textline) == "$PhysicalNames") exit
end do
read(fgmsh,*,iostat=error) ngregions
if (error /= 0) call error_stop('problem reading in number of physical names in gmsh file '//trim(filename))
if (debug) write(*,*) 'number of physical names: ngregions = ',ngregions

! run through physical entities finding dimension and gregion_numbers
allocate(gregion_number(ngregions),gregion_dimensions(ngregions),gregion_name(ngregions))
do n=1,ngregions
  read(fgmsh,*,iostat=error) gregion_dimensions(n),gregion_number(n),gregion_name(n)
  if (error /= 0) call error_stop('problem reading physical name in gmsh file '//trim(filename))
! for Pointwise compatibility, add on <> delimiters if not already present, and add dimension onto Unspecified regions to make them unique
  if (trim(gregion_name(n)) == "Unspecified") write(gregion_name(n),'(a,i1)') "Unspecified_dim",gregion_dimensions(n)
  if (gregion_name(n)(1:1) /= "<") gregion_name(n) = "<"//trim(gregion_name(n))//">"
  if (debug) write(*,*) gregion_dimensions(n), gregion_number(n), trim(gregion_name(n))
end do
if (debug) write(*,*) ' gmesh dimensions = ',maxval(gregion_dimensions)
if (check) then
  if (gmesh(gmesh_number)%dimensions /= maxval(gregion_dimensions)) call error_stop( &
    "dimension mismatch between msh files: all .msh files (of different centring) must come from the same simulation")
else
  gmesh(gmesh_number)%dimensions = maxval(gregion_dimensions)
  write(*,'(a,i1,a)') "INFO: "//trim(gmesh(gmesh_number)%basename)// &
    " gmsh file is ",gmesh(gmesh_number)%dimensions," dimensional based upon included physical names"
end if

if (.not.check) then
  allocate(gmesh(gmesh_number)%gregion(maxval(gregion_number))) ! create a listing of the gmsh physical regions referenced by their gmsh number
  gmesh(gmesh_number)%gregion(:)%region_number = 0 ! default value of 0 indicates this region is not allocated to an arb region
  gmesh(gmesh_number)%gregion(:)%centring = ''
end if
do n=1,ngregions
! region possibilities for non-check invocation
! 1) nothing has been set: centring is calculated from dimension related to the rest of the mesh file, and type is set to gmsh
! 2) region is identified, but type is system - error
! 3) region is identified, type is not gmsh - check centring, set/check dimensions, and continue, noting that data may be read in for this region
! 4) region is identified, type is gmsh - check centring, set/check dimensions, set/append location string

  region_number = region_number_from_name(name=gregion_name(n))
  formatline = '(a,'//trim(dindexformat(gregion_number(n)))//',a)'
  write(location,fmt=formatline) 'gmsh physical entity number ',gregion_number(n),' from file '//trim(filename)

! work out whether we're going to have to guess the centring of this region, and if so, do it
  guess_centring = .false.
  if (.not.check) then
    if (region_number == 0) then
      guess_centring = .true.
    else if (trim(region(region_number)%centring) == "") then
      guess_centring = .true.
    end if
    if (guess_centring) then
      if (gregion_dimensions(n) == gmesh(gmesh_number)%dimensions) then
        centring = "cell" ! for 3D 3: for 2D 2: for 1D 1.
      else if (gregion_dimensions(n) == 0 .and. gmesh(gmesh_number)%dimensions /= 1) then
  ! if dimension is 0 then this is a node region, unless mesh is one 1D, when a 0 dimension element defaults to a face and nodes have to be specified
        centring = "node" ! for 3D 0: for 2D 0: for 1D no default.
      else 
        centring = "face" ! for 3D 2,1: for 2D 1: for 1D 0.
        if (gregion_dimensions(n) < gmesh(gmesh_number)%dimensions-1) &
          write(*,'(a)') 'WARNING: region '//trim(gregion_name(n))//' in gmsh file '//trim(filename)// &
          ' assumed to have face centring based on dimensions relative to the other elements in this file, however the '// &
          'dimension is one less than it should be probably indicating an error.  Maybe some regions need to have their '// &
          'centring explicitly set in the arb input file.'
      end if
    end if
  end if

  if (region_number == 0) then
! region does not yet exist: determine centring and create new region
    if (check) call error_stop('region '//trim(gregion_name(n))//' in gmsh file '//trim(filename)// &
      ' is not properly defined on current read: all centring files with the same basename must come from the '// &
      'same simulation and have the same physical entities defined')
    if (debug) write(*,*) ' before read: region '//trim(gregion_name(n))//': not previously defined'
! centring is now set for this case above
! create new region and set gregion region_number at the same time
    default_element%name = gregion_name(n)
    default_element%centring = centring
    default_element%type = 'gmsh'
    default_element%dimensions = gregion_dimensions(n)
    default_element%location%active = .false.
    default_element%location%description = location
    default_element%initial_location%active = .false.
    default_element%initial_location%description = ""
    call resize_region_array(new_element=default_element,change=1)
    region_number = ubound(region,1)

    gmesh(gmesh_number)%gregion(gregion_number(n))%region_number = region_number
    gmesh(gmesh_number)%gregion(gregion_number(n))%centring = centring

  else
! this region already has a valid fortran definition

! if this is being called with check, then the region should already be defined
    if (check) then
      if (gmesh(gmesh_number)%gregion(gregion_number(n))%region_number /= region_number) call error_stop('region '// &
        trim(gregion_name(n))//' in gmsh file '//trim(filename)//' has inconsistent region_number on reread')
    else

      if (debug) write(*,*) ' before read: region '//trim(gregion_name(n))//': region number ',region_number, &
        ': centring = '//trim(region(region_number)%centring)//': type = '//trim(region(region_number)%type)// &
        ': location '//trim(region(region_number)%location%description)//': dimensions = ',region(region_number)%dimensions

! first check that it isn't a system region
! we don't check on internal names as the fortran has no record of them
      if (trim(region(region_number)%type) == "system") call error_stop('the region '//trim(gregion_name(n))// &
        ' in gmsh file '//trim(filename)//' is attempting to use a system region name - you need to change this region name')

! now check on centring, setting it if it was deemed necessary above
      if (guess_centring) then
        region(region_number)%centring = centring
      else if (trim(region(region_number)%centring) == "none") then
        call error_stop('region '//trim(gregion_name(n))// &
          ' in gmsh file '//trim(filename)//' has a centring of '//trim(region(region_number)%centring)// &
          ' that is inconsistent with a previous definition of this region')
      end if

! set/check on dimensions
      if (region(region_number)%dimensions == -1) then
        region(region_number)%dimensions = gregion_dimensions(n) ! set dimensions if not already set
      else if (region(region_number)%dimensions /= gregion_dimensions(n)) then ! or if already set but inconsistent, then this is an error
        call error_stop('region '//trim(gregion_name(n))// &
          ' in gmsh file '//trim(filename)//' has dimensions that are inconsistent with a previous definition of this region')
      end if

! and if this is a gmsh region, keep a record of all locations that are used for read ins
      if (trim(region(region_number)%type) == 'gmsh' .and. trim(region(region_number)%location%description) == '') then
        region(region_number)%location%description = trim(location)
      else if (trim(region(region_number)%type) == 'gmsh') then
        region(region_number)%location%description = trim(region(region_number)%location%description)//' + '//trim(location) ! concatenating
      else
! otherwise let the user know that a region is about to be ignored
        write(*,'(a)') "NOTE: region "//trim(gregion_name(n))//" defined in the gmsh file "//trim(filename)// &
          " conflicts with a region definition given in the arb input file:  the region definition in the current gmsh file "// &
          "will be ignored"
      end if

! this allows data from multiple mesh files to be read into the same region
      gmesh(gmesh_number)%gregion(gregion_number(n))%region_number = region_number
      gmesh(gmesh_number)%gregion(gregion_number(n))%centring = region(region_number)%centring

    end if

  end if

  if (debug) write(*,*) ' after read: region '//trim(gregion_name(n))//': region number ',region_number, &
    ': centring = '//trim(region(region_number)%centring)//': type = '//trim(region(region_number)%type)// &
    ': location '//trim(region(region_number)%location%description)//': dimensions = ',region(region_number)%dimensions

end do
deallocate(gregion_number,gregion_dimensions,gregion_name)

end subroutine read_gmesh_regions

!-----------------------------------------------------------------

end module gmesh_module

!-----------------------------------------------------------------
