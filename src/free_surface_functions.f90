! file src/free_surface_functions.f90
!
! Copyright 2009-2015 Dalton Harvie (daltonh@unimelb.edu.au)
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
arb_external_preamble ! an arb flag to say everything between this and the next arb_external_(setup|preamble|contents) is included in the preamble of equation_module.f90

! statements specifying different data types and parameters
double precision :: normal_magnitude_tol = 1.d-8 ! how close in squared sense normal magnitude must be to 1
double precision :: phitol_default = 1.d-8 ! default tolerance on phi calculations
double precision :: extratol = 1.d-2 ! for convergence of phi loops, this is multiplied by phitol to get actual phi accuracy within these loops

! these are expandable lists that used throughout, and need to be individually allocated for each thread
type(vector_list_type), dimension(:), allocatable, save :: cellknode_xr
type(scalar_list_type), dimension(:), allocatable, save :: cellknode_d
type(integer_list_type), dimension(:), allocatable, save :: cellknode_kkorder
type(integer_list_type), dimension(:), allocatable, save :: face_kkin, face_kkout
type(vector_list_type), dimension(:), allocatable, save :: face_normal
type(scalar_list_type), dimension(:), allocatable, save :: face_d
type(scalar_list_type), dimension(:), allocatable, save :: flux_list, phif_list, areadvol_list, deltaphi_list, cc_list

! these are the only subroutines that are accessible from outside this module
public cellvofd, facevofphi, cellvofphishape, cellvofphiadjust

!-----------------------------------------------------------------
arb_external_setup ! an arb flag to say everything between this and the next arb_external_(setup|preamble|contents) is included in the setup section (allocate_meta_arrays subroutine) of equation_module.f90

! allocate some lists that will be used in the free surface routines
allocate(cellknode_xr(nthreads))
allocate(cellknode_d(nthreads))
allocate(cellknode_kkorder(nthreads))
allocate(face_kkin(nthreads),face_kkout(nthreads))
allocate(face_normal(nthreads))
allocate(face_d(nthreads))
allocate(flux_list(nthreads), phif_list(nthreads), areadvol_list(nthreads), deltaphi_list(nthreads), cc_list(nthreads))
! now preallocate all of these lists to the maximum possible sizes to avoid run-time length checking
do n = 1, nthreads
  allocate(cellknode_xr(n)%elements(totaldimensions,maximum_cellknodes))
  allocate(cellknode_d(n)%elements(maximum_cellknodes))
  allocate(cellknode_kkorder(n)%elements(maximum_cellknodes))
  allocate(face_normal(n)%elements(totaldimensions,maximum_celljfaces))
  allocate(face_d(n)%elements(maximum_celljfaces))
  allocate(flux_list(n)%elements(maximum_celljfaces))
  allocate(phif_list(n)%elements(maximum_celljfaces))
  allocate(areadvol_list(n)%elements(maximum_celljfaces))
  allocate(deltaphi_list(n)%elements(maximum_celljfaces+2))
  allocate(cc_list(n)%elements(maximum_celljfaces))
  allocate(face_kkin(n)%elements(maximum_faceknodes))
  allocate(face_kkout(n)%elements(maximum_faceknodes))
end do

!-----------------------------------------------------------------
arb_external_contents ! an arb flag to say everything between this and the next arb_external_(setup|preamble|contents) is included in the main section of equation_module.f90

!-----------------------------------------------------------------

arb_external_operator cellvofd ! an arb flag to indicate an external operator is contained
subroutine cellvofd(thread,m,ilast,jlast,klast,error_string,deriv,msomeloop_phi,msomeloop_normal_l1,msomeloop_normal_l2, &
  msomeloop_normal_l3,msomeloop_phitol,method)

! in this subroutine we find the d variable such that the volume fraction in the cell i is bounded by the line
!  n \cdot (x - x_c) - d = 0
! unit normal n points into the disperse (phi=1) phase
! called by the arb function cellvofd
! i is cell index
! someloop(thread)%funk(m) is the variable that is to contain d (and possibly its derivative)
! the msomeloops contain the funk indices that contain: phi, the three unitnormals (normal1->normal3) and phitol
! if any msomeloop index = -1, then the someloop variable is not defined
! method is an integer that specifies how d is calculated:  1 = linearone, 2 = lineartwo, 3 = parabolic fit to max, min and centroid, 4 = exactpiecewise means match volumes on the nodes exactly, but use linear interpolation between the node, 5 = exact, 6 = best (best method available for the dimension)

use general_module
! these are passed to all external routines
integer :: thread,m,ilast,jlast,klast,deriv
character(len=1000) :: error_string
! specific arguments passed to cellvofd
integer :: msomeloop_phi,msomeloop_phitol,msomeloop_normal_l1,msomeloop_normal_l2,msomeloop_normal_l3,method
! local variables
integer, dimension(totaldimensions) :: msomeloop_normal
integer :: k, kk, l, kk_max, kk_min, i, kkkcurrent, kkcurrent, kcurrent, kkk, j, jj, location, kknext, n, method_l
double precision :: normal_magnitude, d_max, d_min, d, phi, phitol, d_node, vcurrent, vtarget, vnext, dnext, dcurrent, &
  d1, d2, d3, d4, v1, v2, v3, v4, reciprocal_sign, faceareamax, dtol, vtol
double precision, dimension(0:2) :: aatotal ! this should be private is it is not saved
double precision, dimension(maximum_celljfaces,0:2) :: aaface ! this should be private is it is not saved
double precision, dimension(totaldimensions) :: normal, pass_vector
integer, parameter :: nmax = 100000
logical, parameter :: reciprocal = .true. ! if phi < 0.5 solve for 1-phi instead, to reduce the number of geometrical operations
logical :: debug = .false.

if (debug) write(50,'(80(1h+)/a)') 'subroutine cellvofd'

if (deriv > 0) call error_stop("subroutine cellvofd cannot handle derivative yet")

! form normal indices into a vector
msomeloop_normal = [msomeloop_normal_l1,msomeloop_normal_l2,msomeloop_normal_l3]
i = ilast

if (debug) then
  write(50,*) 'm,ilast,jlast,klast,thread = ',m,ilast,jlast,klast,thread
  write(50,*) 'deriv = ',deriv
  write(50,*) 'msomeloop_phi = ',msomeloop_phi
  write(50,*) 'msomeloop_normal = ',msomeloop_normal
  write(50,*) 'msomeloop_phitol = ',msomeloop_phitol
  write(50,*) 'method = ',method
end if

if (cell(i)%dimensions == 0) call error_stop("cellvofd being called on zero dimensional cell")

! set method to be used based on cell dimension if best method in use
if (cell(i)%dimensions == 1) then
  method_l = 1 ! all methods are equivalent to this simplest method for lines
else if (method == 0) then ! the method == 0 flag indicates the 'best' method
  if (cell(i)%dimensions == 2.and.cell(i)%type == 1) then
    method_l = 5 ! exact
  else
    method_l = 1 ! linear for 3D domain and boundary cells
  end if
else
  method_l = method
end if
if (debug) then
  write(50,*) 'cell(i)%dimensions = ',cell(i)%dimensions
  write(50,*) 'method_l = ',method_l
end if

! save phi in a temporary variable
if (msomeloop_phi <= 0) call error_stop('error with msomeloop_phi passed to cellvofd')
phi = someloop(thread)%funk(msomeloop_phi)%v

! find phitol
if (msomeloop_phitol > 0) then
  phitol = someloop(thread)%funk(msomeloop_phitol)%v
else
  phitol = phitol_default
end if

! check that we are in interface region, and if not, silently exit with d = 0
! NB: in facevofphi if cell has phi in this range then d is not used
if (phi < phitol.or.phi > 1.d0-phitol) then
  someloop(thread)%funk(m)%v = 0.d0 ! set the final d to zero and silently exit
  return
end if

! if phi is small, swap calculation around to reduce geometric calculations
reciprocal_sign = 1.d0
if (reciprocal) then
  if (phi < 0.5d0) then
    reciprocal_sign = -1.d0
    phi = 1.d0 - phi
  end if
end if

! set normal variable based on msomeloop m's
do l = 1, 3
  if (msomeloop_normal(l) > 0) then
    normal(l) = reciprocal_sign*someloop(thread)%funk(msomeloop_normal(l))%v
  else
    normal(l) = 0.d0
  end if
end do
! check on normal magnitudes here
normal_magnitude = dot_product(normal,normal)
if (debug) then
  write(50,*) 'phi = ',phi
  write(50,*) 'phitol = ',phitol
  write(50,*) 'normal = ',normal
  write(50,*) 'normal_magnitude = ',normal_magnitude
end if
!if (abs(normal_magnitude - 1.d0) > normal_magnitude_tol) call error_stop('normal passed to cellvofd is not a unit normal')
if (abs(normal_magnitude - 1.d0) > normal_magnitude_tol) then
  someloop(thread)%funk(m)%v = 0.d0 ! set the final d to zero and silently exit
  return
end if

! push all (relative) nodes from cell into node_x_list
cellknode_xr(thread)%length = 0 ! reset node list
do kk = 1, ubound(cell(i)%knode,1)
  k = cell(i)%knode(kk)
  pass_vector=node(k)%x-cell(i)%x
  call add_to_vector_list(list=cellknode_xr(thread),new_element=pass_vector)
  if (debug) write(50,*) 'k,kk,x_relative = ',k,kk,cellknode_xr(thread)%elements(:,kk)
end do

! loop through all nodes finding d_max and d_min
kk_max = 0
kk_min = 0
d_max = -huge(0.d0) ! this will be larger than zero here
d_min = +huge(0.d0) ! this will be smaller than zero here
if (method_l > 3) cellknode_d(thread)%length = 0
do kk = 1, cellknode_xr(thread)%length
  if (debug) write(50,*) 'kk,cellknode_xr(thread)%elements(:,kk) = ',kk,cellknode_xr(thread)%elements(:,kk)
  d_node = dot_product(normal,cellknode_xr(thread)%elements(:,kk))
  if (method_l > 3) call add_to_scalar_list(list=cellknode_d(thread),new_element=d_node) ! for exact method need d calculated for each node
  if (d_node > d_max) then
    d_max = d_node
    kk_max = kk
  end if
  if (d_node < d_min) then
    d_min = d_node
    kk_min = kk
  end if
end do

if (debug) then
  write(50,*) 'kk_max,d_max = ',kk_max,d_max
  write(50,*) 'kk_min,d_min = ',kk_min,d_min
end if

! first deal with (nearly) full or empty cells
if (phi < phitol) then

  d = d_max

else if (phi > 1.d0-phitol) then

  d = d_min

else if (method_l == 1) then
! linearone method, using linear interpolation over one range between d_max (phi=0) and d_min (phi=1)
! for a single dimension line, this is exact, so actually all methods are equivalent to this for a line

  d = phi*d_min+(1.d0-phi)*d_max

else if (method_l == 2) then
! lineartwo method, using linear interpolation over the two ranges between d_max (phi=0), centroid (d=0, phi=0.5) and d_min (phi=1)
! for now exact doesn't work in 3D, so this is the best (method 6) option available

  if (phi < 0.5d0) then
    d = -(2.d0*phi-1.d0)*d_max
  else
    d = (2.d0*phi-1.d0)*d_min
  end if

! method 3 is supposed to be parabolic, between centre and two extremes, but not coded yet

else if (method_l >= 4 .and. method_l <= 5) then
! exact and exactpiecewise methods

! create list of each node kk index (ie, position in cell(i)%knode) in order of increasing d
  cellknode_kkorder(thread)%length = ubound(cell(i)%knode,1)
! initialise list as 1,2,3,... etc
  do kk = 1, cellknode_kkorder(thread)%length
    cellknode_kkorder(thread)%elements(kk) = kk
  end do

  if (debug) then
    write(50,*) 'before sorting knodes:'
    write(50,*) 'd: ',(cellknode_d(thread)%elements(kk),kk=1,cellknode_d(thread)%length)
    write(50,*) 'kkorder: ',(cellknode_kkorder(thread)%elements(kk),kk=1,cellknode_kkorder(thread)%length)
    write(50,*) 'dorder: ',(cellknode_d(thread)%elements(cellknode_kkorder(thread)%elements(kk)),kk=1, &
      cellknode_kkorder(thread)%length)
  end if

! and now order it
  call sort_scalar_index_list(index_list=cellknode_kkorder(thread),value_list=cellknode_d(thread),direction=1.d0)

  if (debug) then
    write(50,*) 'after sorting knodes:'
    write(50,*) 'kkorder: ',(cellknode_kkorder(thread)%elements(kk),kk=1,cellknode_kkorder(thread)%length)
    write(50,*) 'dorder: ',(cellknode_d(thread)%elements(cellknode_kkorder(thread)%elements(kk)),kk=1, &
      cellknode_kkorder(thread)%length)
  end if

  faceareamax = 0.d0
  do jj = 1, ubound(cell(i)%jface,1)
    faceareamax = max(faceareamax,face(cell(i)%jface(jj))%area)
  end do
  dtol = phitol*cell(i)%vol*extratol/faceareamax
  if (debug) write(50,*) 'faceareamax,dtol = ',faceareamax,dtol
  aaface = 0.d0 ! these are the area polynomial coefficients for each individual face - zero all
  vtarget = cell(i)%vol*(1.d0-phi) ! volume of continuous phase corresponding to phi
  vnext = -vtarget ! volume of continuous phase (phi=0) with interface passing through last next node, now relative to vtarget (so we are actually finding vcurrent = 0 now)
  if (debug) write(50,*) 'vtarget = ',vtarget,': vnext = ',vnext
! now loop through node, moving it from inside the continuous phase (phi=0) to outside the continuous phase (phi=1)
  do kkkcurrent = 1, cellknode_kkorder(thread)%length-1
    kkcurrent = cellknode_kkorder(thread)%elements(kkkcurrent)
    kcurrent = cell(i)%knode(kkcurrent)
    vcurrent = vnext ! update v to the current node
    if (debug) then
      write(50,*) 'kkcurrent, kcurrent, vcurrent'
      write(50,*) kkcurrent, kcurrent, vcurrent
    end if
! loop through each face seeing if this node is a member
    FACELOOP: do jj = 1, ubound(cell(i)%jface,1)
      j = cell(i)%jface(jj)
      location = location_in_list(array=face(j)%knode,element=kcurrent)
      if (debug) write(50,*) 'checking face: j = ',j,': face(j)%knode = ',face(j)%knode,': location = ',location
      if (location > 0) then ! node is on this face
        if (debug) write(50,*) 'face contains kcurrent:  remove previous aaface'
        aaface(jj,:) = 0.d0
! reset kkin/kkout lists and add the current node to the kkin list
        face_kkin(thread)%length = 0
        face_kkout(thread)%length = 0
        call add_to_integer_list(list=face_kkin(thread),new_element=kkcurrent)
! check if any nodes are in the kkout list, forming it as we go (if any are in this list, then the whole list will be required)
        do kkk = kkkcurrent+1, cellknode_kkorder(thread)%length
          kk = cellknode_kkorder(thread)%elements(kkk)
          location = location_in_list(array=face(j)%knode,element=cell(i)%knode(kk))
          if (location > 0) then
            if (face_kkout(thread)%length == 0) then
              if (cellknode_d(thread)%elements(kk) - cellknode_d(thread)%elements(kkcurrent) < dtol) then
                if (debug) write(50,*) 'face is current but d change over face is less than dtol: ignore aaface calculation'
                cycle FACELOOP
              end if
            end if
            call add_to_integer_list(list=face_kkout(thread),new_element=kk)
          end if
        end do
        if (face_kkout(thread)%length > 0) then
          if (debug) write(50,*) 'face is current: calculate new aaface'
! face is still (or just become) current, so calculate contribution to cross-sectional area in terms
!  of the aa polynomial coefficients
! face_kkout is now complete, but kkin needs the earlier elements checked
          do kkk = 1, kkkcurrent-1
            kk = cellknode_kkorder(thread)%elements(kkk)
            location = location_in_list(array=face(j)%knode,element=cell(i)%knode(kk))
            if (location > 0) call add_to_integer_list(list=face_kkin(thread),new_element=kk)
          end do
          if (debug) then
            write(50,*) 'face_kkin -> knodes: ',(cell(i)%knode(face_kkin(thread)%elements(n)),n=1,face_kkin(thread)%length)
            write(50,*) 'face_kkin -> d: ',(cellknode_d(thread)%elements(face_kkin(thread)%elements(n)),n=1, &
              face_kkin(thread)%length)
            write(50,*) 'face_kkout -> knodes: ',(cell(i)%knode(face_kkout(thread)%elements(n)),n=1,face_kkout(thread)%length)
            write(50,*) 'face_kkout -> d: ',(cellknode_d(thread)%elements(face_kkout(thread)%elements(n)),n=1, &
              face_kkout(thread)%length)
          end if
! calculate aaface here
! function calc_aaface(i,j,d,x,kkin,kkout,kk_max,kk_min)
          aaface(jj,:) = calc_aaface(i=i,j=j,d=cellknode_d(thread),x=cellknode_xr(thread),kkin=face_kkin(thread), &
            kkout=face_kkout(thread),kk_max=kk_max,kk_min=kk_min)
          if (debug) write(50,*) 'face is current and requires contribution: aaface(jj,:) = ',aaface(jj,:)
        else
          if (debug) write(50,*) 'face has just become no longer current (contains kcurrent but all nodes are in)'
        end if
      else
        if (debug) write(50,*) 'face does not contain kcurrent node:  aaface unchanged'
      end if
    end do FACELOOP

    dcurrent = cellknode_d(thread)%elements(kkcurrent)
    kknext = cellknode_kkorder(thread)%elements(kkkcurrent+1)
    dnext = cellknode_d(thread)%elements(kknext)
    if (dnext - dcurrent > dtol) then
      if (debug) write(50,*) 'calculating new vnext'
      do n = 0, 2
        aatotal(n) = sum(aaface(:,n)) 
      end do
      vnext = calc_v(aatotal,dcurrent=dcurrent,d=dnext) + vcurrent
    else
      if (debug) write(50,*) 'dnext - dcurrent small, so not calculating volume increment for this node combination'
      vnext = vcurrent
    end if

    if (debug) then
      write(50,*) 'aatotal = ',aatotal
      write(50,*) 'dcurrent = ',dcurrent,': dnext = ',dnext
      write(50,*) 'vcurrent = ',vcurrent,': vnext = ',vnext
      write(50,*) 'kkcurrent = ',kkcurrent,': kknext = ',kknext
    end if
    if (vnext >= 0.d0) exit ! we have bound vtarget between vcurrent and vnext, so exit
  end do

  if (debug) write(50,*) 'vtarget bracketed'

  vtol = cell(i)%vol*phitol*extratol ! like dtol, vtol is tighter than phitol
! first guard against the root not being bounded or one of the bounds being very close to a root
  if (vnext <= vtol) then
    d = dnext
  else if (vcurrent >= -vtol) then
    d = dcurrent
  else if (abs(vnext-vcurrent) > vtol.and.abs(dnext-dcurrent) > dtol) then
! if we are here then the root has been bounded and the range of v is significant
    if (method_l == 4) then ! exactpiecewise: exact matching of the verticies, but linear interpolation between 
      d = -vcurrent*(dnext-dcurrent)/(vnext-vcurrent)+dcurrent
    else ! exact: iterate to find exact d
! bisection method
! rename variables first, and subtract vtarget from v to make it a root finding process
      d2 = dnext ! top of node range and initial top of iteration range
      v2 = vnext
      d1 = dcurrent ! bottom of node range and initial bottom of iteration range
      v1 = vcurrent
      do n = 1, nmax
        if (debug) write(50,*) 'start top: d2,v2 = ',d2,v2
        if (debug) write(50,*) 'start bottom: d1,v1 = ',d1,v1
        d3 = (d1+d2)/2.d0
        if (abs(d2-d1) <= dtol) then
          d = d3
          exit ! converged based on size of d range
        end if
        v3 = calc_v(aatotal,dcurrent=dcurrent,d=d3) + vcurrent
        if (abs(v3) <= phitol*cell(i)%vol*extratol) then
          d = d3
          exit ! converged based on v3
        end if
        if (debug) write(50,*) 'new midpoint: d3,v3 = ',d3,v3
        if (.true.) then ! ridder's method 
          d4 = d3 + (d3-d1)*sign(1.d0,v1-v2)*v3/sqrt(v3**2-v1*v2) ! create better estimate using midpoint function
          v4 = calc_v(aatotal,dcurrent=dcurrent,d=d4) + vcurrent ! and evaluate v4
          if (debug) write(50,*) 'new ridders: d4,v4 = ',d4,v4
          if (debug) then
            write(50,*) 'd1,d2,d3,d4'
            write(50,*) d1,d2,d3,d4
            write(50,*) 'v1,v2,v3,v4'
            write(50,*) v1,v2,v3,v4
          end if
          if (abs(v4) <= phitol*cell(i)%vol*extratol) then
            d = d4
            exit ! converged based on v4
          else if (v4 > 0.d0) then
            d2 = d4
            v2 = v4
            if (v3 < 0.d0) then
              d1 = d3
              v1 = v3
            end if
          else
            d1 = d4
            v1 = v4
            if (v3 > 0.d0) then
              d2 = d3
              v2 = v3
            end if
          end if
        else ! straight bisection
          if (v3 > 0.d0) then
            d2 = d3
          else
            d1 = d3
          end if
        end if
        if (n == nmax - 10) then
          debug = .true.
          write(50,*) 'ERROR: d is not converging in subroutine cellvofd'
          write(50,*) 'n,i,j,thread'
          write(50,*) n,i,j,thread
          write(50,*) 'phi,phitol,normal_magnitude'
          write(50,*) phi,phitol,normal_magnitude
          write(50,*) 'normal'
          write(50,*) normal
          write(50,*) 'd1,d2,d3,d4'
          write(50,*) d1,d2,d3,d4
          write(50,*) 'v1,v2,v3,v4'
          write(50,*) v1,v2,v3,v4
          write(50,*) 'aatotal'
          write(50,*) aatotal
        end if
        if (n == nmax) call error_stop("d is not converging in subroutine cellvofd")
      end do
      if (debug) write(50,*) 'bisection method took n = ',n,' iterations'
    end if
  else
! root has been bounded but the range is very small
    d = (dnext+dcurrent)/2.d0
  end if

else
  call error_stop("subroutine cellvofd cannot handle this method yet")
end if

if (debug) write(50,*) 'd = ',d
someloop(thread)%funk(m)%v = d*reciprocal_sign ! set the final someloop value

if (debug) write(50,'(a/80(1h+))') 'subroutine cellvofd'

end subroutine cellvofd

!-----------------------------------------------------------------

pure function calc_v(aatotal,dcurrent,d)

! with the cross-sectional interface area defined by
!  aatotal(2)*d**2+aatotal(1)*d+aatotal(0)
! integrate from dcurrent to d to find the included volume
double precision, dimension(0:2), intent(in) :: aatotal
double precision, intent(in) :: dcurrent,d
double precision :: calc_v
integer :: n

calc_v = 0.d0
do n = 0, 2
  calc_v = calc_v + aatotal(n)/dfloat(n+1)*(d**float(n+1)-dcurrent**float(n+1))
end do

end function calc_v

!-----------------------------------------------------------------

!pure function calc_aaface(i,j,d,x,kkin,kkout,kk_max,kk_min)
function calc_aaface(i,j,d,x,kkin,kkout,kk_max,kk_min)
! not pure as it calls error_stop, which cannot be pure as it writes to screen

! calculate the polynomial coefficients that represent the cross-sectional interface area between the current face, as per
!  area = aaface(2)*d**2+aaface(1)*d+aaface(0)
use general_module
double precision, dimension(0:2) :: calc_aaface ! the polynomical coefficients to be calculated
type(integer_list_type), intent(in) :: kkin, kkout ! these lists contain the nodes of the face, referenced by their cell(i)%knode index, and grouped as either in or out of the continuous phase
type(scalar_list_type), intent(in) :: d ! list of d values for each cell(i)%knode
type(vector_list_type), intent(in) :: x ! list of x values for each cell(i)%knode, relative to cell centre
integer, intent(in) :: i, j ! the considered cell and face indices
integer, intent(in) :: kk_max, kk_min ! the cell(i)%knode indices of the maximum and minimum d's
! local variables
double precision, dimension(totaldimensions,0:2) :: alpha ! local calculation vector
double precision :: d2d1, dmaxdmin, l3mag, d3
integer :: kk_1, kk_2
double precision, dimension(totaldimensions) :: l3

if (face(j)%dimensions == 1) then
  calc_aaface = 0.d0
  kk_2 = kkout%elements(1) ! for the 2D case there should only be one here
  kk_1 = kkin%elements(1) ! and one here
  d2d1 = d%elements(kk_2)-d%elements(kk_1)
  if (d2d1 < 1.d-10*face(j)%dx) return
  dmaxdmin = d%elements(kk_max)-d%elements(kk_min)
  if (dmaxdmin < 1.d-10*face(j)%dx) return
! now form alpha vectors, which are vector polynomial coefficients that give a vector from the cell dmax/dmin axis to the relevant edge (here also a face)
  alpha(:,2) = 0.d0
  alpha(:,1) = (x%elements(:,kk_2)-x%elements(:,kk_1))/d2d1 - (x%elements(:,kk_max)-x%elements(:,kk_min))/dmaxdmin
  alpha(:,0) = (-d%elements(kk_1)*x%elements(:,kk_2)+d%elements(kk_2)*x%elements(:,kk_1))/d2d1 - &
                (-d%elements(kk_min)*x%elements(:,kk_max)+d%elements(kk_max)*x%elements(:,kk_min))/dmaxdmin
! find a unit vector in this direction, to convert the vector to a scalar
! use the mid point d to find this vector
  d3 = (d%elements(kk_2)+d%elements(kk_1))/2.d0
  l3 = alpha(:,1)*d3 + alpha(:,0)
  l3mag = vector_magnitude(l3)
  if (l3mag < 1.d-10*face(j)%dx) return
  calc_aaface(1) = dot_product(alpha(:,1),l3)/l3mag
  calc_aaface(0) = dot_product(alpha(:,0),l3)/l3mag
else
  call error_stop("calc_aaface can't handle this type of face yet")
! continue
end if

end function calc_aaface

!-----------------------------------------------------------------

arb_external_operator facevofphi
subroutine facevofphi(thread,m,ilast,jlast,klast,error_string,deriv,msomeloop_phi,msomeloop_d,msomeloop_normal_l1, &
  msomeloop_normal_l2,msomeloop_normal_l3,msomeloop_phitol,method)

! in this subroutine we find the face centred advected phi variable based on the interface location (d and normals)

!  n \cdot (x - x_c) - d = 0
! unit normal n points into the disperse (phi=1) phase
! called by the arb function facevofphi
! i is cell index
! someloop(thread)%funk(m) is the variable that is to contain d (and possibly its derivative)
! the msomeloops contain the funk indices that contain: phi, phitol, and the three unitnormals (normal1->normal3)
! if any msomeloop index = -1, then the someloop variable is not defined
! method is an integer that specifies how d is calculated:  1 = linearone, 2 = lineartwo, 3 = parabolic fit to max, min and centroid, 4 = match volumes (exact)

use general_module
! these are passed to all external routines
integer :: thread,m,ilast,jlast,klast,deriv
character(len=1000) :: error_string
! specific arguments passed to facevofphi
integer :: msomeloop_phi,msomeloop_d,msomeloop_phitol,msomeloop_normal_l1,msomeloop_normal_l2,msomeloop_normal_l3,method
! local variables
integer, dimension(totaldimensions) :: msomeloop_normal
integer :: k, kk, l, kk_max, kk_min, i, j
double precision :: normal_magnitude, d_max, d_min, d, phi, phitol, dtol, d_node
double precision, dimension(totaldimensions) :: normal, pass_vector
logical, parameter :: debug = .false.

if (debug) write(50,'(80(1h+)/a)') 'subroutine facevofphi'

if (deriv > 0) call error_stop("subroutine facevofphi cannot handle derivative yet")

i = ilast ! this is the upwind cell to
j = jlast ! this face, over which we are calculating the advection

if (debug) then
  write(50,*) 'm,ilast,jlast,klast,thread = ',m,ilast,jlast,klast,thread
  write(50,*) 'deriv = ',deriv
  write(50,*) 'msomeloop_phi = ',msomeloop_phi
  write(50,*) 'msomeloop_d = ',msomeloop_d
  write(50,*) 'msomeloop_normal = ',msomeloop_normal
  write(50,*) 'msomeloop_phitol = ',msomeloop_phitol
  write(50,*) 'method = ',method
end if

! examine phi first
! save phi in a temporary variable
if (msomeloop_phi <= 0) call error_stop('error with msomeloop_phi passed to facevofphi')
phi = someloop(thread)%funk(msomeloop_phi)%v
! find phitol
if (msomeloop_phitol > 0) then
  phitol = someloop(thread)%funk(msomeloop_phitol)%v
else
  phitol = phitol_default
end if

if (debug) then
  write(50,*) 'phi = ',phi
  write(50,*) 'phitol = ',phitol
end if

if (phi < phitol.or.phi > 1.d0-phitol) then
  someloop(thread)%funk(m)%v = phi ! if this isn't an interface cell then upwinding is used
  if (debug) write(50,*) 'facevofphi = ',someloop(thread)%funk(m)%v 
  if (debug) write(50,*) 'upwinding used as this isn''t an interface cell according to phi and phitol'
  return
end if

! now examine the normal
! first off, form this into a vector
msomeloop_normal = [msomeloop_normal_l1,msomeloop_normal_l2,msomeloop_normal_l3]
! set normal variable based on msomeloop m's
do l = 1, 3
  if (msomeloop_normal(l) > 0) then
    normal(l) = someloop(thread)%funk(msomeloop_normal(l))%v
  else
    normal(l) = 0.d0
  end if
end do
! check on normal magnitudes here
normal_magnitude = dot_product(normal,normal)
if (debug) then
  write(50,*) 'normal = ',normal
  write(50,*) 'normal_magnitude = ',normal_magnitude
end if

if (abs(normal_magnitude - 1.d0) > normal_magnitude_tol) then
  someloop(thread)%funk(m)%v = phi ! if the normal here isn't valid then also use upwinding
  if (debug) write(50,*) 'facevofphi = ',someloop(thread)%funk(m)%v 
  if (debug) write(50,*) 'upwinding used as the normal magnitude here is too small'
  return
end if

! now to do the calcs
d = someloop(thread)%funk(msomeloop_d)%v

! push all (relative to cell) nodes from face into node_x_list
cellknode_xr(thread)%length = 0 ! reset node list
do kk = 1, ubound(face(j)%knode,1)
  k = face(j)%knode(kk)
  pass_vector = node(k)%x-cell(i)%x
  call add_to_vector_list(list=cellknode_xr(thread),new_element=pass_vector)
  if (debug) write(50,*) 'k,kk,x_relative = ',k,kk,cellknode_xr(thread)%elements(:,kk)
end do

! loop through all nodes finding d_max and d_min
kk_max = 0
kk_min = 0
d_max = -huge(0.d0) ! not necessarily positive here
d_min = +huge(0.d0) ! not necessarily negative here
if (method > 3) cellknode_d(thread)%length = 0
do kk = 1, cellknode_xr(thread)%length
  d_node = dot_product(normal,cellknode_xr(thread)%elements(:,kk))
  if (method > 3) call add_to_scalar_list(list=cellknode_d(thread),new_element=d_node)
  if (debug) write(50,*) 'kk,d_node = ',kk,d_node
  if (d_node > d_max) then
    d_max = d_node
    kk_max = kk
  end if
  if (d_node < d_min) then
    d_min = d_node
    kk_min = kk
  end if
end do

if (debug) then
  write(50,*) 'kk_max,d_max = ',kk_max,d_max
  write(50,*) 'kk_min,d_min = ',kk_min,d_min
end if

! check the instances of a totally full or empty face
dtol = phitol*cell(i)%dx_max
if (d >= d_max) then
  someloop(thread)%funk(m)%v = 0.d0
  if (debug) write(50,*) 'facevofphi = ',someloop(thread)%funk(m)%v 
  if (debug) write(50,*) 'totally empty facevofphi based on d_max and d'
  return
else if (d <= d_min) then
  someloop(thread)%funk(m)%v = 1.d0
  if (debug) write(50,*) 'facevofphi = ',someloop(thread)%funk(m)%v 
  if (debug) write(50,*) 'totally full facevofphi based on d_max and d'
  return
else if (d_max-d_min < dtol) then
  someloop(thread)%funk(m)%v = 0.5d0
  if (debug) write(50,*) 'facevofphi = ',someloop(thread)%funk(m)%v 
  if (debug) write(50,*) 'half full facevofphi as d_max-d_min is very small'
  return
end if

! if we are here then d_max > d > d_min and the face contains an interface
! use a variety of methods to calculate facevofphi

if (debug) write(50,*) 'intermediate phi should be calculated'

if (method == 1) then
! linearone method, using linear interpolation over one range between d_max (phi=0) and d_min (phi=1)
! for a single dimension line, this is exact

  someloop(thread)%funk(m)%v = (d_max-d)/(d_max-d_min)! set the final someloop value

else if (method == 2) then
! lineartwo method, using linear interpolation over the two ranges between d_max (phi=0), centroid (d=0, phi=0.5) and d_min (phi=1)

  call error_stop("subroutine facevofphi cannot handle this method yet")
  if (phi < 0.5d0) then
    d = -(2.d0*phi-1.d0)*d_max
  else
    d = (2.d0*phi-1.d0)*d_min
  end if

! method 3 is supposed to be parabolic, between centre and two extremes, but not coded yet
else
  call error_stop("subroutine cellvofd cannot handle this method yet")
end if

if (debug) write(50,*) 'facevofphi = ',someloop(thread)%funk(m)%v 

if (debug) write(50,'(a/80(1h+))') 'subroutine facevofphi'

end subroutine facevofphi

!-----------------------------------------------------------------

arb_external_operator cellvofphishape
subroutine cellvofphishape(thread,m,ilast,jlast,klast,error_string,deriv, &
  msomeloop_size1,msomeloop_size2,msomeloop_size3, &
  msomeloop_centre1,msomeloop_centre2,msomeloop_centre3, &
  msomeloop_axis1,msomeloop_axis2,msomeloop_axis3, &
  msomeloop_phitol,msomeloop_levelset,shape)

use general_module
! these are passed to all external routines
integer :: thread,m,ilast,jlast,klast,deriv
character(len=1000) :: error_string
! specific arguments passed to cellvofd
integer :: msomeloop_size1,msomeloop_size2,msomeloop_size3
integer :: msomeloop_centre1,msomeloop_centre2,msomeloop_centre3
integer :: msomeloop_axis1,msomeloop_axis2,msomeloop_axis3
integer :: msomeloop_phitol,msomeloop_levelset,shape
! local variables
integer, dimension(totaldimensions) :: msomeloop_size, msomeloop_centre, msomeloop_axis
double precision, dimension(totaldimensions) :: size, centre, axis, x_sample, x_minimum, pass_vector
integer :: i, l, jj, kk, j, sample_points_total, sample_points_in_cell, sample_points_in_shape, n1, n2
logical :: cell_interface, cell_is_in, any_rotation
double precision :: phitol, rotation_angle, levelset
double precision, dimension(totaldimensions,totaldimensions) :: rotation_total
logical, parameter :: debug = .false.

if (debug) write(50,'(80(1h+)/a)') 'subroutine cellvofphishape'

if (deriv > 0) call error_stop("subroutine cellvofphishape cannot handle derivative yet")

! first off, form the vectors into arrays
msomeloop_size = [msomeloop_size1,msomeloop_size2,msomeloop_size3]
msomeloop_centre = [msomeloop_centre1,msomeloop_centre2,msomeloop_centre3]
msomeloop_axis = [msomeloop_axis1,msomeloop_axis2,msomeloop_axis3]
i = ilast

if (debug) then
  write(50,*) 'm,ilast,jlast,klast,thread = ',m,ilast,jlast,klast,thread
  write(50,*) 'msomeloop_size = ',msomeloop_size
  write(50,*) 'msomeloop_centre = ',msomeloop_centre
  write(50,*) 'msomeloop_axis = ',msomeloop_axis
  write(50,*) 'msomeloop_phitol = ',msomeloop_phitol
  write(50,*) 'msomeloop_levelset = ',msomeloop_levelset
  write(50,*) 'shape = ',shape
end if

! save phitol in a temporary variable
if (msomeloop_phitol > 0) then
  phitol = someloop(thread)%funk(msomeloop_phitol)%v
else
  phitol = phitol_default
end if

! find levelset
if (msomeloop_levelset > 0) then
  levelset = someloop(thread)%funk(msomeloop_levelset)%v
else
  levelset = 0.d0
end if

! set size, centre and axis variables based on msomeloop m's
do l = 1, 3
  if (msomeloop_size(l) > 0) then
    size(l) = someloop(thread)%funk(msomeloop_size(l))%v
  else
    size(l) = huge(1.d0)
  end if
end do
do l = 1, 3
  if (msomeloop_centre(l) > 0) then
    centre(l) = someloop(thread)%funk(msomeloop_centre(l))%v
  else
    centre(l) = 0.d0
  end if
end do
any_rotation = .false.
call identity_matrix(rotation_total) ! by default rotation_total is the identity matrix, ie, no rotation (although the any_rotation logical now means that this identity matrix isn't used)
do l = 1, 3
  if (msomeloop_axis(l) > 0) then
    axis(l) = someloop(thread)%funk(msomeloop_axis(l))%v
    any_rotation = .true.
  else
    axis(l) = 0.d0
  end if
end do
if (any_rotation) then ! form the matrix that will convert vector x - centre into reference frame that has object's centreline along z axis
  rotation_angle = vector_magnitude(axis) ! rotation angle magnitude is given by magnitude of rotation vector, right now in degrees
  if (debug) write(50,*) 'rotation_angle (degrees) = ',rotation_angle
  if (rotation_angle > tinyish) then
    axis = axis/rotation_angle ! normalise axis to give a unit vector
    rotation_angle = -rotation_angle*pi/180.d0 ! negative as we want the rotation_total matrix to perform a rotation from the actual back to shape coordinate system (ie, reverse rotation), and also convert to radians
    rotation_total = rotation_matrix(axis,rotation_angle)
    if (debug) then
      write(50,*) 'axis:'
      do n1 = 1, 3
        write(50,'(3(1x,g12.5))') axis(n1)
      end do
      write(50,*) 'rotation_total:'
      do n1 = 1, 3
        write(50,'(3(1x,g12.5))') (rotation_total(n1,n2),n2=1,3)
      end do
    end if
  end if
end if

! if this is a sphere or cube, then set size to be equal in each dimension
if (shape == 1) size = minval(size) 
if (shape == 3) size = minval(size) 

if (debug) then
  write(50,*) 'shape = ',shape
  write(50,*) 'size = ',size
  write(50,*) 'centre = ',centre
  write(50,*) 'axis = ',axis
end if

! cycle through cell nodes, determining if cell is either fully in, fully out or contains at interface with the shape
! NOTE, this will not capture shapes that just brush through a cell between nodes and face centres - for that have to check every cell using random technique
! or check every face using a variety of points
! based on node points
cell_interface = .false. ! 
pass_vector = node(cell(i)%knode(1))%x ! avoid temporary array warnings
cell_is_in = is_point_in_shape(shape,size,centre,rotation_total,any_rotation,levelset,pass_vector)
do kk = 2, ubound(cell(i)%knode,1)
  pass_vector = node(cell(i)%knode(kk))%x ! avoid temporary array warnings
  if (cell_is_in.neqv.is_point_in_shape(shape,size,centre,rotation_total,any_rotation,levelset,pass_vector)) then
    cell_interface = .true.
    exit
  end if
end do
! also check on face centres
if (.not.cell_interface) then
  do jj = 1, ubound(cell(i)%jface,1)
    pass_vector = face(cell(i)%jface(jj))%x ! avoid temporary array warnings
    if (cell_is_in.neqv.is_point_in_shape(shape,size,centre,rotation_total,any_rotation,levelset,pass_vector)) then
      cell_interface = .true.
      exit
    end if
  end do
end if

if (debug) write(50,*) 'cell_is_in,cell_interface = ',cell_is_in,cell_interface

if (.not.cell_interface) then
  if (cell_is_in) then
    someloop(thread)%funk(m)%v = 1.d0
  else
    someloop(thread)%funk(m)%v = 0.d0
  end if
else

! save all face normals/ds in a vector/scalar list for fast calculation
  face_normal(thread)%length = 0
  face_d(thread)%length = 0
  do jj = 1, ubound(cell(i)%jface,1)
    j = cell(i)%jface(jj)
    pass_vector = face(j)%norm(:,1)
! if the cell is not the face's downcell, need to reverse the normal direction to make sure that normal is pointing away from the cell
! note, this will work with glued faces as next to a glued face the normal always points outwards anyway
    if (i /= face(j)%icell(1)) pass_vector = -pass_vector
    call add_to_vector_list(list=face_normal(thread),new_element=pass_vector)
! face plane is defined by: facenormal dot x = face_d(thread)
    call add_to_scalar_list(list=face_d(thread), &
      new_element=dot_product(face_normal(thread)%elements(:,face_normal(thread)%length),face(j)%x))
  end do
  
! find the minimum coordinates in each dimension of this cell
  x_minimum = huge(1.d0)
  do kk = 1, ubound(cell(i)%knode,1)
    do l = 1, totaldimensions
      x_minimum(l) = min(x_minimum(l),node(cell(i)%knode(kk))%x(l))
    end do
  end do
  if (debug) write(50,*) 'x_minimum = ',x_minimum
  if (debug) write(50,*) 'cell(i)dx = ',cell(i)%dx

! sample_points_total = 100
  sample_points_total = int(1.d0/phitol) ! relate to phitol soon
  sample_points_in_cell = 0
  sample_points_in_shape = 0

  SAMPLE_LOOP: do n1 = 1, sample_points_total
! form sample point
    call random_number(x_sample) ! gives three random numbers in the range 0->1
    do l = 1, totaldimensions
      x_sample(l) = x_minimum(l) + cell(i)%dx(l)*x_sample(l) ! translate and scale these numbers to be contained within rectangular box that completely contains cell
    end do
    if (debug) write(50,*) 'x_sample = ',x_sample
! check that it is in the cell
    do n2 = 1, face_normal(thread)%length
      if (debug) then
        write(50,*) 'n2 = ',n2
        write(50,*) 'face_normal(thread) = ',face_normal(thread)%elements(:,n2)
        write(50,*) 'face_d(thread) = ',face_d(thread)%elements(n2)
        write(50,*) 'dot_product(face_normal(thread)%elements(:,n2),x_sample) - face_d(thread)%elements(n2) = ', &
                     dot_product(face_normal(thread)%elements(:,n2),x_sample) - face_d(thread)%elements(n2)
      end if
      if (dot_product(face_normal(thread)%elements(:,n2),x_sample) > face_d(thread)%elements(n2)) cycle SAMPLE_LOOP
      if (debug) write(50,*) 'calculated as in cell'
    end do
    sample_points_in_cell = sample_points_in_cell + 1
! check that it is in the shape
    if (is_point_in_shape(shape,size,centre,rotation_total,any_rotation,levelset,x_sample)) sample_points_in_shape = sample_points_in_shape + 1
  end do SAMPLE_LOOP

  if (debug) write(50,*) 'sample_points_in_cell,sample_points_in_shape = ',sample_points_in_cell,sample_points_in_shape
  someloop(thread)%funk(m)%v = dfloat(sample_points_in_shape)/dfloat(max(sample_points_in_cell,1))

end if

if (debug) write(50,*) 'phi = ',someloop(thread)%funk(m)%v 

if (debug) write(50,'(a/80(1h+))') 'subroutine cellvofphishape'

end subroutine cellvofphishape

!-----------------------------------------------------------------

function is_point_in_shape(shape,size,centre,rotation_total,any_rotation,levelset,x)

! on the boundary is in
! now rewritten to accept rotation_total matrix, and so not need axis vector

use general_module
logical :: is_point_in_shape
logical, intent(in) :: any_rotation
double precision, dimension(totaldimensions,totaldimensions), intent(in) :: rotation_total
double precision, dimension(totaldimensions), intent(in) :: size, centre, x
double precision, dimension(totaldimensions) :: x_rotated ! position of x relative cente relative to unrotated configuration of shape
double precision :: levelset ! for levelset based approaches (such as gyroid) use this variable to determine the surface location
integer :: shape
double precision :: d
integer :: l, l2

x_rotated = x - centre
if (any_rotation) x_rotated = matmul(rotation_total,x_rotated)

if (shape <= 2) then ! ellipsoid (or sphere which is an ellipsoid with all sizes equal)

  d = 0.d0
  do l = 1, totaldimensions
    d = d + (2.d0*x_rotated(l)/size(l))**2
  end do

  if (d <= 1.d0) then
    is_point_in_shape = .true.
  else
    is_point_in_shape = .false.
  end if

else if (shape <= 4) then ! box (or cube which is a box with all sizes equal)

  is_point_in_shape = .false.
  do l = 1, totaldimensions
    if (abs(x_rotated(l)) > size(l)/2.d0) return
  end do
  is_point_in_shape = .true.

else if (shape == 5) then ! cylinder, with centreline along the z axis, and size 1 as diameter and size 2 as length

  is_point_in_shape = .false.
  if (abs(x_rotated(3)) > size(2)/2.d0) return
  if (max(abs(x_rotated(1)),abs(x_rotated(2))) > size(1)/2.d0) return ! these checks should be faster than the exact square ones below
  if (x_rotated(1)**2 + x_rotated(2)**2 > size(1)**2/4.d0) return
  is_point_in_shape = .true.

else if (shape == 6) then ! gyroid

  is_point_in_shape = .false.
  d = 0.d0
  do l = 1, totaldimensions
    l2 = l + 1
    if (l2 == 4) l2 = 1
    d = d + sin(2.d0*pi*x_rotated(l)/size(l))*cos(2.d0*pi*x_rotated(l2)/size(l2))
  end do
  if (d > levelset) return
  is_point_in_shape = .true.

else
  call error_stop("shape is not yet supported in is_point_in_shape")
end if

end function is_point_in_shape

!-----------------------------------------------------------------

arb_external_operator cellvofphiadjust
subroutine cellvofphiadjust(thread,m,ilast,jlast,klast,error_string,deriv,msomeloop_phi_r1,m_phif,m_flux,msomeloop_dt)

! in this subroutine we calculate a phi adjustment which needs to be applied to all outwardly directed vof phi fluxes
!   to ensure volume conservation
! right now does not handle derivative and calculates explicitly

use general_module
! these are passed to all external routines
integer :: thread,m,ilast,jlast,klast,deriv
character(len=1000) :: error_string
! specific arguments passed to cellvofd
integer :: msomeloop_phi_r1,m_phif,m_flux,msomeloop_dt
! local variables
double precision :: phi_r1, deltaphi, flux, phif, direction, dt, aaflux, aad, aau, aatarget, aa, dd, philimited, phiunlimited
integer :: i, j, jj, n, idirection, k
logical, parameter :: debug = .false.

if (debug) write(50,'(80(1h+)/a)') 'subroutine cellvofphiadjust'

i = ilast ! this is the cell we are in: jlast and klast have no meaning in this routine

if (debug) then
  write(50,*) 'm,ilast,jlast,klast,thread = ',m,ilast,jlast,klast,thread
  write(50,*) 'deriv = ',deriv
  write(50,*) 'msomeloop_phi_r1 = ',msomeloop_phi_r1
  write(50,*) 'm_phif = ',m_phif
  write(50,*) 'm_flux = ',m_flux
  write(50,*) 'msomeloop_dt = ',msomeloop_dt
end if

if (cell(i)%type == 2) then
  if (debug) write(50,*) 'skipping boundary cell'
  return
end if

! save phi[r=1] in a temporary variable
if (msomeloop_phi_r1 <= 0) call error_stop('error with msomeloop_phi_r1 passed to cellvofphiadjust')
phi_r1 = someloop(thread)%funk(msomeloop_phi_r1)%v

! check that both phif and flux are given
if (m_phif <= 0) call error_stop('error with m_phif passed to cellvofphiadjust')
if (m_flux <= 0) call error_stop('error with m_flux passed to cellvofphiadjust')

! save dt in a temporary variable
if (msomeloop_dt <= 0) call error_stop('error with msomeloop_dt passed to cellvofphiadjust')
dt = someloop(thread)%funk(msomeloop_dt)%v

if (debug) then
  write(50,*) 'dt = ',dt
  write(50,*) 'phi_r1 = ',phi_r1
end if

deltaphi = 0.d0 ! default

! save face flux and phif values in local scalar lists
flux_list(thread)%length=0
phif_list(thread)%length=0
areadvol_list(thread)%length=0
do jj = 1, ubound(cell(i)%jface,1)
  j = cell(i)%jface(jj)
  if (i == face(j)%icell(1)) then
    idirection = 1 ! normal points outwards (cell is downcell)
  else
    idirection = -1 ! inwards (cell is upcell)
  end if
! now calculate the flux using the created someloop in face j
  call update_someloop(thread,m_flux,i,j,0,error_string)
  flux = someloop(thread)%funk(m_flux)%v
! depending on direction of flux, also calculate the advection phif, in face j
  if (flux*dfloat(idirection) > 0.d0) then
    call add_to_scalar_list(list=flux_list(thread),new_element=flux) ! flux_list now has same sign as u_f, with direction and zeroing down ysing areadvol
    call update_someloop(thread,m_phif,i,j,0,error_string)
!   phif = someloop(thread)%funk(m_phif)%v
    phif = max(min(someloop(thread)%funk(m_phif)%v,1.d0),0.d0) ! now ensure the phif is bounded
    call add_to_scalar_list(list=phif_list(thread),new_element=phif)
    call add_to_scalar_list(list=areadvol_list(thread),new_element=dfloat(idirection)*face(j)%area/cell(i)%vol)
  else
    call add_to_scalar_list(list=flux_list(thread),new_element=0.d0) ! if the flux is coming in then set to zero
    call add_to_scalar_list(list=phif_list(thread),new_element=0.d0) ! and these will not be used
    call add_to_scalar_list(list=areadvol_list(thread),new_element=0.d0)
  end if
end do
 
if (debug) then
  do jj = 1, ubound(cell(i)%jface,1)
    j = cell(i)%jface(jj)
    if (jj == 1) write(50,*) &
      'jj,j,divop(i,j),flux_list(thread)%elements(jj),phif_list(thread)%elements(jj),areadvol_list(thread)%elements(jj)'
    write(50,*) jj,j,divop(i,j),flux_list(thread)%elements(jj),phif_list(thread)%elements(jj),areadvol_list(thread)%elements(jj)
  end do
end if

! calculate divergence of flux, and if greater than one, decrease fluxes
aaflux = aadiv(flux_list(thread)%elements,phif_list(thread)%elements,areadvol_list(thread)%elements,dt,deltaphi=1.d1, &
  length=flux_list(thread)%length) ! use a large deltaphi to ensure all local phifs are 1.d0
if (debug) write(50,*) 'aaflux = ',aaflux
if (aaflux > 1.d0) then
  flux_list(thread)%elements = flux_list(thread)%elements/aaflux
  aaflux = 1.d0
  if (debug) then
    write(50,*) 'aaflux is too large: adjusting fluxes: aaflux = ',aaflux
    write(50,*) 'flux_list(thread)%elements(1:flux_list(thread)%length)'
    write(50,*) flux_list(thread)%elements(1:flux_list(thread)%length)
  end if
end if

! loop through increments to phi, bounding 
deltaphi = 0.d0
aau = aadiv(flux_list(thread)%elements,phif_list(thread)%elements,areadvol_list(thread)%elements,dt,deltaphi, &
  length=flux_list(thread)%length)
if (debug) write(50,*) 'first calc: aau = ',aau
if (aau < phi_r1 + aaflux - 1.d0 .or. aau > phi_r1 ) then
  if (debug) write(50,*) 'aau out of range'
  if (aau > phi_r1) then
    aatarget = phi_r1
    direction = -1.d0 ! need to decrease phif
    aa = 0.d0 ! constant needed in derivative calculation
  else
    aatarget = phi_r1 + aaflux - 1.d0
    direction = 1.d0 ! need to increase phif
    aa = 1.d0
  end if
  if (debug) write(50,*) 'aatarget, direction = ',aatarget,direction
! create ordered list of deltaphi increments
  deltaphi_list(thread)%length = 0
  call add_to_scalar_list(list=deltaphi_list(thread),new_element=0.d0)
  do n = 1, phif_list(thread)%length
    if (direction > 0.d0) then
      call add_to_scalar_list(list=deltaphi_list(thread),new_element=1.d0-phif_list(thread)%elements(n))
    else
      call add_to_scalar_list(list=deltaphi_list(thread),new_element=-phif_list(thread)%elements(n))
    end if
  end do
  call add_to_scalar_list(list=deltaphi_list(thread),new_element=direction)
! sort list in the direction
  call sort_scalar_list(list=deltaphi_list(thread),direction=direction)
  if (debug) write(50,*) 'sorted deltaphi_list(thread): ',deltaphi_list(thread)%elements(1:deltaphi_list(thread)%length)
! loop through each element of ordered deltaphi_list(thread), until aau and aad bound aatarget
  do n = 2, deltaphi_list(thread)%length
    aad = aau
    if (abs(deltaphi_list(thread)%elements(n)-deltaphi_list(thread)%elements(n-1)) < 1.d-18) cycle ! deltaphi values are the same, so aau values will be too
    aau = aadiv(flux_list(thread)%elements,phif_list(thread)%elements,areadvol_list(thread)%elements,dt,deltaphi_list(thread)%elements(n), &
      length=flux_list(thread)%length)
    if (debug) write(50,*) 'in loop: n,aau,deltaphi_list(thread)%elements = ',n,aau,deltaphi_list(thread)%elements(n)
    if ((aau-aatarget)*(aad-aatarget) <= 0.d0) exit
  end do
  if (n > deltaphi_list(thread)%length) then
! aatarget was never bounded, so use highest possible increment (this is an error or round-off problem)
    deltaphi = deltaphi_list(thread)%elements(deltaphi_list(thread)%length)
    if (debug) write(50,*) 'aatarget was not bounded: deltaphi = ',deltaphi
  elseif (abs(aau-aad) > 1.d-10) then
! we've bounded aatarget, so interpolate to find deltaphi
    deltaphi = deltaphi_list(thread)%elements(n-1) + (deltaphi_list(thread)%elements(n)-deltaphi_list(thread)%elements(n-1))*(aatarget-aad)/(aau-aad)
    if (debug) then
      write(50,*) 'final interpolation based upon: n = ',n
      write(50,*) 'aau,deltaphi_list(thread)%elements(n)'
      write(50,*) aau,deltaphi_list(thread)%elements(n)
      write(50,*) 'aad,deltaphi_list(thread)%elements(n-1)'
      write(50,*) aad,deltaphi_list(thread)%elements(n-1)
      write(50,*) 'deltaphi = ',deltaphi
    end if
  else
! difference between aau and aad is small, err on side of largest increment
    deltaphi = deltaphi_list(thread)%elements(n)
    if (debug) write(50,*) 'difference between aau and aad small: deltaphi = ',deltaphi
  end if
  if (debug) write(50,*) 'check aadiv = ',aadiv(flux_list(thread)%elements,phif_list(thread)%elements,areadvol_list(thread)%elements, &
    dt,deltaphi,length=flux_list(thread)%length)

! if derivative is needed, then calculate here
  if (deriv > 0) then
    dd = 0.d0 ! this is the demoninator of the derivative multiplier
    cc_list(thread)%length = 0
    do n = 1, phif_list(thread)%length
      phiunlimited = phif_list(thread)%elements(n) + deltaphi
      if (phiunlimited > 0.d0.and.phiunlimited < 1.d0) dd = dd + areadvol_list(thread)%elements(n)*flux_list(thread)%elements(n)
      if (abs(areadvol_list(thread)%elements(n)) > 0.d0) then
        philimited = max(min(phiunlimited,1.d0),0.d0)
        call add_to_scalar_list(list=cc_list(thread),new_element=-areadvol_list(thread)%elements(n)*(philimited-aa))
      else
        call add_to_scalar_list(list=cc_list(thread),new_element=0.d0)
      end if
    end do

! now add these to the funk derivatives if dd is large enough
    if (abs(dd) > 1.d-10) then
      do n = 1, cc_list(thread)%length ! this will be the number of faces
        if (abs(cc_list(thread)%elements(n)) > 0.d0) then
          j = cell(i)%jface(n)
          call update_someloop(thread,m_flux,i,j,0,error_string)
          call add_to_dv(thread,someloop(thread)%funk(m),cc_list(thread)%elements(n)/dd,someloop(thread)%funk(m_flux))
        end if
      end do
    end if
  end if

end if

someloop(thread)%funk(m)%v = deltaphi
  
if (debug) write(50,*) 'deltaphi = ',someloop(thread)%funk(m)%v 

if (debug) write(50,'(a/80(1h+))') 'subroutine cellvofphiadjust'

end subroutine cellvofphiadjust

!-----------------------------------------------------------------

function aadiv(fluxl,phifl,areadvoll,dt,deltaphi,length)

use general_module
double precision, dimension(:) :: fluxl,phifl,areadvoll ! local versions of these scalar list arrays
double precision :: deltaphi, aadiv, dt
integer :: n, length

aadiv = 0.d0
do n = 1, length
  aadiv = aadiv + fluxl(n)*max(min(phifl(n)+deltaphi,1.d0),0.d0)*areadvoll(n)
end do
aadiv = dt*aadiv

end function aadiv

!-----------------------------------------------------------------
