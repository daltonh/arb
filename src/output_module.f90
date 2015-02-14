! file src/output_module.f90
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
module output_module

implicit none
private
public output, output_txt, output_stat, write_gmesh, output_step

! this data structure holds info each variable for output_step
type output_step_variable_type
  character(len=100) :: name
  character(len=100) :: units
  integer :: m ! var index
  integer :: ns ! ns index of particular funk value
end type output_step_variable_type

! reference list of all the gtypes
type(output_step_variable_type), dimension(:), allocatable :: output_step_variable

!-----------------------------------------------------------------
contains

!-----------------------------------------------------------------

subroutine output(debug_dump,intermediate)

! dumps the data to a file fit for printing

use general_module
use equation_module
use gmesh_module
use solver_module
integer :: gmesh_number
character(len=100) :: output_option, vtkoutput_option, datoutput_option
character(len=1000) :: formatline
logical, optional :: debug_dump ! dumps all component variables to the file output.debug.msh
logical :: debug_dump_local
logical, optional :: intermediate ! if this is an nonconverged intermediate output
logical :: intermediate_local
logical, parameter :: debug = .false.
                  
if (debug) write(*,'(80(1h+)/a)') 'subroutine output'

intermediate_local = .false.
if (present(intermediate)) intermediate_local = intermediate

call update_and_check_outputs ! update any output-only variables

! now output txt file if a single or zero dimensional problem
if (.false..or.maximum_dimensions <=1 ) call output_txt ! write txt files containing data

do gmesh_number = 0, ubound(gmesh,1)
  formatline = '(a,'//trim(dindexformat(gmesh_number))//',a)'

  output_option = trim(check_option(gmesh(gmesh_number)%options,output_gmesh_options))
  if (trim(output_option) == "centringoutput" .or. trim(output_option) == "centringmeshoutput") then
    if (debug.or..true.) write(*,fmt=formatline) 'INFO: writing gmesh file (',gmesh_number,') having basename '// &
      trim(gmesh(gmesh_number)%basename)
    call write_gmesh(gmesh_number=gmesh_number,debug_dump=.false.,centring='cell',intermediate=intermediate_local) ! write gmsh output file for cell centred variables
    call write_gmesh(gmesh_number=gmesh_number,debug_dump=.false.,centring='face',intermediate=intermediate_local) ! write gmsh output file for face centred variables
    call write_gmesh(gmesh_number=gmesh_number,debug_dump=.false.,centring='node',intermediate=intermediate_local) ! write gmsh output file for face centred variables
    call write_gmesh(gmesh_number=gmesh_number,debug_dump=.false.,centring='none',intermediate=intermediate_local) ! write gmsh output file for none centred variables
  else if (trim(output_option) == "output" .or. trim(output_option) == "meshoutput") then
    if (debug.or..true.) write(*,fmt=formatline) 'INFO: writing gmesh file (',gmesh_number,') having basename '// &
      trim(gmesh(gmesh_number)%basename)
    call write_gmesh(gmesh_number=gmesh_number,debug_dump=.false.,centring='all',intermediate=intermediate_local) ! write gmsh output file for all variables
  else if (trim(output_option) == "nooutput") then
    if (debug) write(*,fmt=formatline) 'INFO: skipping write for gmesh file (',gmesh_number,') having basename '// &
      trim(gmesh(gmesh_number)%basename)
  end if

! repeat process for vtk output
  vtkoutput_option = trim(check_option(gmesh(gmesh_number)%options,vtkoutput_gmesh_options))
  if (trim(vtkoutput_option) == "centringvtkoutput" .or. trim(vtkoutput_option) == "centringmeshvtkoutput") then
    if (debug.or..true.) write(*,fmt=formatline) 'INFO: writing vtk gmesh file (',gmesh_number,') having basename '// &
      trim(gmesh(gmesh_number)%basename)
    call write_gmesh(gmesh_number=gmesh_number,debug_dump=.false.,centring='cell',fileformat='vtk',intermediate=intermediate_local)
    call write_gmesh(gmesh_number=gmesh_number,debug_dump=.false.,centring='face',fileformat='vtk',intermediate=intermediate_local)
    call write_gmesh(gmesh_number=gmesh_number,debug_dump=.false.,centring='node',fileformat='vtk',intermediate=intermediate_local)
  else if (trim(vtkoutput_option) == "vtkoutput" .or. trim(vtkoutput_option) == "meshvtkoutput") then
    if (debug.or..true.) write(*,fmt=formatline) 'INFO: writing vtk gmesh file (',gmesh_number,') having basename '// &
      trim(gmesh(gmesh_number)%basename)
    call write_gmesh(gmesh_number=gmesh_number,debug_dump=.false.,centring='all',fileformat='vtk',intermediate=intermediate_local)
  else if (trim(output_option) == "novtkoutput") then
    if (debug) write(*,fmt=formatline) 'INFO: skipping write for vtk gmesh file (',gmesh_number,') having basename '// &
      trim(gmesh(gmesh_number)%basename)
  end if

! repeat process for dat output
  datoutput_option = trim(check_option(gmesh(gmesh_number)%options,datoutput_gmesh_options))
  if (trim(datoutput_option) == "centringdatoutput" .or. trim(datoutput_option) == "centringmeshdatoutput") then
    if (debug.or..true.) write(*,fmt=formatline) 'INFO: writing dat gmesh file (',gmesh_number,') having basename '// &
      trim(gmesh(gmesh_number)%basename)
    call write_gmesh(gmesh_number=gmesh_number,debug_dump=.false.,centring='cell',fileformat='dat',intermediate=intermediate_local)
    call write_gmesh(gmesh_number=gmesh_number,debug_dump=.false.,centring='face',fileformat='dat',intermediate=intermediate_local)
    call write_gmesh(gmesh_number=gmesh_number,debug_dump=.false.,centring='node',fileformat='dat',intermediate=intermediate_local)
  else if (trim(datoutput_option) == "datoutput" .or. trim(datoutput_option) == "meshdatoutput") then
    if (debug.or..true.) write(*,fmt=formatline) 'INFO: writing dat gmesh file (',gmesh_number,') having basename '// &
      trim(gmesh(gmesh_number)%basename)
    call write_gmesh(gmesh_number=gmesh_number,debug_dump=.false.,centring='all',fileformat='dat',intermediate=intermediate_local)
  else if (trim(output_option) == "nodatoutput") then
    if (debug) write(*,fmt=formatline) 'INFO: skipping write for dat gmesh file (',gmesh_number,') having basename '// &
      trim(gmesh(gmesh_number)%basename)
  end if

end do

! write gmsh file with all data output as components and elementdata
debug_dump_local = .false.
if (present(debug_dump)) debug_dump_local = debug_dump

if (.false..or.debug_dump_local) call write_gmesh(gmesh_number=0,debug_dump=.true.,centring='all', &
  intermediate=.false.)

if (.true.) call output_stat ! write some statistics about the data

if (trim(output_step_file) == "output") call output_step(action="write",do_update_outputs=.false.)

if (debug) write(*,'(a/80(1h-))') 'subroutine output'

end subroutine output

!-----------------------------------------------------------------

subroutine output_txt

! dumps the data to a file fit for printing

use general_module
use equation_module

integer :: error, i, j, k, l, m, list_length
integer, dimension(:), allocatable :: local_list
character(len=100) :: filename, cellxname(3), facexname(3), nodexname(3)
character(len=1000) :: formatline
character(len=1) :: delimiter=' '
logical :: therel
character(len=100) :: txtoutput = 'cell' ! what to output: cell|face|none|all
logical, parameter :: debug = .false.
                  
if (debug) write(*,'(80(1h+)/a)') 'subroutine output_txt'

do l = 1,3
  write(cellxname(l),'(a,i1,a)') '"<cellx[l=',l,']> [?]"'
  write(facexname(l),'(a,i1,a)') '"<facex[l=',l,']> [?]"'
  write(nodexname(l),'(a,i1,a)') '"<nodex[l=',l,']> [?]"'
end do

!----------------------------------------------------
! open output file
filename = "output/output.txt"
therel = .false.
inquire (file=trim(filename),exist=therel)
open(foutput,file=trim(filename),access='append',iostat=error)
if (error /= 0) call error_stop('problem opening file '//trim(filename))

if (therel) write(foutput,'(///)') ! 3 carriage returns between newtsteps
formatline = '(a/a,'//trim(indexformat)//')'
write(foutput,fmt=formatline) '#########################################','# NEWTSTEP = ',newtstep
if (transient_simulation) then
  formatline = '(a,'//trim(indexformat)//')'
  write(foutput,fmt=formatline) '# TIMESTEP = ',timestep
end if

!----------------------------------------------------
! output of cell centred variables
list_length = allocatable_size(var_list(var_list_number(centring="cell",type="all"))%list)

if (list_length > 0.and.(trim(txtoutput) == 'cell'.or.trim(txtoutput) == 'all')) then
  allocate(local_list(list_length))
  local_list = var_list(var_list_number(centring="cell",type="all"))%list

  write(foutput,'(//a)') '# CELL CENTRED GENERAL VARIABLES'
  formatline = '(a8'//repeat(',a,'//trim(stringformat),totaldimensions+ubound(local_list,1))//')'
  write(foutput,fmt=formatline) '"icell"',(delimiter,cellxname(l),l=1,totaldimensions), &
    (delimiter,'"'//trim(var(local_list(m))%name)//' ['//trim(var(local_list(m))%units)//']"',m=1,ubound(local_list,1))
  formatline = '('//trim(indexformat)//repeat(',a,'//trim(realformat),totaldimensions)// &
    repeat(',a,'//trim(realformat),ubound(local_list,1))//')'
  do i = 1, itotal
    write(foutput,fmt=formatline) i,(delimiter,trunk_dble(cell(i)%x(l)),l=1,totaldimensions), &
      (delimiter,trunk_dble(var_value(local_list(m),nsvar(m=local_list(m),ijk=i,noerror=.true., &
      error_string='Error occurred while txtfile outputting variable '//trim(var(local_list(m))%name)))),m=1,ubound(local_list,1))
  end do

  deallocate(local_list)
end if

!----------------------------------------------------
! output of face centred variables
list_length = allocatable_size(var_list(var_list_number(centring="face",type="all"))%list)

if (list_length > 0.and.(trim(txtoutput) == 'face'.or.trim(txtoutput) == 'all')) then
  allocate(local_list(list_length))
  local_list = var_list(var_list_number(centring="face",type="all"))%list

  write(foutput,'(//a)') '# FACE CENTRED GENERAL VARIABLES'
  formatline = '(a8'//repeat(',a,'//trim(stringformat),totaldimensions+ubound(local_list,1))//')'
  write(foutput,fmt=formatline) '"jface"',(delimiter,facexname(l),l=1,totaldimensions), &
    (delimiter,'"'//trim(var(local_list(m))%name)//' ['//trim(var(local_list(m))%units)//']"',m=1,ubound(local_list,1))
  formatline = '('//trim(indexformat)//repeat(',a,'//trim(realformat),totaldimensions)// &
    repeat(',a,'//trim(realformat),ubound(local_list,1))//')'
  do j = 1, jtotal
    write(foutput,fmt=formatline) j,(delimiter,trunk_dble(face(j)%x(l)),l=1,totaldimensions), &
      (delimiter,trunk_dble(var_value(local_list(m),nsvar(m=local_list(m),ijk=j,noerror=.true., &
      error_string='Error occurred while txtfile outputting variable '//trim(var(local_list(m))%name)))),m=1,ubound(local_list,1))
  end do

  deallocate(local_list)
end if

!----------------------------------------------------
! output of node centred variables
list_length = allocatable_size(var_list(var_list_number(centring="node",type="all"))%list)

if (list_length > 0.and.(trim(txtoutput) == 'node'.or.trim(txtoutput) == 'all')) then
  allocate(local_list(list_length))
  local_list = var_list(var_list_number(centring="node",type="all"))%list

  write(foutput,'(//a)') '# NODE CENTRED GENERAL VARIABLES'
  formatline = '(a8'//repeat(',a,'//trim(stringformat),totaldimensions+ubound(local_list,1))//')'
  write(foutput,fmt=formatline) '"knode"',(delimiter,nodexname(l),l=1,totaldimensions), &
    (delimiter,'"'//trim(var(local_list(m))%name)//' ['//trim(var(local_list(m))%units)//']"',m=1,ubound(local_list,1))
  formatline = '('//trim(indexformat)//repeat(',a,'//trim(realformat),totaldimensions)// &
    repeat(',a,'//trim(realformat),ubound(local_list,1))//')'
  do k = 1, ktotal
    write(foutput,fmt=formatline) k,(delimiter,trunk_dble(node(k)%x(l)),l=1,totaldimensions), &
      (delimiter,trunk_dble(var_value(local_list(m),nsvar(m=local_list(m),ijk=k,noerror=.true., &
      error_string='Error occurred while txtfile outputting variable '//trim(var(local_list(m))%name)))),m=1,ubound(local_list,1))
  end do

  deallocate(local_list)
end if

!----------------------------------------------------
! output of none centred variables
list_length = allocatable_size(var_list(var_list_number(centring="none",type="all"))%list)

if (list_length > 0.and.(trim(txtoutput) == 'none'.or.trim(txtoutput) == 'all')) then
  allocate(local_list(list_length))
  local_list = var_list(var_list_number(centring="none",type="all"))%list

  write(foutput,'(//a)') '# NONE CENTRED GENERAL VARIABLES'
  formatline = '(a8'//repeat(',a,'//trim(stringformat),ubound(local_list,1))//')'
  if (transient_simulation) then
    write(foutput,fmt=formatline) '"tstep"', &
      (delimiter,'"'//trim(var(local_list(m))%name)//' ['//trim(var(local_list(m))%units)//']"',m=1,ubound(local_list,1))
  else
    write(foutput,fmt=formatline) '"nstep"', &
      (delimiter,'"'//trim(var(local_list(m))%name)//' ['//trim(var(local_list(m))%units)//']"',m=1,ubound(local_list,1))
  end if

  formatline = '('//trim(indexformat)//repeat(',a,'//trim(realformat),ubound(local_list,1))//')'
  if (transient_simulation) then
    write(foutput,fmt=formatline) timestep,(delimiter,trunk_dble(var_value(local_list(m),1)),m=1,ubound(local_list,1))
  else
    write(foutput,fmt=formatline) newtstep,(delimiter,trunk_dble(var_value(local_list(m),1)),m=1,ubound(local_list,1))
  end if

  deallocate(local_list)
end if

close(foutput)

!----------------------------------------------------

if (debug) write(*,'(a/80(1h-))') 'subroutine output_txt'

end subroutine output_txt

!-----------------------------------------------------------------

subroutine output_stat

! prints out some current statistics about the data

use general_module
integer :: error, ns, m, max_loc, min_loc
character(len=100) :: filename
character(len=1000) :: formatline, textline
real :: total_update_time, maximum_update_time
logical :: therel
logical, parameter :: debug = .false.
                  
if (debug) write(*,'(80(1h+)/a)') 'subroutine output_stat'

filename = "output/output.stat"
therel = .false.
inquire (file=trim(filename),exist=therel)
open(foutput,file=trim(filename),access='append',iostat=error)
if (error /= 0) call error_stop('problem opening file '//trim(filename))

if (therel) then
  write(foutput,'(//)')
else
  formatline = '(a,f4.2,a)'
  write(foutput,fmt=formatline) '# output from program arb, version ',version,' ('//trim(versionname)// &
    '), written by dalton harvie'
  call print_simulation_info(foutput,comment="#")
  write(foutput,*)
end if

formatline = '(a/a,'//trim(indexformat)//'/a,'//trim(indexformat)//'/)'
write(foutput,fmt=formatline) '#########################################','# NEWTSTEP = ',newtstep,'# TIMESTEP = ',timestep
  
! for calculating relative times, first loop through finding total and maximum update times
total_update_time = 0.d0
maximum_update_time = 0.d0
do m = 1, ubound(var,1)
  if (trim(var(m)%type) == 'local') cycle ! do not include local variables in this analysis as generally they may not be correctly located
  total_update_time = total_update_time + var(m)%update_time
  maximum_update_time = max(maximum_update_time,var(m)%update_time/dfloat(max(var(m)%update_number,1)))
end do

! general variables
do m = 1, ubound(var,1)
  if (trim(var(m)%type) == 'local') cycle ! do not include local variables in this analysis as generally they may not be correctly located
  max_loc = 1
  min_loc = 1
  do ns = 2, ubound(var(m)%funk,1)
    if (var(m)%funk(ns)%v > var(m)%funk(max_loc)%v) max_loc = ns
    if (var(m)%funk(ns)%v < var(m)%funk(min_loc)%v) min_loc = ns
  end do
! new textline formulation
  formatline = '(a,a,'//trim(realformat)//',a,a,a,'//trim(indexformat)//',a,'//trim(realformat)//',a,a,a,'//trim(indexformat)//')'
  write(textline,fmt=formatline) trim(var(m)%type)//' '//trim(var(m)%name),': max ',trunk_dble(var(m)%funk(max_loc)%v),' at ', &
    var(m)%centring,' ',ijkvar(m,max_loc),': min ',trunk_dble(var(m)%funk(min_loc)%v),' at ',var(m)%centring,' ',ijkvar(m,min_loc)
  if (trim(var(m)%type) == 'equation' .or. trim(var(m)%type) == 'unknown') then
    formatline = '(a,'//trim(realformat)//')'
    write(textline,fmt=formatline) trim(textline)//': magnitude = ',var(m)%magnitude
  end if

  if (output_variable_update_times) then
    formatline = '(a,2(a,'//trim(realformat)//'),a,'//trim(indexformat)//',2(a,'//trim(realformat)//'))'
    write(textline,fmt=formatline) trim(textline),&
      ': total update time = ',var(m)%update_time,': relative total update time = ',var(m)%update_time/total_update_time, &
      ': total updates = ',var(m)%update_number,': average update time = ', &
      var(m)%update_time/dfloat(max(var(m)%update_number,1)),': relative average update time = ', &
      var(m)%update_time/(dfloat(max(var(m)%update_number,1))*maximum_update_time)
  end if

  write(foutput,'(a)') trim(textline)

! if (trim(var(m)%type) == 'equation' .or. trim(var(m)%type) == 'unknown') then
! for unknown and equation variables also print out their (current) magnitude
!   formatline = '(a,a,'//trim(realformat)//',a,a,a,'//trim(indexformat)//',a,'//trim(realformat)//',a,a,a,'//trim(indexformat)// &
!     ',a,'//trim(realformat)//')'
!   write(foutput,fmt=formatline) trim(var(m)%type)//' '//trim(var(m)%name),': max ',trunk_dble(var(m)%funk(max_loc)%v),' at ', &
!     var(m)%centring,' ',ijkvar(m,max_loc),': min ',trunk_dble(var(m)%funk(min_loc)%v),' at ',var(m)%centring,' ',ijkvar(m,min_loc), &
!     ': magnitude = ',var(m)%magnitude
! else
!   formatline = '(a,a,'//trim(realformat)//',a,a,a,'//trim(indexformat)//',a,'//trim(realformat)//',a,a,a,'//trim(indexformat)//')'
!   write(foutput,fmt=formatline) trim(var(m)%type)//' '//trim(var(m)%name),': max ',trunk_dble(var(m)%funk(max_loc)%v),' at ', &
!     var(m)%centring,' ',ijkvar(m,max_loc),': min ',trunk_dble(var(m)%funk(min_loc)%v),' at ',var(m)%centring,' ',ijkvar(m,min_loc)
! end if
end do

close(foutput)

!----------------------------------------------------

if (debug) write(*,'(a/80(1h-))') 'subroutine output_stat'

end subroutine output_stat

!-----------------------------------------------------------------

subroutine write_gmesh(gmesh_number,debug_dump,centring,fileformat,intermediate)

! dumps the data to a file fit for printing

use general_module
use equation_module
use gmesh_module
integer :: gmesh_number ! output data concerned with this gmesh
logical :: debug_dump ! outputs components of all variables without any gradients
character(len=*) :: centring ! whether to output only cell, face, none or all centred elements (default all)
character(len=3), optional :: fileformat ! format of file to write (msh|vtk|dat) - if not present, defaults to msh
character(len=3) :: fileformatl ! local version of fileformat
logical :: intermediate ! if this is an intermediate newton step, rather than a final one
integer :: error, i, j, k, m, kk, nnodes, n, mc, nrank, ns, gelement, ngelements, ijk, mvar, jj, step
double precision, dimension(:), allocatable :: cellvaluel
double precision, dimension(:,:), allocatable :: cellgradl
character(len=1000) :: filename, linkname, formatline, system_command
character(len=100) :: data_option
integer, dimension(:), allocatable :: select_cells, select_faces
logical, parameter :: select_elements = .false.
logical, parameter :: use_newtstep = .false. ! if true then the step number will be set to the newtstep, which is required to output several newtsteps en-route to convergence of a steady-state simulation
logical, parameter :: debug = .false.
                  
if (debug) write(*,'(80(1h+)/a)') 'subroutine write_gmesh'

!----------------------------------------------------
! separate debugging section for the select_elements option
if (select_elements) then
! find all cells that have the error velocity greater than a certain magnitude
  mvar = var_number_from_name('<u_error[l=1]>')
  if (mvar == 0) call error_stop('variable not found')
  do ns = 1, ubound(region(var(mvar)%region_number)%ijk,1)
    if (sqrt(var(mvar)%funk(ns)%v**2+var(mvar+1)%funk(ns)%v**2+var(mvar+2)%funk(ns)%v**2) > 1.3d0) &
      call push_integer_array(array=select_cells,new_element=region(var(mvar)%region_number)%ijk(ns))
  end do
! find the faces that surround these faces
  do n = 1, ubound(select_cells,1)
    i = select_cells(n)
    do jj = 1, ubound(cell(i)%jface,1)
      j = cell(i)%jface(jj)
      if (location_in_list(array=select_faces,element=j) == 0) call push_integer_array(array=select_faces,new_element=j)
    end do
  end do
  write(*,*) 'select_cells = ',select_cells
  write(*,*) 'select_faces = ',select_faces
end if
  
!----------------------------------------------------
! find local fileformat (msh|vtk|dat)
fileformatl = 'msh'
if (present(fileformat)) fileformatl = fileformat

! assemble filename based on options
filename = trim(gmesh(gmesh_number)%basename)
!if (debug_dump) filename = trim(filename)//'.debug'
if (debug_dump) filename = 'debug.'//trim(filename)
if (centring /= 'all')  filename = trim(filename)//'.'//centring
if (intermediate.and..not.debug_dump) then
  formatline = '(a,'//trim(dindexformat(newtstep))//')'
  write(filename,fmt=formatline) trim(filename)//'.newt',newtstep
end if
if (transient_simulation.and..not.debug_dump) then
  formatline = '(a,'//trim(dindexformat(timestep))//')'
  write(filename,fmt=formatline) trim(filename)//'.',timestep
end if

filename = trim(filename)//'.'//fileformatl

! create latest link before the filename has the leading directory
if (.not.debug_dump) then
  linkname = "output/latest."//trim(gmesh(gmesh_number)%basename)
  if (centring /= 'all')  linkname = trim(linkname)//'.'//centring
  linkname = trim(linkname)//'.'//fileformatl
  write(system_command,'(a)') 'ln -sf '//trim(filename)//' '//trim(linkname)
  call system(system_command)
end if

! add leading directory and open file
filename = "output/"//trim(filename)
open(foutput,file=trim(filename),status='replace',iostat=error)
if (error /= 0) call error_stop('problem opening file '//trim(filename))

! write gmsh format intro stuff
! TODO: look at data-size

! step is either timestep or newtstep, depending on whether simulation is transient or not
if (transient_simulation.and..not.intermediate) then
  step = timestep
else if (use_newtstep.or.intermediate) then ! for intermediate output it makes sense to employ newtstep so that files form a sequence
  step = newtstep
else
  step = 0 ! this makes it easier to use gmsh's plugins for steady-state problems
end if

if (fileformatl == 'msh') then
  call write_mesh_msh(gmesh_number,centring)
else if (debug_dump) then
  call error_stop('fileformat '//trim(fileformatl)//' does not support the debug_dump option')
else if (fileformatl == 'vtk') then
  call write_mesh_vtk(gmesh_number,centring,ngelements)
else if (fileformatl == 'dat') then
  call write_dat(gmesh_number,centring,ngelements)
else
  call error_stop('fileformat '//trim(fileformatl)//' not supported')
end if

! if we are not outputting data then return here
if (fileformatl == 'msh') then
  if ((trim(check_option(gmesh(gmesh_number)%options,output_gmesh_options)) == "centringmeshoutput".or. &
    trim(check_option(gmesh(gmesh_number)%options,output_gmesh_options)) == "meshoutput").and..not.debug_dump) then
    close(foutput)
    return
  end if
else if (fileformatl == 'vtk') then
  if (trim(check_option(gmesh(gmesh_number)%options,vtkoutput_gmesh_options)) == "centringmeshvtkoutput".or. &
    trim(check_option(gmesh(gmesh_number)%options,vtkoutput_gmesh_options)) == "meshvtkoutput") then
    close(foutput)
    return
  end if
  formatline = '(/a,'//trim(indexformat)//')'
  write(foutput,fmt=formatline) 'CELL_DATA ',ngelements
else if (fileformatl == 'dat') then
! dat file is completely written in write_dat subroutine so return
  close(foutput)
  return
end if

!---------------------------------------------------------------------------------
! component variables

if (fileformatl == 'msh') then ! other file formats only support compound variables
  do m = 1, ubound(var,1) ! loop through all component variables

    if (.not.(var(m)%centring == centring.or.centring == 'all')) cycle ! cycle if not the correct centring
    if (trim(check_option(var(m)%options,output_options)) == "nooutput".and..not.debug_dump) cycle ! cycle if we are not outputting this variable
    if (debug_dump.and.trim(var(m)%type) == 'local') cycle ! don't dump local variables in debug as they may have undefined elements (such as lastface/cell averaging)

    if (var(m)%centring == 'none') then
  ! none centred var components, done as Data regardless of any options

      call write_data_description_msh('component',m,'Data',step,1,1)

      formatline = '('//trim(indexformat)//',a,'//trim(realformat)//')'
      write(foutput,fmt=formatline) 1,' ',trunk_dble(var_value(m,1))
      write(foutput,'(a)') '$EndData'
      if (debug) write(91,*) 'component data (none): m = ',m,': var = ',trim(var(m)%name)

    else if (gmesh(gmesh_number)%ngelements > 0) then
  ! either cell or face centred - some elements must be defined for this

  ! count elements of the var's centring
      ngelements = 0
      do gelement = 1, ubound(gmesh(gmesh_number)%gelement,1)
        if (var(m)%centring == 'cell') then
          ijk = gmesh(gmesh_number)%gelement(gelement)%icell
        else if (var(m)%centring == 'face') then
          ijk = gmesh(gmesh_number)%gelement(gelement)%jface
        else
          ijk = gmesh(gmesh_number)%gelement(gelement)%knode
        end if
        if (ijk == 0) cycle
        ns = region(var(m)%region_number)%ns(ijk)
        if (ns == 0) cycle
        if (select_elements) then
          if (var(m)%centring == 'cell'.and.location_in_list(array=select_cells,element=ijk) == 0) cycle
          if (var(m)%centring == 'face'.and.location_in_list(array=select_faces,element=ijk) == 0) cycle
        end if
        ngelements = ngelements + 1
      end do

  ! find data option - if it is specified for the gmesh, then this overwrites variable value
      data_option = trim(check_option(gmesh(gmesh_number)%options,data_options))
      if (data_option == '') data_option = trim(check_option(var(m)%options,data_options))

      if (debug_dump.or.trim(data_option) == "elementdata".or.var(m)%centring /= 'cell') then
! NB: face and node centred data can't be output with elementnodedata as the grad calcs required cell centred data

! write out in ElementData format
        call write_data_description_msh('component',m,'ElementData',step,1,ngelements)
        do gelement = 1, ubound(gmesh(gmesh_number)%gelement,1)
          if (var(m)%centring == 'cell') then
            ijk = gmesh(gmesh_number)%gelement(gelement)%icell
          else if (var(m)%centring == 'face') then
            ijk = gmesh(gmesh_number)%gelement(gelement)%jface
          else
            ijk = gmesh(gmesh_number)%gelement(gelement)%knode
          end if
          if (ijk == 0) cycle
          ns = region(var(m)%region_number)%ns(ijk)
          if (ns == 0) cycle
          if (select_elements) then
            if (var(m)%centring == 'cell'.and.location_in_list(array=select_cells,element=ijk) == 0) cycle
            if (var(m)%centring == 'face'.and.location_in_list(array=select_faces,element=ijk) == 0) cycle
          end if
          if (debug) write(91,*) 'component elementdata: gelement = ',gelement,': centring = ',var(m)%centring, &
            ': ijk = ',ijk,': ns = ',ns,': m = ',m,': var = ',trim(var(m)%name)
          formatline = '('//trim(indexformat)//',a,'//trim(realformat)//')'
          write(foutput,fmt=formatline) gelement,' ',trunk_dble(var_value(m,ns))
        end do
        write(foutput,'(a)') '$EndElementData'

      else if (trim(data_option) == "elementnodedata".or.trim(data_option) == "elementnodelimiteddata") then

!       if (trim(data_option) == "elementnodelimiteddata") &
!         write(*,*) 'WARNING: elementnodelimiteddata not implemented yet in write_gmesh'

  ! write out in ElementNodeData format - only for cell centred
        call write_data_description_msh('component',m,'ElementNodeData',step,1,ngelements)
        allocate(cellgradl(totaldimensions,1))
        do gelement = 1, ubound(gmesh(gmesh_number)%gelement,1)
          i = gmesh(gmesh_number)%gelement(gelement)%icell
          if (i == 0) cycle
          ns = region(var(m)%region_number)%ns(i)
          if (ns == 0) cycle
          if (select_elements) then
            if (location_in_list(array=select_cells,element=i) == 0) cycle
          end if
          if (trim(data_option) == "elementnodelimiteddata") then
            cellgradl(:,1) = varcgrad_nodelimited(i,m)
          else
            cellgradl(:,1) = varcgrad(i,m)
          end if
          if (debug) write(91,*) 'component elementnodedata: gelement = ',gelement,': centring = ',var(m)%centring, &
            ': i = ',i,': ns = ',ns,': m = ',m,': var = ',trim(var(m)%name),': cellgrad = ',cellgradl(:,1)
          nnodes = gtype_list(cell(i)%gtype)%nnodes
          formatline = '('//trim(indexformat)//',a,i3'//repeat(',a,'//trim(realformat),nnodes)//')'
          write(foutput,fmt=formatline) gelement,' ',nnodes, &
            (' ',trunk_dble(var_value(m,ns)+dot_product(cellgradl(:,1),node(cell(i)%knode(kk))%x-cell(i)%x)),kk=1,nnodes)
        end do
        deallocate(cellgradl)
        write(foutput,'(a)') '$EndElementNodeData'

      end if

    end if

  end do

  ! return here if debug_dump only
  if (debug_dump) then
    close(foutput)
    return
  end if
end if

!---------------------------------------------------------------------------------
! compound variables

do mc = 1, ubound(compound,1) ! loop through all compound variables

  if (.not.(compound(mc)%centring == centring.or.centring == 'all')) cycle ! cycle if not the correct centring
  if (trim(check_option(compound(mc)%options,output_options)) == "nooutput") cycle ! cycle if we are not outputting this variable

  nrank = ubound(compound(mc)%component,1)

  if (compound(mc)%centring == 'none') then
    if (fileformatl == 'vtk') cycle
! none centred var components, done as Data

    call write_data_description_msh('compound',mc,'Data',step,nrank,1)
    if (debug) write(91,*) 'compound data (none): mc = ',mc,': compound = ',trim(compound(mc)%name)
    allocate(cellvaluel(nrank))
    do n = 1, nrank
      m = compound(mc)%component(n)
      if (m == 0) then
        cellvaluel(n) = 0.d0
      else
        cellvaluel(n) = var_value(m,1)
      end if
    end do
    formatline = '('//trim(indexformat)//repeat(',a,'//trim(realformat),nrank)//')'
    write(foutput,fmt=formatline) 1,(' ',trunk_dble(cellvaluel(n)),n=1,nrank)
    deallocate(cellvaluel)
    write(foutput,'(a)') '$EndData'

  else if (gmesh(gmesh_number)%ngelements > 0) then
! either cell, face or node centred

! count elements of the var's centring if msh fileformat
    if (fileformatl == 'msh') then
      ngelements = 0
      do gelement = 1, ubound(gmesh(gmesh_number)%gelement,1)
        if (compound(mc)%centring == 'cell') then
          ijk = gmesh(gmesh_number)%gelement(gelement)%icell
        else if (compound(mc)%centring == 'face') then
          ijk = gmesh(gmesh_number)%gelement(gelement)%jface
        else
          ijk = gmesh(gmesh_number)%gelement(gelement)%knode
        end if
        if (ijk == 0) cycle
        ns = region(compound(mc)%region_number)%ns(ijk)
        if (ns == 0) cycle
        if (select_elements) then
          if (compound(mc)%centring == 'cell'.and.location_in_list(array=select_cells,element=ijk) == 0) cycle
          if (compound(mc)%centring == 'face'.and.location_in_list(array=select_faces,element=ijk) == 0) cycle
        end if
        ngelements = ngelements + 1
      end do
    end if

! find data option - if it is specified for the gmesh, then this overwrites variable value
    data_option = trim(check_option(gmesh(gmesh_number)%options,data_options))
    if (data_option == '') data_option = trim(check_option(compound(mc)%options,data_options))

! write out in ElementData format - this is one data object per cell
    if (trim(data_option) == "elementdata".or.compound(mc)%centring /= 'cell'.or.fileformatl == 'vtk') then

! write data intro statements
      if (fileformatl == 'msh') then
        call write_data_description_msh('compound',mc,'ElementData',step,nrank,ngelements)
      else if (fileformatl == 'vtk') then
! vtk format is picky - remove spaces from label name
        write(foutput,'(/a)') changecase('U',trim(compound(mc)%rank))//'S '// &
          removespaces(trim(compound(mc)%name))//' double'
        if (nrank == 1) write(foutput,'(a)') 'LOOKUP_TABLE default' ! required only for scalars
      end if

      allocate(cellvaluel(nrank))
      do gelement = 1, ubound(gmesh(gmesh_number)%gelement,1)
        if (compound(mc)%centring == 'cell') then
          ijk = gmesh(gmesh_number)%gelement(gelement)%icell
        else if (compound(mc)%centring == 'face') then
          ijk = gmesh(gmesh_number)%gelement(gelement)%jface
        else
          ijk = gmesh(gmesh_number)%gelement(gelement)%knode
        end if
!------------------
        if (fileformatl == 'msh') then
          if (ijk == 0) cycle
          ns = region(compound(mc)%region_number)%ns(ijk)
          if (ns == 0) cycle
          if (select_elements) then
            if (compound(mc)%centring == 'cell'.and.location_in_list(array=select_cells,element=ijk) == 0) cycle
            if (compound(mc)%centring == 'face'.and.location_in_list(array=select_faces,element=ijk) == 0) cycle
          end if
          if (debug) write(91,*) 'compound elementdata: gelement = ',gelement,': centring = ',compound(mc)%centring, &
            ': ijk = ',ijk,': ns = ',ns,': mc = ',mc,': compound = ',trim(compound(mc)%name)
          do n = 1, nrank
            m = compound(mc)%component(n)
            if (m == 0) then
              cellvaluel(n) = 0.d0
            else
              cellvaluel(n) = var_value(m,ns)
            end if
          end do
          formatline = '('//trim(indexformat)//repeat(',a,'//trim(realformat),nrank)//')'
          write(foutput,fmt=formatline) gelement,(' ',trunk_dble(cellvaluel(n)),n=1,nrank)
!------------------
        else if (fileformatl == 'vtk') then
          cellvaluel = 0.d0
          if (ijk == 0) then
! if this gelement doesn't have an ijk index (which will correspond to the centring of the variable and the gmesh output), then it won't have been included in the list of cells if we have some specific centring
! if we are doing all centring, then need to output a value regardless
            if (centring /= 'all') cycle
          else
            ns = region(compound(mc)%region_number)%ns(ijk)
! even if this variable doesn't have a value in this face/cell element, vtk still requires a value output
            if (ns /= 0) then
              do n = 1, nrank
                m = compound(mc)%component(n)
                if (m /= 0) cellvaluel(n) = var_value(m,ns)
              end do
            end if
          end if
          formatline = '('//trim(realformat)//repeat(',a,'//trim(realformat),nrank-1)//')'
          write(foutput,fmt=formatline) trunk_dble(cellvaluel(1)),(' ',trunk_dble(cellvaluel(n)),n=2,nrank)
!------------------
        end if
      end do
      deallocate(cellvaluel)

      if (fileformatl == 'msh') write(foutput,'(a)') '$EndElementData'

! write out in ElementNodeData format - which is a value for every node within every cell
! only for cell centred data in msh file format
    else if (trim(data_option) == "elementnodedata".or.trim(data_option) == "elementnodelimiteddata") then

!     if (trim(data_option) == "elementnodelimiteddata") &
!       write(*,*) 'WARNING: elementnodelimiteddata not implemented yet in write_gmesh'

      call write_data_description_msh('compound',mc,'ElementNodeData',step,nrank,ngelements)
      allocate(cellvaluel(nrank))
      allocate(cellgradl(totaldimensions,nrank))
      do gelement = 1, ubound(gmesh(gmesh_number)%gelement,1)
        i = gmesh(gmesh_number)%gelement(gelement)%icell
        if (i == 0) cycle
        ns = region(compound(mc)%region_number)%ns(i)
        if (ns == 0) cycle
        if (select_elements) then
          if (location_in_list(array=select_cells,element=i) == 0) cycle
        end if
        if (debug) write(91,*) 'compound elementnodedata: gelement = ',gelement,': centring = ',compound(mc)%centring, &
          ': i = ',i,': ns = ',ns,': mc = ',mc,': compound = ',trim(compound(mc)%name)
        do n = 1, nrank
          m = compound(mc)%component(n)
          if (m == 0) then
            cellvaluel(n) = 0.d0
            cellgradl(:,n) = [ 0.d0, 0.d0, 0.d0 ]
          else
            cellvaluel(n) = var_value(m,ns)
!           cellgradl(:,n) = varcgrad(i,m)
            if (trim(data_option) == "elementnodelimiteddata") then
              cellgradl(:,n) = varcgrad_nodelimited(i,m)
            else
              cellgradl(:,n) = varcgrad(i,m)
            end if
          end if
        end do
        nnodes = gtype_list(cell(i)%gtype)%nnodes
        formatline = '('//trim(indexformat)//',a,i3'//repeat(',a,'//trim(realformat),nnodes*nrank)//')'
        write(foutput,fmt=formatline) gelement,' ',nnodes, &
          (( ' ', trunk_dble(cellvaluel(n) + dot_product(cellgradl(:,n),node(cell(i)%knode(kk))%x-cell(i)%x )), &
          n=1,nrank),kk=1,nnodes)
      end do
      deallocate(cellvaluel,cellgradl)
      write(foutput,'(a)') '$EndElementNodeData'

    end if

  end if

end do

close(foutput)

if (debug) write(*,'(80(1h+)/a)') 'subroutine write_gmesh'

end subroutine write_gmesh

!-----------------------------------------------------------------

subroutine output_step(action,do_update_outputs)

! this subroutine controls the writing of the output.step file, containing a small amount of regularly-written data
!  (typically written once per timestep or newtstep)

use general_module
use equation_module
use gmesh_module
character(len=*) :: action
logical, optional, intent(in) :: do_update_outputs
integer :: mm, m, mc, nvar, region_number, nvar_local, ns, error, name_length
integer, dimension(:), allocatable :: m_list
character(len=100) :: filename, cut_name
character(len=10000) :: formatline
character(len=2) :: cont_bit
character(len=1) :: separator = ',' ! for csv files
logical :: therel, do_update_outputs_local
logical, parameter :: debug = .false.
                  
if (debug) write(*,'(80(1h+)/a)') 'subroutine output_step'

if (trim(action) == "setup") then

! set output.step file writing option is specified as default
  if (trim(output_step_file) == "default" .or. trim(output_step_file) == "on") then
    if (transient_simulation) then
      output_step_file = "timestep"
    else
      output_step_file = "newtstep"
    end if
  end if
  if (.not.(trim(output_step_file) == "timestep" .or. trim(output_step_file) == "newtstep" .or. &
    trim(output_step_file) == "final")) then
    if (trim(output_step_file) /= "off") then
      write(*,'(a)') "WARNING: output_step_file = "//trim(output_step_file)// &
        " is not understood: instead setting output_step_file = off"
      output_step_file = "off"
    end if
  end if
  if (debug) write(*,*) "final output_step_file = "//trim(output_step_file)

  if (trim(output_step_file) == "off") return

! run through all compounds, checking whether either compounds or components have outputstep set, and totalling number of variables
  nvar = 0
  do mc = 1, ubound(compound,1) ! loop through all compound variables
    region_number = compound(mc)%region_number
    if (region_number == 0) then
      nvar_local = 1
    else
      nvar_local = ubound(region(region_number)%ijk,1)
    end if
    do mm = 1, ubound(compound(mc)%component,1)
      m = compound(mc)%component(mm)
      if (m == 0) cycle
      if ( trim(check_option(compound(mc)%options,stepoutput_options)) == "stepoutput" .or. &
           trim(check_option(var(m)%options,stepoutput_options)) == "stepoutput" .or. &
           trim(check_option(compound(mc)%options,stepoutput_options)) == "stepoutputnoupdate" .or. &
           trim(check_option(var(m)%options,stepoutput_options)) == "stepoutputnoupdate" ) then
        nvar = nvar + nvar_local
        call push_array(array=m_list,new_element=m)
        if (debug) write(*,*) "  positive stepoutput for variable "//trim(var(m)%name)//" (",m,") with ", &
          nvar_local," elements from region_number ",var(m)%region_number
      end if
    end do
  end do
  if (debug) write(*,*) "number of variables to be output: nvar = ",nvar

! allocate data structure and set name, m and ns
  if (nvar > 0) then
    allocate(output_step_variable(nvar))
    nvar = 0
    do mm = 1, ubound(m_list,1)
      m = m_list(mm)
      region_number = var(m)%region_number
      if (region_number == 0) then
        nvar = nvar + 1
        output_step_variable(nvar)%name = "'"//trim(var(m)%name)//"'"
        output_step_variable(nvar)%m = m
        output_step_variable(nvar)%ns = 1
      else
! reform name by including ns in [] 
        name_length = len_trim(var(m)%name)
        if (var(m)%name(name_length-1:name_length-1) == "]") then
          cut_name = var(m)%name(1:name_length-2)//","
        else
          cut_name = var(m)%name(1:name_length-1)//"["
        end if
        do ns = 1, ubound(var(m)%funk,1)
          nvar = nvar + 1
          formatline = '(a,'//trim(dindexformat(ns))//',a)'
          write(output_step_variable(nvar)%name,fmt=formatline) "'"//trim(cut_name)//"n=",ns,"]>'"
          output_step_variable(nvar)%m = m
          output_step_variable(nvar)%ns = ns
        end do
      end if
      output_step_variable(nvar)%units = "'["//trim(var(m)%units)//"]'" ! units are delimited by []
    end do
    deallocate(m_list)
  end if

! open file
! filename = "output/output.step"
  filename = "output/output_step.csv"
  therel = .false.
  inquire (file=trim(filename),exist=therel)
  open(foutputstep,file=trim(filename),access='append',iostat=error)
  if (error /= 0) call error_stop('problem opening file '//trim(filename))

! write out a header to the file, including variable names which are commented out if this is a continuation run
  formatline = '(a,f4.2,a)'
  write(foutputstep,fmt=formatline) '# output from program arb, version ',version,' ('//trim(versionname)// &
    '), written by dalton harvie'
  call print_simulation_info(foutputstep,comment="#")
  if (therel) then
    write(foutputstep,'(a)') '# continuation from previous run'
    cont_bit = "# "
  else
    cont_bit = ""
  end if

! write out variable names and units (v0.42)
  if (transient_simulation) then
    formatline = '(a'//repeat(',a,a',ubound(output_step_variable,1)+1)//')'
    write(foutputstep,fmt=formatline) trim(cont_bit)//"'<timestep>'",separator,"'<newtstep>'", &
      (separator,trim(output_step_variable(nvar)%name),nvar=1,ubound(output_step_variable,1))
    write(foutputstep,fmt=formatline) trim(cont_bit)//"'[1]'",separator,"'[1]'", &
      (separator,trim(output_step_variable(nvar)%units),nvar=1,ubound(output_step_variable,1))
  else
    formatline = '(a'//repeat(',a,a',ubound(output_step_variable,1))//')'
    write(foutputstep,fmt=formatline) trim(cont_bit)//"'<newtstep>'", &
      (separator,trim(output_step_variable(nvar)%name),nvar=1,ubound(output_step_variable,1))
    write(foutputstep,fmt=formatline) trim(cont_bit)//"'[1]'", &
      (separator,trim(output_step_variable(nvar)%units),nvar=1,ubound(output_step_variable,1))
  end if
  call flush(foutputstep)

else if (trim(action) == "write") then
! write the data

! see if outputs need to be updated
  do_update_outputs_local = .true.
  if (present(do_update_outputs)) then
    do_update_outputs_local = do_update_outputs
  end if
  if (do_update_outputs_local) call update_outputs(stepoutput=.true.) ! update any output-only variables

  if (transient_simulation) then
    formatline = '('//trim(dindexformat(timestep))//',a,'//trim(dindexformat(newtstep))// &
      repeat(',a,'//trim(realformat),ubound(output_step_variable,1))//')'
    write(foutputstep,fmt=formatline) timestep,separator,newtstep, &
      (separator,trunk_dble(var_value(output_step_variable(nvar)%m,output_step_variable(nvar)%ns)), &
      nvar=1,ubound(output_step_variable,1))
  else
    formatline = '('//trim(dindexformat(newtstep))//repeat(',a,'//trim(realformat),ubound(output_step_variable,1))//')'
    write(foutputstep,fmt=formatline) newtstep, &
      (separator,trunk_dble(var_value(output_step_variable(nvar)%m,output_step_variable(nvar)%ns)), &
      nvar=1,ubound(output_step_variable,1))
  end if
  call flush(foutputstep)

else if (trim(action) == "close") then
! close file if open

  if (trim(output_step_file) /= "off") close(foutputstep)

else
  call error_stop("unknown action = "//trim(action)//" given to subroutine output_step")
end if

if (debug) write(*,'(a/80(1h-))') 'subroutine output_step'

end subroutine output_step

!-----------------------------------------------------------------

subroutine write_data_description_msh(structure,m,output_format,step,nrank,ngelements)

! little subroutine to write to foutput the descriptive strings for a component or variable

use general_module
character(len=*) :: structure ! whether this is the compound or component
character(len=*) :: output_format ! gmsh output format
integer :: m ! number of the compound or component
integer :: step ! either timestep or newton step
integer :: nrank ! number of components in object
integer :: ngelements ! number of elements being output
character(len=1000) :: formatline

! first write an output section for arb reprocessing
write(foutput,'(a)') '$arb'//trim(output_format)
formatline = '(a,6(/a))'
if (trim(structure) == "component") then
  write(foutput,fmt=formatline) '"name='//trim(var(m)%name)//'"', &
                                '"centring='//trim(var(m)%centring)//'"', &
                                '"region='//trim(var(m)%region)//'"', &
                                '"rank='//trim(var(m)%rank)//'"', &
                                '"type='//trim(var(m)%type)//'"', &
                                '"units='//trim(var(m)%units)//'"', &
                                '"structure=component"'
else if (trim(structure) == "compound") then
  write(foutput,fmt=formatline) '"name='//trim(compound(m)%name)//'"', &
                                '"centring='//trim(compound(m)%centring)//'"', &
                                '"region='//trim(compound(m)%region)//'"', &
                                '"rank='//trim(compound(m)%rank)//'"', &
                                '"type='//trim(compound(m)%type)//'"', &
                                '"units='//trim(compound(m)%units)//'"', &
                                '"structure=compound"'
end if
write(foutput,'(a)') '$Endarb'//trim(output_format)

! now write gmesh data descriptor
formatline = '(a/i1/a/i1/i1/'//trim(dindexformat(step))//'/i1/'//trim(dindexformat(ngelements))//')'
if (trim(structure) == "component") then
  write(foutput,fmt=formatline) '$'//trim(output_format),1,'"'//trim(var(m)%name)//'"',0,3,step,nrank,ngelements
else if (trim(structure) == "compound") then
  write(foutput,fmt=formatline) '$'//trim(output_format),1,'"'//trim(compound(m)%name)//'"',0,3,step,nrank,ngelements
end if

end subroutine write_data_description_msh

!-----------------------------------------------------------------

subroutine write_mesh_msh(gmesh_number,centring)

! writes the mesh to foutput in the (gmsh) format msh

use general_module
use gmesh_module
integer :: gmesh_number ! output data concerned with this gmesh
character(len=*) :: centring ! whether to output only cell, face, none or all centred elements (default all)
integer :: gregion, region_number, gnode, k, ngelements, gelement, i, j, l, dimensions, geo_entity
character(len=1000) :: formatline
logical, parameter :: debug = .false.
                  
if (debug) write(*,'(80(1h+)/a)') 'subroutine write_mesh_msh'

write(foutput,'(a/a/a)') '$MeshFormat','2.2 0 8','$EndMeshFormat'
write(foutput,'(a)') '$arbSimulationInfo'
call print_simulation_info(foutput)
write(foutput,'(a)') '$EndarbSimulationInfo'

if (centring /= 'none') then ! mesh data not required for none centred file

! list regions (irrespective of centring)
  if (gmesh(gmesh_number)%ngregions > 0) then
    formatline = '(a/'//trim(indexformat)//')'
    write(foutput,fmt=formatline) '$PhysicalNames',gmesh(gmesh_number)%ngregions
    do gregion = 1, ubound(gmesh(gmesh_number)%gregion,1)
      region_number = gmesh(gmesh_number)%gregion(gregion)%region_number
      dimensions = region(region_number)%dimensions
      if (region_number /= 0) write(foutput,'(i1,a,i3,a,a,a)') dimensions,' ',gregion, &
        ' "',trim(region(region_number)%name),'"'
    end do
    write(foutput,'(a)') '$EndPhysicalNames'
  end if

! list nodes (irrespective of centring)
  if (gmesh(gmesh_number)%ngnodes > 0) then
    formatline = '(a/'//trim(indexformat)//')'
    write(foutput,fmt=formatline) '$Nodes',gmesh(gmesh_number)%ngnodes
    formatline = '('//trim(indexformat)//',3(a,'//trim(realformat)//'))'
    do gnode = 1, ubound(gmesh(gmesh_number)%knode_from_gnode,1)
      k = gmesh(gmesh_number)%knode_from_gnode(gnode)
!     if (k /= 0) write(foutput,fmt=formatline) gnode,' ',node(k)%x(1),' ',node(k)%x(2),' ',node(k)%x(3)
      if (k /= 0) write(foutput,fmt=formatline) gnode,(' ',trunk_dble(node(k)%x(l)*gmesh(gmesh_number)%output_scale), &
        l=1,totaldimensions)
    end do
    write(foutput,'(a)') '$EndNodes'
  end if

! list domain cells and boundary faces as elements, using repetition to output multiple gregions
! note: elements are output multiple times for all regions that they belong to, even if the gregion centring isn't the same as the file (centringoutput) centring
! so, if centring of file is all and an element is both a face and a node (say), doesn't matter which is chosen here for output purposes (line in file will be the same)
  if (gmesh(gmesh_number)%ngelements > 0) then

    ngelements = gmesh(gmesh_number)%ngelements
    if (centring == 'cell') ngelements = gmesh(gmesh_number)%ngelements_cell
    if (centring == 'face') ngelements = gmesh(gmesh_number)%ngelements_face
    if (centring == 'node') ngelements = gmesh(gmesh_number)%ngelements_node
    formatline = '(a/'//trim(indexformat)//')'
    write(foutput,fmt=formatline) '$Elements', ngelements

! this code repeats elements for regions that are not necessarily of the same centring as the (centringoutput) file, but the overhead is low
! specifically overhead is only for boundary cells and faces that elements of more than one boundary face region (in the cell centred file)
! and for elements that are both faces and nodes (ie, a 1D simulation) and in more than one region (in the node and face centred files)
! no overhead for normal output
! if an element is not a member of any gregion then it will have one 0 entry, and it doesn't matter whether i, j or k (provided they are non-zero) are used to output the element
    do gelement = 1, ubound(gmesh(gmesh_number)%gelement,1)
      i = gmesh(gmesh_number)%gelement(gelement)%icell 
      j = gmesh(gmesh_number)%gelement(gelement)%jface
      k = gmesh(gmesh_number)%gelement(gelement)%knode
      if (i /= 0.and.(centring == 'cell'.or.centring == 'all')) then
! output elements in their reverse region order for consistency with gmsh
        do gregion = ubound(gmesh(gmesh_number)%gelement(gelement)%gregions,1), 1, -1
! now only output two integer tags per element: the physical entity and the 'elementary geometrical entity' which is now (v0.50) set equal to the physical entity number plus 3 if part of a physical region, or the dimension otherwise if not a point - If a point, then the gnode number + 3 + ngregions
          if (cell(i)%dimensions == 0) then
            formatline = '('//trim(indexformat)//',3(a,i3,)'//repeat(',a,'//trim(indexformat),ubound(cell(i)%knode,1)+1)//')'
            geo_entity = 3 + gmesh(gmesh_number)%ngregions + gmesh(gmesh_number)%gnode_from_knode(cell(i)%knode(1)) ! if it is a point set equal to gnode + ngregions + 3
          else
            formatline = '('//trim(indexformat)//',4(a,i3,)'//repeat(',a,'//trim(indexformat),ubound(cell(i)%knode,1))//')'
            geo_entity = gmesh(gmesh_number)%gelement(gelement)%gregions(gregion)
            if (geo_entity == 0) then
              geo_entity = cell(i)%dimensions
            else
              geo_entity = geo_entity + 3
            end if
          end if
          write(foutput,fmt=formatline) gelement,' ',cell(i)%gtype,' ',2,' ', &
            gmesh(gmesh_number)%gelement(gelement)%gregions(gregion),' ', &
            geo_entity, &
            (' ',gmesh(gmesh_number)%gnode_from_knode(cell(i)%knode(l)),l=1,ubound(cell(i)%knode,1))
        end do
      else if (j /= 0.and.(centring == 'face'.or.centring == 'all')) then
        do gregion = ubound(gmesh(gmesh_number)%gelement(gelement)%gregions,1), 1, -1
          if (face(j)%dimensions == 0) then
            formatline = '('//trim(indexformat)//',3(a,i3,)'//repeat(',a,'//trim(indexformat),ubound(face(j)%knode,1)+1)//')'
            geo_entity = 3 + gmesh(gmesh_number)%ngregions + gmesh(gmesh_number)%gnode_from_knode(face(j)%knode(1)) ! if it is a point set equal to gnode + ngregions + 3
          else
            formatline = '('//trim(indexformat)//',4(a,i3,)'//repeat(',a,'//trim(indexformat),ubound(face(j)%knode,1))//')'
            geo_entity = gmesh(gmesh_number)%gelement(gelement)%gregions(gregion)
            if (geo_entity == 0) then
              geo_entity = face(j)%dimensions
            else
              geo_entity = geo_entity + 3
            end if
          end if
          write(foutput,fmt=formatline) gelement,' ',face(j)%gtype,' ',2,' ', &
            gmesh(gmesh_number)%gelement(gelement)%gregions(gregion),' ', &
            geo_entity, &
            (' ',gmesh(gmesh_number)%gnode_from_knode(face(j)%knode(l)),l=1,ubound(face(j)%knode,1))
        end do
      else if (k /= 0.and.(centring == 'node'.or.centring == 'all')) then
! nodes elements have only one knode attached, and have are of node_gtype
        formatline = '('//trim(indexformat)//',3(a,i3,),2(a,'//trim(indexformat)//'))' ! elementary node element region can become large
        do gregion = ubound(gmesh(gmesh_number)%gelement(gelement)%gregions,1), 1, -1
          geo_entity = 3 + gmesh(gmesh_number)%ngregions + gmesh(gmesh_number)%gnode_from_knode(k) ! for a point set equal to gnode + ngregions + 3
          write(foutput,fmt=formatline) gelement,' ',node_gtype,' ',2,' ', &
            gmesh(gmesh_number)%gelement(gelement)%gregions(gregion),' ', &
! point mesh elements in gmsh require a unique element number - use the gnode + the maximum physical entity for this mesh + 3
            geo_entity, &
            ' ',gmesh(gmesh_number)%gnode_from_knode(k)
        end do
      end if

    end do
    write(foutput,'(a)') '$EndElements'
  end if

end if

if (debug) write(*,'(a/80(1h-))') 'subroutine write_mesh_msh'

end subroutine write_mesh_msh

!-----------------------------------------------------------------

subroutine write_mesh_vtk(gmesh_number,centring,ngelements)

! writes the mesh to foutput in the format vtk

use general_module
use gmesh_module
integer :: gmesh_number ! output data concerned with this gmesh
character(len=*) :: centring ! whether to output only cell, face, none or all centred elements (default all)
integer :: ngelements ! number of elements that will be output
integer :: gnode, k, gelement, i, j, l, nsize
character(len=1000) :: formatline
logical, parameter :: debug = .false.
                  
if (debug) write(*,'(80(1h+)/a)') 'subroutine write_mesh_vtk'

! this vtk format does not support comments and is fiddly with line breaks
write(foutput,'(a//a/a)') '# vtk DataFile Version 3.0','ASCII','DATASET UNSTRUCTURED_GRID'

if (centring /= 'none') then ! mesh data not required for none centred file

! NB: regions are not used in vtk format

! list nodes (irrespective of centring)
  if (gmesh(gmesh_number)%ngnodes > 0) then
    formatline = '(/a,'//trim(indexformat)//',a)'
    write(foutput,fmt=formatline) 'POINTS ',gmesh(gmesh_number)%ngnodes,' double'
    formatline = '('//trim(realformat)//',2(a,'//trim(realformat)//'))'
    do gnode = 1, ubound(gmesh(gmesh_number)%knode_from_gnode,1)
      k = gmesh(gmesh_number)%knode_from_gnode(gnode)
      if (k /= 0) write(foutput,fmt=formatline) trunk_dble(node(k)%x(1)*gmesh(gmesh_number)%output_scale), &
        (' ',trunk_dble(node(k)%x(l)*gmesh(gmesh_number)%output_scale),l=2,totaldimensions)
    end do
  end if

! list domain cells and boundary faces as elements, without repetition
  if (gmesh(gmesh_number)%ngelements > 0) then ! the stored ngelements includes repetition, suitable for full gmsh format
! first count the total number of cell list entries
    nsize = 0
    ngelements = 0
    do gelement = 1, ubound(gmesh(gmesh_number)%gelement,1)
      i = gmesh(gmesh_number)%gelement(gelement)%icell 
      j = gmesh(gmesh_number)%gelement(gelement)%jface
      k = gmesh(gmesh_number)%gelement(gelement)%knode
      if (i /= 0.and.(centring == 'cell'.or.centring == 'all')) then
        ngelements = ngelements + 1
        nsize = nsize + 1 + ubound(cell(i)%knode,1)
        if (gtype_list(cell(i)%gtype)%vtk_type == 0) &
          call error_stop("the vtk file format does not allow "//trim(gtype_list(cell(i)%gtype)%name)//" elements")
      else if (j /= 0.and.(centring == 'face'.or.centring == 'all')) then
        ngelements = ngelements + 1
        nsize = nsize + 1 + ubound(face(j)%knode,1)
        if (gtype_list(face(j)%gtype)%vtk_type == 0) &
          call error_stop("the vtk file format does not allow "//trim(gtype_list(face(j)%gtype)%name)//" elements")
      else if (k /= 0.and.(centring == 'node'.or.centring == 'all')) then
        ngelements = ngelements + 1
        nsize = nsize + 1 + 1
        if (gtype_list(node_gtype)%vtk_type == 0) &
          call error_stop("the vtk file format does not allow "//trim(gtype_list(face(j)%gtype)%name)//" elements")
      end if
    end do
! now write the cell node lists
! NB: index of first node is 0
    formatline = '(/a,'//trim(indexformat)//',a,'//trim(indexformat)//')'
    write(foutput,fmt=formatline) 'CELLS ',ngelements,' ',nsize
    do gelement = 1, ubound(gmesh(gmesh_number)%gelement,1)
      i = gmesh(gmesh_number)%gelement(gelement)%icell 
      j = gmesh(gmesh_number)%gelement(gelement)%jface
      k = gmesh(gmesh_number)%gelement(gelement)%knode
      if (i /= 0.and.(centring == 'cell'.or.centring == 'all')) then
        formatline = '(i2'//repeat(',a,'//trim(indexformat),ubound(cell(i)%knode,1))//')'
        write(foutput,fmt=formatline) ubound(cell(i)%knode,1), &
          (' ',gmesh(gmesh_number)%gnode_from_knode(cell(i)%knode(gtype_list(cell(i)%gtype)%vtk_nodes(l)))-1, &
          l=1,ubound(cell(i)%knode,1))
      else if (j /= 0.and.(centring == 'face'.or.centring == 'all')) then
        formatline = '(i2'//repeat(',a,'//trim(indexformat),ubound(face(j)%knode,1))//')'
        write(foutput,fmt=formatline) ubound(face(j)%knode,1), &
          (' ',gmesh(gmesh_number)%gnode_from_knode(face(j)%knode(gtype_list(face(j)%gtype)%vtk_nodes(l)))-1, &
          l=1,ubound(face(j)%knode,1))
      else if (k /= 0.and.(centring == 'node'.or.centring == 'all')) then
        formatline = '(i2,a,'//trim(indexformat)//')'
        write(foutput,fmt=formatline) 1,' ',gmesh(gmesh_number)%gnode_from_knode(k)
      end if
    end do
! now write cell types
    formatline = '(/a,'//trim(indexformat)//')'
    write(foutput,fmt=formatline) 'CELL_TYPES ',ngelements
    do gelement = 1, ubound(gmesh(gmesh_number)%gelement,1)
      i = gmesh(gmesh_number)%gelement(gelement)%icell 
      j = gmesh(gmesh_number)%gelement(gelement)%jface
      k = gmesh(gmesh_number)%gelement(gelement)%knode
      if (i /= 0.and.(centring == 'cell'.or.centring == 'all')) then
        write(foutput,'(i2)') gtype_list(cell(i)%gtype)%vtk_type
      else if (j /= 0.and.(centring == 'face'.or.centring == 'all')) then
        write(foutput,'(i2)') gtype_list(face(j)%gtype)%vtk_type
      else if (k /= 0.and.(centring == 'node'.or.centring == 'all')) then
        write(foutput,'(i2)') gtype_list(node_gtype)%vtk_type
      end if
    end do

  end if

end if

if (debug) write(*,'(a/80(1h-))') 'subroutine write_mesh_vtk'

end subroutine write_mesh_vtk

!-----------------------------------------------------------------

subroutine write_dat(gmesh_number,centring,ngelements)

! writes the mesh to foutput in the tecplot asci format dat

use general_module
use equation_module
use gmesh_module
integer :: gmesh_number ! output data concerned with this gmesh
character(len=*) :: centring ! whether to output only cell, face, none or all centred elements (default all)
integer :: ngelements ! number of elements that will be output
integer :: gnode, k, gelement, i, j, l, data_dimension, element_vertices, mc, m, mm, ijk, ns
character(len=1000) :: formatline
character(len=10000) :: var_location_string ! good for about 5000 variables to be output
character(len=100) :: zonename, zonetype
integer, dimension(:), allocatable :: var_output_list ! list of variables to be output
double precision :: cellvaluel
logical, parameter :: debug = .false.
                  
if (debug) write(*,'(80(1h+)/a)') 'subroutine write_dat'

!call error_stop('WARNING: write_dat does not support node centred data')

data_dimension = max(maximum_dimensions,1)
if (data_dimension < 1) write(*,*) 'WARNING: data_dimension is too small to output a dat file'

if (trim(simulation_info%title) == '') then
  write(foutput,'(a)') 'TITLE = "arb"'
else
  write(foutput,'(a)') 'TITLE = "'//trim(simulation_info%title)//'"'
end if

! mesh data not required for none centred file
if (trim(centring) == 'none') then
  write(foutput,'(a)') '# no mesh or data written as the file has none centring'
  return
end if

! file not written for dataset that has no nodes
if (gmesh(gmesh_number)%ngnodes == 0) then
  write(foutput,'(a)') '# no mesh or data written as the gmesh has no nodes'
  return
end if

! now count the number of elements that will be written
ngelements = 0
if (gmesh(gmesh_number)%ngelements > 0) then ! the stored ngelements includes repetition, suitable for full gmsh format
! first count the total number of elements
  do gelement = 1, ubound(gmesh(gmesh_number)%gelement,1)
    i = gmesh(gmesh_number)%gelement(gelement)%icell 
    j = gmesh(gmesh_number)%gelement(gelement)%jface
    k = gmesh(gmesh_number)%gelement(gelement)%knode
    if (i /= 0.and.(centring == 'cell'.or.centring == 'all')) then
      ngelements = ngelements + 1
      if (maxval(gtype_list(cell(i)%gtype)%dat_nodes(data_dimension,:)) == 0) &
        call error_stop("the dat file format does not allow "//trim(gtype_list(cell(i)%gtype)%name)//" elements")
    else if (j /= 0.and.(centring == 'face'.or.centring == 'all')) then
      ngelements = ngelements + 1
      if (maxval(gtype_list(face(j)%gtype)%dat_nodes(data_dimension,:)) == 0) &
        call error_stop("the dat file format does not allow "//trim(gtype_list(face(j)%gtype)%name)//" elements")
    else if (k /= 0.and.(centring == 'node'.or.centring == 'all')) then
      ngelements = ngelements + 1
      if (maxval(gtype_list(node_gtype)%dat_nodes(data_dimension,:)) == 0) &
        call error_stop("the dat file format does not allow "//trim(gtype_list(face(j)%gtype)%name)//" elements")
    end if
  end do
end if

! file not written for dataset that has no elements
if (ngelements == 0) then
  write(foutput,'(a)') '# no mesh or data written as the gmesh has no elements'
  return
end if

! assemble zone title name based on step number
if (transient_simulation) then
  formatline = '(a,'//trim(dindexformat(timestep))//')'
  write(zonename,fmt=formatline) 'timestep = ',timestep
else
  formatline = '(a,'//trim(dindexformat(newtstep))//')'
  write(zonename,fmt=formatline) 'newtstep = ',newtstep
end if

! find zone type based on maximum dimension in simulation
! for tecplot we have to use the same element type for each zone, so repeats are used to construct a triangle from a quadrilateral (for example)
if (data_dimension == 1) then
  zonetype = 'FELINESEG'
  element_vertices = 2
else if (data_dimension == 2) then
  zonetype = 'FEQUADRILATERAL'
  element_vertices = 4
else
  zonetype = 'FEBRICK'
  element_vertices = 8
end if

! assemble list of component variables to be output, based on compound output options
! also assemble a statement of the following form for the zone header: VARLOCATION=([3,4]=CELLCENTERED)
allocate(var_output_list(0)) ! allocate this with no length so it can still be referenced as an array
var_location_string = ''
if (trim(check_option(gmesh(gmesh_number)%options,datoutput_gmesh_options)) == "centringdatoutput".or. &
  trim(check_option(gmesh(gmesh_number)%options,datoutput_gmesh_options)) == "datoutput") then
  do mc = 1, ubound(compound,1) ! loop through all compound variables
    if (.not.(compound(mc)%centring == centring.or.centring == 'all')) cycle ! cycle if not the correct centring
    if (trim(check_option(compound(mc)%options,output_options)) == "nooutput") cycle ! cycle if we are not outputting this variable
    if (trim(compound(mc)%centring) == 'none') cycle ! cycle if not cell or face centred
    do mm = 1, ubound(compound(mc)%component,1)
      m = compound(mc)%component(mm)
      if (m > 0) then
        call push_integer_array(array=var_output_list,new_element=m)
        if (ubound(var_output_list,1) == 1) then
          var_location_string = ', VARLOCATION=([4'
        else
          formatline = '(a,'//trim(dindexformat(ubound(var_output_list,1)+3))//')'
          write(var_location_string,fmt=formatline) trim(var_location_string)//',',ubound(var_output_list,1)+3
        end if
      end if
    end do
  end do
  if (ubound(var_output_list,1) > 0) var_location_string = trim(var_location_string)//']=CELLCENTERED)'
end if

formatline = '(a'//repeat(',a',ubound(var_output_list,1))//')'
write(foutput,fmt=formatline) 'VARIABLES = "<cellx[l=1]>", "<cellx[l=2]>", "<cellx[l=3]>"', &
  (', "'//trim(var(var_output_list(mm))%name)//'"',mm=1,ubound(var_output_list,1))
formatline = '(a,'//trim(dindexformat(gmesh(gmesh_number)%ngnodes))//',a,'//trim(dindexformat(ngelements))//',a)'
write(foutput,fmt=formatline) 'ZONE T = "'//trim(zonename)//'", DATAPACKING = BLOCK, NODES = ',gmesh(gmesh_number)%ngnodes, &
                              ', ELEMENTS = ',ngelements,', ZONETYPE = '//trim(zonetype)//trim(var_location_string)

! NB: regions are not used in dat format

! list nodes (irrespective of centring)
formatline = '('//trim(realformat)//')'
do l = 1, totaldimensions
  do gnode = 1, ubound(gmesh(gmesh_number)%knode_from_gnode,1)
    k = gmesh(gmesh_number)%knode_from_gnode(gnode)
    if (k /= 0) write(foutput,fmt=formatline) trunk_dble(node(k)%x(l)*gmesh(gmesh_number)%output_scale)
  end do
end do

! now list data on a component basis
do mm = 1, ubound(var_output_list,1)
  m = var_output_list(mm)
  do gelement = 1, ubound(gmesh(gmesh_number)%gelement,1)
    if (var(m)%centring == 'cell') then
      ijk = gmesh(gmesh_number)%gelement(gelement)%icell
    else if (var(m)%centring == 'face') then
      ijk = gmesh(gmesh_number)%gelement(gelement)%jface
    else
      ijk = gmesh(gmesh_number)%gelement(gelement)%knode
    end if
    cellvaluel = 0.d0
    if (ijk == 0) then
! if this gelement doesn't have an ijk index (which will correspond to the centring of the variable and the gmesh output), then it won't have been included in the list of cells if we have some specific centring
! if we are doing all centring, then need to output a value regardless
      if (centring /= 'all') cycle
    else
      ns = region(var(m)%region_number)%ns(ijk)
! even if this variable doesn't have a value in this face/cell element, dat still requires a value output
      if (ns /= 0) cellvaluel = var_value(m,ns)
    end if
    formatline = '('//trim(realformat)//')'
    write(foutput,fmt=formatline) trunk_dble(cellvaluel)
  end do
end do

! list nodes contained within domain cells and boundary faces as elements, without repetition
formatline = '('//trim(indexformat)//repeat(',1x,'//trim(indexformat),element_vertices-1)//')'
do gelement = 1, ubound(gmesh(gmesh_number)%gelement,1)
  i = gmesh(gmesh_number)%gelement(gelement)%icell 
  j = gmesh(gmesh_number)%gelement(gelement)%jface
  k = gmesh(gmesh_number)%gelement(gelement)%knode
  if (i /= 0.and.(centring == 'cell'.or.centring == 'all')) then
    write(foutput,fmt=formatline) (gmesh(gmesh_number)%gnode_from_knode(cell(i)%knode(gtype_list(cell(i)%gtype)% &
      dat_nodes(data_dimension,l))),l=1,element_vertices)
  else if (j /= 0.and.(centring == 'face'.or.centring == 'all')) then
    write(foutput,fmt=formatline) (gmesh(gmesh_number)%gnode_from_knode(face(j)%knode(gtype_list(face(j)%gtype)% &
      dat_nodes(data_dimension,l))),l=1,element_vertices)
  else if (k /= 0.and.(centring == 'node'.or.centring == 'all')) then
    write(foutput,fmt=formatline) (gmesh(gmesh_number)%gnode_from_knode(k),l=1,element_vertices)
  end if
end do

if (allocated(var_output_list)) deallocate(var_output_list)

if (debug) write(*,'(a/80(1h-))') 'subroutine write_dat'

end subroutine write_dat

!-----------------------------------------------------------------

end module output_module

!-----------------------------------------------------------------
