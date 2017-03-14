! file src/contributed/numerical_recipes/numerical_recipes_dummy_module.f90
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
module numerical_recipes_module
! provides a dummy routine for missing numerical_recipes calls

implicit none

!-----------------------------------------------------------------
contains

!-----------------------------------------------------------------

subroutine numerical_recipes_linear_solver(a,b,error,overwritea)

! this is a dummy routine routine

double precision, dimension(:,:) :: a
double precision, dimension(:,:) :: b
logical, optional :: overwritea
logical :: error

call numerical_recipes_problem

end subroutine numerical_recipes_linear_solver

!-----------------------------------------------------------------

subroutine numerical_recipes_singular_value_decomposition(a,u,w,v,error)

! this is a dummy routine routine

double precision, dimension(:,:) :: a, u, v
double precision, dimension(:) :: w
logical :: error

call numerical_recipes_problem

end subroutine numerical_recipes_singular_value_decomposition

!-----------------------------------------------------------------

subroutine numerical_recipes_problem

write(*,*)
write(*,*) 'ERROR: the numerical_recipes routines have not been compiled into or alongside the arb executable.'
write(*,*) 'For these routines to be active the following files must be present in the src/contributed/numerical_recipes folder:'
write(*,'(a,3(/a))') 'dludcmp.f','dlubksb.f','dsvdcmp.f','dpythag.f'
write(*,*) 'Note that these routines are subject to a (not free) commercial license.'
write(*,*) 'If these files are present then try doing `make clean'' from within the obj directory, and then remaking arb again.'
write(*,*) 'Look to see whether during the make process the numerical_recipes routines are marked as present.'
write(*,*) 'Alternatively, you can try using the lapack routines as an alternative, '// &
  'as they are freely available using both the intel and gfortran compilers'
write(*,*) 'Manuals and license information available at http://center.shao.ac.cn/mppg/zu/mirror/nr/bookf.html'
stop

end subroutine numerical_recipes_problem

!-----------------------------------------------------------------

end module numerical_recipes_module

!-----------------------------------------------------------------
