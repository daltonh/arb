! file src/solver_module.f90
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
module solver_module

implicit none
private
public newtsolver, residual, update_magnitudes, check_variable_validity, update_and_check_derived_and_equations, &
  update_and_check_unknowns, update_and_check_constants, update_and_check_transients, update_and_check_newtients, &
  update_and_check_initial_transients, update_and_check_initial_newtients, update_and_check_outputs, setup_solver

! type of linear solver
character(len=100) :: linear_solver = "default" ! (default, userable) type of linear solver used: default will choose optimal solver available.  Specific options are: none, intelpardiso, intelpardisoooc, suitesparse, hslma28, pardiso, iterative

! backstepping parameters for the newton-raphson method
! recommended defaults for each parameter are in braces
logical :: backstepping = .true. ! (.true., userable) whether to use backstepping or not - no reason not to
double precision, parameter :: alf = 1.d-5 ! (1.d-5) small factor that ensures that newtres is decreasing by a multiple of the initial rate of decrease - everyone suggests 1.d-4, but a bit smaller seems to work better for some problems
double precision :: lambdamin = 1.d-10 ! (1.d-10, userable) minimum absolute backstepping lambda allowed - this can be set very small if lambda_limit_false_root is on
logical, parameter :: lambda_limit_cautiously = .false. ! (.false.) limit lambda so that solution is approached slowly (cautiously), hopefully avoiding spurious steps into unstable regions
double precision, parameter :: lambda_limit_cautiously_factor = 1.d-2 ! (1.d-2) product of average unknown change and newtres which must be satisfied for step to be accepted - the lower the value the more cautious the approach to the solution is - 1.d1 is reckless but possibly fast, 1.d-2 is a good middle-of-the-road value for reasonably stable systems, 1.d-4 is very cautious (slow but safe).  These factors are critically dependent on the unknown magnitudes being correct!
logical :: lambda_limit_false_root = .true. ! (.true., userable) attempt to identify and move past false solution roots by imposing a lower limit on lambda that is a function of the current newtres
double precision :: lambda_limit_false_root_factor = 1.d-4 ! (1.d-4, userable) aggression used in limiting lambda - a higher number means a more agressive limiting approach but cause solution to shoot off into unstable regions - 1.d-7 is a middle of the road value
logical :: sticky_lambda = .true. ! (.true., userable) use lambda from previous step as the basis for the current iteration
integer :: sticky_lambda_increase = 1 ! (1, userable) multiply previous lambda by 2^sticky_lambda_increase at each new newton iteration
integer, parameter :: normtype = 2 ! (2 = L2 norm) newton loop residual norm type to use (0=infinity norm, 1=abs average norm, 2=rms norm)
logical :: weight_large_equation_errors = .false. ! (.false., userable) when calculating the residual, increase the weighting of equations that have large normalised magnitudes
double precision :: weight_large_equation_errors_factor = 1.d0 ! (1.d0, userable) approximately the normalised equation size that will increase the residual by 1/2

! debugging options:
logical, parameter :: print_all_equations_on_delphi_error = .true. ! (.true.) if there is a problem with a delphi, print a list of equations that depend or are collocated with that unknown
logical, parameter :: print_dependent_equations_on_delphi_error = .true. ! (.true.) if there is a problem with a delphi, print a list of all equations and their derivatives
logical, parameter :: manage_funk_dv_memory = .true. ! (.true.) whether to deallocate and allocate derived and equation dvs each time to minimise memory requirements
logical, parameter :: check_solution_accuracy = .false. ! (.false.) calculate how well solution satisfies linear equation - requires manage_funk_dv_memory to be false

! debugging array:
integer, dimension(:), allocatable :: debug_list_p ! debug_list_p is a list of all unknown delphis that have a problem, referenced by their p index

!-----------------------------------------------------------------
contains

!-----------------------------------------------------------------

subroutine newtsolver(ierror)

! solves equations using newton's method

use general_module
use equation_module
integer :: m, ierror, p, n, ns, merr, nserr, backstep
double precision :: newtresold, lambda, varmax, vartmp, varave
double precision :: active_lambdamin = 0.d0
double precision, save :: lambda_previous_step = -1.d0 ! lambda saved from previous newton step - negative indicates not set yet
character(len=1000) :: formatline
logical :: first_delphi_error = .true.
logical, parameter :: manual_lambda = .false. ! debugging option to allow entering lambda manually
logical, parameter :: debug = .false.
                  
if (debug) write(*,'(80(1h+)/a)') 'subroutine newtsolver'

ierror = 0
lambda = 1.d0
!lambda = 1.d-9
newtresold = newtres ! save this for convergence comparison
if (lambda_previous_step > 0.d0.and.sticky_lambda) lambda = min(dfloat(2**sticky_lambda_increase)*lambda_previous_step,1.d0) ! try twice the lambda used previously

! call a debugging routine to find which equation is most dependent upon it
if (.false.) call find_sensitive_equation(varname='<n+>',icell=6447)

! save old unknown values as phiold
do n = 1, allocatable_size(var_list(var_list_number_unknown)%list)
  m = var_list(var_list_number_unknown)%list(n)
  do ns = 1, ubound(var(m)%funk,1)
    phiold(var(m)%funk(ns)%pp(1)) = var(m)%funk(ns)%v ! as pp(1) is the sequential index of the unknown
  end do
end do

! call the main linear solver routines if atleast one equation is being solved

if (manage_funk_dv_memory) then
  call time_process
  if (debug) write(*,*) 'deallocating derived funk dvs'
  call memory_manage_dvs(type="derived",action="deallocate")
  call time_process(description='deallocating derived funk memory')
end if

if (trim(linear_solver) == "iterative") then
  if (debug) write(*,*) 'calling iterative_mainsolver'
  call time_process
  call iterative_mainsolver(ierror)
  call time_process(description='iterative mainsolver')
  ! if there is a problem with the linear matrix solver then return
  if (debug) write(*,*) 'in newtsolver after iterative_mainsolver, ierror = ',ierror
else
  if (debug) write(*,*) 'calling mainsolver'
  !call time_process
  call mainsolver(ierror)
  !call time_process(description='mainsolver')
  ! if there is a problem with the linear matrix solver then return
  if (debug) write(*,*) 'in newtsolver after mainsolver, ierror = ',ierror
end if

if (manage_funk_dv_memory) then
  if (debug) write(*,*) 'reallocating derived funk dvs'
  call time_process
  call memory_manage_dvs(type="derived",action="reallocate")
  call time_process(description='reallocating derived funk memory')
end if

if (ierror /= 0) return

! temp &&&&
!if (newtstep == 1) then
!  delphi(15) = 1.d+14
!  delphi(5) = 1.d+16
!end if

! check on validity of delphi and for convergence file find largest normalised magnitude delphi
p = 0
merr = 0
nserr = 0
varmax = 0.d0
varave = 0.d0
do n = 1, allocatable_size(var_list(var_list_number_unknown)%list)
  m = var_list(var_list_number_unknown)%list(n)
  do ns = 1, ubound(var(m)%funk,1)
    p = p + 1
    if (.not.number_is_valid(delphi(p)).or.delphi(p)*lambdamin > var(m)%magnitude*normalised_variable_limit) then
      if (first_delphi_error) then
        write(*,'(a/2x,a/2x,a/2x,a)') 'ERROR: the update for unknown variable '//trim(var(m)%name)//' is not valid (NaN, infty, '// &
          'or very large) after the Newton-Raphson solver.','Something is amiss with the equations when evaluated using the '// &
          'current unknown variable estimates as','the Newton-Raphson method is trying to change '//trim(var(m)%name)// &
          ' by an unreasonable amount to satisfy the system.','It is likely that a dependency between this variable '// &
          'and one of the equations has been lost or is very weak.'
        write(*,'(a,g11.4,a)') '  Wayward value is ',delphi(p),' at ',trim(variable_location_string(m,ns))
        write(*,'(a)') '  There may be multiple delphi problems (only one error message is output to stdout).  To debug set '// &
          'convergence_details_file = .true. in general_module.f90 as this file will then contain the locations of all the '// &
          'delphi errors.  Also set the delphi debugging logicals at the top of solver_module.f90 to be true for more '// &
          'debugging output.'
        first_delphi_error = .false. ! only write long error to screen for first problem
      end if
      if (convergence_details_file) then
        write(fconverge,'(a)') 'ERROR: the update for unknown variable '//trim(var(m)%name)//' is not valid (NaN, infty or '// &
          'very large) after the Newton-Raphson solver.'
        write(fconverge,'(a,g11.4,a)') '  Wayward value is ',delphi(p),' at ',trim(variable_location_string(m,ns))
      end if
      if (.false.) then
        write(*,*) 'WARNING: continuing anyway with this update zeroed'
        delphi(p) = 0.d0
      else if (print_all_equations_on_delphi_error.or.print_dependent_equations_on_delphi_error) then
! this error is terminal, so diagnose by constructing a list of the problem equations
        ierror = 1
        call push_integer_array(array=debug_list_p,new_element=p)
      else ! return immediately
        ierror = 1
        return
      end if
    end if

! find variable that has the largest normalised change
    vartmp = delphi(p)/var(m)%magnitude
    varave = varave + vartmp**2
    if (abs(vartmp) > abs(varmax)) then
      merr = m
      nserr = ns
      varmax = vartmp
    end if
  end do
end do
varave = sqrt(varave/dble(max(ptotal,1)))

if (debug) write(*,*) 'in newtsolver after checking validity'

if (convergence_details_file) then
  if (merr /= 0.and.nserr /= 0) then
    write(fconverge,'(a,g11.4)') 'INFO: maximum normalised change in any unknown variable corresponds to unknown '// &
      trim(var(merr)%name)//' at '//trim(variable_location_string(merr,nserr))//': normalised change is ',varmax
  else
    write(fconverge,'(a)') 'INFO: no unknown variables changes were found'
  end if
  call flush(fconverge)
end if

! check on linear solution accuracy if an error in one of the delphi's is detected
if ((check_solution_accuracy.or.ierror > 0).and..not.manage_funk_dv_memory) call check_linear_solution_accuracy

! now print equations if there is an error and this is required
if (ierror > 0.and.(print_all_equations_on_delphi_error.or.print_dependent_equations_on_delphi_error)) then
! this will list each problem delphi and the equations that it depends on and is colocated with
  if (print_dependent_equations_on_delphi_error) call find_debug_list_p_dependent_equations
  if (allocated(debug_list_p)) deallocate(debug_list_p)
! this will now list all equations
  if (print_all_equations_on_delphi_error) call list_all_equations
! finally return
  return
end if

if (debug) write(*,*) 'in newtsolver before backstepping'

backstep = 0

back_loop: do ! entrance point for repeat steps

  backstep = backstep + 1 ! only used as an indicator of progress
  ierror = 0

  if (backstepping.and.debug) then
    formatline = "(a,"//trim(dindexformat(backstep))//",a)"
    write(*,fmt=formatline) repeat('+',backline)//' backstep ',backstep,' starting '//repeat('+',backline)
    if (convergence_details_file) then
      write(fconverge,fmt=formatline) repeat('+',backline)//' backstep ',backstep,' starting '//repeat('+',backline)
      call flush(fconverge)
    end if
  end if

  if (manual_lambda) then
    write(*,*) 'MANUAL_LAMBDA: current newtres = ',newtres,': previous lambda = ',lambda
    write(*,*) 'Enter new lambda:'
    read(*,*) lambda
  end if

! update all vars/derivatives and calculate new newtres value

  if (debug) write(*,*) 'in newtsolver before update with ierror = ',ierror

  call time_process
  call update_and_check_unknowns(initial=.false.,lambda=lambda,ierror=ierror)
  call time_process(description='within newtsolver update and check unknowns')
  if (debug) write(*,*) 'in newtsolver after update and check unknowns: ierror = ',ierror

  call time_process
  if (ierror == 0) call update_and_check_derived_and_equations(ierror=ierror)
  call time_process(description='within newtsolver update and check derived and equations')
  if (debug) write(*,*) 'in newtsolver after update and check derived and equations: ierror = ',ierror

  if (ierror == 0) then
    call residual(ierror=ierror)
    if (debug) write(*,*) 'in newtsolver after residual: ierror = ',ierror
  else
    if (debug) write(*,*) 'residual not called as ierror not zero'
  end if

  if (manual_lambda) then
    if (lambda >= 0.d0) then
      write(*,*) 'newtres = ',newtres,': newtresold = ',newtresold
      cycle back_loop
    else
      call error_stop('stopped simulation on negative lambda') ! if a negative lambda is entered stop simulation
    end if
  end if
  
  if (check_stopfile("stopback")) then

    write(*,'(a)') 'INFO: user requested simulation stop via "kill" file'
    exit back_loop ! if not backstepping then exit backstepping loop

  else if (backstepping) then

! save lambda that was used for next steps
    lambda_previous_step = lambda

! if backstepping is on check that
!   1) newtres has decreased since newtresold, or it is already less than newtrestol
!   2) there are no errors
!   3) if limit_cautiously is on, check that the product of the average change in unknowns times the newtres is less than some factor

    if ( ierror == 0 .and. & ! if update_checker detected an error
      (newtres < (1.d0-2.d0*alf*lambda)*newtresold .or. newtres < newtrestol) &
      .and. (.not.lambda_limit_cautiously.or.abs(lambda*varave*newtres) < lambda_limit_cautiously_factor) ) then

      write(*,'(3(a,g10.3))') "BACKSTEPPING: converged newtres = ",newtres, &
        ": ave unknown change = ",lambda*varave,": lambda = ",lambda
      if (convergence_details_file) &
        write(fconverge,'(4(a,g16.9))') "BACKSTEPPING: converged newtres = ",newtres, &
        ": max/ave unknown change = ",lambda*varmax,",",lambda*varave,": lambda = ",lambda
      exit back_loop ! residual has decreased from previous iteration (= newtres when lambda=0)

    else 

      if (debug.and.ierror == 3) write(*,'(a)') "BACKSTEPPING: newtres was not valid (NaN or infty)"
      if (debug.and.ierror == 2) write(*,'(a)') "BACKSTEPPING: a variable constraint was violated (ie positive/negative option)"
      if (debug.and.ierror == 1) write(*,'(a)') "BACKSTEPPING: an invalid variable was detected (NaN, infty or very large)"
      if (ierror == 0) then
        write(*,'(4(a,g10.3))') "BACKSTEPPING: intermediate newtres = ",newtres, &
          ": ave unknown change = ",lambda*varave,": lambda = ",lambda
      else
        write(*,'(a,i1,a,g10.3)') "BACKSTEPPING: error ",ierror," was detected: lambda = ",lambda
      end if
      if (convergence_details_file) then
        if (debug.and.ierror == 3) write(fconverge,'(a)') "BACKSTEPPING: newtres was not valid (NaN or infty)"
        if (debug.and.ierror == 2) write(fconverge,'(a)') &
          "BACKSTEPPING: a variable constraint was violated (ie positive/negative option)"
        if (debug.and.ierror == 1) write(fconverge,'(a)') &
          "BACKSTEPPING: an invalid variable was detected (NaN, infty or very large)"
        if (ierror == 0) then
          write(fconverge,'(4(a,g16.9))') "BACKSTEPPING: intermediate newtres = ",newtres, &
            ": max/ave unknown change = ",lambda*varmax,",",lambda*varave,": lambda = ",lambda
        else
          write(fconverge,'(a,i1,a,g10.3)') "BACKSTEPPING: error ",ierror," was detected: lambda = ",lambda
        end if
        call flush(fconverge)
      end if

! decrease lambda to use next
      lambda = lambda/2.d0

      if (lambda < lambdamin) then ! unreasonably small lambda
        write(*,'(a,g10.3)') "BACKSTEPPING: UNRESOLVABLE ERROR: lambda cannot be reduced below lambdamin = ",lambdamin
        if (convergence_details_file) write(fconverge,'(a,g10.3)') &
          "BACKSTEPPING: UNRESOLVABLE ERROR: lambda cannot be reduced below lambdamin = ",lambdamin
        ierror = 1
        return
      end if

! use an empiricism to limit the smallest lambda to avoid honing in on false roots
      if (lambda_limit_false_root.and.ierror == 0) then
!       active_lambdamin = lambda_limit_false_root_factor/newtres
!       active_lambdamin = lambda_limit_false_root_factor/(newtres*varave)
        active_lambdamin = lambda_limit_false_root_factor/sqrt(newtres*varave)
        if (lambda < active_lambdamin) then ! active lambda limiting
          write(*,'(a,g10.3)') "BACKSTEPPING: a possible false solution root has been found "// &
            "(lambda < active_lambdamin): exiting backstepping loop: active_lambdamin = ",active_lambdamin
          if (convergence_details_file) write(fconverge,'(a,g16.9)') &
            "BACKSTEPPING: a possible false solution root has been found "// &
            "(lambda < active_lambdamin): exiting backstepping loop: active_lambdamin = ",active_lambdamin
          exit back_loop ! residual has decreased from previous iteration (= newtres when lambda=0)
        end if
      end if

    end if

  else if ( ierror /= 0 ) then
    write(*,'(a/a)') "ERROR: update_checker has detected a problem with the solution", &
      "  You could try turning on backstepping in subroutine newtsolver"
    ierror = 1
    return
  else 
    exit back_loop ! if not backstepping then exit backstepping loop
  end if

  if (backstepping.and.debug) then
    formatline = "(a,"//trim(dindexformat(backstep))//",a)"
    write(*,fmt=formatline) repeat('-',backline)//' backstep ',backstep,' ending '//repeat('-',backline+2)
    if (convergence_details_file) then
      write(fconverge,fmt=formatline) repeat('-',backline)//' backstep ',backstep,' ending '//repeat('-',backline+2)
      call flush(fconverge)
    end if
  end if

end do back_loop

if (debug) write(*,'(a/80(1h-))') 'subroutine newtsolver'

end subroutine newtsolver

!-----------------------------------------------------------------

subroutine mainsolver(ierror)

! here we setup sparse matrices to be used to produce one improvement in
!  the fields (delphi) using newton's method

use general_module
use equation_module
use intel_pardiso_module
use pardiso_module
use hsl_ma28d_module
use suitesparse_module

double precision, dimension(:), allocatable :: aa
integer, dimension(:), allocatable :: iaa, jaa
integer :: ierror, p, nz, nz_last_row, nz_col, nn, nz_full_row, nz_full, n, m, ns, pp, npp, nsm, mm
double precision :: aa_max, varmag, normalisation_multiplier
character(len=1000) :: formatline
logical :: singular
logical, parameter :: normalise_matrix = .false. ! use the variable magnitudes (unknowns/equations) to normalise the matrix coefficients and rhs
logical, parameter :: perturb_singular = .true. ! if the linear solver carks it complaining about a singularity, try perturbing the solution instead
logical, parameter :: reduce = .false. ! true to reduce the matrix according to order of magnitude of elements*varmag
double precision :: aa_reduce = 1.d-1 ! if reducing matrix, relative level at which components*varmag are zeroed
logical, parameter :: condition_number = .false. ! calculate the condition number of the matrix rather than solving the system - requires lots of memory as matrix is reconstructed in full
logical, parameter :: condition_number_estimate = .false. ! calculate an estimate of the condition number of the matrix - requires the suitesparse solvers to be compiled in
logical, parameter :: diagonal_dominance = .false. ! calculate some statistics about the diagonal dominance of the equations
logical, parameter :: matrix_test = .false. ! do a test matrix inversion using a made-up test matrix instead of solving PDE system
logical, parameter :: dump_matrix = .false. ! dump contents and solution to matrix in fort.91
integer, parameter :: dump_matrix_max = 1000 ! maximum number of elements to include when dumping matrix
logical, parameter :: debug = .false.
logical :: debug_sparse = .false.

if (debug) debug_sparse = .true.
if (debug_sparse) write(*,'(80(1h+)/a)') 'subroutine mainsolver'

ierror = 1

! sparse matrix produced is stored in compressed sparse row format (3 array variation), with 1 indexing (csr1)
! aa is value (length nz)
! jaa is column index (length nz)
! iaa is array of where new rows start (length n+1)

call time_process
! calculate exact size that sparse matrix will be and size it
!if (debug_sparse) write(*,'(a)') 'sizing sparse matrix'
n = ptotal
nz = 0
do nn = 1, allocatable_size(var_list(var_list_number_equation)%list)
  m = var_list(var_list_number_equation)%list(nn)
  do ns = 1, ubound(var(m)%funk,1)
    nz = nz + var(m)%funk(ns)%ndv
  end do
end do
allocate(aa(nz),jaa(nz),iaa(n+1))
if (convergence_details_file) then
  formatline = '(a,'//trim(dindexformat(n))//',a,'//trim(dindexformat(nz))//',a)'
  write(fconverge,fmt=formatline) 'INFO: the sparse matrix to invert (the jacobian) currently has ',n,' rows and ',nz,' elements'
  call flush(fconverge)
end if
nz_full = nz

! calculate new aa and rhs (stored as delphi) matrix
! under newton's method, \tens{J} \dot \vect{\delta \phi} = - \vect{F}
! ie, aa = \tens{J}, rhs = -\vect{F}, rhs_returned = \vect{\delta \Phi}
! nz is the number of nonzero elements in aa
! n is the total number of equations (rank of matrix)

if (debug) write(80,*) 'looping through now: n = ',n

normalisation_multiplier = 1.d0
nz = 0
! loop through all equation lines
p = 0
do nn = 1, allocatable_size(var_list(var_list_number_equation)%list)
  m = var_list(var_list_number_equation)%list(nn)
  if (normalise_matrix) normalisation_multiplier = 1.d0/var(m)%magnitude ! reciprocal of the equation magnitude, applied to all lhs and rhs elements
  if (debug) write(80,'(a,i8,a)') 'nn = ',nn,': equation = '//trim(var(m)%name)
  
  do ns = 1, ubound(var(m)%funk,1)
    p = p + 1

    delphi(p) = -var(m)%funk(ns)%v*normalisation_multiplier
    iaa(p) = nz + 1  ! row index
    if (debug) write(80,'(a,i8,a,g11.4)') 'new row: p = ',p,': delphi(p) = ',delphi(p)
!   if (debug) write(80,*) 'var(m)%funk(ns)%ndv = ',var(m)%funk(ns)%ndv,': var(m)%funk(ns)%pp = ',var(m)%funk(ns)%pp, &
!     ': var(m)%funk(ns)%dv = ',var(m)%funk(ns)%dv

    nz_last_row = nz ! last entry in matrix of previous row is saved
    nz = nz + var(m)%funk(ns)%ndv
    if (nz>ubound(aa,1)) then
      write(*,*) 'ERROR: bounds of aa exceded in mainsolver: nz = ',nz
      stop
    end if
    jaa(nz_last_row+1:nz) = var(m)%funk(ns)%pp(1:var(m)%funk(ns)%ndv) ! column index (icn for hsl/coordinate, ja for intel storage)
    aa(nz_last_row+1:nz) = var(m)%funk(ns)%dv(1:var(m)%funk(ns)%ndv) ! matrix value
    if (debug) then
      do npp = 1, var(m)%funk(ns)%ndv
        pp = var(m)%funk(ns)%pp(npp)
        mm = unknown_var_from_pp(pp)
        if (mm == 0) call error_stop("problem in mainsolver with debugging 1")
        nsm = pp - var(mm)%funk(1)%pp(1) + 1 ! the ns for the unknown can be found from the unknown derivatives
        write(80,'(2(a,i8),a,g11.4)') '  adding entry: unknown = '//trim(var(mm)%name)//': pp = ',pp, &
          ': ijk = ',ijkvar(mm,nsm),': dv = ',var(m)%funk(ns)%dv(npp)
      end do
    end if
    if (normalise_matrix) then
      do nz_col = nz_last_row + 1, nz ! loop through aa entries on the current row (equation)
        aa(nz_col) = aa(nz_col)*normalisation_multiplier*var(unknown_var_from_pp(jaa(nz_col)))%magnitude ! multiply each element by the unknown magnitude and divide by the equation magnitude
      end do
    end if

    if (reduce) then
      aa_max = 0.d0
      do nz_col = nz_last_row + 1, nz ! loop through aa entries on the current row (equation)
        varmag = var(unknown_var_from_pp(jaa(nz_col)))%magnitude
        if (abs(aa(nz_col)*varmag) > abs(aa_max)) aa_max = aa(nz_col)*varmag ! find one which has the largest magnitude
      end do
      if (abs(aa_max) < 1.d-20) then
        if (debug) write(80,*) 'aa_max was too small for reducing row entries: aa_max = ',aa_max
      else
        if (debug) write(80,*) 'reducing row entries to aa_max = ',aa_max
        if (debug) write(80,*) 'final matrix entries:'
        nz_full_row = nz
        nz = nz_last_row
        do nz_col = nz_last_row + 1, nz_full_row
          varmag = var(unknown_var_from_pp(jaa(nz_col)))%magnitude
          if (abs(aa(nz_col)*varmag/aa_max) > aa_reduce) then
            nz = nz + 1
            aa(nz) = aa(nz_col)
            jaa(nz) = jaa(nz_col)
          end if
          if (debug) write(80,*) 'nz_col = ',nz_col,': aa(nz_col) = ',aa(nz_col),': varmag = ',varmag
        end do
        if (debug) write(80,*) 'reducing current nz from nz_full_row = ',nz_full_row,' to nz = ',nz
      end if
    end if

  end do
end do

iaa(n+1) = nz + 1

if (reduce) then
  formatline = '(a,g11.4,a,i15)'
  write(*,fmt=formatline) 'INFO: ', &
    dble(nz_full-nz)/dble(nz_full)*100.d0,'% of the matrix was zeroed during normalisation: new nz = ',nz
  if (nz < nz_full) then
    call resize_array(array=aa,new_size=nz,keep_data=.true.)
    call resize_array(array=jaa,new_size=nz,keep_data=.true.)
  end if
end if
call time_process(description='assemble jacobian')

! try test matrix as per intel manual rather than solving the real system
if (matrix_test) then
  nz = 13
  n = 5
  call resize_array(array=aa,new_size=13,keep_data=.false.)
  aa = [1, -1, -3, -2, 5, 4, 6, 4, -4, 2, 7, 8, -5] 
  call resize_array(array=jaa,new_size=13,keep_data=.false.)
  jaa = [1, 2, 4, 1, 2, 3, 4, 5, 1, 3, 4, 2, 5]
  call resize_array(array=iaa,new_size=6,keep_data=.false.)
  iaa = [1, 4, 6, 9, 12, 14]
  call resize_array(array=delphi,new_size=5,keep_data=.false.)
  delphi = 1.d0
  write(91,*) 'csr 1 basis matrix formed in mainsolver:'
  formatline = '(a,i3,a,g11.4,a,i3,a,i3)'
  do nn = 1, nz
    write(91,fmt=formatline) 'aa(',nn,') = ',aa(nn),': jaa(',nn,') = ',jaa(nn)
  end do
  formatline = '(a,i3,a,i3)'
  do nn = 1, n+1
    write(91,fmt=formatline) 'iaa(',nn,') = ',iaa(nn)
  end do
end if

if (dump_matrix) then
! formatline = '(a,i8,a,g11.4,a,i8,a,i8)'
  formatline = '(a,i8,a,g15.8,a,i8,a,i8)'
  do nn = 1, min(ubound(aa,1),dump_matrix_max)
    write(91,fmt=formatline) 'aa(',nn,') = ',aa(nn),': jaa(',nn,') = ',jaa(nn)
  end do
  formatline = '(a,i8,a,i8)'
  do nn = 1, min(ubound(iaa,1),dump_matrix_max)
    write(91,fmt=formatline) 'iaa(',nn,') = ',iaa(nn)
  end do
end if

if (diagonal_dominance) then
  write(*,'(a)') 'SPECIAL: calling calc_diagonal_dominance subroutine'
  call calc_diagonal_dominance
end if

if (manage_funk_dv_memory) then
  call time_process
  if (debug_sparse) write(*,*) 'deallocating equation funk dvs'
  call memory_manage_dvs(type="equation",action="deallocate")
  call time_process(description='deallocating equation funk memory')
end if

!if (condition_number_estimate) then
if (condition_number_estimate.and.newtstep == 1) then
  write(*,'(a)') 'SPECIAL: calling condition_number_estimate subroutine'
  call calc_condition_number_estimate(aa,iaa,jaa)
end if

if (condition_number) then
  write(*,'(a)') 'SPECIAL: calling condition_number subroutine'
  call calc_condition_number(aa,iaa,jaa)
end if

if (debug_sparse) write(*,*) 'jacobian matrix has been formed, now calling linear solver'

! call appropriate third-party linear solver

call time_process
singular = .false.
if (trim(linear_solver) == "pardiso") then
! pardiso (native) solver

  call pardiso_linear_solver(aa,iaa,jaa,delphi,ierror,nthreads)
  if (ierror == -4) singular = .true.

elseif (trim(linear_solver) == "pardisoiterative") then
! pardiso (native) solver, but using iterative solver

  call pardiso_linear_solver(aa,iaa,jaa,delphi,ierror,nthreads,iterative=.true.)
  if (ierror == -4) singular = .true.

elseif (trim(linear_solver) == "intelpardiso") then
! intel pardiso solver

  call intel_pardiso_linear_solver(aa,iaa,jaa,delphi,ierror,nthreads)
  if (ierror == -4) singular = .true.

else if (trim(linear_solver) == "intelpardisoooc") then
! intel pardiso solver out of core

  call intel_pardiso_linear_solver(aa,iaa,jaa,delphi,ierror,nthreads,ooc=.true.)
  if (ierror == -4) singular = .true.

else if (trim(linear_solver) == "suitesparse") then
! suitesparse umf solver

  call suitesparse_linear_solver(aa,iaa,jaa,delphi,ierror)
  if (ierror == 1) singular = .true.

else if (trim(linear_solver) == "hslma28") then
! hsl_ma28 solver

  call hsl_ma28_linear_solver(aa,iaa,jaa,delphi,ierror)

else if (trim(linear_solver) == "none") then

  call error_stop('no linear solver specifically chosen but here we are trying to solve some equations')

else
  call error_stop('linear solver type '//trim(linear_solver)//' not known')
endif
call time_process(description='linear solver')

if (ierror /= 0) then
  write(*,*) 'WARNING: error from '//trim(linear_solver)//' linear solver: ierror = ',ierror
  if (perturb_singular.and.singular) then
    write(*,*) 'WARNING: error implies some type of singularity: there is probably something wrong with the equations, '// &
      'but perturbing solution and continuing regardless'
    pp = 0
    do nn = 1, allocatable_size(var_list(var_list_number_unknown)%list)
      m = var_list(var_list_number_unknown)%list(nn)
      do ns = 1, ubound(var(m)%funk,1)
        pp = pp + 1 ! should be faster than looking up pp from unknown
        delphi(pp) = var(m)%magnitude*min(newtres,1.d-1) ! perturb solution by a small proportion of each variable's magnitude
      end do
    end do
    ierror = 0
  end if
else if (normalise_matrix) then
! if matrix was normalised then need to recover unnormalised delphis
  pp = 0
  do nn = 1, allocatable_size(var_list(var_list_number_unknown)%list)
    m = var_list(var_list_number_unknown)%list(nn)
    do ns = 1, ubound(var(m)%funk,1)
      pp = pp + 1 ! should be faster than looking up pp from unknown
      delphi(pp) = delphi(pp)*var(m)%magnitude
    end do
  end do
end if

if (matrix_test.or.dump_matrix) then
  write(91,*) 'solution returned in delphi'
! formatline = '(a,i8,a,g11.4)'
  formatline = '(a,i8,a,g15.8)'
  do nn = 1, min(n,dump_matrix_max)
    write(91,fmt=formatline) 'delphi(',nn,') = ',delphi(nn)
  end do
  if (matrix_test) stop
end if

if (allocated(aa)) deallocate(aa)
if (allocated(iaa)) deallocate(iaa)
if (allocated(jaa)) deallocate(jaa)
  
if (manage_funk_dv_memory) then
  if (debug_sparse) write(*,*) 'reallocating equation funk dvs'
  call time_process
  call memory_manage_dvs(type="equation",action="reallocate")
  call time_process(description='reallocating equation funk memory')
end if

if (debug_sparse) write(*,'(a/80(1h-))') 'subroutine mainsolver'

end subroutine mainsolver

!-----------------------------------------------------------------

subroutine iterative_mainsolver(ierror)

! here we use a homegrown iterative technique to solve the linear system, using equation funk data directly

use general_module
use equation_module

integer :: nn, m, ns, i, j, iterstep, ierror, ii
double precision :: alpha_ff, beta_ff, f, f_old, lambda_ff
character(len=1000) :: formatline
double precision, allocatable, dimension(:) :: deldelphi, ff, alpha_delphi, beta_delphi, ff_m, delff
logical :: singular
logical, parameter :: matrix_test = .false. ! do a test matrix inversion using a made-up test matrix instead of solving PDE system
logical, parameter :: dump_matrix = .false. ! dump contents and solution to matrix in fort.91
integer, parameter :: dump_matrix_max = 1000 ! maximum number of elements to include when dumping matrix
logical, parameter :: debug = .false.
logical :: debug_sparse = .false.

if (debug) debug_sparse = .true.
if (debug_sparse) write(*,'(80(1h+)/a)') 'subroutine iterative_mainsolver'

ierror = 1

! check on allocations
if (.not.allocated(deldelphi)) then
  allocate(deldelphi(ptotal),ff(ptotal),delff(ptotal),alpha_delphi(ptotal),beta_delphi(ptotal),ff_m(ptotal))
end if

! set initial ff array (linear equation error array) from newton's equations
! also set residual multiplier, ff_m
j = 0
do nn = 1, allocatable_size(var_list(var_list_number_equation)%list)
  m = var_list(var_list_number_equation)%list(nn)
  do ns = 1, ubound(var(m)%funk,1)
    j = j + 1
    ff(j) = var(m)%funk(ns)%v
    ff_m(j) = 1.d0/(var(m)%magnitude**2)
  end do
end do

! calculate initial residual
f = ff_residual(ff_m,ff)

! calculate alpha_delphi
alpha_delphi = 0
j = 0
do nn = 1, allocatable_size(var_list(var_list_number_equation)%list)
  m = var_list(var_list_number_equation)%list(nn)
  do ns = 1, ubound(var(m)%funk,1)
    j = j + 1
    do ii = 1, var(m)%funk(ns)%ndv
      i = var(m)%funk(ns)%pp(ii)
      alpha_delphi(i) = alpha_delphi(i) + var(m)%funk(ns)%dv(ii)**2*ff_m(j)
    end do
  end do
end do

write(*,*) 'iterstep,f'
write(*,*) 0,f

! start iteration loop
iteration_loop: do iterstep = 1, 1000

! calculate beta_delphi
  beta_delphi = 0
  j = 0
  do nn = 1, allocatable_size(var_list(var_list_number_equation)%list)
    m = var_list(var_list_number_equation)%list(nn)
    do ns = 1, ubound(var(m)%funk,1)
      j = j + 1
      do ii = 1, var(m)%funk(ns)%ndv
        i = var(m)%funk(ns)%pp(ii)
        beta_delphi(i) = beta_delphi(i) + var(m)%funk(ns)%dv(ii)*ff(j)*ff_m(j)
      end do
    end do
  end do
  
! calculate change to delphi, deldelphi
  do i = 1, ptotal
    if (abs(alpha_delphi(i)) < 1.d-20) call error_stop("problem alpha_delphi(i)")
    deldelphi(i) = -beta_delphi(i)/alpha_delphi(i)
  end do

! calculate corresponding change to ff
! and at the same time, calculate the alpha_ff and beta_ff factors required to calculate lambda_ff
  delff = 0
  alpha_ff = 0
  beta_ff = 0
  j = 0
  do nn = 1, allocatable_size(var_list(var_list_number_equation)%list)
    m = var_list(var_list_number_equation)%list(nn)
    do ns = 1, ubound(var(m)%funk,1)
      j = j + 1
      do ii = 1, var(m)%funk(ns)%ndv
        i = var(m)%funk(ns)%pp(ii)
        delff(j) = delff(j) + var(m)%funk(ns)%dv(ii)*deldelphi(i)
      end do
      alpha_ff = alpha_ff + delff(j)**2*ff_m(j)
      beta_ff = beta_ff + delff(j)*ff(j)*ff_m(j)
    end do
  end do
  if (abs(alpha_ff) < 1.d-20) call error_stop("problem alpha_ff")
  lambda_ff = -beta_ff/alpha_ff

! update ff, delpha and residual f
  do j = 1, ptotal
    ff(j) = ff(j) + lambda_ff*delff(j)
    delphi(j) = delphi(j) + lambda_ff*deldelphi(j)
  end do
  f = ff_residual(ff_m,ff)

  write(*,*) 'iterstep,f,lambda_ff'
  write(*,*) iterstep,f,lambda_ff

end do iteration_loop
  
ierror = 0

stop

if (debug_sparse) write(*,'(a/80(1h-))') 'subroutine iterative_mainsolver'

end subroutine iterative_mainsolver

!-----------------------------------------------------------------

double precision function ff_residual(ff_m,ff)

use general_module
double precision, allocatable, dimension(:) :: ff ! the array of linear equation errors
double precision, allocatable, dimension(:) :: ff_m ! a multiplier for each equation
integer :: j

ff_residual = 0.d0
do j = 1, ubound(ff,1)
  ff_residual = ff_residual + ff(j)**2*ff_m(j)
end do
ff_residual = sqrt(max(ff_residual,0.d0)/dble(max(ptotal,1)))

end function ff_residual

!-----------------------------------------------------------------

subroutine residual(ierror)

! finds residual
! if a problem is found then ierror is set to 3 here

use general_module
use equation_module
double precision :: aatmp, aatmpmax
integer :: ns, m, p, nserr, merr, nvar, ierror
character(len=1000) :: formatline
logical, parameter :: debug = .false.

if (debug) write(*,'(80(1h+)/a)') 'subroutine residual'

newtres = 0.d0
ierror = 0

aatmpmax = 0.d0
nserr = 0
merr = 0

p = 0
do nvar = 1, allocatable_size(var_list(var_list_number_equation)%list)
  m = var_list(var_list_number_equation)%list(nvar)
  do ns = 1, ubound(var(m)%funk,1)
    p = p + 1
    aatmp = abs(var(m)%funk(ns)%v)/var(m)%magnitude
    if (weight_large_equation_errors) &
      aatmp = (float(ptotal)*aatmp/weight_large_equation_errors_factor+1.d0)/ &
        (aatmp/weight_large_equation_errors_factor + 1.d0)*aatmp
    if (normtype == 0) then
      newtres = max(newtres,aatmp)  ! infinity norm
    else if (normtype.eq.1) then
      newtres = newtres + aatmp ! 1 norm
    else
      newtres = newtres + aatmp**2 ! 2 norm
    endif
! also record maximum error location
    if (aatmp.gt.aatmpmax) then
      aatmpmax = aatmp
      merr = m
      nserr = ns
    endif
  end do
end do

if (normtype == 1.or.normtype == 2) newtres = newtres/dble(max(ptotal,1))

if (normtype == 2) newtres = sqrt(newtres)

if (.not.number_is_valid(newtres)) ierror = 3

if (debug) then ! this is done in check_variable_validity routinely now if convergence_details_file is on
  if (min(nserr,merr) > 0) then
    formatline = '(a,'//trim(dindexformat(nserr))//',a,'//trim(dindexformat(merr))//',a,g10.3,a,g10.3,a)'
    write(*,fmt=formatline) 'INFO: maximum error associated with equation: ns = ',nserr,': m = ',merr,'('//trim(var(merr)%name)// &
      ') : funk = ',var(merr)%funk(nserr)%v,': equation_magnitude = ',var(merr)%magnitude,' at '// &
      trim(variable_location_string(merr,nserr))
  else
    write(*,'(a)') 'INFO: equation with maximum error not found'
  end if
end if

if (debug) write(*,'(a/80(1h-))') 'subroutine residual'

end subroutine residual

!-----------------------------------------------------------------

subroutine check_variable_validity(type,ierror)

! here we run through variables of type type checking that they are valid numbers (no NaNs) and not too large
! if a problem is found then ierror is set to 1 here

use general_module

character(len=*) :: type ! variable type: constant, transient, unknown, derived, equation, output, condition, local
integer :: ierror, n, p, pp, mm, ns, nvar, m, mmax, nsmax, mdermax, nsdermax, nsm, j, jj, i, ii
double precision :: varmax, vartmp, dermax
character(len=1000) :: formatline, error_string
logical, parameter :: debug = .false.
logical :: debug_sparse = .false.
logical, parameter :: derivative_details = .true. ! output the equation derivatives for the equation with the largest normalised error

if (debug) debug_sparse = .true.

if (debug_sparse) write(*,'(80(1h+)/a)') 'subroutine check_variable_validity'

ierror = 0
mmax = 0
nsmax = 0
varmax = 0.d0
dermax = 0.d0
mdermax = 0
nsdermax = 0

! check for NANs in all stored variable types
variable_loop: do nvar = 1, allocatable_size(var_list(var_list_number(centring="all",type=trim(type)))%list)
  m = var_list(var_list_number(centring="all",type=trim(type)))%list(nvar)
  do ns = 1, ubound(var(m)%funk,1)

    if (debug) write(*,*) 'm = ',m,': name = ',trim(var(m)%name),': type = ',trim(var(m)%type),': ns = ',ns

    if (debug) write(*,*) 'checking on validity'

! check on the validity of the value
    if (.not.number_is_valid(var(m)%funk(ns)%v)) then
      ierror = 1
      write(*,'(a)') 'ERROR: at least one element of '//trim(var(m)%type)//' variable '//trim(var(m)%name)// &
        ' was invalid (NaN or infty) at '//trim(variable_location_string(m,ns))
      if (convergence_details_file) write(fconverge,'(a)') &
        'ERROR: at least one element of '//trim(var(m)%type)//' variable '//trim(var(m)%name)// &
        ' was invalid (NaN or infty) at '//trim(variable_location_string(m,ns))
      if (debug_sparse) then
        cycle
      else
        cycle variable_loop
      end if
    end if

! for unknowns and equations, also check on relative magnitudes if they are valid (ie, have been calculated)
! for other variables, look for the location of the largest magnitude for reporting purposes

    if (trim(type) == "unknown".or.trim(type) == "equation") then

      if (var(m)%magnitude > 0.d0) then
        if (debug) write(*,*) 'checking on relative magnitude of variables'
        vartmp = abs(var(m)%funk(ns)%v/var(m)%magnitude)
        if (vartmp > normalised_variable_limit) then
          ierror = 1
          formatline = '(a/a,'//trim(floatformat)//'/a,'//trim(floatformat)//'/a/a)'
          write(*,fmt=formatline) 'ERROR: an element of '//trim(type)//' variable '//trim(var(m)%name)// &
            ' is much larger than its specified magnitude', &
            '  |variable| = ',abs(var(m)%funk(ns)%v), &
            '  order of magnitude = ',abs(var(m)%magnitude), &
            '  location = '//trim(variable_location_string(m,ns)), &
            'You could adjust the magnitude for this variable or make normalised_variable_limit larger in general_module.f90, '// &
            'however, if the order of magnitude specified for this variable is correct, this error most likely indicates '// &
            'a convergence problem.'
          if (convergence_details_file) &
            write(fconverge,'(a)') 'ERROR: an element of '//trim(var(m)%type)//' variable '//trim(var(m)%name)// &
              ' is much larger than its specified magnitude at '//trim(variable_location_string(m,ns))
          if (.not.debug_sparse) cycle variable_loop
        end if
      else
! no magnitude has been defined yet, so set vartmp to negative so that it won't get picked up in the comparison against other
!  variables that may have a legitimate magnitude known unknown/equation
        vartmp = -1.d0
      end if

    else
      vartmp = abs(var(m)%funk(ns)%v) ! just recorded for the rest of the variables
    end if
    if (convergence_details_file.and.vartmp > varmax) then
      varmax = vartmp
      mmax = m
      nsmax = ns
    end if

! also check on validity of any derivatives

    if (trim(type) == "derived".or.trim(type) == "equation") then

      if (debug) write(*,*) 'checking on validity of derivatives'

      do n = 1,var(m)%funk(ns)%ndv
        if (.not.number_is_valid(var(m)%funk(ns)%dv(n))) then
          ierror = 1
          pp = var(m)%funk(ns)%pp(n)
! search for unknown variable that pp refers to
          mm = unknown_var_from_pp(pp)
          if (mm == 0) then
            write(*,*) 'ERROR: unknown variable not found in update_checker'
            write(*,'(5(a,i6))') 'p = ',p,': m = ',m,': ns = ',ns,': pp = ',pp,': mm = ',mm
            stop
          end if
          write(*,'(a)') 'ERROR: one derivative of '//trim(var(m)%type)//' variable '//trim(var(m)%name)// &
            ' with respect to unknown '//trim(var(mm)%name)//' was invalid (NaN or infty) at '// &
            trim(variable_location_string(m,ns))
          if (convergence_details_file) write(fconverge,'(a)') 'ERROR: one derivative of '//trim(var(m)%type)//' variable '// &
            trim(var(m)%name)//' with respect to unknown '//trim(var(mm)%name)//' was invalid (NaN or infty) at '// &
            trim(variable_location_string(m,ns))
          if (debug_sparse) then
            cycle
          else
            cycle variable_loop
          end if
        end if
        if (abs(var(m)%funk(ns)%dv(n)) > dermax) then
          dermax = abs(var(m)%funk(ns)%dv(n))
          nsdermax = ns
          mdermax = m
        end if
      end do

    end if

    if (debug) write(*,'(a)') 'var(m)%name = '//trim(var(m)%name)// &
      ': var(m)%funk(ns) = '//trim(print_funk(var(m)%funk(ns)))

  end do

end do variable_loop

if (convergence_details_file.and.mmax /= 0.and.nsmax /= 0.and.ierror == 0) then
  if (trim(type) == "unknown" .or. trim(type) == "equation") then
    write(fconverge,'(a,g11.4)') 'INFO: the '//trim(type)//' with the maximum normalised magnitude is '// &
    trim(var(mmax)%name)//' at '//trim(variable_location_string(mmax,nsmax))//': normalised magnitude = ',varmax
    if (trim(type) == "equation".and.derivative_details) then
      write(fconverge,'(a,g11.4)') '  WITH equation value = ',var(mmax)%funk(nsmax)%v
! also output the derivative magnitudes for this equation
      do n = 1,var(mmax)%funk(nsmax)%ndv
        pp = var(mmax)%funk(nsmax)%pp(n)
        mm = unknown_var_from_pp(pp)
        if (mm == 0) call error_stop("problem outputing derivative magnitudes in subroutine check_variable_validity")
        nsm = pp - var(mm)%funk(1)%pp(1) + 1 ! the ns for the unknown can be found from the unknown derivatives
        write(fconverge,'(a,g11.4,a,i10)') '  WITH unnormalised derivative magnitude = ',var(mmax)%funk(nsmax)%dv(n),' for '// &
          trim(var(mm)%centring)//' unknown '//trim(var(mm)%name)//' at ijk = ',ijkvar(mm,nsm)
      end do
! use these debugging statements to extract surrounding variable derivatives too
      if (.false.) then
! <foo_f>, face centred
        m = var_number_from_name("<q_f>")
        do jj = 1, ubound(cell(ijkvar(mmax,nsmax))%jface,1)
          j = cell(ijkvar(mmax,nsmax))%jface(jj)
          error_string = "debugging"
          ns = nsvar(m,j,error_string)
          write(fconverge,'(a,i10,a,g11.4)') trim(var(m)%centring)//" "//trim(var(m)%type)//" "//trim(var(m)%name)//" at j = ",j, &
            " has value = ",var(m)%funk(ns)%v
          do n = 1,var(m)%funk(ns)%ndv
            pp = var(m)%funk(ns)%pp(n)
            mm = unknown_var_from_pp(pp)
            if (mm == 0) call error_stop("problem outputing derivative magnitudes in subroutine check_variable_validity")
            nsm = pp - var(mm)%funk(1)%pp(1) + 1 ! the ns for the unknown can be found from the unknown derivatives
            write(fconverge,'(a,g11.4,a,i10)') '  WITH unnormalised derivative magnitude = ',var(m)%funk(ns)%dv(n),' for '// &
              trim(var(mm)%centring)//' unknown '//trim(var(mm)%name)//' at ijk = ',ijkvar(mm,nsm)
          end do
        end do
      end if
      if (.false.) then
! <foo limiter>, cell centred
        m = var_number_from_name("<q limiter>")
!       do ii = 1, ubound(cell(ijkvar(mmax,nsmax))%jface,1)+1 ! will pull out all adjacent surrounding cells including itself
        do ii = 1, ubound(cell(ijkvar(mmax,nsmax))%icell,1) ! will pull out all surrounding cells including itself
          i = cell(ijkvar(mmax,nsmax))%icell(ii)
          error_string = "debugging"
          ns = nsvar(m,i,error_string)
          write(fconverge,'(a,i10,a,g11.4)') trim(var(m)%centring)//" "//trim(var(m)%type)//" "//trim(var(m)%name)//" at i = ",i, &
            " has value = ",var(m)%funk(ns)%v
          do n = 1,var(m)%funk(ns)%ndv
            pp = var(m)%funk(ns)%pp(n)
            mm = unknown_var_from_pp(pp)
            if (mm == 0) call error_stop("problem outputing derivative magnitudes in subroutine check_variable_validity")
            nsm = pp - var(mm)%funk(1)%pp(1) + 1 ! the ns for the unknown can be found from the unknown derivatives
            write(fconverge,'(a,g11.4,a,i10)') '  WITH unnormalised derivative magnitude = ',var(m)%funk(ns)%dv(n),' for '// &
              trim(var(mm)%centring)//' unknown '//trim(var(mm)%name)//' at ijk = ',ijkvar(mm,nsm)
          end do
        end do
      end if
    end if
  else
    write(fconverge,'(a,g11.4)') 'INFO: the '//trim(type)//' with the maximum (unnormalised) magnitude is '// &
    trim(var(mmax)%name)//' at '//trim(variable_location_string(mmax,nsmax))//': magnitude = ',varmax
  end if
end if

if ((trim(type) == "derived".or.trim(type) == "equation").and.convergence_details_file.and.mdermax /= 0.and.nsdermax /= 0 &
  .and.ierror == 0) &
  write(fconverge,'(a,g11.4)') 'INFO: the '//trim(type)//' with the maximum (unnormalised) derivative magnitude is '// &
  trim(var(mdermax)%name)//' at '//trim(variable_location_string(mdermax,nsdermax))//': magnitude = ',dermax

if (debug_sparse) write(*,'(a/80(1h-))') 'subroutine check_variable_validity'

end subroutine check_variable_validity

!-----------------------------------------------------------------

subroutine update_magnitudes(ierror)

! here we run through each equation and unknown variable finding a representative order of magnitude
! we also check that these magnitudes are sensible

use general_module

integer :: ierror, n, pp, mm, ns, nvar, m
double precision :: old_magnitude, equation_magnitude
logical :: first
logical, parameter :: rms_magnitude = .true.
logical, parameter :: debug = .false.
logical :: debug_sparse = .false.
double precision :: error_magnitude = 1.d-40 ! value for a magnitude that indicates an error

if (debug) debug_sparse = .true.

if (debug_sparse) write(*,'(80(1h+)/a)') 'subroutine update_magnitudes'

ierror = 0
first = .false.

! update order of magnitude of unknowns
unknown_magnitude_loop: do nvar = 1, allocatable_size(var_list(var_list_number_unknown)%list)
  m = var_list(var_list_number_unknown)%list(nvar)
  if (var(m)%magnitude < 0.d0) first = .true.
  if (.not.(var(m)%dynamic_magnitude.or.first)) cycle
  old_magnitude = var(m)%magnitude
  if (var(m)%magnitude_constant > 0.and.first) then
    var(m)%magnitude = abs(var(var(m)%magnitude_constant)%funk(1)%v)
    write(*,'(a,g10.3)') 'INFO: found magnitude for unknown '//trim(var(m)%name)//' based on constant '// &
      trim(var(var(m)%magnitude_constant)%name)//': magnitude = ',var(m)%magnitude
    if (convergence_details_file) write(fconverge,'(a,g10.3)') &
      'INFO: found magnitude for unknown '//trim(var(m)%name)//' based on constant '// &
      trim(var(var(m)%magnitude_constant)%name)//': magnitude = ',var(m)%magnitude
  else
    if (rms_magnitude) then
      var(m)%magnitude = sqrt(max(sum(var(m)%funk(:)%v**2)/dble(max(ubound(var(m)%funk,1),1)),0.d0))
    else
      var(m)%magnitude = maxval(abs(var(m)%funk(:)%v))
    end if
    write(*,'(a,g10.3)') 'INFO: found magnitude for unknown '//trim(var(m)%name)//' based on initial values: '// &
      'magnitude = ',var(m)%magnitude
    if (convergence_details_file) write(fconverge,'(a,g10.3)') &
      'INFO: found magnitude for unknown '//trim(var(m)%name)//' based on initial values: magnitude = ',var(m)%magnitude
  end if
  if (debug_sparse) write(*,'(2(a,g10.3))') 'unknown '//trim(var(m)%name)//': raw magnitude ',var(m)%magnitude, &
    ': old_magnitude = ',old_magnitude
  if (first) then
    if (var(m)%magnitude < error_magnitude) call error_stop("a zero initial magnitude was calculated for this variable. "// &
      "Every unknown variable needs a finite order of magnitude.  Either initialise this "// &
      "unknown with non-zero values, or set the magnitude for this explicitly via the option magnitude=[value|constant] in "// &
      "the input file.")
  end if
  if (.not.first) then
    var(m)%magnitude = max(min(var(m)%magnitude,old_magnitude*var(m)%dynamic_magnitude_multiplier), &
      old_magnitude/var(m)%dynamic_magnitude_multiplier) ! underrelax new unknown magnitude to avoid sudden changes
  else
    first = .false.
  end if
  if (debug_sparse) write(*,'(a,g13.6)') '  final: magnitude = ',var(m)%magnitude
  if (var(m)%magnitude < error_magnitude) call error_stop("The magnitude for unknown variable "//trim(var(m)%name)// &
    "is approaching zero.  The best scenario is that the solution to the problem has this variable equal to "// &
    "zero.  If this is the case then consider setting the magnitude of this unknown statically "// &
    "(staticmagnitude option in the input file) or decrease the dynamic magnitude multiplier for this variable (via "// &
    "the option dynamicmagnitudemultiplier=value).")
end do unknown_magnitude_loop

! update order of magnitude of equations
equation_magnitude_loop: do nvar = 1, allocatable_size(var_list(var_list_number_equation)%list)
  m = var_list(var_list_number_equation)%list(nvar)
  if (var(m)%magnitude < 0.d0) first = .true.
  if (.not.(var(m)%dynamic_magnitude.or.first)) cycle
  old_magnitude = var(m)%magnitude
  if (var(m)%magnitude_constant > 0.and.first) then
    var(m)%magnitude = abs(var(var(m)%magnitude_constant)%funk(1)%v)
    write(*,'(a,g10.3)') 'INFO: found magnitude for equation '//trim(var(m)%name)//' based on constant '// &
      trim(var(var(m)%magnitude_constant)%name)//': magnitude = ',var(m)%magnitude
    if (convergence_details_file) write(fconverge,'(a,g10.3)') &
      'INFO: found magnitude for equation '//trim(var(m)%name)//' based on constant '// &
      trim(var(var(m)%magnitude_constant)%name)//': magnitude = ',var(m)%magnitude
  else
    var(m)%magnitude = 0.d0
    do ns = 1, ubound(var(m)%funk,1)
! need to think about this in terms of the convergence criterion
      equation_magnitude = abs(var(m)%funk(ns)%v)
      do n = 1,var(m)%funk(ns)%ndv
        pp = var(m)%funk(ns)%pp(n)
! search for unknown variable that pp refers to
        mm = unknown_var_from_pp(pp)
        if (mm == 0) then
          write(*,*) 'ERROR: unknown variable not found in update_checker'
          write(*,'(4(a,i6))') 'm = ',m,': ns = ',ns,': pp = ',pp,': mm = ',mm
          stop
        end if
        equation_magnitude = max(equation_magnitude,abs(var(m)%funk(ns)%dv(n)*var(mm)%magnitude))
        if (debug.and..false.) then
          write(*,'(4(a,i6))') 'm = ',m,': ns = ',ns,': pp = ',pp,': mm = ',mm
          write(*,'(3(a,g14.6))') 'equation_magnitude = ',equation_magnitude,': var(m)%funk(ns)%dv(n) = ', &
            var(m)%funk(ns)%dv(n),': var(mm)%magnitude = ',var(mm)%magnitude 
        end if
      end do
      if (rms_magnitude) then
        var(m)%magnitude = var(m)%magnitude + equation_magnitude**2
      else
        var(m)%magnitude = max(var(m)%magnitude,equation_magnitude)
      end if
      if (debug) write(*,'(a,g13.6,a,g13.6,a)') 'equation_magnitude = ',equation_magnitude, &
        'old_magnitude = ',old_magnitude,': var(m)%name = '//trim(var(m)%name)// &
        ': var(m)%funk(ns) = '//trim(print_funk(var(m)%funk(ns)))
      if (equation_magnitude < error_magnitude) then
        write(*,'(a/a,g14.6,a)') 'ERROR: the maximum product of a jacobian element times its corresponding unknown variable', &
          '  magnitude for '//trim(var(m)%type)//' '//trim(var(m)%name)//' is ',equation_magnitude,' at '// &
          trim(variable_location_string(m,ns))
        if (convergence_details_file) write(fconverge,'(a/a,g14.6,a)') &
          'ERROR: the maximum product of a jacobian element times its corresponding unknown variable', &
          '  magnitude for '//trim(var(m)%type)//' '//trim(var(m)%name)//' is ',equation_magnitude,' at '// &
          trim(variable_location_string(m,ns))
        write(*,'(a)') '  This indicates that this '//trim(var(m)%type)//' is currently not dependent on any of the unknown '// &
          'variables.  Equivalently, the total derivative of this '//trim(var(m)%type)//' with respect to any of the '// &
          'unknown variables is zero (or very small).  Note that every equation must depend on atleast one unknown variable '// &
          'at all times, otherwise the jacobian has a zero row and is singular.  One possibility is that one of the variables '// &
          'used has the option ''noderivative'' set explicitly or implicitly (by being an output variable for example).  You '// &
          'may be able to find more clues in the output/convergence_details.txt file, created if convergence_details_file '// &
          'is set to true in general_module.f90.  As a last resort set debug = .true. in subroutine update_magnitudes.'
        ierror = 1
        cycle equation_magnitude_loop
      end if
    end do
    if (rms_magnitude) var(m)%magnitude = sqrt(max(var(m)%magnitude/dble(max(ubound(var(m)%funk,1),1)),0.d0))
    if (convergence_details_file.and.first) write(fconverge,'(a,g10.3)') &
      'INFO: found magnitude for equation '//trim(var(m)%name)//' based on initial values: magnitude = ',var(m)%magnitude
  end if
  if (debug_sparse) write(*,'(2(a,g10.3))') 'equation '//trim(var(m)%name)//': raw magnitude ',var(m)%magnitude, &
    ': old_magnitude = ',old_magnitude 
  if (.not.first) then
    var(m)%magnitude = max(min(var(m)%magnitude,old_magnitude*var(m)%dynamic_magnitude_multiplier), &
      old_magnitude/var(m)%dynamic_magnitude_multiplier) ! underrelax new unknown magnitude to avoid sudden changes
  else
    first = .false.
  end if
  if (debug_sparse) write(*,'(a,g13.6)') '  final: magnitude = ',var(m)%magnitude
  if (var(m)%magnitude < error_magnitude) &
    call error_stop("The magnitude for equation variable "//trim(var(m)%name)//"is approaching zero.")
end do equation_magnitude_loop

if (debug_sparse) write(*,'(a/80(1h-))') 'subroutine update_magnitudes'

end subroutine update_magnitudes

!-----------------------------------------------------------------

subroutine calc_condition_number(aa,iaa,jaa)

! diagnostic routine to calculate the condition number of the jacobian matrix using
!  svd
! requires lots of memory!

use lapack_module

double precision, dimension(:) :: aa ! already allocated
integer, dimension(:) :: iaa, jaa ! already allocated
double precision, dimension(:,:), allocatable :: aa_full
double precision, dimension(:), allocatable :: w
double precision, dimension(:,:), allocatable :: u, v
double precision :: wmin, wmax
integer :: n, nz, nn, nr
logical :: error

! arrays produced by the mainsolver subroutine are in compressed sparse row (csr) format with 1 indexing
! find array bounds and check that they are consistent
n = ubound(iaa,1) - 1
nz = iaa(n+1) - 1
write(60,*) 'n = ',n,': nz = ',nz
if (ubound(aa,1) /= nz.or.ubound(jaa,1) /= nz) stop &
  'ERROR: some of the csr matrices that are entering calc_condition_number are incorrectly sized'

! first thing is allocate all variables and reconstruct aa matrix using nonsparse storage from csr1
allocate(aa_full(n,n),u(n,n),w(n),v(n,n))
aa_full = 0.d0
u = 0.d0
w = 0.d0
v = 0.d0
do nr = 1, n
  if (iaa(nr) < iaa(nr+1)) then
    do nn = iaa(nr), iaa(nr+1)-1
      aa_full(nr,jaa(nn)) = aa(nn)
    end do
  end if
end do
  
! now do svd of aa_full
call lapack_singular_value_decomposition(a=aa_full,u=u,w=w,v=v,error=error)

if (error) then
  write(*,*) 'ERROR: problem doing svd in condition_number'
  stop
end if
  
do nn = 1, n
  write(60,*) 'nn = ',nn,': w(nn) = ',w(nn)
end do
wmin = minval(w)
wmax = maxval(w)
write(60,*) 'wmin = ',wmin,': wmax = ',wmax,': wmax/wmin = ',wmax/wmin
write(*,'(a,g13.6)') 'SPECIAL: condition number = ',wmax/wmin

end subroutine calc_condition_number

!-----------------------------------------------------------------

subroutine calc_condition_number_estimate(aa,iaa,jaa)

! diagnostic routine to calculate estimate condition number of the jacobian matrix using
!  the 1-norm estimate due to higham88
! requires the suitesparse routine

use donest_module
use suitesparse_module

double precision, dimension(:), allocatable :: aa ! already allocated
integer, dimension(:), allocatable :: iaa, jaa ! already allocated
double precision, dimension(:), allocatable :: x, colsum
double precision :: onenorm, onenormi ! 1-norm of A and A^-1
integer :: n, nz, kase, nn
integer :: ierror
logical, parameter :: debug = .true.

! arrays produced by the mainsolver subroutine are in compressed sparse row (csr) format with 1 indexing
! find array bounds and check that they are consistent
n = ubound(iaa,1) - 1
nz = iaa(n+1) - 1
if (debug) write(*,*) 'n = ',n,': nz = ',nz
if (ubound(aa,1) /= nz.or.ubound(jaa,1) /= nz) stop &
  'ERROR: some of the csr matrices that are entering calc_condition_number_estimate are incorrectly sized'

! first calculate onenorm for A which is maximum column sum of absolute values
allocate(colsum(n))
colsum = 0.d0
do nn = 1, nz
  colsum(jaa(nn)) = colsum(jaa(nn)) + abs(aa(nn))
end do
onenorm = maxval(colsum)
if (debug) write(*,*) 'onenorm = ',onenorm
deallocate(colsum)

! now use higham88's routine to estimate the 1-norm for the inverse of A, onenormi
allocate(x(n))

kase = 0
ierror = 0
do
  call donest_interface(kase=kase,x=x,est=onenormi)
  if (kase == 0) then
    exit ! estimation is complete
  else if (kase == 1) then
    call suitesparse_linear_solver(aa,iaa,jaa,x,ierror) ! solve the system
  else if (kase == 2) then
    call suitesparse_linear_solver(aa,iaa,jaa,x,ierror,trans=.true.) ! solve the transpose system
  end if
  if (debug) write(*,*) 'finished donest phase: kase = ',kase,': onenormi = ',onenormi,': ierror = ',ierror
  if (ierror /= 0) exit
end do

deallocate(x)

if (ierror == 0) then
  write(*,'(a,g13.6)') 'SPECIAL: condition number estimate = ',onenorm*onenormi
else
  write(*,*) 'SPECIAL WARNING: error during linear solve phase when estimating condition number'
end if

end subroutine calc_condition_number_estimate

!-----------------------------------------------------------------

subroutine calc_diagonal_dominance

! diagnostic routine to calculate the diagonal dominance of the jacobian matrix

use general_module

integer :: neq, meq, nseq, ijkeq, nn, pp, nsun, mun, ijkun, ijkdiagonal, mdiagonal, min_norm_ijk
double precision :: row_sum, row_abs_sum, diagonal, norm_diagonal, min_norm_diagonal
character(len=1000) :: formatline
logical, parameter :: debug = .true.

! loop through all equations finding those that have the same centring and element location as an unknown
! right now assumes that the first unknown that the equation depends upon at the same location is the one that is relevant to this equation
! difficult to generalise this to more that one unknown at the current location without serious complications
do neq = 1, allocatable_size(var_list(var_list_number_equation)%list)
  meq = var_list(var_list_number_equation)%list(neq)
  if (debug) write(90,'(a)') 'Examining equation '//trim(var(meq)%name)//' with centring '//trim(var(meq)%centring)
  if (trim(var(meq)%centring) == 'none') cycle
! reset equation counters
  min_norm_diagonal = huge(1.d0)
  min_norm_ijk = 0
  do nseq = 1, ubound(var(meq)%funk,1)
! find location of equation
    ijkeq = region(var(meq)%region_number)%ijk(nseq)
! reset location counters
    row_sum = 0.d0
    row_abs_sum = 0.d0
    diagonal = 0.d0
    ijkdiagonal = 0
    mdiagonal = 0
    norm_diagonal = 0.d0
    do nn = 1, var(meq)%funk(nseq)%ndv
      row_sum = row_sum + var(meq)%funk(nseq)%dv(nn) 
      row_abs_sum = row_abs_sum + abs(var(meq)%funk(nseq)%dv(nn))
      pp = var(meq)%funk(nseq)%pp(nn) 
      mun = unknown_var_from_pp(pp)
      if (mun == 0) call error_stop('mun equals zero in calc_diagonal_dominance')
! check that centring of equation and unknown are consistent, otherwise skip
      if (trim(var(mun)%centring) /= trim(var(meq)%centring)) cycle
      nsun = pp - var(mun)%funk(1)%pp(1) + 1 ! unknown pps can be found from their derivatives
      ijkun = region(var(mun)%region_number)%ijk(nsun) ! finally calculate 
      if (ijkeq == ijkun) then
! here we have the same location and centring as the equation, so assume that this pp entry corresponds to the matrix diagonal
        ijkdiagonal = ijkun
        mdiagonal = mun
        diagonal = var(meq)%funk(nseq)%dv(nn)
      end if
    end do
    if (mdiagonal > 0) then
      norm_diagonal = abs(diagonal)/max(row_abs_sum-abs(diagonal),tiny(1.d0))
      if (norm_diagonal < min_norm_diagonal) then
        min_norm_ijk = ijkeq
        min_norm_diagonal = norm_diagonal
      end if
    end if
      
    if (debug) then
      if (mdiagonal > 0) then
        formatline = '(a,'//indexformat//',a,'//indexformat//',a,'//floatformat//')'
        write(90,fmt=formatline) 'eq = '//trim(var(meq)%name)//': centring = '//trim(var(mdiagonal)%centring)//':nseq = ' &
          ,nseq,': ijk = ',ijkeq,': un = '//trim(var(mdiagonal)%name)//': norm_diagonal = ',norm_diagonal
      else
        formatline = '(a,'//indexformat//',a,'//indexformat//',a)'
        write(90,fmt=formatline) 'eq = '//trim(var(meq)%name)//': centring = '//trim(var(meq)%centring)//':nseq = ' &
          ,nseq,': ijk = ',ijkeq,': unknown not found'
      end if
    end if
  end do
  if (min_norm_ijk /= 0) then
    formatline = '(a,'//floatformat//',a,'//indexformat//')'
    write(*,fmt=formatline) 'SPECIAL: equation '//trim(var(meq)%name)//' has min_norm_diagonal = ',min_norm_diagonal,' at ijk = ', &
      min_norm_ijk
  else
    write(*,fmt=formatline) 'SPECIAL: no unknowns found for equation '//trim(var(meq)%name)//' to find min_norm_diagonal'
  end if
end do

end subroutine calc_diagonal_dominance

!-----------------------------------------------------------------

subroutine find_sensitive_equation(varname,icell)

use general_module
character(len=*) :: varname
integer :: icell
integer :: m, nvar, ns, p, n, pp, um, uns, up, em, ens
double precision :: ederiv, deriv, evalue, value
character(len=1000) :: formatline
logical, parameter :: debug_file = .true.

! first loop through each unknown finding number of varname
um = 0 ! this is the unknown variable number that we are examining
do nvar = 1, allocatable_size(var_list(var_list_number_unknown)%list)
  m = var_list(var_list_number_unknown)%list(nvar)
  if (trim(var(m)%name) == trim(varname)) then
    um = m
    exit
  end if
end do
if (um == 0) call error_stop("unknown variable "//trim(varname)//" not found in find_sensitive_equation")
if (trim(var(um)%centring) /= "cell") &
  call error_stop("unknown variable "//trim(varname)//" has the wrong centring in find_sensitive_equation")

! now calculate ns for particular icell, and also the p number (up) for the unknown that we are searching for
uns = region(var(um)%region_number)%ns(icell)
if (uns == 0) call error_stop("cannot unknown variable "//trim(varname)//" for particular icell in find_sensitive_equation")
up = var(um)%funk(1)%pp(1) + uns - 1

if (debug_file) then
  formatline = '(a,'//trim(indexformat)//',a,g11.4)'
  write(84,fmt=formatline) 'unknown = '//trim(var(um)%name)//': icell = ',region(var(um)%region_number)%ijk(uns), &
    ': value = ',var(um)%funk(uns)%v
end if

! now loop through each equation searching for derivatives w.r.t. up
em = 0 ! equation number m that has the largest derivative magnitude w.r.t. up
ens = 0 ! ns for this equation
ederiv = 0.d0 ! maximum normalised derivative
evalue = 0.d0 ! normalised equation value for this equation
p = 0
equation_magnitude_loop: do nvar = 1, allocatable_size(var_list(var_list_number_equation)%list)
  m = var_list(var_list_number_equation)%list(nvar)
  do ns = 1, ubound(var(m)%funk,1)
    p = p + 1
    do n = 1,var(m)%funk(ns)%ndv
      pp = var(m)%funk(ns)%pp(n)
      if (pp == up) then
        deriv = var(m)%funk(ns)%dv(n)/var(m)%magnitude
        value = var(m)%funk(ns)%v/var(m)%magnitude
        if (debug_file) then
          if (trim(var(m)%centring) == 'cell') then
            formatline = '(a,'//trim(indexformat)//',3(a,g11.4))'
            write(84,fmt=formatline) 'equation = '//trim(var(m)%name)//': icell = ',region(var(m)%region_number)%ijk(ns), &
              ': deriv = ',deriv,': value = ',value,': product = ',deriv*value
          else if (trim(var(m)%centring) == 'face') then
            formatline = '(a,'//trim(indexformat)//',3(a,g11.4))'
            write(84,fmt=formatline) 'equation = '//trim(var(m)%name)//': jface = ',region(var(m)%region_number)%ijk(ns), &
              ': deriv = ',deriv,': value = ',value,': product = ',deriv*value
          else if (trim(var(m)%centring) == 'none') then
            formatline = '(a,3(a,g11.4))'
            write(84,fmt=formatline) 'equation = '//trim(var(m)%name)//': none centred ',': deriv = ',deriv,': value = ', &
              value,': product = ',deriv*value
          end if
        end if
        if (abs(deriv*value) > abs(ederiv*evalue)) then
          em = m
          ens = ns
          ederiv = deriv
          evalue = value
        end if
      end if
    end do
  end do
end do equation_magnitude_loop

formatline = '(a,'//trim(indexformat)//',a)'
write(*,fmt=formatline) 'DEBUGGING INFO: w.r.t. unknown = '//trim(varname)//' and icell = ',icell,' most heavily influences:'
formatline = '(a)'
write(*,fmt=formatline) '  equation = '//trim(var(em)%name)
formatline = '(a,'//trim(indexformat)//')'
if (trim(var(em)%centring) == 'cell') then
  write(*,fmt=formatline) '  icell = ',region(var(em)%region_number)%ijk(ens)
else if (trim(var(em)%centring) == 'face') then
  write(*,fmt=formatline) '  jface = ',region(var(em)%region_number)%ijk(ens)
else if (trim(var(em)%centring) == 'none') then
  write(*,fmt=formatline) '  none centred equation'
end if
formatline = '(a,g11.4)'
write(*,fmt=formatline) '  ederiv = ',ederiv
write(*,fmt=formatline) '  evalue = ',evalue
write(*,fmt=formatline) '  eproduct = ',evalue*ederiv

if (debug_file) then
  write(84,*) 'LARGEST DERIV MAGNITUDE CORRESPONDS TO:'
  if (debug_file) then
    if (trim(var(em)%centring) == 'cell') then
      formatline = '(a,'//trim(indexformat)//',3(a,g11.4))'
      write(84,fmt=formatline) 'equation = '//trim(var(em)%name)//': icell = ',region(var(em)%region_number)%ijk(ens), &
        ': ederiv = ',ederiv,': evalue = ',evalue,': eproduct = ',ederiv*evalue
    else if (trim(var(em)%centring) == 'face') then
      formatline = '(a,'//trim(indexformat)//',3(a,g11.4))'
      write(84,fmt=formatline) 'equation = '//trim(var(em)%name)//': jface = ',region(var(em)%region_number)%ijk(ens), &
        ': ederiv = ',ederiv,': evalue = ',evalue,': eproduct = ',ederiv*evalue
    else if (trim(var(em)%centring) == 'none') then
      formatline = '(a,3(a,g11.4))'
      write(84,fmt=formatline) 'equation = '//trim(var(em)%name)//': none centred ', &
        ': ederiv = ',ederiv,': evalue = ',evalue,': eproduct = ',ederiv*evalue
    end if
  end if
end if

end subroutine find_sensitive_equation

!-----------------------------------------------------------------

subroutine update_and_check_derived_and_equations(ierror,setup)

! this wrapper routine does both the update and checking of these variables
! if ierror is present then it will return with the error
! if ierror is not present, then any error will halt the simulation
! if setup is present and true, then data will be read from a file if the individual variable option is specified (v0.50)

use general_module
use equation_module
logical, optional :: setup
integer, optional :: ierror
integer :: ierrorl
logical :: setupl ! local version of setup

if (present(setup)) then
  setupl = setup
else
  setupl = .false.
end if

call update_derived_and_equations(setupl)

call check_variable_validity(type='derived',ierror=ierrorl)
if (ierrorl /= 0) then
  if (present(ierror)) then
    ierror = ierrorl
    return
  else
    call error_stop("problem with update of a derived variable: NAN detected")
  end if
end if

call check_variable_constraints(type='derived',ierror=ierrorl)
if (ierrorl /= 0) then
  if (present(ierror)) then
    ierror = ierrorl
    return
  else
    call error_stop("problem with update of a derived variable: constraints violated")
  end if
end if

call check_variable_validity(type='equation',ierror=ierrorl)
if (ierrorl /= 0) then
  if (present(ierror)) then
    ierror = ierrorl
    return
  else
    call error_stop("problem with update of an equation variable: NAN detected")
  end if
end if

call check_variable_constraints(type='equation',ierror=ierrorl)
if (ierrorl /= 0) then
  if (present(ierror)) then
    ierror = ierrorl
    return
  else
    call error_stop("problem with update of an equation variable: constraints violated")
  end if
end if

! also check locals here, as these are the final variables to be updated
call check_variable_constraints(type='local',ierror=ierrorl)
if (ierrorl /= 0) then
  if (present(ierror)) then
    ierror = ierrorl
    return
  else
    call error_stop("problem with update of a local variable: constraints violated")
  end if
end if

if (present(ierror)) ierror = 0

end subroutine update_and_check_derived_and_equations

!-----------------------------------------------------------------

subroutine update_and_check_unknowns(initial,lambda,ierror)

! this wrapper routine does both the update and checking of these variables
! if ierror is present then it will return with the error
! if ierror is not present, then any error will halt the simulation

use general_module
use equation_module
logical :: initial ! whether this is the initial or update invocation of this subroutine - must be specified
double precision, optional :: lambda ! backstepping parameter that is only used (and needs to be passed in) during non-initial update invocation
integer, optional :: ierror ! optional error code, which if not passed in will cause routine to die on finding an error
integer :: ierrorl

if (initial) then
  call update_unknowns(initial=initial)
else
  if (.not.present(lambda)) call error_stop("horrible internal error in update_and_check_knowns around lambda")
  call update_unknowns(initial=initial,lambda=lambda)
end if

call check_variable_validity(type='unknown',ierror=ierrorl)
if (ierrorl /= 0) then
  if (present(ierror)) then
    ierror = ierrorl
    return
  else
    call error_stop("problem with initialisation of an unknown variable: NAN detected")
  end if
end if

call check_variable_constraints(type='unknown',ierror=ierrorl)
if (ierrorl /= 0) then
  if (present(ierror)) then
    ierror = ierrorl
    return
  else
    call error_stop("problem with initialisation of an equation variable: constraints violated")
  end if
end if

if (present(ierror)) ierror = 0

end subroutine update_and_check_unknowns

!-----------------------------------------------------------------

subroutine update_and_check_newtients(ierror)

! this wrapper routine does both the update and checking of these variables
! if ierror is present then it will return with the error
! if ierror is not present, then any error will halt the simulation

use general_module
use equation_module
integer, optional :: ierror
integer :: ierrorl

call update_newtients

call check_variable_validity(type='newtient',ierror=ierrorl)
if (ierrorl /= 0) then
  if (present(ierror)) then
    ierror = ierrorl
    return
  else
    call error_stop("problem with initialisation of a newtient variable: NAN detected")
  end if
end if

if (present(ierror)) ierror = 0

end subroutine update_and_check_newtients

!-----------------------------------------------------------------

subroutine update_and_check_transients(ierror)

! this wrapper routine does both the update and checking of these variables
! if ierror is present then it will return with the error
! if ierror is not present, then any error will halt the simulation

use general_module
use equation_module
integer, optional :: ierror
integer :: ierrorl

call update_transients

call check_variable_validity(type='transient',ierror=ierrorl)
if (ierrorl /= 0) then
  if (present(ierror)) then
    ierror = ierrorl
    return
  else
    call error_stop("problem with initialisation of a transient variable: NAN detected")
  end if
end if

if (present(ierror)) ierror = 0

end subroutine update_and_check_transients

!-----------------------------------------------------------------

subroutine update_and_check_initial_newtients(ierror)

! this wrapper routine does both the update and checking of these variables
! if ierror is present then it will return with the error
! if ierror is not present, then any error will halt the simulation

use general_module
use equation_module
integer, optional :: ierror
integer :: ierrorl

call update_initial_newtients

call check_variable_validity(type='newtient',ierror=ierrorl)
if (ierrorl /= 0) then
  if (present(ierror)) then
    ierror = ierrorl
    return
  else
    call error_stop("problem with initialisation of an initial_newtient variable")
  end if
end if

if (present(ierror)) ierror = 0

end subroutine update_and_check_initial_newtients

!-----------------------------------------------------------------

subroutine update_and_check_initial_transients(ierror)

! this wrapper routine does both the update and checking of these variables
! if ierror is present then it will return with the error
! if ierror is not present, then any error will halt the simulation

use general_module
use equation_module
integer, optional :: ierror
integer :: ierrorl

call update_initial_transients

call check_variable_validity(type='transient',ierror=ierrorl)
if (ierrorl /= 0) then
  if (present(ierror)) then
    ierror = ierrorl
    return
  else
    call error_stop("problem with initialisation of an initial_transient variable: NAN detected")
  end if
end if

if (present(ierror)) ierror = 0

end subroutine update_and_check_initial_transients

!-----------------------------------------------------------------

subroutine update_and_check_constants(ierror)

! this wrapper routine does both the update and checking of these variables
! if ierror is present then it will return with the error
! if ierror is not present, then any error will halt the simulation

use general_module
use equation_module
integer, optional :: ierror
integer :: ierrorl
logical, parameter :: debug = .false.

if (debug) write(*,'(80(1h+)/a)') 'subroutine update_and_check_constants'

if (debug) write(*,*) 'INFO: about to update_constants'
call update_constants

if (debug) write(*,*) 'INFO: about to check_variable_validity for constants'
call check_variable_validity(type='constant',ierror=ierrorl)
if (ierrorl /= 0) then
  if (present(ierror)) then
    ierror = ierrorl
    return
  else
    call error_stop("problem with initialisation of a constant variable: NAN detected")
  end if
end if

if (present(ierror)) ierror = 0

if (debug) write(*,'(a/80(1h-))') 'subroutine update_and_check_constants'

end subroutine update_and_check_constants

!-----------------------------------------------------------------

subroutine update_and_check_outputs(ierror)

! this wrapper routine does both the update and checking of these variables
! if ierror is present then it will return with the error
! if ierror is not present, then any error will halt the simulation

use general_module
use equation_module
integer, optional :: ierror
integer :: ierrorl

call update_outputs

call check_variable_validity(type='output',ierror=ierrorl)
if (ierrorl /= 0) then
  if (present(ierror)) then
    ierror = ierrorl
    return
  else
    call error_stop("problem with initialisation of an output variable: NAN detected")
  end if
end if

if (present(ierror)) ierror = 0

end subroutine update_and_check_outputs

!-----------------------------------------------------------------

subroutine find_debug_list_p_dependent_equations

! debug_list_p is a list of unknowns (referenced by p) that have problems
! here we find whatever equations depend on this unknown, and put these into the arrays debug_list_m and debug_list_ns

use general_module
integer :: pp, mm, m, ns, um, uns, ppp, mindvm, mindvns
logical :: orphaned_unknown, completely_orphaned_unknown
double precision :: mindv

mindv = huge(1.d0)
write(*,'(a)') 'NOTE: writing details of problem unknown updates to fort.96'

do pp = 1, allocatable_size(debug_list_p)

  orphaned_unknown = .true.
  completely_orphaned_unknown = .true.
  um = unknown_var_from_pp(debug_list_p(pp))
  if (um == 0) call error_stop("problem finding unknown variable number in subroutine find_debug_list_p_dependent_equations")
  uns = debug_list_p(pp) - var(um)%funk(1)%pp(1) + 1

  write(96,'(a,i8,a,i8,a)') 'The unknown '//trim(var(um)%name)//' with p = ',debug_list_p(pp),' and '// &
    trim(ijkstring(var(um)%centring))//' = ',ijkvar(um,uns),' is related to the equations:'

  do mm = 1, allocatable_size(var_list(var_list_number_equation)%list)
    m = var_list(var_list_number_equation)%list(mm)
    do ns = 1, ubound(var(m)%funk,1)

      ppp = location_in_list_dummy(array=var(m)%funk(ns)%pp(1:var(m)%funk(ns)%ndv),element=debug_list_p(pp))

      if (ppp /= 0) then
        write(96,'(a,i3,a,i8,a,i8,a,g12.5)') '  depends '//trim(var(m)%name)//': m = ',m,': ns = ',ns,': '// &
          trim(ijkstring(var(m)%centring))//' = ',ijkvar(m,ns),': dv = ',var(m)%funk(ns)%dv(ppp)
        completely_orphaned_unknown = .false.
      end if

      if (trim(var(m)%centring) == trim(var(um)%centring)) then
        if (ijkvar(m,ns) == ijkvar(um,uns)) then
          write(96,'(a,i3,a,i8,a,i8)') '  colocated '//trim(var(m)%name)//': m = ',m,': ns = ',ns,': '// &
            trim(ijkstring(var(m)%centring))//' = ',ijkvar(m,ns)
          if (ppp /= 0) then
            orphaned_unknown = .false.
            if (var(m)%funk(ns)%dv(ppp) < mindv) then
              mindv = var(m)%funk(ns)%dv(ppp)
              mindvm = um
              mindvns = uns
            end if
          end if
        end if
      end if

    end do
  end do

  if (orphaned_unknown) write(96,*) 'ORPHANED UNKNOWN'
  if (completely_orphaned_unknown) write(96,*) 'COMPLETELY ORPHANED UNKNOWN'

end do

write(96,'(a,g15.8,3(a,i8))') 'minimum colocated dv = ',mindv,': at '//trim(ijkstring(var(mindvm)%centring))//' = ', &
  ijkvar(mindvm,mindvns),': mindvm = ',mindvm,': mindvns = ',mindvns

end subroutine find_debug_list_p_dependent_equations

!-----------------------------------------------------------------

subroutine list_all_equations

! list all equations with their derivatives

use general_module
integer :: ppp, mm, m, ns, um, uns
character(len=2000) :: textline

write(*,'(a)') 'NOTE: writing details of all equations to fort.97'

do mm = 1, allocatable_size(var_list(var_list_number_equation)%list)
  m = var_list(var_list_number_equation)%list(mm)
  do ns = 1, ubound(var(m)%funk,1)

    write(textline,'(a,i8,a,g12.5,a)') trim(var(m)%name)//':'//trim(ijkstring(var(m)%centring))//'=',ijkvar(m,ns),':v=', &
      var(m)%funk(ns)%v,': depends upon'

    do ppp = 1, var(m)%funk(ns)%ndv

      um = unknown_var_from_pp(var(m)%funk(ns)%pp(ppp))
      if (um == 0) call error_stop("problem finding unknown variable number in subroutine list_all_equations")
      uns = var(m)%funk(ns)%pp(ppp) - var(um)%funk(1)%pp(1) + 1
      write(textline,'(a,i8,a,i8,a,g12.5)') trim(textline)//': '//trim(var(um)%name)//':p = ',var(m)%funk(ns)%pp(ppp), &
        ':'//trim(ijkstring(var(um)%centring))//'=',ijkvar(um,uns),':dv=',var(m)%funk(ns)%dv(ppp)

    end do
    write(97,'(a)') trim(textline)

  end do
end do

end subroutine list_all_equations

!-----------------------------------------------------------------

subroutine check_linear_solution_accuracy

! check on linear solution accuracy
! manage_funk_dv_memory must be off as we are using equation derivative variables after the linear system has been inverted

use general_module
integer :: n, m, ns, p
double precision :: lhscheck, newtrescheck, sumcheck, sumrmscheck, newtrescheckold, summaxcheck
logical, parameter :: solution_accuracy_file = .false. ! write details of individual equations to fort.94

if (solution_accuracy_file) write(*,'(a)') 'NOTE: writing details of matrix accuracy to fort.94'
newtrescheck = 0.d0
sumrmscheck = 0.d0
summaxcheck = 0.d0
do n = 1, allocatable_size(var_list(var_list_number_equation)%list)
  m = var_list(var_list_number_equation)%list(n)
  do ns = 1, ubound(var(m)%funk,1)
    lhscheck = 0.d0
    sumcheck = 0.d0
    if (solution_accuracy_file) write(94,'(a,i8)') 'equation '//trim(var(m)%name)//' at '//trim(ijkstring(var(m)%centring))// &
      ' = ',ijkvar(m,ns)
    do p = 1, var(m)%funk(ns)%ndv
      lhscheck = lhscheck + var(m)%funk(ns)%dv(p)*delphi(var(m)%funk(ns)%pp(p))
      if (solution_accuracy_file) write(94,'(a,i8,3(a,g12.5))') '  p = ',p,': var(m)%funk(ns)%dv(p) = ',var(m)%funk(ns)%dv(p), &
        ': delphi(var(m)%funk(ns)%pp(p)) = ',delphi(var(m)%funk(ns)%pp(p)),': lhscheck_part = ', &
        var(m)%funk(ns)%dv(p)*delphi(var(m)%funk(ns)%pp(p))
    end do
    sumcheck = lhscheck + var(m)%funk(ns)%v
    if (solution_accuracy_file) write(94,'(3(a,g12.5))') '  lhscheck = ',lhscheck,': rhs = ',-var(m)%funk(ns)%v,': sumcheck = ', &
      sumcheck
    newtrescheck = newtrescheck + lhscheck**2
    sumrmscheck = sumrmscheck + sumcheck**2
    summaxcheck = max(summaxcheck,abs(sumcheck))
  end do
end do
newtrescheck = sqrt(newtrescheck/dble(max(ptotal,1)))
sumrmscheck = sqrt(sumrmscheck/dble(max(ptotal,1)))
write(*,'(3(a,g10.3))') 'INFO: newtrescheck from LHS = ',newtrescheck,': sumrmscheck (should be zero) = ',sumrmscheck, &
  ': summaxcheck (should be zero) = ',summaxcheck
if (solution_accuracy_file) write(94,'(3(a,g10.3))') 'INFO: newtrescheck from LHS = ',newtrescheck, &
  ': sumrmscheck (should be zero) = ',sumrmscheck,': summaxcheck (should be zero) = ',summaxcheck
if (convergence_details_file) write(fconverge,'(3(a,g10.3))') 'INFO: newtrescheck from LHS = ',newtrescheck, &
  ': sumrmscheck (should be zero) = ',sumrmscheck,': summaxcheck (should be zero) = ',summaxcheck

if (.false.) then
  newtrescheck = 0.d0
  do n = 1, allocatable_size(var_list(var_list_number_equation)%list)
    m = var_list(var_list_number_equation)%list(n)
    do ns = 1, ubound(var(m)%funk,1)
      newtrescheck = newtrescheck + var(m)%funk(ns)%v**2
    end do
  end do
  newtrescheck = sqrt(newtrescheck/dble(max(ptotal,1)))
  write(*,'(a,g10.3)') 'INFO: newtrescheck from RHS = ',newtrescheck
  if (convergence_details_file) write(fconverge,'(a,g16.9)') 'INFO: newtrescheck from RHS = ',newtrescheck
  newtrescheckold = newtrescheck
end if

end subroutine check_linear_solution_accuracy

!-----------------------------------------------------------------

subroutine setup_solver

! run through requested solver options, including user set ones (from input files)
! NB, as a general rule, options do not have underscores in them
! ref: solver options

use general_module
use equation_module
use intel_pardiso_module
use pardiso_module
use hsl_ma28d_module
use suitesparse_module
integer :: n
character(len=100) :: option_name
logical :: error

!do n = allocatable_character_size(solver_options), 1, -1
do n = 1, allocatable_character_size(solver_options) ! precedence is now as read, that is, options will be applied in the order that they are written
  option_name = extract_option_name(solver_options(n),error)
  if (error) then
    write(*,'(a)') "WARNING: could not determine what the desired solver option is from the following: "//trim(solver_options(n))
  else if (trim(option_name) == "linearsolver") then
! solver_method
    linear_solver = trim(extract_option_string(solver_options(n),error))
    if (error) call error_stop("could not determine the required linearsolver from the solver option "//trim(solver_options(n)))
    if (trim(linear_solver(1:11)) == "suitesparse") linear_solver = "suitesparse" ! v0.50 standardise linear solver name here
    write(*,'(a)') 'INFO: setting linearsolver = '//trim(linear_solver)
  else if (trim(option_name) == "backstepping") then
! backstepping
    backstepping = extract_option_logical(solver_options(n),error)
    if (error) call error_stop("could not determine backstepping from the solver option "// &
      trim(solver_options(n)))
    write(*,'(a,l1)') 'INFO: setting solver backstepping = ',backstepping
  else if (trim(option_name) == "stickylambda") then
! stickylambda
    sticky_lambda = extract_option_logical(solver_options(n),error)
    if (error) call error_stop("could not determine stickylambda from the solver option "// &
      trim(solver_options(n)))
    write(*,'(a,l1)') 'INFO: setting solver sticky_lambda = ',sticky_lambda
  else if (trim(option_name) == "stickylambdaincrease") then
! stickylambdaincrease
    sticky_lambda_increase = extract_option_integer(solver_options(n),error)
    if (error) then
      call error_stop("could not determine stickylambdaincrease from the solver option "//trim(solver_options(n)))
    else if (sticky_lambda_increase <= 1) then
      call error_stop("requested solver option stickylambdaincrease should be greater than one")
    end if
    write(*,'(a,i1)') 'INFO: setting solver stickylambdaincrease = ',sticky_lambda_increase
  else if (trim(option_name) == "lambdalimitfalseroot") then
! lambda_limit_false_root
    lambda_limit_false_root = extract_option_logical(solver_options(n),error)
    if (error) call error_stop("could not determine lambdalimitfalseroot from the solver option "// &
      trim(solver_options(n)))
    write(*,'(a,l1)') 'INFO: setting solver lambdalimitfalseroot = ',lambda_limit_false_root
  else if (trim(option_name) == "lambdalimitfalserootfactor") then
! lambda_limit_false_root_factor
    lambda_limit_false_root_factor = extract_option_double_precision(solver_options(n),error)
    if (error) then
      call error_stop("could not determine lambdalimitfalserootfactor from the solver option "// &
        trim(solver_options(n)))
    else if (lambda_limit_false_root_factor <= 0.d0) then
      call error_stop("requested solver lambdalimitfalserootfactor should be greater than zero")
    end if
    write(*,'(a,g10.3)') 'INFO: setting solver lambdalimitfalserootfactor = ',lambda_limit_false_root_factor
  else if (trim(option_name) == "lambdamin") then
! lambdamin
    lambdamin = extract_option_double_precision(solver_options(n),error)
    if (error) then
      call error_stop("could not determine lambdamin from the solver option "// &
        trim(solver_options(n)))
    else if (lambdamin <= 0.d0) then
      call error_stop("requested solver lambdamin should be greater than zero")
    end if
    write(*,'(a,g10.3)') 'INFO: setting solver lambdamin = ',lambdamin
  else if (trim(option_name) == "weightlargeequationerrors") then
! weight_large_equation_errors
    weight_large_equation_errors = extract_option_logical(solver_options(n),error)
    if (error) call error_stop("could not determine weightlargeequationerrors from the solver option "// &
      trim(solver_options(n)))
    write(*,'(a,l1)') 'INFO: setting solver weightlargeequationerrors = ',weight_large_equation_errors
  else if (trim(option_name) == "weightlargeequationerrorsfactor") then
! weight_large_equation_errors_factor
    weight_large_equation_errors_factor = extract_option_double_precision(solver_options(n),error)
    if (error) then
      call error_stop("could not determine weightlargeequationerrorsfactor from the solver option "// &
        trim(solver_options(n)))
    else if (weight_large_equation_errors_factor <= 0.d0) then
      call error_stop("requested solver weightlargeequationerrorsfactor should be greater than zero")
    end if
    write(*,'(a,g10.3)') 'INFO: setting solver weightlargeequationerrorsfactor = ',weight_large_equation_errors_factor
  else
    call error_stop(trim(option_name)//" is not a (valid) solver option that can be set from the input file")
  end if
end do

!------------------------------------------
! if the linear_solver has not been chosen then do it now
! also check on choice

if (trim(linear_solver) == "default") then
  if (intel_pardiso_linear_solver_check()) then
    linear_solver = "intelpardiso"
  else if (suitesparse_linear_solver_check()) then
    linear_solver = "suitesparse"
  else if (pardiso_linear_solver_check()) then
    linear_solver = "pardiso"
  else if (hsl_ma28_linear_solver_check()) then
    linear_solver = "hslma28"
  else
    call error_stop('no linear solver available: you could try making the GPL licensed suitesparse '// &
      'solver: instructions are within the src/contributed/suitesparse directory')
  end if
  write(*,'(a)') 'INFO: choosing '//trim(linear_solver)//' linear solver'
else if (.not.(trim(linear_solver) == "intelpardiso".or.trim(linear_solver) == "intelpardisoooc".or. &
  trim(linear_solver) == "suitesparse".or.trim(linear_solver) == "hslma28".or.trim(linear_solver) == "pardiso".or. &
  trim(linear_solver) == "pardisoiterative".or.trim(linear_solver) == "iterative".or.trim(linear_solver) == "none")) then
  call error_stop('unknown linear solver specified: '//trim(linear_solver))
end if

if (trim(linear_solver) == "intelpardisoooc".and.nthreads > 1) &
  call error_stop('intelpardisoooc linear solver can only be used in a serial (non-omp) calculation')

end subroutine setup_solver

!-----------------------------------------------------------------

end module solver_module
!-----------------------------------------------------------------
