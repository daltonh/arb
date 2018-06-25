---
title: 'simulation options'
author: Dalton Harvie
date: 6/12/16
---



<!-- add ##Gmsh Regions -->

#Simulation Options

There are three categories of simulation options that can be set within an arb file, with the names corresponding to the fortran modules in which the options are stored.  The three types of simulation options/modules are kernel, solver and general, corresponding to the modules [kernel_module.f90], [solver_module.f90] and [general_module.f90].  (There are also `DEFAULT` and `OVERRIDE` options available for bulk-setting the options of regions and variables.  These are covered at the end of this chapter right now).

The fortran variables that store the individual options, along with a brief explanation of their use is generally given near the top of each respective module.  For example, near the top of [solver_module.f90] (search for ref: solver options) is a list of the solver options, including the following:
```fortran
character(len=100) :: linear_solver = "default" ! (default, userable) type of linear solver used: default will choose optimal solver available, starting with all of the direct solvers.  Specific options are: none, direct (choosing best available direct method), iterative (choosing best available iterative method), intelpardiso, intelpardisoooc, intelpardisosafer, suitesparse, hslma28, pardiso, sparse, mgmres, multigrid, bicg, bicgstab, descent, doglegdescent, flexible

! backstepping parameters for the newton-raphson method
! recommended defaults for each parameter are in braces
logical :: backstepping = .true. ! (.true., userable) whether to use backstepping or not - no reason not to
double precision, parameter :: alf = 1.d-5 ! (1.d-5) small factor that ensures that newtres is decreasing by a multiple of the initial rate of decrease - everyone suggests 1.d-4, but a bit smaller seems to work better for some problems
double precision :: lambdamin = 1.d-10 ! (1.d-10, userable) minimum absolute backstepping lambda allowed - this can be set very small if lambda_limit_false_root is on
integer :: sticky_lambda_increase = 1 ! (1, userable) multiply previous lambda by 2^sticky_lambda_increase at each new newton iteration
```
Regarding the syntax used in this list:

-  the braces within the comment line following each variable indicate what the default value for the option should be
-  only options that also have the string `userable` within these braces can be set from the arb file (the others, such as `alf` in the above, have to be set within the source code if changes are required)
-  option values may be integers, character strings, floats (double precision) or logicals.  For reference these are interpreted within the fortran using (eg) `function extract_option_string` within [general_module.f90].  Strings should be unquoted (for now - at this stage there is no need to quote any of the possible strings).  Some logicals only accept the `option = .true.` or `option = .false.` syntax, others the `option` versus `nooption` syntax, and others both.  Generally I am moving towards both option types accepting either format, with a slight preference at this stage for the `option`/`nooption` syntax (TODO kernel and solver options, general and variable options compatible with this).
-  the name used in the arb file is the same as the fortran name, except with the underscore characters completely removed (ie, option name written as a single concatenated word).

For example, the fortran solver option string variable `linear_solver` is set to the value `pardiso` using the arb syntax
```arb
SOLVER_OPTIONS linearsolver=pardiso
```
Multiple options can be included on the one line, as in
```arb
KERNEL_OPTIONS polynomialorder=2,polynomialaverageorder=2,automaximumseparation=.false.
```
As mentioned above, for logical variables the `option`/`nooption` syntax is preferable (which is equivalent to omitting `.true.`), as in
```arb
GENERAL_OPTIONS automaximumseparation # turns on automaximumseparation
GENERAL_OPTIONS noautomaximumseparation # turns off automaximumseparation
GENERAL_OPTIONS automaximumseparation=.true. # equivalently turns on automaximumseparation, possibly legacy
GENERAL_OPTIONS automaximumseparation=.false. # equivalently turns off automaximumseparation, possibly legacy
```

More details of specific options are given in this chapter.

##General options

###Transient versus steady-state simulation type

arb can be run either in steady-state mode, or in transient mode.  In steady-state mode a backstepped Newton-Raphson procedure is used to advance the solution towards convergence, and the steps used by this `newton` procedure as referred to as `newtsteps`.  In transient mode the `newtsteps` loop is nested with a transient `timestep` loop.  General options are available to choose the simulation type, and to control the progress and running of both of the `timestep` and `newtstep` loops.

Note that the `timestep` loop need not be for simulating a physically transient problem, as there is no particular need to associate each `timestep` with a physical time.  A transient simulation can just as well be used for running multiple steady-state simulations in series, useful (for example) for performing a parametric study whereby a certain parameter is changed as a function of the `timestep` (note that `arb_batcher` is also available for performing parametric studies).

To choose between the simulation types set the `transientsimulation` option using
```arb
GENERAL_OPTIONS transientsimulation=.true. # specifies that the timestep loop will be used
GENERAL_OPTIONS transientsimulation=.false. # specifies that the timestep loop will not be used, ie, a steady-state simulation (default)
```
The default simulation type is steady-state, unless any transient variables or dynamic regions have been defined (in which case the default simulation type becomes transient).  This variable can be referenced by `<transientsimulation>` (0 or 1) in any expressions.

###Newtient simulations

During the newton loop variables are updated and the jacobian matrix calculated at the end of the loop, at the same time as the newton loop residual (`newtres`) is calculated and convergence criteria checked.  arb has the ability to also update variables again once the newton loop has converged but before the next `newtstep` is undertaken - if this is required then the simulation is referred to as a `newtient` simulation, and the variables/regions that are to be updated in this fashion are known as `NEWTIENT` variables/regions.  The purpose of `NEWTIENT` variables/regions is to aid convergence for some types of equations (however they are rarely employed or needed for well-defined problems).

The commands to specify a newtient simulation are
```arb
GENERAL_OPTIONS newtientsimulation=.true. # specifies that newtient variables are to be updated after the newton loop has converged
GENERAL_OPTIONS newtientsimulation=.false. # no newtient variables are to be updated (default)
```
Again, the default is false unless any `NEWTIENT` variables or regions are defined.

###Transient (timestep) loop options

These options are concerned with the outer timestep loop:

- `timestep`: timestep index, with initial value being initial timestep.  This variable is mainly useful for restarts when you overwrite the `timestep` value read in from the mesh files with another starting value.  Setting this variable will overwrite the initial (default = 0) timestep or read-in restart timestep.
- `timestepmax`: maximum number of timesteps performed in total, or more accurately, the timestep after which the simulation stops (in the case of restarts and the initial newtstep isn't 0 - default is huge)
- `timestepmin`: minimum number of timesteps performed in total, or more accurately, the timestep that must be completed before the simulation stops (default 0)
- `timestepadditional`: minimum number of timesteps that must be completed during current run, useful when restarting a simulation (default 0)
- `timestepout`: maximum number of timesteps between output, with zero indicating no output (default 0).  Output can also be triggered by `CONDITION` variables (especially when using dynamic timestep sizing).

###Timestep rewinding

'Timestep rewinding' is a process whereby the simulation stops, rewinds and restarts from a previous timestep.  `CONDITION` variables are used to trigger a timestep rewind, typically when there is some failure in convergence or when some accuracy criterion is not met.  Upon rewind, one or more variables will be altered such that when the timesteps are redone the subsequent timesteps will converge or be more accurate.

####Timestep rewinding general options

The following `GENERAL_OPTIONS` are used to control the timestep rewinding process:

- `timesteprewind`: sets the maximum number of timesteps to remember for timestep rewinding purposes with 0 turning off timestep rewinding.  This option can either be set using a logical format or integer format, as in
```arb
GENERAL_OPTIONS timestepwind # turns on timestep rewinding, and sets number of timesteprewind steps to 1
GENERAL_OPTIONS notimestepwind # turns off timestep rewinding, and sets number of timesteprewind steps to 0
GENERAL_OPTIONS timestepwind=3 # turns on timestep rewinding, and sets number of timesteprewind steps to 3
GENERAL_OPTIONS timestepwind=0 # turns off timestep rewinding, and sets number of timesteprewind steps to 0
```
The higher `timesteprewind` is set, the more data points will be available for a rewind, and the more memory/computational expense required (although in practice this probably isn't a big issue given the other memory/computational requirements).  The default is for `notimesteprewind` (ie, off).
- `timesteprewindmax`: This is the maximum number of consecutive timestep rewinds that can be done.  More specifically, timestep rewinding will be possible until the number of timestep rewinds that have been done (recorded in `<timesteprewindsdone>`) equals `timesteprewindmax`.  The purpose of this variable is to limit the number of timestep rewinds that can be done in the situation where rewinding does not seem to improve convergence performance.  Note also that 'consecutive' here has a special meaning, related to the definition of variable `<timesteprewindsdone>`.  Specifically, only after `timesteprewind` successful timesteps have been achieved will the `<timesteprewindsdone>` be reset to zero - see definition of variable below.  The default is 10.
- `newtstepmaxiftimesteprewind`: this sets the maximum number of newtsteps that can be conducted if timestep rewinding is on, AND that the maximum number of timestep rewinds have not already been done.  The purpose of this variable is to limit the number of newtsteps taking place if the computational time would be better spent rewinding and starting with a (eg) smaller timestep.  Note that if the number of `newtsteps` is limited by this option, it does not automatically trigger a timestep rewind.  Instead, an accuracy `timesteprewind` `CONDITION` should also be included to detect when the newton loop hasn't converged.  See examples below.  The default is huge.

####Timestep rewinding variable and region options

In addition to the `GENERAL_OPTIONS` listed above, certain variable and region options should be used to control the timestep rewinding process.  Note that these variables have no effect if the general option `timesteprewind` isn't set (in practice this means that the general option `timesteprewind` can be turned on and off without changing these variable/region options).

- `timesteprewind`: this logical controls whether timestep rewinding data is stored and rewound for this particular variable.  Note that the number of timestep data points stored is the same for all variables that have this option (and is controlled by the general option `timesteprewind`).  By default this logical is on for all `UNKNOWN` and `TRANSIENT` variables and dynamic regions, and off for everything else.  It cannot be applied to any `LOCAL` variables as these variables are not stored.  Note that it can also be applied to constants, as in
```arb
NONE_CONSTANT <dt> 1.d-1 timesteprewind,timesteprewindmultiplier=0.5d0
```
For simple transient simulations this logical probably doesn't need to be set for individual variables/regions, with the defaults just working.  However the defaults won't work and more thought is needed when (eg) the value of a derived or output is carried over the end of the timestep.  For example:
```arb
NONE_TRANSIENT <dt> "1.d0" "<dt new>" timesteprewindmultiplier=0.5d0
# some equations etc
NONE_DERIVED <dt new> "<dt>*<my fancy expression>" timesteprewind
```
Here `<dt new>` is used to initialise `<dt>` in each timestep, so this variable needs to be included in the timestep rewinding process (otherwise `<dt>` would be initialised to the faulty value upon rewind, rather than the value it had when entering the rewound timestep).  Similarly, this situation could arise when using an `OUTPUT` variable to store when output was written:
```arb
NONE_OUTPUT <t_output_last> "<t>" timesteprewind # this variable will be written when output is written, so will need rewinding
```
If you want to include all `DERIVED` and `OUTPUT` variables and regions within the timestep rewinding process without having to work out which are required you can just include the default options specific to these types of variables near the top of the arb file, as in
```arb
DEFAULT_DERIVED_OPTIONS timesteprewind # an option specific to any derived variable or region
DEFAULT_OUTPUT_OPTIONS timesteprewind # an option specific to any derived variable or region
```
This will work but could incur a significant performance and memory penalty, so use it to debug, and then find the specific variables required.
- `timesteprewindmultiplier = N` or `timesteprewindmultiplier = <variable>`: This variable controls how any variables are changed upon a timestep rewind.  In the first form `N` is a float, while in the second `<variable>` must refer to a `NONE` centred variable (may be a `LOCAL` or a stored variable).  If the variable form is used the it's value is evaluated once all of the data has been rewound to the start of the restarting timestep.  All multiplier variables are calculated before any are applied.  Typically the multiplier is applied to make the physical timestep duration smaller, as in
```arb
NONE_TRANSIENT <dt> "1.d0" "1.2d0*<dt[r=1]>" timesteprewindmultiplier=0.5d0
```

####Timestep rewinding system variables:

There are a number of system variables that are concerned with the timestep rewinding process:

- `<timesteprewindmax>`: this is just the integer value of `timesteprewind`, being the maximum number of timestep rewind data points that are stored.
- `<timesteprewindsdone>`: records the number of timestep rewinds that have been done without `<timesteprewindstored>` reaching the `timesteprewind` integer (=`<timesteprewindmax>`).  This can be used to make condition or rewind multiplier a function of how many rewinds have been done.
- `<timesteprewindstored>`: records how many timestep rewind data points are currently stored.  After a timestep has been rewound this will equal 1, and will increase for every sequential successful timestep until it reaches `<timesteprewindmax>`.
- `<newtstepfailed>`: is 0 unless the previous newton loop had some type of numerical failure, in which case = 1.
- `<newtstepconverged>`: is 1 if the last newton loop has converged, 0 otherwise.  This should take into account user newton loop `CONDITION`s as well as comparing `<newtres>-<newtrestol>`.

Other relevant system variables include `<newtstep>`, `<newtres>`, `<newtrestol>`.

####Timestep rewinding `CONDITION` examples

When timestep rewinding occurs is controlled by `CONDITION` variables that have the option `timesteprewindcondition`.  As with the other `CONDITION` variables, if any `timesteprewindcondition` variable becomes positive then a timesteprewind is triggered.  The use of these conditions is illustrated by:

- Failure of the linear solver:  The variable `<newtstepfailed>` has the value 1 when the last attempted newtstep iteration produced a linear solver or other numerical failure (Nan, Ifty etc), or 0 otherwise.  Typically you want the timestep to rewind in such an instance, accomplished by:
```arb
CONDITION <newtstep timesteprewind failure condition> "<newtstepfailed>" timesteprewindcondition
```
- Newton loop takes more iterations to convergence than ideal:  For this you can compare the number of newtsteps to some constant, as in
```arb
NONE_CONSTANT <newtstep accept> 2.d0
CONDITION <newtstep timesteprewind accept condition> "<newtstep>-<newtstep accept>" stepoutput,timesteprewindcondition
```
Note that using the above condition, the newton loop will first converge, and then this condition will trigger a timestep rewind.  This only makes sense if you regard the number of newtsteps required to achieve convergence as some type of accuracy quantifier, (which is probably reasonable).  The alternative is next:
- Newton loop is taking too long, kill it and rewind:  For this you firstly need to limit the number of newton steps taken using the global variable `newtstepmaxiftimesteprewind`.  Then you need to check on the accuracy of convergence once the newton loop has been (prematurely) terminated:
```arb
GENERAL_OPTIONS newtstepmaxiftimesteprewind=10
CONDITION <newtstep timesteprewind convergence condition> "<newtres>-<newtrestol>" stepoutput,timesteprewindcondition
```

###Newton (newtstep) loop options

```arb
! newtstep variables (newton loop)
integer :: newtstep = 0 ! (0, userable) newtstep index (with initial value being initial newtstep, used for restarts)
integer :: newtstepmax = 1000 ! (1000, userable) maximum number of steps performed by newton proceedure
integer :: newtstepmin = 1 ! (1, userable) minimum number of steps performed by newton proceedure
integer :: newtstepout = 0 ! (0, userable) maximum number of newtsteps between output, with zero indicating no output
integer :: newtsteprewind = 0 ! NOT IMPLEMENTED YET - TRIGGERS ERROR (0, userable) maximum number of newtsteps to remember for newtstep rewinding purposes - 0 turns off newtstep rewinding
integer :: newtstepdebugout = 990 ! (990, userable) after this many newtsteps newtstepout is set to 1 to produce debugging output
double precision :: newtrestol = 1.d-10 ! (1.d-10, userable) tolerance that indicates convergence of the newton proceedure
integer :: newtstepmaxiftimesteprewind = huge(1) ! (huge, userable) maximum number of steps performed by newton proceedure if timesteprewind is on and timesteprewindsdone < timesteprewindmax (ie, this is a way to jump out of the newton loop if it isn't converging)
```

###Iterative solver (iterstep) loop options

```arb
! iterstep variables (iterative solver loop, if being used)
integer :: iterstepmax = 1000000 ! (1000000, userable) maximum number of steps performed by linear iteration solver
integer :: iterstepcheck = 100 ! (100, userable) how often linear iteration solver checks for kill file and outputs to screen residuals etc
double precision :: iterrestol = 1.d-11 ! (1.d-11, userable) tolerance that indicates convergence of the linear iteration solver
double precision :: iterresreltol = 0.d0 ! (0.d0, userable) tolerance that indicates convergence of the linear iteration solver, relative to the starting newtres - actual tolerance uses max(iterrestol,iterresreltol*newtres), so if iterresreltol is set to zero then it is inactive and only iterrestol is used
```

###File input and output options

```arb
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
```

##Solver options

```arb
#-------------------------------------------------------------------
# system constants
SOLVER_OPTIONS linearsolver=intelpardisosafer
SOLVER_OPTIONS linearsolver=intelpardiso
```

##Kernel options

```arb
#-------------------------------------------------------------------
# system constants
KERNEL_OPTIONS polynomialorder=3 # setting order of kernel function for face derivatives
KERNEL_OPTIONS polynomialorder=2,polynomialaverageorder=2 # setting order of kernel function for face derivatives
KERNEL_OPTIONS polynomialorder=2 # setting order of kernel function for face derivatives
KERNEL_OPTIONS polynomialaverageorder=2 # setting order of kernel function for face derivatives
KERNEL_OPTIONS automaximumseparation=.false.
KERNEL_OPTIONS automaximumseparation=.false.,minimumseparation=2
KERNEL_OPTIONS checkminw=.true.,minimumminw=0.5d0 # reducing the minw a bit which will decrease the size of the kernels
KERNEL_OPTIONS minimumseparation=2

# for structured meshes 
INCLUDE_TEMPLATE "kernel_presets"
INCLUDE "kernel_1st_order_compact"
##Kernel options
```

<!--There are many options that can be used to change the kernels used. For
example

specifies that when averaging/differentiating quantities to/at faces,
ensure that a second order polynomial would be reproduced precisely. -->

##Default and Override Options

Default or override options are used to add options to specific groups of variables or dynamic (check!) regions.  Default options precede other user set options, so will only take effect if a specific option hasn't been given for a specific variable/region.  Override options trial other user set options, so overwrite any specific user set options.  A default or override option keyword without any trailing options, or instead with the word `CANCEL` cancels their application.

In their simplest forms, this options apply to all variables and regions that are read in during the time that they have values.  So in the following example
```arb
DEFAULT_OPTIONS output
NONE_CONSTANT <a> 1.d0
# more code here
DEFAULT_OPTIONS
NONE_CONSTANT <b> 1.d0
DEFAULT_OPTIONS output
DEFAULT_OPTIONS timesteprewind # note change from v0.57 behaviour, whereby this would reset the default options, it now appends
NONE_CONSTANT <c> 1.d0
DEFAULT_OPTIONS CANCEL # equivalent to no trailing string
```
both `<a>` and `<c>` will have the option `output` set, while `<b>` will not.  `<c>` will additionally have the option `timesteprewind` set.  Note that for `<c>` there is no guarantee as to which of the `output` and `timesteprewind` options will come first (as hashes are used to store these within [setup_equations.pl]).

Options can also be applied to more specific groups of variables/regions, illustrated by:
```arb
OVERRIDE_NONE_DERIVED_REGION_OPTIONS output # only applied to none centred derived dynamic regions
OVERRIDE_CELL_OPTIONS output # applied to all cell centred variables and dynamic regions
OVERRIDE_LOCAL_VARIABLE_OPTIONS output # applied to all local variables
OVERRIDE_VARIABLE_OPTIONS output # applied to all variables, but not dynamic regions
```
Not all qualifiers need be listed for these option keywords, but if they are, they need to be in the order of centring (eg `NONE`), variable/region type (eg `UNKNOWN`) and then variable or region (eg `VARIABLE`).


