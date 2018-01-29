---
title: 'simulation options'
author: Dalton Harvie
date: 6/12/16
---



<!-- add ##Gmsh Regions -->

#Simulation Options

There are three categories of options that can be set within an arb file, with the names corresponding to the fortran modules in which the options are stored.  The three types of options/modules are kernel, solver and general, corresponding to the modules [kernel_module.f90], [solver_module.f90] and [general_module.f90].

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
-  options may be integers, character strings, floats (double precision) or logicals.  For reference these are interpreted within the fortran using (eg) `function extract_option_string` within [general_module.f90].  Strings may be quoted or unquoted.
-  the name used in the arb file is the same as the fortran name, except with the underscore characters completely removed (ie, option name written as a single concatenated word).

For example, the fortran solver option string variable `linear_solver` is set to the value `pardiso` using the arb syntax
```arb
SOLVER_OPTIONS linearsolver=pardiso
```
Multiple options can be included on the one line, as in
```arb
KERNEL_OPTIONS polynomialorder=2,polynomialaverageorder=2,automaximumseparation=.false.
```
For logical variables the specification of `.true.` can be ommited, while to negate a logical option `.false.` must be used, as in
```arb
GENERAL_OPTIONS automaximumseparation # turns on automaximumseparation
GENERAL_OPTIONS noautomaximumseparation # turns off automaximumseparation
GENERAL_OPTIONS automaximumseparation=.true. # equivalently turns on automaximumseparation
GENERAL_OPTIONS automaximumseparation=.false. # equivalently turns off automaximumseparation
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

- `timestep`: timestep index, with initial value being initial timestep.  This variable is mainly useful for restarts when you overwrite the `timestep` value read in from the mesh files with another starting value.  Setting this variable will overwrite the initial (default = 0) timestep or read-in restart timestep.
- `timestepmax`: maximum number of timesteps performed in total, or more accurately, the timestep after which the simulation stops (in the case of restarts and the initial newtstep isn't 0 - default is huge)
- `timestepmin`: minimum number of timesteps performed in total, or more accurately, the timestep that must be completed before the simulation stops (default 0)
- `timestepadditional`: minimum number of timesteps that must be completed during current run, useful when restarting a simulation (default 0)
- `timestepout`: maximum number of timesteps between output, with zero indicating no output (default 0).  Output can also be triggered by `CONDITION` variables (especially when using dynamic timestep sizing).
- `timesteprewind`: maximum number of timesteps to remember for timestep rewinding purposes with 0 turning off timestep rewinding.  This option can either be set using a logical format or integer format, as in
```arb
GENERAL_OPTIONS timestepwind # turns on timestep rewinding, and sets number of timesteprewind steps to 1
GENERAL_OPTIONS notimestepwind # turns off timestep rewinding, and sets number of timesteprewind steps to 0
GENERAL_OPTIONS timestepwind=3 # turns on timestep rewinding, and sets number of timesteprewind steps to 3
GENERAL_OPTIONS timestepwind=0 # turns off timestep rewinding, and sets number of timesteprewind steps to 0
```

###Newton (newtstep) loop options


##Solver options

```arb
#-------------------------------------------------------------------
# system constants
SOLVER_OPTIONS linearsolver=intelpardisosafer
SOLVER_OPTIONS linearsolver=intelparadiso
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

There are many options that can be used to change the kernels used. For
example

specifies that when averaging/differentiating quantities to/at faces,
ensure that a second order polynomial would be reproduced precisely.

