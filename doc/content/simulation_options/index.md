---
title: 'simulation options'
author: Dalton Harvie
date: 6/12/16
---



<!-- add ##Gmsh Regions -->

#Simulation Options

There are three categories of options that can be set within an arb file, with the names corresponding to the fortran modules in which the options are stored.  The three types of options/modules are kernel, solver and general, corresponding to the modules [kernel_module.f90], [solver_module.f90] and [general_module.f90].

The fortran variables that store the individual options, along with an explanation of their use is generally given near the top of each respective module.  For example, near the top of [solver_module.f90] (search for ref: solver options) is a list of the solver options, including the following:
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

More details of specific options are given in this chapter.

##General options



##Simulation options

-   : choose between the two types of simulation.

-   : end input

-   : ignore the text between these statements.

-   : add the following options to every subsequent variable, until
    cleared again using a blank statement. When listed in order, default
    options precede a variable’s individually specified options - hence,
    in the case of conflicting option statements, individual options
    take precedence over default options (ie, the individual options
    have a higher priority).

-   : are the same as , except that they follow a variable’s
    individually specified options, and so in the case of conflicting
    option statements, take precedence over the individual options (ie,
    the override options have a higher priority).

-   : choose the type of linear solver to use.

##Kernel options

```arb
#-------------------------------------------------------------------
# system constants
SOLVER_OPTIONS linearsolver=intelpardisosafer

SOLVER_OPTIONS linearsolver=intelparadiso
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

