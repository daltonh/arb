---
title: 'simulation options'
author: Dalton Harvie
date: 6/12/16
---







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

