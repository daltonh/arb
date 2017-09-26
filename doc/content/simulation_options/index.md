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

##Kernel options

There are many options that can be used to change the kernels used. For
example

specifies that when averaging/differentiating quantities to/at faces,
ensure that a second order polynomial would be reproduced precisely.

