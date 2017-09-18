---
title: 'introduction'
author: Dalton Harvie
date: 6/12/16
---

# An introduction to arb

## What is arb?

arb solves arbitrary partial differential equations on unstructured meshes using (principally) the finite volume method.  The code is written in fortran, with some meta-programming done in perl with help from the symbolic algebra package maxima.

The primary strengths of arb are:

* All equations and variables are defined using 'maths-type' expressions, and so can be easily tailored to each application.  For example,
```arb
CELL_UNKNOWN <temperature> [K] "<P>*<V>/(<R>)" ON <allcells> # represents an unknown temperature field stored at cell centres, with units Kelvin, its initial expression in terms of other variables, and the region over which it is defined (being all cells within the simulation domain)
```
* All equations are solved simultaneously using a back-stepped Newton-Raphson method, so implicitly discretised equations can be solved efficiently; and
* The unstructured meshes over which the equations are solved can be componsed of all sorts of convex polygons/polyhedrons (0D-3D).

Compared to other partial differential equation solvers the disadvantages of arb are:

* There is no graphical user interface for problem setup, so the learning curve is steep; and
* There are some more efficient (either faster or less memory intensive) algorithms out there for specific problems.

## What is needed to run arb?

arb requires a UNIX type environment to run, and has been tested on both the Apple OsX and ubuntu linux platforms. Certain third party programs are used by arb:

-   A fortran compiler; the Intel compiler
    [ifort](http://software.intel.com/en-us/intel-compilers/ "intel-compilers")
    or GNU compiler [gfortran](http://gcc.gnu.org/wiki/GFortran);
-   The computer algebra system
    [maxima](http://maxima.sourceforge.net/ "Computer algebra system");
-   A sparse matrix linear solver:
    [UMFPACK](http://www.cise.ufl.edu/research/sparse/umfpack/),
    [pardiso](http://www.pardiso-project.org/) (both the native and
    ifort versions) or a [Harwell Subroutine
    Library](http://www.hsl.rl.ac.uk/) routine; and
-   The mesh generation and post-processing package
    [gmsh](http://geuz.org/gmsh/).

By combining gfortran with the UMFPACK sparse linear solver, arb can be run using freely available GPL licensed software.

Further details about how to install arb are given in [Installation].

## Licence

arb is released under the [GNU General Public License](http://www.gnu.org/licenses/gpl.html).  Full details of the licence are in the [arb licence file](<<<arbroot>>>/licence/arb_licence.txt).

