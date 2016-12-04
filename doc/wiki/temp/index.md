%
%
%

# An introduction to arb

## What is arb?

arb solves arbitrary partial differential equations on unstructured meshes using (principally) the finite volume method.  The code is written in fortran, with some meta-programming done in perl with help from the symbolic algebra package maxima.

The primary strengths of arb are:

* All equations and variables are defined using 'maths-type' expressions, and so can be easily tailored to each application.  For example,
```{.arb hl='vim'}
CELL_UNKNOWN <temperature> [K] "<P>*<V>/(<R>)" ON <all cells> # represents an unknown temperature field, with units Kelvin, its initial expression in terms of other variables, and the region (of in this case cells) over which it is defined
```
* All equations are solved simultaneously using a back-stepped Newton-Raphson method, so implicitly discretised equations can be solved efficiently; and
* The unstructured meshes over which the equations are solved can be componsed of all sorts of convex polygons/polyhedrons (0D-3D).

```fortran
CELL_UNKNOWN <temperature> [K] "<P>*<V>/(<R>)" ON <all cells> # represents an unknown temperature field, with units Kelvin, its initial expression in terms of other variables, and the region (of in this case cells) over which it is defined
```
another thing
