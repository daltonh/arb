%
%
%

# An introduction to arb

## What is arb?

arb solves arbitrary partial differential equations on unstructured meshes using the finite volume method.  The code is written in fortran95, with some meta-programming done in perl with help from maxima.

The primary strengths of arb are:

* All equations and variables are defined using 'maths-type' expressions written by the user, and hence can be tailored to each application, for example;
```arb
CELL_UNKNOWN <temperature> [K] "<P>*<V>/(<R>)" ON <all cells> # an unknown temperature field, with units Kelvin, its expression in terms of other variables, and the region (of in this case cells) over which it is defined
```
* All equations are solved simultaneously using a Newton-Raphson method, so implicitly discretised equations can be solved efficiently; and
* The unstructured mesh over which the equations are solved can be componsed of all sorts of convex polygons/polyhedrons.

The disadvantages of arb are:

* There is no graphical user interface for problem setup, so the learning curve is steeper; and
* There are some more efficient (either faster or less memory intensive) algorithms out there for specific problems.

## What is needed to run arb?

arb requires a UNIX type environment to run, and has been tested on both the Apple OsX and ubuntu linux platforms. Certain third party programs are used by arb:

* A fortran compiler; ifort and gfortran are supported;
* The computer algebra system [maxima](http://maxima.sourceforge.net);
* A sparse matrix linear solver: [UMFPACK](http://www.cise.ufl.edu/research/sparse/umfpack/), [pardiso](http://www.pardiso-project.org/), and (currently a single) Harwell Subroutine Library routines are supported; and
* The mesh generator [gmsh](http://geuz.org/gmsh/).

Further details about how to install arb are given in [Installation](../installation/index.html).

## Licence

arb is copyright Dalton Harvie (2009–2015), but released under the [GNU General Public License](http://www.gnu.org/licenses/gpl.html).  Full details of the licence are in the [arb licence file](../working_dir/licence/arb_licence.txt).

<!---
```bash
echo "some bash string";
```

$t=4$
-->

