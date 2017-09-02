---
title: 'installation'
author: Dalton Harvie
date: 6/12/16
---

Installation
============

The fast way
------------

Getting up and running is relatively straightforward, and primarily
consists of installing the software and packages on which arb depends.

### Install dependencies on Ubuntu

On recent versions of ubuntu (tested on 10.04, 12.04 and 14.04) you can
run the following [ubuntu install script](<<workingdir>>/misc/install_dependencies/install_dependencies_on_ubuntu.sh)
which will use apt-get to install the required packages. This doesn't
take long.

On ubuntu 8.04 some of the software versions in the standard repositories are 
too old to be installed via this method.

### Install dependencies on OsX

Installation on OsX is similarly straightforward, being based on
[macports](http://www.macports.org/) (which needs to be installed
first, along with Apple's Xcode developer package). However, macports generally compiles software, so it will take
some time to install all the dependencies from scratch (maybe an hour?).
The install script for OsX is the [macports install
script](<<workingdir>>/misc/install_dependencies/install_dependencies_on_osx_using_macports.sh).

### Run it

Once these dependencies are installed the following commands will
download an arb working directory, extract and unpack it, run the default tutorial 1 test simulation and visualise
the results using gmsh:

```sh
wget http://people.eng.unimelb.edu.au/daltonh/downloads/arb/code/latest.tar
tar -xf latest.tar
cd arb_*
./unpack
./arb
gmsh output/output.msh
```

### Installation Problems

If you run into problems, or want to customise your install then the next section may help, although some parts are out of date.


The slow way
------------

###  Maxima

Equation generation is performed using the Maxima Computer Algebra
system. It is released under the [GNU General Public License](http://www.gnu.org/licenses/gpl.html) and is available for free. To check
whether you have it installed already try typing

```sh
maxima
```

If the program is installed you will enter a symbolic maths environment. Then check that the command

```maxima
load(f90)
```

finds these libraries, and then quit using

```maxima
quit();
```

#### If you need to install it:

On ubuntu linux and the f90 package can be installed using

```sh
sudo apt-get install maxima maxima-share
```

A precompiled version of maxima for the mac is available from [sourceforge](http://sourceforge.net/projects/maxima/files/). This is
a fastest way of getting things going. Download the package and copy the
application Maxima.app to your directory as directed. To make it
available from the command line place the [maxima wrapper script](<<workingdir>>/misc/maxima_OsX/maxima) somewhere in your path
(for example in a bin directory).

On OsX can be installed using either or package managers. For fink
enable the unstable branch and use

    sudo fink install maxima

For macports replace with in the above. Compilation will take some time.

### A fortran compiler and the blas/lapack libraries

Two different compilers have been tested with arb: (Intel) and . The is
probably the faster of the two as currently also supports OMP execution.
It also includes the Intel Maths Kernel library which itself includes
the excellent Pardiso linear solver routines (see section
\[sec:pardiso\] below): however this compiler is not free except on
linux and even then, only under specific non-commercial circumstances.
The GNU compiler is an easier option to get going, and is freely
available on both the OsX and linux platforms. It does not include the
Pardiso routines but with the new interface to the UMFPACK routines this
isn’t a tremendous disadvantage (except for the OMP caveat).

Compiler choice is made automatically when the script is run (defaulting
to if it exists, otherwise using ). These defaults can be overwritten
with the options or .

The non-commercial download site for ifort on linux is . For OsX the
compiler must be bought. Make sure you install both the compiler and MKL
(Math Kernel Libraries). versions of 11.1.069 and newer (including
Composer XE) have been tested on both linux (v12 and v13) and OsX (v12
only).

On ubuntu linux and the lapack/blas libraries can be installed using

    sudo apt-get install gfortran liblapack-dev libblas-dev

version 4.2.4 on ubuntu 8.04 doesn’t seem to work on some computers
(internal compiler error) whereas version 4.4.3 on ubuntu 10.04 does.
The version on ubuntu 12.04 is fine too. On ubuntu 8.04 you could try
downloading a newer binary version of but I haven’t tried this.

The easiest way to install an up-to-date version of is via a precompiled
package. One that worked for me is found - currently (23/2/11) this is
version 4.6. Alternatively this and this gives information about other
precompiled versions. Check that once installed the executable is in
your path - that is, typing at the command line should find the
compiler. Version 4.5 of should also work fine.

can also be installed by the package managers but there are some issues
with this. is included as part of fink’s gcc packages (for example ) but
it will need to be installed as 64bit, otherwise there will be some
compatibility problems with UMFPACK (unless this is modified). With
macports it is also part of gcc but needs to be specified as a variant:

    sudo port install gcc46 +gfortran

Again, compilation will take some time. I have not tested this fully.

###  pardiso

The pardiso sparse linear matrix solver is included as part of the Intel
Math Kernel Library which is packaged with the (see above).

If using the intel compiler then this solver will automatically become
available (check the initial output when running to see if this has been
found). There is currently no interface to use this solver external to
the Intel Math Kernel library.

###  umfpack

The sparse linear solver is part of the suitesparse collection of sparse
matrix routines written by Prof. Tim Davis. It is written in c and
released under the GNU GPL (see ).

UMFPACK depends on . METIS is freely available but not free
distributable. For more details see the .

The installation process for the UMFPACK/METIS combination has been
automated so that it can be easily used with arb. To install these
packages and compile them in a form that is suitable for arb:

    cd src/contributed/suitesparse
    make

The command will download version 3.6.0 of UMFPACK and version 4.0.3 of
METIS (using curl - install on ubuntu using if you don’t have it
already), and then compile these libraries using the gcc compiler. A
wrapper script for using UMFPACK from fortran (included with UMFPACK)
will also be compiled. The files will be placed in the
src/contributed/suitesparse directory, so will not overwrite any
alternative suitesparse or metis libraries already on your system. You
need to have installed on your system to build these libraries.

If all goes well the following files will be placed in
src/contributed/suitesparse to be used by arb:

    libamd.a
    libcamd.a
    libccolamd.a
    libcholmod.a
    libcolamd.a
    libmetis.a
    libumfpack.a
    umf4_f77wrapper.o

All of these files are required for UMFPACK to successfully run.

You also need to have a version of the blas libraries available for to
use UMFPACK. On OsX these should already be installed as part of Xcode
(see section \[sec:compiler\]). On ubuntu linux they can be installed
using if they’re not already present.

To remove the compiled libraries and files type

    make clean

from the directory. This will leave only the downloaded files (ready to
be reused). To remove the suitesparse and metis downloads as well, type

    make clean_all

Note that UMFPACK library compilation is dependent on the type of
machine architecture and the libraries will need to be remade if
transferred from one machine to another - do a and then a again from
within the directory.

arb has been tested with the following combinations of
umfpack/suitesparse and metis:

-   UMFPACK.tar.gz 02-Jun-2010 11:46 and metis-4.0.tar.gz (4.0.1)

-   SuiteSparse-3.6.0.tar.gz and metis-4.0.tar.gz (4.0.3)

### Perl

On ubuntu or OsX you should already have a version of perl installed.

###  hsl

An interface to the subroutine MA28D has been implemented. With the
availability of UMFPACK, there is no real reason to use this, except for
development. For more information see the directory. An interface to
MA48D is under development (don’t hold your breath though!).

### Numerical Recipes

These are an alternative to using some of the lapack routines. There is
no reason to use these, except for development. For more information see
the directory.

###  gmsh

While not integral to the arb code, the mesh and data format which arb
uses is that developed for gmsh. Gmsh is a mesh element generator which
can be run using scripts or via a graphical interface and can be used
for post-processing (visualisation) too. Gmsh uses the .

There is some great introductory material available on the on the use of
this program, particularly these online .

    sudo apt-get install gmsh

On earlier ubuntu versions the repository version is too old.

Download a version from the . Version 2.4 is the minimum required for
arb - most importantly the msh file format produced needs to be 2.1 or
greater. I have had no problems using development versions that are
available.

###  paraview

There is now capability to output in the format, used by (for example)
ParaView. To install ParaView on ubuntu use:

    sudo apt-get install paraview

On the mac there are binaries available. Input of this file format is
not supported.

###  tecplot

Tecplot is proprietary visualisation software. can output to its asci
format, but not input from this format.

