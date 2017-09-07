---
title: 'coding reference'
author: Dalton Harvie
date: 5/9/17
---

# Coding Reference

## The arb code directory

*This section is now out of date as of v0.57.  Now arb can be used from outside its code directory (referred to as the `arb_dir`), and this is the preferred method of use.*

As arb is distributed as source code that is compiled for each
application, arb is not installed in the traditional sense. Instead, for
each new simulation a new self-contained copy of the source files is
unpacked from a source tarball. Once created arb is run from a working
directory which contains a specific structure of files and
subdirectories. Two routines, and , are provided to automate the process
of managing this required file/subdirectory structure.

## Unpacking and packing the code

### Unpacking

To create a new version of arb, a new working directory should be
created or that contains the five files

    archive.tar.gz
    unpack
    readme
    licence
    version

Using the command

    ./unpack

from within this directory will unpack the archive ready for use.

### Packing

To pack a simulation ready to transport or backup, use the command

    ./pack

from within the working directory. This will create a subdirectory with
a name of the form which contains all files necessary to run arb.
Following the command with a name, as in

    ./pack a_name

will create the archive in a subdirectory named instead of the default.

The script accepts a number of options. By default only files within the
or directories that are specific to this manual are included in the
archive. The options , or specify that all files within either the , or
both directories are contained within the archive. By default only
source code within the directory that is not subject to a non-free third
party licence is included in the archive. Using the options or causes
all files in these directories to be archived (including the build
suitesparse libraries). Using means that no third party software is
included in the archive and example input files will be copied to the
working directory. The option means that all files in the build
directory will be included in the archive. This may be useful if you
want to transport the simulation to another machine that may not have
maxima installed (for example).

Use to list other options.

## The working directory and file structure

Once the archive is unpacked the working directory will contain the
following subdirectories and files/links:

-   directory: contains the main fortran source code of arb, and the
    necessary to build everything (except for the
    contributed libraries). It also contains the meta-programming perl
    script and a template file which are used to create the fortran file
    within which is specific to each problem.

-   directory: This directory may/should/can contain contributed third
    party code that can be used by arb, along with associated interface
    modules — for example, linear solver routines. There is a separate
    subdirectory for each package. Each directory contains error
    handling modules that handle runtime cases where the third party
    routines are not available, and also some brief
    installation instructions. Most directories work on the drop-box
    principle — if the required files are available then they will be
    included in the arb executable.

-   directory: all building is done within this directory. Some notable
    files are which contains all of the fortran coding that is specific
    to the current problem, and which is the only input file read by the
    executable fortran file.

-   directory: temporary files are stored within this directory.

-   directory: includes files produced during the setup of the problem
    (running of ). If you’re having to debug a problem setup then this
    is the place to look. The file contains a lot of detail regarding
    the equation setup. The file is a completely unwrapped version of
    the last run input, in which any d files are unwrapped, with any
    relevant text strings substituted. This file can be input directly
    to arb again if desired.

-   directory: output files from a simulation are placed in here.

-   directory: contains miscellaneous files — for example: a script
    which builds a file from any file in the working directory; a script
    which automates the running of consecutive arb runs; and a script
    which runs all cases in the directory as a check.

-   directory: contains documentation including this manual.

-   directory: example problem-specific files with their associated
    geometry files (structure files with extension, and mesh files
    with extension) are stored here. Looking through these examples is
    currently the best way to learn about the language syntax and
    generally understand how to run arb simulations.

-   directory: New for version 0.4, this directory contains template
    files for doing common chunks of setup. For example, there is a
    directory which contains all of the code chunks necessary to run
    Navier-Stokes problems. There are also directories to implement
    high-order limited advection in both two and three dimensions. These
    pieces of template code can be used via the and commands.

-   script: packs the directory for transportation to another location
    or computer. This script has many useful options — try .

-   directory: contains licence details, including the (GNU GPL) licence
    under which arb is released and the specific version of the code. A
    history file within the directory records the distribution history
    of this particular code directory.

-   files: these files and an associated or file contain all the
    problem-specific information required for a particular simulation.

-   script: this shell script is a wrapper script for setting-up, making
    and running . You can pass options to this script to control the
    compilation process — for example choose between the gnu and intel
    compilers, whether using OMP or not, whether you are restarting a
    simulation or starting from scratch. This script works out when the
    equation meta-programming has to be redone or not, and when
    recompilation is necessary, although with other options you can
    overwrite this behaviour. To list the options type .

## Main Code Structure

![la lune](lalune.jpg "Voyage to the moon")
