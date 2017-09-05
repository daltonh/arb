---
title: 'variables'
author: Dalton Harvie
date: 5/10/17
---

# Variables

*This section needs some rewritting: there is only one input file now*

There are eight types of user defined variables: constant, transient,
derived, unknown, equation, output, condition and local. Each of these
are stored in arb using the same general data structure (fortran type ).
Any of these variables can be defined by a user-written expression in
which is read by and interpreted by maxima. Additionally, the constant
type may be defined in and there given (only) a numerical value. Along
with the user defined variables, there are also system defined variables
which can be used in user-written expressions.

All variables have an associated compound variable type (scalar, vector
or tensor) which is used mainly for output purposes.

Details of both the user and system defined variables are given in this
section.

### Constant type variable defined in \[sec:equation\_constants\]

*Synopsis:*

Constant variables are evaluated once at the start of a simulation. If
defined in they are defined using an expression which may contain only
system variables and other constants — in the latter case the constants
must have been defined in either the file or previously (above) in the
file.

*Defining statements:*

    CELL_CONSTANT <name> [multiplier*units] "expression" ON <region> options # comments
    FACE_CONSTANT <name> [multiplier*units] "expression" ON <region> options # comments
    NONE_CONSTANT <name> [multiplier*units] "expression" options # comments
    CONSTANT <name> [multiplier*units] "expression" options # comments

*Statement components:*

-   *(required)*: This keyword specifies the centring of the variable.
    Constants that have cell or face centring vary over the simulation
    domain, and have values associated with each cell or face,
    respectively (subject to the statement, below). None centred
    constants have one value that is not linked to any spatial location.
    If the centring specifier is omitted from the keyword (as in ) then
    none centring is assumed (ie., keyword is equivalent to keyword ).

-   *(required)*: Each variable must have a unique name, delimited by
    the and characters. Besides these characters, the variable may
    contain spaces and any other non-alphanumeric characters except for
    double quotation marks (which demarcate the expression strings) and
    hash character (which indicates that a comment follows). If the name
    ends with a direction index, as in or , then the variable is
    considered to be a component of a three dimensional vector compound.
    Similarly, if the name ends with a double direction index, as in ,
    the variable is considered to be a component of a three by three
    tensor compound. Components of compounds that are not explicitly
    defined are given a zero value (when used in dot and double dot
    products for example). All defined components that are members of
    the same compound must be of the same variable type, have the same
    centring, be defined over the same region and have the same units
    and multiplier. Certain names are reserved for system variables (see
    section \[sec:system\_variables\]).

-   *(optional)*: When reading in numerical constants, each value is
    multiplied by this value. At present not in use in .

-   *(optional):* A string which specifies the units for the variable.
    At present this string is not interpreted by the code at all and the
    user must ensure that the units used are consistent.

-   *(required)*: When a constant is defined in , this double-quoted
    expression is used to specify the value of the constant. As they may
    contain system variables and also other constants, they may vary
    throughout the domain. For more details regarding the syntax of
    these expressions, see section \[sec:language\].

-   *(optional)*: This part of the statement determines over what region
    the variable should be defined. It is only applicable for cell and
    face centred variables, and must in these cases refer to a region
    that has the same centring as the variable. If omitted then by
    default a cell centred constant will be defined on and a face
    centred constant on . Note that referring to a variable value
    outside of its region of definition will produce an error when
    running .

-   *(optional)*: This is a comma separated list of options. Options
    earlier in the list take precedence over later ones. Valid options
    for the constant variable type include:

    -   : The compound variable that this component is a member of to be
        written to each applicable file. The opposite option exists.
        Default is for unknown variables, output variables, derived
        cell-centred variables, and transient variables that do not
        correspond to the oldest stored timestep (that is,
        $\code{rstep}<\code{rstepmax}$). The default is for
        everything else.

    -   : The compound variable that this component is a member of is to
        be included in the file. The opposite option exists. By default
        only this option is set only for unknown, output and transient
        non-centred variables that are at the current timestep (that is,
        $\code{rstep}=0$). The option also exists which specifies that
        the variable should be included in the file, but that its value
        is not updated before being printed. This option is useful for
        outputing variables which should only be updated when a file is
        actually written (for example, a variable that records the time
        when output occurs).

    -   : This component to be written to each applicable file. Default
        is for all variables.

    -   : The compound variable that is component is a member will be
        read from each applicable file. The opposite option exists.
        Default is for all unknown and transient variables and for
        everything else.

    -   : This component to be read from each applicable file. Default
        is for all variables.

    -   : This compound will be written using the gmsh format. Any data
        format options specified for each file will overwrite
        this option. Other options include , , , and . Default is and .

-   *(optional)*: Anything written beyond the first appearing on each
    line of the input file is regarded as a comment.

*Examples:*

    CELL_CONSTANT <test constant> "<cellx[l=1]>^2" ON <boundaries> # a test
    FACE_CONSTANT <test constant 2> [m] "<facex[l=2]>" # another test

### Constant type variable defined in 

*Synopsis:*

Constant variables defined in are set to numerical values read directly
by the executable, rather than expressions interpreted by maxima.

*Defining statements:*

    CELL_CONSTANT <name> [multiplier*units] value ON <region> options # comments
    FACE_CONSTANT <name> [multiplier*units] value ON <region> options # comments
    NONE_CONSTANT <name> [multiplier*units] value options # comments
    CONSTANT <name> [multiplier*units] value options # comments

*Statement components:*

The components of these statements are the same as in section
\[sec:equation\_constants\] with the exception of:

-   *(required)*: A numerical value of real or double precision type.

*Examples:*

    CONSTANT <mu> [Pa.s] 1.0d-3 # fluid viscosity
    NONE_CONSTANT <rho> [997*kg/m^3] 1.0 # fluid density

### Constant type variable defined per region in 

*Synopsis:*

This definition can be used in the file to assign different numerical
values to either a cell or face centred constant in specific regions.
Two statements are required for this type of constant definition: The
first defines the list of regions where the next constant will be set ()
and the second defines the constant and sets/lists the corresponding
numerical values (). The region names in the statement must have the
same centring as the following statement. Furthermore, the over which
the constant is defined must include all of the regions listed within
the previous statement.

*Defining statements:*

    REGION_LIST <region1> <region2> ... <regionN> # comments
    CELL_REGION_CONSTANT <name> [multiplier*units] value1 value2 ... valueN ON <region> options # comments
    FACE_REGION_CONSTANT <name> [multiplier*units] value1 value2 ... valueN ON <region> options # comments

*Statement components:*

The components of these statements are the same as in section
\[sec:equation\_constants\] with the exception of:

-   ... *(required)*: A list of regions that have the same centring as
    the following statement.

-   ... *(required)*: A list of numerical values for the constant,
    corresponding in a one-to-one fashion with the list of regions given
    in the previous statement.

*Examples:*

    REGION_LIST <inlet> <outlet> # some face regions
    FACE_REGION_CONSTANT <electric field> [V/m] 10 20. ON <boundaries> 

### Transient type variable defined in 

*Synopsis:*

Transient variables are used only in transient simulations, and are
evaluated at the start of each timestep. Transient variables are
typically used to store previous timestep values, or to provide constant
data to a simulation that depends explicitly on the time.

*Defining statements:*

    CELL_TRANSIENT <name> [multiplier*units] "initial expression" "expression" ON <region> options # comments
    FACE_TRANSIENT <name> [multiplier*units] "initial expression" "expression" ON <region> options # comments
    NONE_TRANSIENT <name> [multiplier*units] "initial expression" "expression" options # comments
    NONE_TRANSIENT <name> [multiplier*units] "" "expression" options # this will use the update expression as the initial expression
    NONE_TRANSIENT <name> [multiplier*units] "expression" options # the initial expression here depends on rstep

*Statement components:*

Along with the information presented in section
\[sec:equation\_constants\], the following applies to transient
variables:

-   *(required)*: If no centring is specified then none centring
    is assumed.

-   *(required)*: Along with the rules detailed in section
    \[sec:equation\_constants\], transient variables are associated with
    particular relative timesteps. Relative timesteps, described using
    the term in this document, indicate how many timesteps previous to
    the current one the variable refers to. The value of a variable is
    defined in a similar manner to the direction of a variable, using an
    index in square brackets at the end of the variable name: For
    example, would be the time corresponding to the end of the current
    timestep, would be time from the previous timestep, the time from
    the (earlier) timestep before that one and so on. If an index is
    omitted from a definition, then is assumed. Actually, any type of
    variable can be associated with any particular relative timestep,
    but it is rare to do this with anything other than a
    transient variable.

-   *(optional)*: This expression is applied once (only) at the start of
    a simulation, and represents the variable’s initial condition. These
    initial expressions are applied in the order of increasing (relative
    timestep), meaning that the current (latest) time value is
    calculated first, followed by the previous timestep value, and then
    one before etc. This expression should not depend on any transient
    variables that have a higher (an earlier timestep) or that are from
    the same timestep (equal ) but defined later in the input file. If
    an initial expression is not given at all (no quotation marks
    present for this field), then the value of zero is assumed if the
    variable has , or the update expression otherwise. If the initial
    expression is not specified but quotation marks are present for this
    field, then the update expression is substituted for the initial
    expression - ie, a shorthand way of repeating the update expression.

-   *(required)*: This expression for the transient variable is applied
    once at the start of each timestep. These expressions are applied in
    the order of decreasing (relative timestep), meaning that the
    earliest time value is calculated first, followed by the next
    timestep value, until the current time ($\code{rstep}=0$)
    is reached. Circular references are not allowed in the expression
    (in practice this is not limiting).

-   *(optional)*: If ommitted then by default a cell centred transient
    will be defined on and a face centred transient on .

-   *(optional)*: This is a comma separated list of options. Valid
    options for transient variables are the same as those for constants,
    as detailed in section \[sec:equation\_constants\])

*Examples:*

    NONE_TRANSIENT <t[r=0]> "0.d0" "<t[r=1]>+<dt>" # current end-of-timestep time (r=0)
    NONE_TRANSIENT <t[r=1]> "<t>-<dt>" "<t>" # time at last step (r=1)
    NONE_TRANSIENT <t[r=2]> "<t[r=1]>-<dt>" "<t[r=1]>" # time at step before last step (r=2, assuming a constant dt)
    NONE_TRANSIENT <z[r=1]> [m] "<z>-<w_0>*<dt>" "<z_real>" # position of ball at last step (r=1)
    NONE_TRANSIENT <w[r=1]> [m/s] "<w>" "<w_real>" # velocity of ball at last step (r=1)

### Derived type variable defined in \[sec:equation\_deriveds\]

*Synopsis:*

Derived variables depend on the unknown variables and other previously
defined (ie, above in the file) derived variables.

*Defining statements:*

    CELL_DERIVED <name> [multiplier*units] "expression" ON <region> options # comments
    FACE_DERIVED <name> [multiplier*units] "expression" ON <region> options # comments
    NONE_DERIVED <name> [multiplier*units] "expression" options # comments
    DERIVED <name> [multiplier*units] "expression" options # comments

*Statement components:*

Along with the information presented in section
\[sec:equation\_constants\], the following applies to derived variables:

-   *(required)*: If no centring is specified then none centring
    is assumed.

-   *(required)*: This is an expression for the derived variable in
    terms of constant, transient, unknown, previously defined derived
    (appearing above in ) and system variables.

-   *(optional)*: If ommitted then by default a cell centred derived
    will be defined on and a face centred derived on .

-   *(optional)*: This is a comma separated list of options. Valid
    options for derived variables (as well as those given in
    section \[sec:equation\_constants\]) include:

    -   : Normally the derivative of this variable’s expression is
        calculated with respect to each unknown variable (the Jacobian)
        when performing the Newton-Raphson solution procedure. Including
        this option sets this derivative to zero. This may be required
        for functions for which the derivative cannot be calculated or
        for functions that undergo step changes (not continuous) which
        are not ammeniable to solution via the Newton-Raphson procedure.
        Using this option will usually slow convergence.

    -   : Including one of these options causes the code to check the
        sign of the derived variable. In theory this could be used for
        quantities like concentrations that are only physically
        meaningful when being positive. By using an expression such as
        and including the option an upper limit for a variable can also
        be enforced. In practice using these types of limiting
        conditions to prevent equation singularities slows convergence
        to an unfeasibly slow rate. It is usually better to choose the
        form of the equations so that they are stable even for small
        unphysical excursions, and then check once convergence has been
        achieved that the results are physical.

*Examples:*

    FACE_DERIVED <tau[l=1,1]> "<p> - <mu>*2.d0*facegrad[l=1](<u[l=1]>)" output
    CELL_DERIVED <graddivp[l=1]> "celldivgrad[l=1](<p>)" # divergence based pressure gradient

### Unknown type variable defined in 

*Synopsis:*

Unknown variables are those upon which the equations and derived
variables ultimately depend.

*Defining statements:*

    CELL_UNKNOWN <name> [multiplier*units] magnitude "expression" ON <region> options # comments
    FACE_UNKNOWN <name> [multiplier*units] magnitude "expression" ON <region> options # comments
    NONE_UNKNOWN <name> [multiplier*units] magnitude "expression" options # comments
    UNKNOWN <name> [multiplier*units] magnitude "expression" ON <region> options # comments

*Statement components:*

Along with the information presented in section
\[sec:equation\_deriveds\], the following applies to unknown variables:

-   *(required)*: If no centring is specified then cell centring
    is assumed.

-   *(required)*: An order of magnitude estimate (postive and greater
    than zero real or double precision value) must be specified for all
    unknown variables. This magnitude is used when checking on the
    convergence of the solution.

-   *(required)*: For an unknown variable the expression specifies the
    variable’s initial value. The expression may contain constant
    variables, derived variables, previously defined unknown
    variables, (initial) transient variables and system variables.

-   *(optional)*: If ommitted then by default a cell centred unknown
    will be defined on and a face centred unknown on .

-   *(optional)*: The option is not applicable for unknown variables.

*Examples:*

    CELL_UNKNOWN <u[l=1]> 1.d0 "<u_av>" # a velocity component
    CELL_UNKNOWN <p> [] 1.d0 "1.d0-<cellx[l=1]>" # pressure
    NONE_UNKNOWN <p_in> [Pa] 1.d0 "1.d0" # the pressure at the inlet

### Equation type variable defined in 

*Synopsis:*

Equation variables represent the equations to be satisfied. The equation
expressions should be formulated so that when the equation is satisfied,
the expression equals zero. The number of equations must equal the
number of unknown variables. Furthermore, for the system to be well
posed the equations must be unknown (no single equation can be made from
a combination of the other equations).

*Defining statements:*

    CELL_EQUATION <name> [multiplier*units] "expression" ON <region> options # comments
    FACE_EQUATION <name> [multiplier*units] "expression" ON <region> options # comments
    NONE_EQUATION <name> [multiplier*units] "expression" options # comments
    EQUATION <name> [multiplier*units] "expression" options # comments

*Statement components:*

Along with the information presented in section
\[sec:equation\_deriveds\], the following applies to equation variables:

-   *(required)*: If no centring is specified then none centring
    is assumed.

-   *(required)*: For an equation variable the expression should equal
    zero when the equation is satisfied. The expression may contain
    constant, transient, derived, unknown and system variables.

-   *(optional)*: If ommitted then by default a cell centred equation
    will be defined on and a face centred equation on .

*Examples:*

    CELL_EQUATION <continuity> "celldiv(<u_f>)" ON <domain> # continuity
    FACE_EQUATION <outlet noslip> "dot(<u[l=:]>,<facetang1[l=:]>)" ON <outlet> # no component tangential to outlet
    NONE_EQUATION <p_in for flowrate> "<u_av_calc>-<u_av>" # set flowrate through inlet to give required average velocity

### Output type variable defined in 

*Synopsis:*

Output variables are evaluated once convergence of the solution has been
reached: They are only for output purposes.

*Defining statements:*

    CELL_OUTPUT <name> [multiplier*units] "expression" ON <region> options # comments
    FACE_OUTPUT <name> [multiplier*units] "expression" ON <region> options # comments
    NONE_OUTPUT <name> [multiplier*units] "expression" options # comments
    OUTPUT <name> [multiplier*units] "expression" options # comments

*Statement components:*

Along with the information presented in section
\[sec:equation\_deriveds\], the following applies to output variables:

-   *(required)*: If no centring is specified then none centring
    is assumed.

-   *(required)*: For an output variable the expression may contain
    constant, transient, derived, unknown, equation and
    system variables.

-   *(optional)*: If ommitted then by default a cell centred output
    variable will be defined on and a face centred output variable on .

-   *(optional)*: The option is not applicable for output variables
    (this option is implicitly set anyway for these variables).

*Examples:*

    NONE_OUTPUT <F_drag> [N] "facesum(<facearea>*dot(<facenorm[l=:]>,<tau[l=:,1]>),<cylinder>)" # force on object in axial direction

### Condition type variable defined in 

*Synopsis:*

Condition variables control the running of the simulation. They can
initiate the following actions: output, stop, convergence and a bell.

*Defining statements:*

    CELL_CONDITION <name> [multiplier*units] "expression" ON <region> options # comments
    FACE_CONDITION <name> [multiplier*units] "expression" ON <region> options # comments
    NONE_CONDITION <name> [multiplier*units] "expression" options # comments
    CONDITION <name> [multiplier*units] "expression" options # comments

*Statement components:*

Along with the information presented in section
\[sec:equation\_deriveds\], the following applies to condition
variables:

-   *(required)*: If no centring is specified then none centring
    is assumed.

-   *(required)*: For a condition variable, if the evaluated expression
    is positive ($>0$) then the condition is satisfied and the
    corresponding action will take place. Note that an action will take
    place if any of the condition variables that correspond to it are
    positive (in fact, after one positive value is found the remainder
    are not even evaluated).

-   *(optional)*: If ommitted then by default a cell centred condition
    variable will be defined on and a face centred condition variable
    on .

-   *(optional)*: In addition to the options discussed for the other
    variables, one or more of the following options may be applied to
    each condition variable to specify what action it corresponds to:

    -   : For transient and steady-state simulations, indicates when the
        Newton loop has converged. Is evaluated at the start of each
        Newton loop.

    -   : For a transient simulation, indicates when the simulation
        should finish. Is evaluated at the end of each
        successful timestep.

    -   : For a transient simulation, indicates when the output files
        should be written. Is evaluated at the end of each
        successful timestep.

    -   : For a transient simulation, indicates when a noise should be
        made (this one’s a bit silly). Is evaluated at the end of each
        successful timestep.

*Examples:*

    NONE_CONDITION <time based stop condition> "<t>-<tend>" stopcondition # when this becomes true (>0.) the simulation stops
    NONE_CONDITION <bouncing bell> "noneif(<z>,-1.d0,1.d0)" bellcondition # is positive when <z> is negative at the end of a timestep
    NONE_CONDITION <output test> "<t>-<tout>-<dtout>" outputcondition # this will be true (>0.) whenever we are <dtout> from last output

### Local type variable defined in 

*Synopsis:*

Local variables are like derived variables, except that they are not
stored, but rather evaluated only when required. Local variables may be
used instead of derived variables to save memory. This strategy makes
sense if the variable is only going to be used once or twice at each
location. Local variables may also be used to split up an otherwise long
expression into smaller (and possibly common) sub-statements, dependent
on the local conditions. For example, in the examples given below, local
variables are used to calculate the second derivative of the normal
velocity to a wall, in the normal direction to a wall.

*Defining statements:*

    CELL_LOCAL <name> [multiplier*units] "expression" ON <region> options # comments
    FACE_LOCAL <name> [multiplier*units] "expression" ON <region> options # comments
    NONE_LOCAL <name> [multiplier*units] "expression" options # comments
    LOCAL <name> [multiplier*units] "expression" options # comments

*Statement components:*

Along with the information presented in section
\[sec:equation\_deriveds\], the following applies to condition
variables:

-   *(required)*: If no centring is specified then none centring
    is assumed.

-   *(required)*: A local variable may depend on ‘local’ variables which
    correspond to the locale of the calling statement: For example, in
    the following examples we refer to the of the face on which the
    local variable is calculated. Note that it would not make sense to
    output this separately over , as the would be undefined.

-   *(optional)*: The output region for a local variable is only really
    used right now to specify what elements are output (should the
    option be set).

*Examples:*

    CELL_LOCAL <u_n> "dot(<u[l=:]>,cellave[lastface](<facenorm[l=:]>))"
    CELL_LOCAL <d u_n d x[l=1]> "cellgrad[l=1](<u_n>)"
    CELL_LOCAL <d u_n d x[l=2]> "cellgrad[l=2](<u_n>)"
    CELL_LOCAL <d u_n d x_n> "dot(<d u_n d x[l=:]>,cellave[lastface](<facenorm[l=:]>))"
    FACE_LOCAL <d^2 u_n d x_n^2> "facegrad(<d u_n d x_n>)" ON <boundaries> output

### System variables \[sec:system\_variables\]

*TODO*

## Expression Language Overview

<!--
*There’s lots missing in this section. The examples files are currently
the best guide as to the language syntax.*
-->

The expression language refers to the psuedo-mathematical language that is used to represent each variable's expression.  arb uses the symbolic algebra program 'maxima' to parse this language and convert these expressions into executable (fortran), so any mathematical operators supported by 'maxima' are able to be used in this language.  In addition to maxima's features, the expression language also supports a number of arb specific discretisation operators that allow spatially varying problems to be expressed in scalar arithmetic.  The discretisation operators are particularly suited to solving transport problems using the Finite Volume Method and are detailed in this section.

### Discretisation Operators

#### Syntax

Discretisation operators produce a single result from the arguments that are contained within their parentheses (). They also accept options, contained within square brackets \[\], and placed between the operator name and any parentheses. Operators are (by convention) typed lowercase (although should parse in uppercase) and contain no underscores, as per

```arb
operator[option1,option2,...](<argument1>,<argument2>,...)
```

Example `facegrad` and `celldiv` operators contained within variable definitions:
```arb
FACE_OUTPUT <phigrad> "facegrad[adjacentcells](<phi>)" ON <allfaces> # operator is facegrad, argument is <phi> and a single option of adjacentcells is specified
CELL_EQUATION <continuity> "celldiv(<u_f>)" ON <domain> # operator is celldiv acting on single argument of <u_f>
```

#### Operator Centring

The centring of an operator refers to the centring of the result it produces.  This centring must match or be consistent with the centring of the context in which the operator is placed.  The centring of most operators corresponds to the first syllable of the operator name, however there are some exceptions.  For the exceptions the centring of the operator is none, and the first syllable of the operator name corresponds to the centring of the first argument passed to the operator.

To illustrate, following the centring rule is `celldiv` which generates the cell centred divergence of a face centred quantity. This operator is cell centred and hence must be used in a cell centred context. Note that the content expression passed into this operator (actually its first argument) is face centred however. Similarly, `facegrad` is the gradient of a cell centred quantity evaluated at a face, so this operator is face centred and must be used in a face centred context, but its argument (content) has cell centring.

```arb
"celldiv(<u_f>)" # cell context centring, face argument centring
"facegrad(<phi>)" # face context centring, cell argument centring
```

Exceptions to the centring rule include the loop-type operators, `max`, `min`, and `sum`. For example, `cellmax` loops through a region of cells finding the maximum value of an expression within those cells. Hence, this operator produces a result which has no centring (none centred) so can be used in any centring context, but its first argument has cell centring.  `facesum` loops through a region of faces summing the values of its first argument, hence producing a none centred result that can be used in any centring context.

```arb
"cellmax(<phi>,0.d0)" # none context centring, cell centring of the first argument <phi>
"facesum(<phi_f>,0.d0,<allfaces>)" # none context centring, face centring of the first argument <phi_f>
```

The centring of specific operators is detailed in the reference section.

#### Operator Arguments

##### Implicit Operator Arguments

Each operator accepts a certain number of arguments, however if an argument is not specified then a default value may be used. For example, `cellmax` uses three arguments:

#. an expression that is to evaluated in each cell (`<expression>`), here denoted by a single variable, but more usually an expression of variables;
#. an initial, default expression for the operator (`<default>`), which again could be an expression of variables; and
#. the cell centred content region over which the maximum will be calculated and within which `<expression>` must be defined (`<region>`).

Using implicit argument notation, operators expect the arguments in a specific order, so `cellmax` expects these three arguments in the manner 
```arb
"cellmax(<expression>,<default>,<region>)"
```
If less than the required number of arguments are passed to an operator, then a default value for the omitted arguments will be assumed (or if no defaults are available or are sensible, an error will be flagged). For example, using
```arb
"cellmax(<expression>)"
```
sets `<default>` to `-<huge>` (the largest negative double precision number that the processor can store) and `<region>` to `<noloop>` if (for example) the expression was being used in a cell centred context. If in doubt about what the default value for an argument is, specify it!

##### Explicit Operator Arguments

The alternative to the implicit argument notation is to specify the arguments explicitly (similar to argument passing in f90). Using explicit notation the order of the arguments that are passed explicitly is irrelevant, however the order of any arguments that are not explicitly named (and hence specified implicitly) still is. For example, the following will all produce the same result

```arb
"cellmax(expression=<expression>,default=<default>,region=<region>)"
"cellmax(<expression>,<default>,<region>)"
"cellmax(region=<region>,default=<default>,expression=<expression>)"
"cellmax(<expression>,region=<region>,<default>)"
"cellmax(region=<region>,<expression>,<default>)"
```

Note in the last case that although `<expression>` was the second argument in the operator, it was the first implicitly named operator, so would be read correctly.  Using a combination of the implicit and explicit passing is often convenient.  For example, for the `cellmax` operator, the following form that uses a default value of `-<huge>` but performs the maximum comparison over a specified region is handy
```arb
"cellmax(<expression>,region=<region>)" # as default is omitted, the last (third) argument must be explicitly named
```

Operator options are similar to variable options.  Some operators require a dimension, and this dimension (direction) is specified via the options.  For example, `celldivgrad` calculates a gradient in a certain direction dimension using the divergence of a face centred scalar.  To find this gradient in the second dimension you use the option `[l=2]`:
```arb
"celldivgrad[l=2](<face centred expression>)"
```

Some options are quite generic (eg, `noderivative`), however most are specific to the operator.  There is no restriction on the order that options are specified.

Details of individual discretisation operators follows.  However ultimate details of each operator (including argument order, options etc) can be found in the code file `src/setup_equations/setup_equations.pl` which shows how they are expanded into working code. Use search strings such as `ref: celldiv` within this perl file to find the specific code.

## Discretisation Operators Listing

### `celldiv` : Divergence

####Summary:
Uses Gauss’ theorem to calculate the divergence of a face centred vector component around a cell.

####Statement:

```arb
"celldiv[options](expression=<expression>)"
```

####Centring:

Operator is (context) cell centred, and expression is face centred.

####Details:

Using Gauss’ theorem to evaluate divergences around cells is probably the defining characteristic of Finite Volume methods. `celldiv` performs this operation.

Specifically, to discretise the divergence of a face centred vector
$\vect[j]{u}$ over a cell $i$ that sits within the domain, Gauss’
theorem gives

$$\begin{aligned}
\frac{1}{\scali[i]{V}} \int_{\scali[i]{V}} \vect{\nabla} \cdot \vect{u} dV & \Rightarrow \frac{1}{\scali[i]{V}} \sum_{j \in \scali[\text{nobcellfaces},i]{\mathbb{J}}} \frac{1}{\scali[j]{S}} \int_{\scali[j]{S}} \vecti[i,j]{N} \cdot \vecti[j]{u} \, dS \\
& = \explain{\sum_{j \in \scali[\text{nobcellfaces},i]{\mathbb{J}}} \frac{\vecti[i,j]{N} \cdot \vecti[j]{n} }{\scali[i]{V}}}{\normalsize\code{celldiv}} \frac{1}{\scali[j]{S}} \int_{\scali[j]{S}} \vecti[j]{n} \cdot \vecti[j]{u} \, dS \nonumber \\
%= \frac{1}{\scal[cell]{V}} \int_{\scal[cell]{S}} \vect[cell]{n} \cdot \vect{u} dS = \frac{1}{\scal[cell]{V}} \sum_j (\vect[cell]{n} \cdot \vecti[j]{n}) (\vect{u} \cdot \vecti[j]{n}) \scali[j]{S} \nonumber \\
& \Rightarrow `celldiv(dot(<u[l=:]>,<facenorm[l=:]>))`
\end{aligned}$$

where $\scali[i]{V}$ and $\scali[j]{S}$ are the volume and total surface area of the cell $i$ and face $j$, respectively, $\vecti[i,j]{N}$ is a unit normal pointing outward from cell $i$ but located at face $j$, $\vecti[j]{n}$ is a normal associated with face $j$, and the sum is conducted over the set of all face elements that surround cell $i$, denoted by $\scali[\text{nobcellfaces},i]{\mathbb{J}}$. In the equivalent coding the face centred vector $\vecti[j]{u}$ is represented by the three component variables , and , and the unit normal associated with the face $j$, $\vecti[j]{n}$, is given by the system component variables , and .  Note that as the divergence of a vector results in a scalar, the above operation produces a scalar for each cell it is performed in.  

The region used by arb in performing the above sum as represented by $\scali[\text{nobcellfaces},i]{\mathbb{J}}$ is ('no-boundary-cell-faces'). This relative region specifies all faces that surround a given cell, unless that cell is a boundary cell. As boundary cells are not fully surrounded by faces Gauss’ theorem can not be applied. Hence, if the operator is used at a boundary cell then the region is taken relative (moved) to the closest domain cell that is adjacent the boundary cell, so this is where becomes evaluated.  Physically it is inadvisable to use an equation that involves a divergence at a boundary cell anyway.

####Options:

[noderivative]   : No derivatives with respect to the unknown variables for the
    Newton-Raphson Jacobian are calculated for this operator (and
    its contents).

####Examples:

```arb
CELL_EQUATION <continuity> "celldiv(<u_f>)" ON <domain> # continuity equation
CELL_EQUATION <momentum[l=1]> "celldiv(<J_f[l=1]>)" ON <domain> # momentum conservation in direction l=1
CELL_EQUATION <momentum[l=2]> "celldiv(<J_f[l=2]>)" ON <domain> # momentum conservation in direction l=2
```

### `cellgrad`, `facegrad` or `nodegrad`: Gradient

####Summary:

Calculates a scalar component of a gradient from surrounding cell values at the location of a cell, face or node.

*Statement:*

\

*Centring:*

is context cell centred and is context face centred. In both cases is
cell centred.

*Details:*

To calculate the gradient of a cell centred scalar $\scali[i]{\phi}$ in
coodinate direction $2$ in cell $i$,
$$\frac{1}{\scali[i]{V}} \int_{\scali[i]{V}} \vecti[2]{e} \cdot \vect{\nabla} \phi dV \Rightarrow \sum_{i' \in \scali[\text{cellcells},i]{\mathbb{I}}} \scali[i,i']{\cellcentred{k}^{(2)}} \scali[i']{\phi} \Rightarrow \code{cellgrad[l=2](phi)} \nonumber$$
where $\vecti[2]{e}$ is a unit vector in coordinate direction $2$,
$\scali[i,i']{\cellcentred{k}^{(2)}}$ is a predetermined kernel for this
operation, and $\scali[\text{cellcells},i]{\mathbb{I}}$ is the set of
all cells in the vicinity of cell $i$ that are used by this kernel.
Kernels to calculate the cell gradient in the other coordinate
directions, that is $\scali[i,i']{\cellcentred{k}^{(1)}}$ and
$\scali[i,i']{\cellcentred{k}^{(3)}}$ also exist.

A gradient of a cell centred quantity evaluated at a face can be
calculated similarly, for example
$$\frac{1}{\scali[j]{S}} \int_{\scali[j]{S}} \vecti[3]{e} \cdot \vect{\nabla} \phi dS \Rightarrow \sum_{i \in \scali[\text{facecells},j]{\mathbb{I}}} \scali[j,i]{\facecentred{k}^{(3)}} \scali[i]{\phi} \Rightarrow \code{facegrad[l=3](phi)} \nonumber$$
Gradients taken in directions relative to the face orientation are also
available using the operator. Index $4$ gives the gradient relative to
the face’s normal, that is
$$\frac{1}{\scali[j]{S}} \int_{\scali[j]{S}} \vecti[j]{n} \cdot \vect{\nabla} \phi dS \Rightarrow \sum_{i \in \scali[\text{facecells},j]{\mathbb{I}}} \scali[j,i]{\facecentred{k}^{(4)}} \scali[i]{\phi} \Rightarrow \code{facegrad[l=4](phi)} \nonumber$$
In computational terms the face normal is represented by (,, ). Indices
$5$ and $6$ give gradients in the directions of the first and second
tangents for each face, respectively, that is
$$\frac{1}{\scali[j]{S}} \int_{\scali[j]{S}} \vecti[j]{t}^{(1)} \cdot \vect{\nabla} \phi dS \Rightarrow \sum_{i \in \scali[\text{facecells},j]{\mathbb{I}}} \scali[j,i]{\facecentred{k}^{(5)}} \scali[i]{\phi} \Rightarrow \code{facegrad[l=5](phi)} \nonumber$$
and
$$\frac{1}{\scali[j]{S}} \int_{\scali[j]{S}} \vecti[j]{t}^{(2)} \cdot \vect{\nabla} \phi dS \Rightarrow \sum_{i \in \scali[\text{facecells},j]{\mathbb{I}}} \scali[j,i]{\facecentred{k}^{(6)}} \scali[i]{\phi} \Rightarrow \code{facegrad[l=6](phi)} \nonumber$$
Computationally $\vecti[j]{t}^{(1)}$ is represented by (,, ) and
$\vecti[j]{t}^{(2)}$ by (,, ), respectively. If the face has one
dimension then $\vecti[j]{t}^{(1)}$ will be directed along the face, and
$\vecti[j]{t}^{(2)}$ will be normal to both $\vecti[j]{t}^{(1)}$ and
$vecti[j]{n}$. If the face has no or two dimensions (a point or a plane)
then there are no preferential directions for these tangents. If no
index is specified on the operator then is assumed.

*Options:*

-   , , etc: This index specifies the direction that the gradient will
    be taken in. For this index represents the dimension the gradient is
    taken in and must be specified. For if the index is specified and is
    $\le 3$, this specifies the dimension the gradient is taken in. For
    an index $\ge 4$, the direction is taken relative to the
    face orientation. specifies a gradient taken in the direction of the
    face normal, a gradient taken in the direction of the first tangent
    to the face and in the direction of the second tangent to the face.
    If the index is not specified for then is assumed — that is, a
    gradient taken normal to the face.

-   for only: Gradient is based on adjacent cells only, but attempts to
    be in the direction of the face normal (it is only an approximation,
    but should be accurate for structured meshes). Note, only works for
    direction — that is, the direction of the face normal.

-   for only: Similar to option in that it is based on adjacent cells
    only, but now it is in the direction of , which is a unit vector
    pointing from the centre of the cell immediate below the face (in
    the face’s normal direction) to the centre of the cell immediately
    above the face. Hence, for unstructured meshes, this gradient is not
    precisely in the same direction as the true .

-   , , etc: This specifies that the contained expression is a component
    of a vector, and that over any glued reflection boundaries, must be
    reflected in this direction. These options only need to be specified
    if the operator is going to be acting over (or next to) a glued,
    reflection boundary that is reflected in a direction that is the
    same as the vector’s component direction.

-   : As previously.

*Examples:*

    FACE_DERIVED <T flux> "-<D>*facegrad(<T>)" ON <all faces> # some type of heat flux occuring across each face
    CELL_DERIVED <dpdx[l=1]> "cellgrad[l=1](<p>)" # gradient of pressure in first dimension

### : Interpolation to cell centring

*Summary:* Interpolates or averages an expression from (mainly) face
centring to cell centring.

*Statement:*

\
*Centring:*

is context cell centred and generally takes a face centred expression
(see option however).

*Details:*

Without any options, predefined kernels are used to interpolate the face
centred expression from the faces that surround a cell to the centroid
of that cell.

*Options:*

-   : Evaluates at the last face that was referenced in the context of
    the operator’s position, but treats the result as having
    cell centring.

-   : As above, but moves through glued boundaries to the actual last
    face that was used (if it was glued).

-   : Evaluates at the cell that is adjacent to the last face that was
    referenced in the context of the operator’s position. In
    this (exception) case is cell centred. For this case only etc
    options may be used/necessary as the cell may be on the other side
    of a glued reflection boundary.

-   : As previously.

### : Interpolation

*Summary:* Interpolates or averages an expression from cell to face
centring.

*Statement:*

*Centring:*

has face context centring. is cell centred. is face centred. is cell
centred.

*Details:*

TODO

*Options:*

-   :

-   :

-   :

-   : As previously.

###  or : Sum

*Summary:* Performs a sum over a region of either cell or face elements.

*Statement:*

\
\
\
 *Centring:*

Operators may be cell, face or none centred. Contents of is cell
centred, contents of is face centred.

*Details:*

This operator sums the contained expression over a region of cell or
face elements. If no region is specified, then default regions are
applied, defined by:

   Operator centring   Expression centring   Default region
  ------------------- --------------------- ----------------
                                            
                                            
                                            
                                            

*Options:*

-   : As previously.

### : Gradient evaluated at a cell calculated via a divergence

### : Gradient limiter for ensuring advection stability

### , or : If conditional statement

###  or : Product performed over a region of elements

### , or : Picks the minimum/maximum from a region of elements

###  or : A delta function to identify specific regions

###  or : Link to other regions



