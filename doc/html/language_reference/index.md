---
title: 'Language Reference'
author: Dalton Harvie
date: 6/12/16
---

# Expression Language Reference

*There’s lots missing in this section. The examples files are currently
the best guide as to the language syntax.*

The expression language refers to the psuedo-mathematical language that is used to represent each variable's expression.  arb uses the symbolic algebra program 'maxima' to parse this language and convert these expressions into executable (fortran), so any mathematical operators supported by 'maxima' are able to be used in this language.  In addition to maxima's features, the expression language also supports a number of discretisation operators that allow spatially varying (multiphysics) problems to be expressed in scalar arithmetic.  The discretisation operators are particularly suited to solving transport problems using the Finite Volume Method.

## Discretisation Operators

### General Notes

Discretisation operators produce a single value from the arguments that are contained within their parentheses (). They also accept options, contained within square brackets \[\], and placed between the operator name and any parentheses. Operators are (by convention) typed lowercase (although should parse in uppercase) and contain no underscores.  

```arb
operator[option1,option2,...](<argument1>,<argument2>,...)
```

Example `facegrad` and `celldiv` operators contained within variable definitions:
```arb
FACE_OUTPUT <phigrad> "facegrad[adjacentcells](<phi>)" ON <allfaces> # operator is facegrad, argument is <phi> and a single option of adjacentcells is specified
CELL_EQUATION <continuity> "celldiv(<u_f>)" ON <domain> # operator is celldiv acting on single argument of <u_f>
```

### Operator Centering

The centring of most operators corresponds to the first syllable of the operator.

Following the rule is celldiv which is the cell centred divergence of a face centred quantity. This operator is cell centred and must be used in this context, hence its context centring is cell. The content expression passed into (actually its first argument) is face centred however. Similarly, facegrad is the gradient of a cell centred quantity evaluated at a face, so this operator is face (context) centred, but its argument (content) has cell centring.

```arb
"celldiv(<u_f>)" # cell context centring, face argument centring
"facegrad(<phi>)" # face context centring, cell argument centring
```

Exceptions to the rule include the loop-type operators, max, min, and sum. For example, cellmax loops through a region of cells finding the maximum value of an expression within those cells. Hence, this operator produces a result which has no centring (none centred) so can be used in any centring context, but its first argument has cell centring.

```arb
"cellmax(<phi>,0.d0)" # none context centring, cell centring of the first argument <phi>
"facesum(<phi_f>,0.d0,<allfaces>)" # none context centring, face centring of the first argument <phi_f>
```

### Operator Arguments

#### Implicit Operator Arguments

Each operator accepts a certain number of arguments, however if an argument is not specified then a default value may be used. For example, uses three arguments: an expression that is to evaluated in each cell (, here denoted by a single variable, but more usually an expression of variables), an initial, default expression for the operator (), and the cell centred content region over which the maximum will be calculated (). Using implicit argument notation, operators expect the arguments in a specific order, so expects these three arguments in the manner 

If less than the required number of arguments are passed to an operator,
then a default value for the omitted arguments will be assumed (or if no
defaults are available or are sensible, an error will be flagged). For
example, using

sets to (the largest negative double precision number that the processor
can store) and to if (for example) the expression was being used in a
cell centred context. If in doubt about what the default value for an
argument is, specify it!

#### Explicit Operator Arguments

The alternative to the implicit argument notation is to specify the
arguments explicitly (similar to argument passing in f90). Using
explicit notation the order of the arguments that are passed explicitly
is irrelevant, however the order of any arguments that are not
explicitly named (and hence specified implicitly) still is. For example,
the following will all produce the same result

\
\
\
\

Note in the last case that although was the second argument in the
operator, it was the first implicitly named operator, so would be read
correctly. Using a combination of the implicit and explicit passing is
often convenient. For example, for the operator, the following form that
uses a default value of but performs the maximum comparison over a
specified region is handy

Operator options are similar to variable options. Some operators require
a dimension, and this dimension (direction) is specified via the
options. For example, calculates a gradient in a certain direction
dimension using the divergence of a face centred scalar. To find this
gradient in the second dimension you use the option :

Some options are quite generic (eg, ), however most are specific to the
operator. There is no restriction on the order that options are
specified.

Options and operators should be written in lowercase (I have started to
make both of these case independent, but no guarantees yet).

Details of individual operators follows. Ultimate details of each
operator (including argument order, options etc) can be found in the
code file which shows how they are expanded into working code. Use
search strings such as within the perl file to find the specific code.

### : Divergence

*Summary:* Uses Gauss’ theorem to calculate the divergence of a face
centred vector component around a cell.

*Statement:*

*Centring:*

Operator is context cell centred, while is face centred.

*Details:*

Using Gauss’ theorem to evaluate divergences around cells is probably
the defining characteristic of Finite Volume methods. performs this
operation.

Specifically, to discretise the divergence of a face centred vector
$\vect[j]{u}$ over a cell $i$ that sits within the domain, Gauss’
theorem gives $$\begin{aligned}
\frac{1}{\scali[i]{V}} \int_{\scali[i]{V}} \vect{\nabla} \cdot \vect{u} dV & \Rightarrow \frac{1}{\scali[i]{V}} \sum_{j \in \scali[\text{nobcellfaces},i]{\mathbb{J}}} \frac{1}{\scali[j]{S}} \int_{\scali[j]{S}} \vecti[i,j]{N} \cdot \vecti[j]{u} \, dS \\
& = \explain{\sum_{j \in \scali[\text{nobcellfaces},i]{\mathbb{J}}} \frac{\vecti[i,j]{N} \cdot \vecti[j]{n} }{\scali[i]{V}}}{\normalsize\code{celldiv}} \frac{1}{\scali[j]{S}} \int_{\scali[j]{S}} \vecti[j]{n} \cdot \vecti[j]{u} \, dS \nonumber \\
%= \frac{1}{\scal[cell]{V}} \int_{\scal[cell]{S}} \vect[cell]{n} \cdot \vect{u} dS = \frac{1}{\scal[cell]{V}} \sum_j (\vect[cell]{n} \cdot \vecti[j]{n}) (\vect{u} \cdot \vecti[j]{n}) \scali[j]{S} \nonumber \\
& \Rightarrow \code{celldiv(dot(<u[l=:]>,<facenorm[l=:]>))}
\end{aligned}$$ where $\scali[i]{V}$ and $\scali[j]{S}$ are the volume
and total surface area of the cell $i$ and face $j$, respectively,
$\vecti[i,j]{N}$ is a unit normal pointing outward from cell $i$ but
located at face $j$, $\vecti[j]{n}$ is a normal associated with face
$j$, and the sum is conducted over the set of all face elements that
surround cell $i$, denoted by
$\scali[\text{nobcellfaces},i]{\mathbb{J}}$. In the equivalent coding
the face centred vector $\vecti[j]{u}$ is represented by the three
component variables , and , and the unit normal associated with the face
$j$, $\vecti[j]{n}$, is given by the system component variables , and .
Note that as the divergence of a vector results in a scalar, the above
operation produces a scalar for each cell it is performed in.

The region used by arb in performing the above sum as represented by
$\scali[\text{nobcellfaces},i]{\mathbb{J}}$ is
(‘no-boundary-cell-faces’). This relative region specifies all faces
that surround a given cell, unless that cell is a boundary cell. As
boundary cells are not fully surrounded by faces Gauss’ theorem can not
be applied. Hence, if the operator is used at a boundary cell then the
region is taken relative (moved) to the closest domain cell that is
adjacent the boundary cell, so this is where becomes evaluated.
Physically it is inadvisable to use an equation that involves a
divergence at a boundary cell anyway.

*Options:*

-   : No derivatives with respect to the unknown variables for the
    Newton-Raphson Jacobian are calculated for this operator (and
    its contents).

*Examples:*

    CELL_EQUATION <continuity> "celldiv(<u_f>)" ON <domain> # continuity equation
    CELL_EQUATION <momentum[l=1]> "celldiv(<J_f[l=1]>)" ON <domain> # momentum conservation in direction l=1
    CELL_EQUATION <momentum[l=2]> "celldiv(<J_f[l=2]>)" ON <domain> # momentum conservation in direction l=2

###  or : Gradient

*Summary:* Calculates a scalar component of a gradient over a cell or
face.

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



