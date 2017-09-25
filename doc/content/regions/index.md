---
title: 'regions'
author: Dalton Harvie
date: 5/10/17
---

<!-- add ##Gmsh Regions -->

# Regions

Regions are sets of elements that are used to locate user-defined
variables and equations. Each region may contain only mesh elements of
the same centring (that is, either cell or face elements, but not both).
Regions may contain elements of different dimensions (see caveat in
previous section regarding gmsh display of this though). Regions can be
defined by the user directly in gmsh when the mesh is generated, or via
statements in the file that are interpreted when is run. There are also
several generic system generated regions. Region names must be delimited
by the characters, and within these delimiters cannot contain the
characters , , or . Apart from these four characters their names may
contain any non-alphanumeric characters.

### Defining regions via gmsh \[sec:gmsh\_regions\]

Regions are specified in gmsh by defining and then naming physical
entities. To do this via the gmsh GUI:

-   Add a physical entity (under the physical groups tab) by selecting
    various elemental entities.

-   Edit the geometry file (using the edit tab) and change the physical
    entity’s name from the numerical name given by gmsh to the required
    delimited name suitable for arb.

-   Save the file.

-   Reload the file again (using the reload tab). If you now check under
    the visibility menu that the physical entity is visible.

You can specify the cell or face designation of any gmsh element using
the following commands within the file. This is seldom necessary
(although it doesn’t hurt either), unless the file it is contained
within contains multiple domains, of differing dimensions (see
discussion in previous section).

    CELL_REGION <gmsh_region_name> 
    FACE_REGION <gmsh_region_name>

### Defining regions within the file

There are several types of region specification statements that can be
used in the file. Regions specified by these statements will overwrite
any regions defined in the files, however a warning is issued (This
allows files to be reread without altering region definition
statements). The specification statements are:

*Compound region:*

    CELL_REGION <name> "COMPOUND +<region1>+<region2>-<region3>" # comments
    FACE_REGION <name> "COMPOUND <region1>-<region2>" # comments

A compound region is defined using other existing regions. All regions
that are used in the definition (ie, , and in the above examples) must
have the (same) centring that is specified by the keyword. If a sign
precedes a region name in the list of regions, then all the mesh
elements that are in the following region are added to the new compound
region, if they are not already members. If a sign precedes a region
name in the list of regions, then all the mesh elements that are in the
following region are removed from the new compound region, if they are
(at that stage) members of the new compound region. If no sign
immediately precedes a region name in the defining list then a sign is
assumed. When constructing a compound region deals with each region in
the defining list sequentially; so whether a mesh element is included in
the compound region or not may depend on the order that the regions are
listed.

*At region:*

    CELL_REGION <name> "AT x1 x2 x3" # comments
    CELL_REGION <name> "AT x1 x2 x3 PART OF <domain>" # comments
    FACE_REGION <name> "AT x1 x2 x3" # comments
    FACE_REGION <name> "AT x1 x2 x3 PART OF <inlet>" # comments

This statement defines a region that contains one cell or one face mesh
element. The element chosen lies closest to the point . The values , and
can be real or double precision floats. An optional confines the choice
of an element to those within . In this case must have the same centring
as the region statement.

*Within box region:*

    CELL_REGION <name> "WITHIN BOX x1_min x2_min x3_min x1_max x2_max x3_max" # comments
    FACE_REGION <name> "WITHIN BOX x1_min x2_min x3_min x1_max x2_max x3_max" # comments

This statement defines a region including all elements (cell or face)
that lie within a box with faces orientated with the coordinate
directions, and location defined by the two corner points having the
minimum () and maximum () coordinate values. An optional function is
planned.

*Boundary of region:*

    CELL_REGION <name> "BOUNDARY OF <region>" # comments
    FACE_REGION <name> "BOUNDARY OF <region>" # comments

This statement defines a region that contains *only* the boundary
elements (either cell or face) that border the region .

*Domain of region:*

    CELL_REGION <name> "DOMAIN OF <region>" # comments
    FACE_REGION <name> "DOMAIN OF <region>" # comments

This statement defines a region that contains *only* the domain elements
(either cell or face) that are associated with the region .

*Associated with region:*

    CELL_REGION <name> "ASSOCIATED WITH <region>" # comments
    FACE_REGION <name> "ASSOCIATED WITH <region>" # comments

This statement defines a region that contains *both* the domain and
boundary elements (either cell or face) that are associated with the
region . Effectively this is a combination of the and statements.

### System generated regions

The following regions are generated by at the start of a simulation. The
names cannot be used for user-defined regions:

<span>lp<span>10cm</span></span> region name & description\
 & all cells\
 & internal domain cells\
 & cells located on the boundary\
 & all faces\
 & internal domain faces\
 & faces located on the boundary\

Additionally, there are a number of system regions which may be used in
user-written expressions (see section \[sec:language\]) which specify
sets of mesh elements relative to the current position. These names
cannot be used for user-defined regions either:

<span>llp<span>8cm</span></span> region name & rel. to& description\
 & cell & faces that surround the current cell\
 & cell & faces that surround the current cell, unless the current cell
is on a boundary. In that instance move to the neighbouring domain cell
and then cycle around the surrounding face cells.\
 & cell & cells that are local to the current cell (more than just the
adjacent cells)\
 & face & cells that are local to the current face (more than just the
adjacent cells)\
 & cell & cells that are strictly adjacent to the current cell\
 & face & cells that are strictly adjacent to the current face (always
two)\
 & face & cell that is adjacent to the current face in the direction of
the normal\
 & face & cell that is adjacent to the current face in the opposite
direction to the normal\
 & face & the cell that is upwind of the face, used when performing
averaging (see section \[sec:language\]. Not really a user region.\
 & face & the cell that is downwind of the face, used when performing
averaging (see section \[sec:language\]. Not really a user region.\
 & cell & surrounding faces used in a cell averaging kernel (see section
\[sec:language\]. Not really a user region.\
 & cell & surrounding cells used in cell derivative kernels (see section
\[sec:language\]. Not really a user region.\
 & cell & surrounding nodes used in a cell averaging kernels (see
section \[sec:language\]. Not really a user region.\
 & face & surrounding cells used in face averaging and derivative
kernels (see section \[sec:language\]. Not really a user region.\
 & face/cell & dummy region which specifies no elements, or the last
element used in an operator’s context.\

