---
title: 'regions'
author: Dalton Harvie
date: 5/10/17
---

# Regions

Regions are sets of elements that are used to locate user-defined variables and equations. Each region may contain only mesh elements of the same centring (that is, one of either cell, face or node elements).  Regions may contain elements of different dimensions (see caveat in previous section regarding gmsh display of this though). Regions can be defined by the user directly in gmsh when the mesh is generated, or via statements in the file that are interpreted when is run. There are also several generic system generated regions. Region names follow similar rules to variable names:  They must be delimited by the `<` and `>` characters, and within these delimiters cannot contain the character (sets) `{{`, `}}`, `#` or `&`. Apart from these their names may contain any non-alphanumeric characters.

## Defining regions via gmsh \[sec:gmsh\_regions\]

Regions are specified in gmsh by defining and then naming physical entities. To do this via the gmsh GUI:

-   Add a physical entity (under the physical groups tab) by selecting various elemental entities.

-   Edit the geometry file (using the edit tab) and change the physical entity’s name from the numerical name given by gmsh to the required delimited name suitable for arb.

-   Save the file.

-   Reload the file again (using the reload tab). If you now check under the visibility menu that the physical entity is visible.

You can specify the cell, face or node designation of any gmsh element using the following:
```arb
CELL_REGION <cell_region_name> 
FACE_REGION <face_region_name>
NODE_REGION <noderegion_name>
```
However, this is seldom necessary (although it doesn't hurt either), unless the `.msh` file it is contained within contains multiple domains, of differing dimensions (see discussion in previous section).

## Defining regions within the file

There are several types of region specification statements that can be used in the file. Regions specified by these statements will overwrite any regions defined in the files, however a warning is issued (This allows files to be reread without altering region definition statements).  Regions that are defined within the arb input file are handled by [equation_module_template.f90] (search for `ref: compound`, or `ref: at` etc).

The specification statements are detailed below:

###Compound region:

```arb
CELL_REGION <cell_region_name> "compound(+<region1>+<region2>-<region3>)" ON <parent_cell_region> # comments
FACE_REGION <face_region_name> "compound(<region4>-<region5>)" ON <parent_face_region> # comments
NODE_REGION <node_region_name> "compound(<region6>-<region7>)" ON <parent_node_region> # comments
```

A compound region is defined using other existing regions. All regions that are used in the definition (eg `<region1>`, `<region2>` etc in the above examples) must have the (same) centring that is specified by the keyword.

The contents of the newly formed region is created using sequential addition (union) and subtraction operations. If a `+` sign precedes a region name in the list of regions, then all the mesh elements that are in the following region are added to the new compound region, if they are not already members. If a `-` sign precedes a region name in the list of regions, then all the mesh elements that are in the following region are removed from the new compound region, if they are (at that stage) members of the new compound region. If no sign immediately precedes a region name in the defining list then a `+` sign is assumed. When constructing a compound region the code deals with each region in the defining list sequentially; so whether a mesh element is included in the compound region or not may depend on the order that the regions are listed.

Finally, as per all region definition statements, the resulting compound region can only include elements that are contained within the parent region, specified using `ON <parent_region>`.  If a parent region is not specified, then all of the elements of the applicable centring is implied (ie, `ON <allcells>` for a `CELL_REGION` statement).

###At region:

```arb
CELL_REGION <name> "at(x1,x2,x3)" ON <domain> # comments
FACE_REGION <name> "at(x1,x2,x3)" ON <inlet> # comments
NODE_REGION <name> "at(x1,x2,x3)"
```

This statement defines a region that contains one cell, face or node mesh element. The element chosen lies closest to the point specified by the coordinates `x1,x2,x3`. The coordinate values can be real or double precision floats.

###Within box region:

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

## System generated regions

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


