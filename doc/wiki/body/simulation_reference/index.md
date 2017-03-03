# Simulation setup
<!-- [sec:setup_reference] -->

## arb coding syntax

### Solver code syntax

Most of the arb input file consists of 'solver code', which follows these basic syntax rules:

*  Each line starts with a single 'keyword', which determines the syntax layout for the remainder of that solver code line
*  Lines can be continued with the & symbol (see below)
*  Anything following the # symbol is a comment

So most lines of solver code have the rough form of,
```arb
KEYWORD <a name> "variable or region expression" ON <a region> listofcommaseparatedoptions # trailing comment
```
Most keywords allow only a subset of the possible arguments to be specified at each instance.

By convention keywords are typed in uppercase, however code should parse correctly if the keywords are in either case (although not tested much).  Also by convention variable and region operators (eg 'celldiv' and 'compound') that are used in expressions are typed in lowercase, however again this should parse correctly in either case.  Options are generally typed as lowercase.  Keywords generally contain underscores between (english) words (eg 'END_BLOCK'), while options are the opposite (eg 'newtstepmin').

The continuation syntax using the ampersand symbol (&) is based on the ideas of free-form fortran (<http://fortranwiki.org/fortran/show/Continuation+lines>):

```arb
NONE_CONSTANT <a> & # comments are allowed
 "<b> + <c>" & # on each line, but 
  nooutput # are concatenated together within the unwrapped_input.arb file
NONE_CONSTANT <b> "<a& 

    &> + <c& # blank lines and
# intermediate lines with just comments or starting and ending continuation characters
 &&
>"& # will work too, as per free-form fortran
```

As the '#', '&', '{{' and '}}' character (sets) each have a special meaning in the solver code context, these character sets cannot be used within solver variable or region names.  Hence, the following are all wrong:
```arb
NONE_CONSTANT <a#3> # an error as the comment would start after '<a'
CELL_CONSTANT <a> "celldiv(<a&>)" # an error as the continuation symbol is used in a variable name
INFO_DESCRIPTION "Jack & Jill" # will break the line at &
```
Escaping these characters could be implemented in the future if the burning need arises.


### String code and string variables

#### String code syntax

Lying alongside solver code is 'string code', which is code that is embedded within the solver code and used to

*  set string replacement variables (termed string variables) that perform replacements on the surrounding solver code, and also to
*  generated solver code directly.

String code is delimited by {{ and }}.  The basic syntax rules of string code are:

*  string replacements are not performed on string code
*  string code can be placed anywhere within the solver code, EXCEPT in
     -  comments (where it is just a comment)
     -  legacy string replacement code (eg, 'REPLACE '<<a>>' WITH '1')
*  there is no limitation to the characters used within string code, EXCEPT for }}, which even when enclosed within a string will terminate the string code
*  needs to be valid perl code

String code is interpreted directly by perl, evaluated using the 'eval' (string) function (<http://perldoc.perl.org/functions/eval.html>).  For all intents and purposes, whatever is enclosed within {{ and }} is run as a perl subroutine, within the (scope of the) StringCode.pm module of setup_equations.pl, and whatever is returned from that eval (think subroutine) is substituted into the solver code in place of the string code.  So the following string code
```arb
{{ return "TRANSIENT_SIMULATION" }}
```
becomes the following solver code when arb is run,
```arb
TRANSIENT_SIMULATION
```
Note that if no return value is specified then any string code block evaluates as an empty string, and hence has no (immediate) effect within the solver code:
```arb
NONE_CON{{ my $a = 1; }}STANT <a> 1.d0 # the solver code keyword becomes NONE_CONSTANT here
```
The file unwrapped_input.arb within the setup_data output subdirectory is generated whenever arb is run, and contains the input file with the string code 'unwrapped' - that is, pure solver code.  The unwrapped_input.arb file can infact be used as a subsequent input to arb.

A print statement within a string code block is printed to the standard output (the screen, or output.scr), and not to the solver code:
```arb
{{ print "INFO: this message will appear on the screen\n"; }}
```

Comments and line continuations are allowed in the string code, in the same way that they are allowed in perl code:

```arb
{{
# new lines and comments within string code follow perl rules, as the code is parsed as perl code

  my $a = 1; # set a local variable to 1
  my $b = $a*3; # setting another local variable

  return $b; # we are returning the value of $b here

}}
```

Note that the & line continuation symbol is not used in string code (no line continuation is required in perl), and that commands need to finish with a semi-colon (;) otherwise quite criptic error messages can result.

#### Local variables versus string variables

As each string code block is evaluated using a separate perl eval call, local variables defined within one string code block are not available within subsequent string code blocks:
```arb
{{
  my $a = 1; set a local variable to 1, noting that 'use string' and 'use warnings' are turned on in perl
  print "$a\n"; # prints the value 1 to the screen
}}
{{
  print "$a\n"; # generates an error as $a is not defined within the scope of this string code block
}}
```

Instead, string variables can be used to pass information outside the scope of a particular string code block.  The perl subroutine 'string_set' is available to set the value of a string variable, and 'string_eval' to return its value.  The following example could be instead achieved using:
```arb
{{
  string_set('$a',1); # set a string variable '$a' to 1
  print string_eval('$a')."\n"; # prints the value 1 to the screen
}}
{{
  print string_eval('$a')."\n"; # prints the value 1 to the screen
}}
```
String variables are also used to perform string replacements within the solver code (detailed below), however, by default if a string variable name starts with '$', the string variable is given the noreplace option which means that the solver code is not searched/replaced for this particular variable.  This option can also be explicitly set at creation time or via the 'string_option' subroutine:

```arb
{{ string_set('<<a>>','1.d0',"replace"); # three arguments are required to set an option using sub string_set }}
{{ string_option('<<a>>','noreplace'); }}
```

One 'gotcha' here is that perl will interpret "$a" as the perl variable $a within double quotations, so to refer to a string variable with a name starting with $ you need to use single quotations, as in '$a'.

Strings variables can also be deleted, and if set without a value, imply the empty string.  Pairs of arguments given to string_set are interpreted as name/value pairs to set:
```arb
{{
  string_set('<<c>>'); # strings set with no value implies ''
  string_delete('<<c>>') # strings can also be deleted
  string_set('<<d>>','1.d0','<<e>>','2.d0') # two strings set here
  string_set('<<f>>','1.d0','<<g>>','2.d0',"noreplace") # two strings set here, both having the noreplace option
}}
```

Two other options are allowed in string_set.  A 'global' option means that rather than setting the string variable within the current local code block (see below), the variable is set within the root (that is, global or first-defined) code block.  This means that even when the current code block is destroyed, a global string variable remains.  Global string variables are useful for setting generic simulation options, and there are a number of system global string variables that are automatically set for each simulation (see ref: string system variables in src/setup_equations/StringCode.pm).  See the Code blocks section below for an example of global variable use.

A 'default' option passed to string_set means 'only set this variable if it is not already set'.  This is useful for determining whether upstream code has already specified an option, and if not, assume a default value.  When 'default' is specified the set operation is only performed if the string is not already defined.  So other options used in conjunction with 'default', such as 'global', 'replace' and 'noreplace', have no effect if the string has already been set.
```arb
{{
  string_set('<<a>>','1.d0');
  string_set('<<a>>','2.d0','default,global,noreplace'); # the global and noreplace options are not used here as <<a>> is already a replace and local (opposite of global) variable
  string_set('<<b>>','2.d0','default,global,noreplace'); # here <<b>> becomes a global and noreplace variable assuming that it is not already set
}}
```

String code allows more flexibility in coding arb, and will be employed more in the template files.  Looping is possible now using (for example);
```arb
{{
  my $code = '';
  for my $n in ( 0 .. 2 ) {
    $code .= "CONSTANT <a".$n."> $n.d0\n";
  }
  return $code;
}}
```
which would evaluate as the solver code
```arb
CONSTANT <a0> 0.d0
CONSTANT <a1> 1.d0
CONSTANT <a2> 2.d0
```
A typical use is to create a variable definition for a vector, as in 
```arb
# this demonstrates expanding a vector using native perl
{{ my $code; foreach my $l ( string_eval('<<dimensions>>','list') ) { $code .=
  "CELL_DERIVED <phi_normal_c[l=$l]> 'celldivgrad[l=$l](<phi_f>)' ON <allcells>\n";
}; return $code; }}
```
There are also functions to automate the loop process for expanding vector and tensor expressions, also based on the dimension range defined in the global string variable '<<dimensions>>':
```arb
# this expands two expressions to become vectors, replacing $l instances
{{ return vector_expand(
  'CELL_DERIVED <phi_normal_c[l=$l]> "celldivgrad[l=$l](<phi_f>)" ON <allcells>',
  'CELL_DERIVED <phi_normal_c2[l=$l]> "celldivgrad[l=$l](<phi_f>)" ON <allcells>'
); }}
# this does the equivalent for a tensor, replaceing $l1 and $l2 instances
{{ return tensor_expand(
  'CONSTANT <tens[l=$l1,$l2]> "$l2 + $l1"'
); }}
```
For the above the expressions need to be single quoted.

With some perl hackery there's lots that can be done if required - eg, arrays and hashes stored within the string variables (but you'll have to learn some perl!).  Variables defined within setup_equations.pl can also be accessed within the string code, as can generic perl subroutines.
```arb
{{
  print "$::transient_simulation\n"; # is a variable defined in main of setup_equations.pl which indicates whether this is a transient simulation or not
  if (!($::transient_simulation)) {
    print "WARNING $ENV{USERNAME}: did you want to run a transient simulation?\n"; # not tested, but should work...
  }
}}
```

### Include statements, string replacements and code blocks

#### Code blocks

Code blocks are separate 'chunks' of code that define the scope of the string variables, as well as the file include paths.  Code blocks can be defined within the one arb file using the 'BLOCK' and 'END_BLOCK' keywords, as in:
```arb
BLOCK
{{ string_set('$a','1.d0'); }}
{{ string_set('$b','2.d0',"global"); }}
{{ print string_eval('$a'); }} # prints 1.d0
{{ print string_eval('$b'); }} # prints 2.d0
END_BLOCK
{{ print string_eval('$a'); }} # error as $a is no longer defined in the parent block
{{ print string_eval('$b'); }} # prints 2.d0 as $b was defined as a global variable
```

#### Include statements

Include statements allow other input files to be included.   These files can be user written, or be from a library of template files within the templates directory.  String replacements (or substitutions) that occur as the file is read in allow these included files to be (effectively) used as functions.
```arb
INCLUDE_TEMPLATE "navier_stokes/complete_equations"
```
The trailing '.arb' file extension is not required within an include statement.

A new code block is started whenever a new file is included:
```arb
INCLUDE_LOCAL "some_arb_code" # a new block is created after the string "some_arb_code" has been read
```
This means that any string variables that are defined within the included file are not available outside of that file (except in its included children), unless these string variables are defined as global.

String code that follows an include statement is included as the first statement in the newly created block:
```arb
INCLUDE_LOCAL "another_arb_code" {{ string_set("<<mu_f>>","<<mu>>"); }} # so the replacement is done on the contents of another_arb_code.arb
```
This allows the template files to be used as functions or called repeatedly with different arguments.

There are several forms of the include statement:

TODO

: choose a template directory to look for any files included via any following statements. If no string is specified, then the include root directory name is set equal to that of its parent (including) file. If the blank string is specified, then is set to the blank string and all templates directories (up to two subdirectory levels) are searched for the following included files. The definitions are hierarchical, in that the definition in a child file does not affect that of the parent.  -   : command to include a file from the most recent directory (or subdirectory thereof), possibly also specifying file-specific string replacements using the syntax (or the shorter ). If an directory has not been specified (or cancelled with a blank statement) then the templates directories will be searched until a matching file is found (up to two subdirectory levels right now).  -   : include the following files from the working directory. This command does not affect and is not influenced by the directory and is (basically) used to include sets of user-written statements (i.e., like a local a function).  Partnering the include file capability is the ability to read in multiple definitions for the same variable. The ultimate position of a variable’s definition is that of the first definition for that variable.  The ultimate expression used for a variable is that given (read in) last. This functionality allows a variable’s expression to be changed from what is used in (say) a template file by specifying a new definition lower in the file, after the template file include statement.  Options can also be added to previously specified options for a variable by including more definition statements (that may only contain options and not expressions) lower in the input file. Similarly for units.  There are two types of string replacements that occur when a line from an input file is parsed: i) file-specific replacements, which occur recursively through ‘child’ file inclusions, and ii) general replacements, which occur throughout all files from their point of definition onwards, until (possibly) cancelled. The following demonstrates a general replacement statement specifying two general replacement strings, using a long and short form: There are certain system generated general replacements that occur automatically unless specifically changed by the user. Use the search hint to find the list of these in .  The above demonstrates how to cancel a search string replacement, using either a long () or short () form. Note that both general and file specific replacements do not occur on a line of an input file if the line is itself a general replacement definition line (specifically, it begins with the keyword), or is an include line for a file (begins with some type of keyword).




If statements
-------------

The following solver code shows an if statement:

```arb
IF 1
  NONE_CONSTANT <a> 1.d0
END_IF
```

Code flow through an IF statement is dependent on what follows the 'IF' keyword:  An empty string or '0' evaluates as false, whereas anything else is true.  So the following are true,
```arb
IF 1
IF asd afsdf sdg sdgfas # some comment
IF 0 adfd
IF "0" # note this!
```
whereas the following are false
```arb
IF 0
IF # possible trailing comment
IF
IF&
  & # line continuation
```

There are also the continuation statements of ELSE_IF and ELSE demonstrated by:
```arb
IF 0
  NONE_CONSTANT <a> 1.d0
ELSE_IF 1
  NONE_CONSTANT <a> 2.d0
ELSE
  NONE_CONSTANT <a> 3.d0
END_IF
```
IF, ELSE_IF, ELSE and END_IF are all solver code keywords, so require their own lines.

In practice the IF statements would be used in conjunction with string replacements or string code:
```arb
IF <<transientflag>> # here <<transientflag>> is a system generated string variable which is replaced by 1 for a transient simulation
  CONSTANT <dt> 0.1d0
ELSE_IF {{ return string_eval('$some_string_variable') }} # here the string code enclosed in {{ }} evaluates as the return value, which here would be the value of the string variable $some_string_variable which would have been previously set
  CONSTANT <dt> 1.d0
ELSE
  CONSTANT <dt> 2.d0
END_IF
```

Note that code within an if 'block' is evaluated as a separate code block, so to make a local string variable available after the if block you would need to either declare it before the block, or declare it as a global variable:
```arb
{{ string_set('$a'); }} # a set with no other argument sets the variable to the empty string, but includes it in this code block
IF 1 # a new code block is created here
  {{
      string_set('$a','1.d0');
      string_set('$b','2.d0','global');
      string_set('$c','1.d0');
  }}
END_IF
{{ print "a = ".string_eval('$a').": b = ".string_eval('$b')."\n"; }} # this will work
{{ print "c = ".string_eval('$c')."\n"; }} # this won't work as the code_block in which $c was defined has been destroyed
```


Meshes
------

arb uses an unstructured mesh composed of cell elements that are
separated by face elements. The dimension of each element is specified
on a per-element basis, consistent with the particular computational
domain that the element is within (that is, not globally). Cell elements
are classified as either boundary cells (that is, on the boundary of a
domain) or domain cells (that is, contained within a domain). Domain
cells have a dimension that is equal to that of the domain they are in
(ie, dimension 3/2/1 if the domain is a volume/surface/line, resp.).
Boundary cells have a dimension that is one less than that of the
associated domain (ie, dimension 2/1/0 if the domain is a
volume/surface/line, resp.). Face elements are any elements that
separate cell elements. Face elements also have a dimension that is one
less than that of the domain they are within. Some face elements are
specified explicitly within a file (if they are part of a physical
entity such as for example), while the remainder are generated by arb
when the mesh is read in. Face elements are also classified as being
either domain faces or boundary faces. Each boundary face has the same
geometry, and is conincident with, a boundary cell. Hence, a mesh has
the same number of boundary faces as boundary cells.

Meshes are read in from files, generally produced by the gmsh program.
Multiple files can be read in by arb for each simulation. arb has been
coded to be able to handle any poly-sided first order elements supported
by the gmsh file format. It has been tested to date (v0.3) with
tetrahedron, boxes and prisms in 3D, triangles and rectangles in 2D,
lines in 1D and points in 0D. Tetrahedron, triangles, lines and points
are the default element geometries created by gmsh.

Meshes and data are also exported by arb using the format. During every
simulation all domains and all output-enabled data will be written by
default to the file. Other files may also be written, corresponding to
any files that are read in (with any associated output-enabled data).
Regions imported from files as well as regions created by arb will be
exported to any written files, however note that as the physical
entities handled by gmsh can only have a single dimension, elements that
have a dimension that is less than any others within a region will not
be associated with that region in any arb-created files. This is
relevant for example when a compound region is created that contains
both domain and boundary cells. When this arb-written file is displayed
by gmsh it will only appear to contain the domain cells.

### Cell and face element specification

The distinction between cell elements and face elements is not made by
gmsh or contained explicitly in the file, but rather must be made by arb
when a file is read in. Gmsh’s behaviour is to only write an element to
a file if it is a member of a physical entity. Further, each physical
entity has a single dimension. So, to decide whether an element is
either a face or cell element, arb does two things when reading in each
file:

1.  The maximum dimension of all physical entities with the file
    is found. This is stored as the dimension of the particular mesh;

2.  When an element is read in that has the same dimension as that of
    the mesh, it is regarded as a cell element. If it has a dimension
    that is one less than that of the mesh, it is regarded as a
    face element. If it has a dimension that is two or more less than
    that of the mesh, then the element is ignored.

So what’s the implication of all this? Generally arb will be able to
work out from each file which elements within it are cell elements and
which are face elements. The only time it won’t is when *there are
multiple domains having different dimensions contained within the one
file*. For example, you have both a volume domain and a surface domain
specified within a file, on which you want separate (but possibly
linked) sets of equations solved.

If you do have multiple domains having different dimensions contained
within the one file then the dimension of all regions (that is, physical
entities) contained within the file that belong to any domains that have
a dimension that is *less than* that of the file need their centring
explicitly specified. For example, if a file contains both a volume and
a surface domain, then all regions associated with the surface domain
must have their centring explicitly specified. Statements for specifying
this cell/face centring for particular regions (gmsh physical entities)
are described in section \[sec:gmsh\_regions\]. Alternatively, there is
another way that may work for your simulation: As arb can read multiple
files for each simulation, it may be easier to place domains of
different dimensions in separate files. The cell/face specification will
then be handled automatically without additional statements in the file
(If you want multiple domains to share common mesh features however this
may be difficult to accomplish using gmsh).

### Data and mesh file rereading

The files written by can contain data. Variables associated with cell
elements can be written in either (a uniform value for each cell) or
(values vary linearly within each cell) gmsh formats. Variables
associated with face elements will only be written in format. Variables
which are centred are written using a special format which gmsh won’t
display. Note that face and cell boundary elements are not written
separately to each file by arb, but rather as a single element. Hence
both cell and face boundary data is associated with a single element in
each file.

One purpose of exporting data to a file is to provide initial conditions
for another (or next) simulation. In this case generally you just have
to specify the file from the previous simulation as the mesh file to be
read in for the next simulation. Note that each arb-written file
contains all the information about a mesh that was originally contained
in the mesh-only gmsh-written file: Hence, when starting a simulation
from an arb-written datafile is it not necessary (nor does it make
sense) to also read in the original gmsh file. Also note that data files
that contain variable values can only refer to mesh elements that are
specified in the same data file (unless some fancy magic is worked in
your equations).

### Mesh read and write options

Mesh and data input and output is specified by statements within the
file:

    MSH_FILE "msh_file_name_including_path" comma,separated,list,of,options # comments

The file name refers to the read location. Options for the default file
should be referred to by (which is the read location if it did exist).
If a file is to be written it will always be written to the directory.
As a result, all file basenames must be unique.

Three types of options are available for each file:

*Output options:*

These options specify what information is to be written to the file.

-   : Both a mesh and all specified variables will be written.

-   : Both a mesh and all specified variables will be written. Output
    will be split between three files, each containing variables of only
    a single centring (cell, face and none). This can be handy for gmsh
    compatibility when doing cutgrid and streamtrace operations
    for example.

-   : Only the mesh is written, split between three files as above.

-   : Only the mesh is written.

-   : Neither the mesh or any data will be written.

-   : Same as the file options, but for output, compatible with ParaView
    (for example). The default is .

-   : Same as the file options, but for output, compatible with Tecplot.
    The default is .

By default all meshes have the option specified, with the exception of
the mesh, which has option .

*Input options:*

These options specify what information is to be read from the file.

-   : Both a mesh and all relevant data will be read.

-   : Both a mesh and all relevant data will be read. In this case the
    existing is split into three, each containing variables of only a
    single centring (cell, face and none) as output from a previous
    simulation employing . In this case the filename should be specified
    without the centring, for example (rather than ).

-   : Only the mesh is read, split between three files as above.

-   : Only the mesh is read.

-   : Neither the mesh or any data will be read.

By default all meshes have the option specified, with the exception of
the mesh, which has option .

*Data format options:*

These options specify how cell centred data will be written to the file.
These options overwrite any data format options specific to individual
variables.

-   : All cell data will be written using the format.

-   : All cell data will be written using the format.

-   : All cell data will be written using the format, but with the
    gradients in each cell limited so that each vertex value is bounded
    by surrounding cell values.

By default all meshes have no data format options specified, the format
instead being determined by the options contained in the individual
variable definitions within input file.

Regions \[sec:regions\]
-----------------------

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

Variables
---------

<span> This section needs some rewritting: there is only one input file
now</span>

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

Simulation options
------------------

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

Kernel options
--------------

There are many options that can be used to change the kernels used. For
example

specifies that when averaging/differentiating quantities to/at faces,
ensure that a second order polynomial would be reproduced precisely.

Glued boundaries
----------------

Used to implement periodic or reflection boundaries by glueing two
boundary face regions together. Boundary regions to be glued must have
the same element structure (size and number). Individual element
matching between the boundaries is accomplished by matching the closest
element locations, relative to the region centroids (much like the and
operators).

Example of a periodic boundary glueing the top and bottom boundaries of
a domain:

Example of a reflection (axis of symmetry) boundary along the left side
of a domain:

In the case of reflection, certain operators (eg, ) need to be aware
when they are operating on the component of a vector, that needs to be
reflected over this reflection boundary. See the options for each
operator.

Simulation Info
---------------

The following strings can be used within an input file to help keep
track of what the file contains. These and other automatically generated
info strings are included as comments in most of the output files.


