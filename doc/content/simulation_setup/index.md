---
title: 'simulation setup'
author: Dalton Harvie
date: 6/12/16
---

# Simulation setup

## arb file syntax

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

By convention keywords are typed in uppercase, however code should parse correctly if the keywords are in either case (although not tested much).  Also by convention variable and region operators (eg `celldiv` and `compound`) that are used in expressions are typed in lowercase, however again this should parse correctly in either case.  Options are generally typed as lowercase.  Keywords generally contain underscores between (english) words (eg `END_BLOCK`), while options and operators are the opposite (eg `celltofacelink` and `newtstepmin`).

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



### String variables and solver code string statements

'String variables' are used to perform replacements on the arb solver code as it is read in (parsed) by [setup_equations.pl].  String variables can be set using either solver code string statements, or via (the more complex but powerful) [embedded perl code](#embedded-perl-code) (see below).  The main purpose of string variables is to allow 'chunks' of arb code to be tailored to specific applications.  The meaning of this will become clearer when discussing [include statements](#include-statements) later.

An example string statement is the following:
```arb
REPLACEMENTS REPLACE "a string" WITH "another string"
```
This will cause all occurrences of the string 'a string' in the remainder of the current code block (see below) to be replaced by the string 'another string', except within other solver code string statements and within any [embedded perl code](#embedded-perl-code).  A more realistic example is
```arb
REPLACEMENTS REPLACE "<c>" WITH "<c1>"
```
which would replace subsequent references to the variable `<c>` to be replaced with references to the variable `<c1>`.

The string statement has a number of variations.  The word `DEFAULT` can be used in place of `REPLACE`.  In this case the string variable will only be set if it hasn't been set already.  The default form is often used within template files to assume a default value of a variable if it has not already been set within the calling file.
```arb
REPLACEMENTS DEFAULT "a string" WITH "another string" # 'DEFAULT' means only set this string if it isn't already set
```
To remove a string variable assignment the `CANCEL` keyword can be used, as in
```arb
REPLACEMENTS CANCEL "a string" # will cancel a previously set string replacement
```
If the variable has not already been set then a cancel statement issues a warning but is otherwise ignored.

The scope of a standard string replacement variable is all code within the remaining code block (including any descendant blocks).  The precise definition of a [code block](#code-block) is detailed below, however most commonly a code block corresponds to the contents of the current arb code file.  Hence, if an arb code file 'A.arb' includes a code file 'B.arb', and a standard string variable is defined in code file 'B.arb', then this string variable will not be defined back in (the remainder of) code file 'A.arb'.  In other words, the scope of a standard string variable is only the remainder of the code block in which the variable is defined and this block's children, and not the parents of the defining code block.  For this reason standard string variables are known as local string variables.

The alternative to a local string variable is a global string variable.  The scope of a global string variable is all code blocks (actually the grandparent of all code blocks), so a global string variable defined in file 'B.arb' in the previous example will be used to replace strings in the remainder of file 'B.arb' as well as within the remainder of its calling file 'A.arb'.  Global variables are set using the `GLOBAL_REPLACEMENTS` keyword (or the legacy version of this, being `GENERAL_REPLACEMENTS`):
```arb
GLOBAL_REPLACEMENTS REPLACE "a string" WITH "a better string" # this replacement will occur in all code blocks subsequent to this statement
GENERAL_REPLACEMENTS REPLACE "a string" WITH "a better string" # GENERAL_REPLACEMENTS is a legacy and entirely equivalent form of the GLOBAL_REPLACEMENTS keyword
```

In terms of how these scoping rules are implemented, string variables are each associated with a specific code block, with replacements performed using any string variable that is defined within the current code block or its parents (see `@code_blocks[]{"string_variables"}` array hash in [StringCode.pm]).  Global string variables are associated with the first code block so are available for replacement in all subsequently formed code blocks.

Finally, there are short forms of the string setting statements illustrated by
```arb
REPLACEMENTS R "a string" W "another string" # R is a short form of REPLACE, and W is a short form of WITH
REPLACEMENTS C "a string" # C is a short form of CANCEL
REPLACEMENTS D "a string" W "another string" # and 'D' is short for 'DEFAULT'
```
Also, multiple strings may be defined on the one line, illustrated by
```arb
GLOBAL_REPLACEMENTS R "string1" W "another string" R "string2" W "another string2" # multiple replacements can be specified on the one line
```

<!-- TODO: separate pandoc call into -B and -A html headers, possibly with separate css option -->


<!-- ### String variables and embedded perl code -->

###Embedded perl code {#embedded-perl-code}

####Embedded code syntax

Lying alongside solver code is embedded perl code, which is code that is embedded within the solver code and used to

*  set and manipulate string variables, and to
*  generated solver code directly.

Embedded perl code is more powerful in setting and manipulating string variables that solver code string statements.  It can also be used to produce 'bespoke' solver code.

Embedded perl code is delimited by {{ and }}.  The basic syntax rules of embedded code are:

*  string replacements are not performed on embedded code
*  embedded code can be placed anywhere within the solver code, except in
     -  comments (where it is just a comment)
     -  solver code string statements (eg, `'REPLACE '<<a>>' WITH '1'`)
*  there is no limitation to the characters used within embedded code, EXCEPT for }}, which even when enclosed within a string will terminate the embedded code
*  needs to be valid perl

Embedded perl code is interpreted directly by perl, evaluated using the 'eval' (string) function (<http://perldoc.perl.org/functions/eval.html>).  For all intents and purposes, whatever is enclosed within {{ and }} is run as a perl subroutine, within the (scope of the) [StringCode.pm] module of [setup_equations.pl], and whatever is returned from that eval (think subroutine) is substituted into the solver code in place of the embedded code.  So the following embedded code
```arb
{{ return "TRANSIENT_SIMULATION" }}
```
becomes the following solver code when arb is run,
```arb
TRANSIENT_SIMULATION
```
Note that if no return value is specified then any embedded code block evaluates as an empty string, and hence has no (immediate) effect within the solver code:
```arb
NONE_CON{{ my $a = 1; }}STANT <a> 1.d0 # the solver code keyword becomes NONE_CONSTANT here
```
The file `unwrapped_input.arb` within the setup_data output subdirectory is generated whenever arb is run, and contains the input file with the embedded code 'unwrapped' - that is, pure arb solver code.  The `unwrapped_input.arb` file can infact be used as a subsequent input to arb.

A print statement within an embedded code block is printed to the standard output (the screen, or `output.scr`), and not to the solver code:
```arb
{{ print "INFO: this message will appear on the screen\n"; }}
```

Comments and line continuations are allowed in embedded perl code, in the same way that they are allowed in perl:

```arb
{{
# new lines and comments within embedded code follow perl rules, as the code is parsed as perl

  my $a = 1; # set a local variable to 1
  my $b = $a*3; # setting another local variable

  return $b; # we are returning the value of $b here

}}
```

Note that the & line continuation symbol is not used in embedded code (no line continuation is required in perl), and that commands need to finish with a semi-colon (;) otherwise quite criptic error messages can result.

#### Using string variables within embedded perl code

As each embedded code chunk is evaluated using a separate perl eval call, perl variables defined within one embedded code chunk are not available within subsequent chunks, as in:
```arb
{{
  my $a = 1; set a local perl variable to 1, noting that 'use string' and 'use warnings' are turned on in perl
  print "$a\n"; # prints the value 1 to the screen
}}
{{
  print "$a\n"; # generates an error as $a is not defined within the scope of this string code block
}}
```

Instead, the previously discussed string variables can be used to pass information between subsequent embedded code chunks.  The perl subroutine 'string_set' is available to set the value of a string variable, and 'string_eval' to return its value.  The following example could be instead achieved using:
```arb
{{
  string_set('$a',1); # set a string variable '$a' to 1
  print string_eval('$a')."\n"; # prints the value 1 to the screen
}}
{{
  print string_eval('$a')."\n"; # prints the value 1 to the screen
}}
```
String variables are also used to perform string replacements within the solver code (detailed above), however, by default if a string variable name starts with '$', the string variable is given the noreplace option which means that the solver code is not searched/replaced for this particular variable.  This option can also be explicitly set at creation time or via the 'string_option' subroutine:

```arb
{{ string_set('<<a>>','1.d0',"replace"); # three arguments are required to set an option using sub string_set }}
{{ string_option('<<a>>','noreplace'); }}
```
Note that the embedded code `string_set` function is called internally whenever a solver code string set statement is parsed.  Hence the following statements are equivalent:
```arb
{{ string_set('<<a>>','1.d0');
REPLACEMENTS R "<<a>>" W "1.d0"
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
The `string_delete` subroutine is called internally by the solver code string `CANCEL` statement.

Two other options are allowed in string_set.  A `global` option means that rather than setting the string variable within the current local code block (see below), the variable is set within the root (that is, global or first-defined) code block.  This means that even when the current code block is destroyed, a global string variable remains.  Global string variables are useful for setting generic simulation options, and there are a number of system global string variables that are automatically set for each simulation (see ref: string system variables in [StringCode.pm]).  See the Code blocks section below for an example of global variable use.  Using the `global` option in `string_set` is equivalent to using a `GLOBAL_REPLACEMENTS` solver code string statement.

A `default` option passed to string_set means 'only set this variable if it is not already set'.  This is useful for determining whether upstream code has already specified an option, and if not, assume a default value.  When `default` is specified the set operation is only performed if the string is not already defined.  So other options used in conjunction with `default`, such as `global`, `replace` and `noreplace`, have no effect if the string has already been set.
```arb
{{
  string_set('<<a>>','1.d0');
  string_set('<<a>>','2.d0','default,global,noreplace'); # the global and noreplace options are not used here as <<a>> is already a replace and local (opposite of global) variable
  string_set('<<b>>','2.d0','default,global,noreplace'); # here <<b>> becomes a global and noreplace variable assuming that it is not already set
}}
```

####Embedded code purpose

Embedded perl code allows more flexibility in coding arb, but is (probably) for more advanced uses.  For example looping is possible using embedded code;
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
There are also functions to automate the loop process for expanding vector and tensor expressions, also based on the dimension range defined in the global string variable `<<dimensions>>`:
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
For the above the expressions need to be single quoted.  By default `<<dimensions>>` is set to `1,2,3`, so if a reduced number of dimensions is required this global variable needs to be explicitly set, as in
```arb
{{ string_set('<<dimensions>>','1,3','global'); }} # for a 2D simulation in the xz plane
```
Note that template files are available to set commonly used coordinate systems.

With some perl hackery there's lots that can be done if required using embedded code - eg, arrays and hashes stored within the string variables (but you'll have to learn some perl!).  Variables defined within [setup_equations.pl] can also be accessed within the string code, as can generic perl subroutines.
```arb
{{
  print "$::transient_simulation\n"; # is a variable defined in main of setup_equations.pl which indicates whether this is a transient simulation or not
  if (!($::transient_simulation)) {
    print "WARNING $ENV{USERNAME}: did you want to run a transient simulation?\n"; # not tested, but should work...
  }
}}
```

### Code blocks

Code blocks are separate 'chunks' of code that define the scope of the string variables, as well as the scope of file include paths.  Code blocks can be defined within the one arb file using the `BLOCK` and `END_BLOCK` keywords, as in:
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

More commonly code blocks are defined when including other files and when parsing if statements.

### Include statements

#### Include statement overview

Include statements allow other input files to be included.   These files can be user written, or be from a library of template files within the [templates directory](#templates_dir).  As an example, 
```arb
INCLUDE_TEMPLATE "navier_stokes/complete_equations"
```
will include the file `complete_equations.arb` from the `templates/navier_stokes` directory, replacing this include statement with the contents of the `complete_equations.arb` file.  Similarly 
```arb
INCLUDE "my_function" # this could be located in a variety of locations searched through
```
would include the arb file `my_function.arb` from the (eg) user's working directory.  Note that a trailing '.arb' file extension is implied within all include statement (and hence not required).

A new code block is started whenever a new file is included:
```arb
INCLUDE_LOCAL "some_arb_code" # a new block is created after the string "some_arb_code" has been read
```
This means that any string variables that are defined within the included file are not available outside of that file (except in its included children), unless these string variables are defined as global.

String replacements (or substitutions) that occur as the file is read in allow included files to be (effectively) used as functions.  More specifically, string code that follows an include statement is included as the first statement in the newly created block:
```arb
INCLUDE_LOCAL "another_arb_code" {{ string_set("<mu_f>","<mu>"); }} # so the replacement is done on the contents of another_arb_code.arb
```
This allows the template files to be used as functions or called repeatedly with different arguments.  An equivalent solver code string set statement can also be used, as in
```arb
INCLUDE_LOCAL "another_arb_code" R "<mu_f>" W "<mu>" # is equivalent to the last case
```

<!--Where the code searches for any included file depends on the specific include statement used, as well as what current paths are listed on an include path 'stack'.  These 'stacks' or list of include paths are associated with each code block (see `@{$code_blocks[$#code_blocks]{"include_path"}}` in [ReadInputFiles.pm]).  When a -->

#### Include statement types

The most basic form of the include statement is
```arb
INCLUDE "filename"
```
For this statement, the code searches (backwards) through a list of 'include paths' to find the named file `filename.arb` and replaces the include statement with the contents of that file.

The include path list which is used to find the file is stored as a 'stack' of paths associated with the current code block (see `@{$code_blocks[$#code_blocks]{"include_path"}}` in [ReadInputFiles.pm], `ref: include_paths`).  The include path stack grows and shrinks via the following rules (see `sub push_code_block` within [ReadInputFiles.pm]):

* When a new code block is formed by opening a new file, the last include path from the stack of the previous code block is added as the first path in the new include path stack.  This will correspond to the location of the included file:  Hence, the first path in any code block path stack will correspond to the location of the file that is currently being read.
* When a new code block is formed without opening a new file (eg, by an `BLOCK` or `IF` statement) then the entire include path stack from the parent code block is copied to the new code block.  Hence, again, the first path in the code block stack will correspond to the location of the currently read file.
* Every time a new file is included, AND if the current upper-most include path does not correspond to the location of the new file, then the file's location is added to the include path stack.
* An empty `INCLUDE` keyword (ie, with no trailing filename) removes the upper-most include path from the current code block include path stack, unless there is only the first path (and hence current file location) left on the stack (in which case a warning is issued).

Other include statements search in specific locations for the specified files and/or directories:

* `INCLUDE_TEMPLATE "template_directory/filename"`: search through the [templates directory](#templates_dir) looking for a `filename.arb` contained within a specific `template_directory`.  When the file is found, the path to the file is added to the include path stack of the current (calling) code block.  If a directory name is specified but not a filename, as in
```arb
INCLUDE_TEMPLATE "template_directory"
```
then `template_directory` is searched for within the [templates directory](#templates_dir) and its location is added to the include path stack of the calling code block.  The `INCLUDE_TEMPLATE` include statement is unique in that the [templates directory](#templates_dir) is searched through recursively (depth prioritised search) to find the specified directory and filename.

* `INCLUDE_ABSOLUTE "/an/absolute/path/and/filename"`:  This include statement includes a file using its absolute path.  The first `/` is optional.  The path to the file will also be added to the current code block's include path stack.  If only a directory name is given then no file is included, but the path is still added to the current code block's include path stack.

* `INCLUDE_LOCAL "path/relative/to/calling/file/filename"`:  This include statement is similar to `INCLUDE_ABSOLUTE` but the path to the file is relative to the location of the file from which it is being included.  If the path starts with `/` however the path is assumed to be absolute.

* `INCLUDE_LAST "path/to/filename"`:  This include statement is similar to `INCLUDE_LOCAL` but the path to the file is relative to the last include path on the current code block's stack.

* `INCLUDE_WORKING "path/to/filename"`:  This include statement is similar to `INCLUDE_LOCAL` but the path to the file is relative to the user's working directory, being the directory from which the `arb` command was run.

* `INCLUDE_ARB "path/to/filename"`:  This include statement is similar to `INCLUDE_LOCAL` but the path to the file is relative to the current arb directory, being the directory which contains the current `bin/arb` script.

<!--: command to include a file from the most recent directory (or subdirectory thereof), possibly also specifying file-specific string replacements using the syntax (or the shorter ). If an directory has not been specified (or cancelled with a blank statement) then the templates directories will be searched until a matching file is found (up to two subdirectory levels right now).  -   : include the following files from the working directory. This command does not affect and is not influenced by the directory and is (basically) used to include sets of user-written statements (i.e., like a local a function).  Partnering the include file capability is the ability to read in multiple definitions for the same variable. The ultimate position of a variable’s definition is that of the first definition for that variable.  The ultimate expression used for a variable is that given (read in) last. This functionality allows a variable’s expression to be changed from what is used in (say) a template file by specifying a new definition lower in the file, after the template file include statement.  Options can also be added to previously specified options for a variable by including more definition statements (that may only contain options and not expressions) lower in the input file. Similarly for units.  There are two types of string replacements that occur when a line from an input file is parsed: i) file-specific replacements, which occur recursively through ‘child’ file inclusions, and ii) general replacements, which occur throughout all files from their point of definition onwards, until (possibly) cancelled. The following demonstrates a general replacement statement specifying two general replacement strings, using a long and short form: There are certain system generated general replacements that occur automatically unless specifically changed by the user. Use the search hint to find the list of these in .  The above demonstrates how to cancel a search string replacement, using either a long () or short () form. Note that both general and file specific replacements do not occur on a line of an input file if the line is itself a general replacement definition line (specifically, it begins with the keyword), or is an include line for a file (begins with some type of keyword).
-->

#### Include statement examples

The include path stacks are designed to make using input files from a variety of locations easy.

A common application would be to include files from number of template directories.  For example
```arb
INCLUDE_TEMPLATE "navier_stokes"
INCLUDE_TEMPLATE "volume_of_fluid"
```
will find the template directories `navier_stokes` and `free_surface/volume_of_fluid` and add these to the current code block include path stack.  Then, subsequent include statements will find file names in either of these two directories, searching within `templates/free_surface/volume_of_fluid` first, then `templates/navier_stokes`, and finally any remaining paths on the current stack (include the location of the calling file), as in
```arb
INCLUDE "u_f" # will find templates/navier_stokes/u_f.arb
INCLUDE "normals" # will find templates/free_surface/volume_of_fluid/normals.arb
INCLUDE "a_users_file" # will find a_users_file.arb if it existed in the same location as the calling file
```

The only time in which more fine grained control of the include path stack is required is when files of the same name are found in multiple locations.  This can be handled by specifying the location of files specifically so that the file's location is always on the top of the include path stack, as in
```arb
INCLUDE_TEMPLATE "navier_stokes/u_f" # found in templates/navier_stokes/u_f.arb
INCLUDE_WORKING "u_f" # found u_f.arb in user's working directory
```
For extra safety (ie, avoiding bugs), paths can be removed from the stacks after the files have been included, as in
```arb
INCLUDE_TEMPLATE "navier_stokes/u_f" # found in templates/navier_stokes/u_f.arb and added templates/navier_stokes directory to the stack
INCLUDE # removes templates/navier_stokes from the current include path stack
INCLUDE_WORKING "u_f" # found u_f.arb in user's working directory and add user's working directory to the stack if not already there
INCLUDE_WORKING "u_f2" # found u_f2.arb in user's working directory, but do not add working directory to the stack as it is already on the top
INCLUDE # remove the user's working directory from the stack unless it was the last path left on the stack
```


###If statements

The following demonstrates a solver code if statement:
```arb
IF 1
  NONE_CONSTANT <a> 1.d0
END_IF
```
Code flow through an `IF` statement is dependent on what follows the `IF` keyword:  An empty string or '0' evaluates as false, whereas anything else is true.  So the following are all true,
```arb
IF 1
IF asd afsdf sdg sdgfas # some comment
IF 0 adfd
IF "0" # note this!
```
whereas the following are all false
```arb
IF 0
IF # possible trailing comment
IF
IF&
  & # line continuation
```

There are also the continuation statements of `ELSE_IF` and `ELSE` demonstrated by:
```arb
IF 0
  NONE_CONSTANT <a> 1.d0
ELSE_IF 1
  NONE_CONSTANT <a> 2.d0
ELSE
  NONE_CONSTANT <a> 3.d0
END_IF
```
`IF`, `ELSE_IF`, `ELSE` and `END_IF` are all solver code keywords, so require their own lines.

In practice `IF` statements are used in conjunction with string replacements or embedded perl code, as in
```arb
IF <<transientflag>> # here <<transientflag>> is a system generated string variable which is replaced by 1 for a transient simulation
  CONSTANT <dt> 0.1d0
ELSE_IF {{ return string_eval('$some_string_variable') }} # here the embedded code enclosed in {{ }} evaluates as the return value, which here would be the value of the string variable $some_string_variable which would have been previously set
  CONSTANT <dt> 1.d0
ELSE
  CONSTANT <dt> 2.d0
END_IF
```

Note that code within an if 'block' is evaluated as a separate code block, so to make a local string variable available after the if block you would need to either declare it before the block (the scope of a variable is dictated by the code block in which it is first defined), or declare it as a global variable:
```arb
{{ string_set('$a'); }} # a set with no other argument sets the variable to the empty string, but defines its scope as being in this code block
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







##Meshes

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


