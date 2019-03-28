---
title: 'arb file syntax'
author: Dalton Harvie
date: 6/12/16
---

# arb file syntax

## Solver code syntax

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



## String variables and solver code string statements

'String variables' are used to perform replacements on the arb solver code as it is read in (parsed) by [setup_equations.pl].  String variables can be set using either solver code string statements, or via (the more complex but powerful) [embedded perl code](#embedded-perl-code) (see below).  The main purpose of string variables is to allow 'chunks' of arb code to be tailored to specific applications, either by the direct replacement of solver code text, or through the use of `IF` statements to include and exclude specific code sections.  This will become clearer when discussing [include statements](#include-statements) later.

An example string statement is the following:
```arb
REPLACEMENTS REPLACE "a string" WITH "another string"
```
This will cause all occurrences of the string 'a string' in the remainder of the current code block (see below) to be replaced by the string 'another string', except within other solver code string statements and within any [embedded perl code](#embedded-perl-code).  A more realistic example is
```arb
REPLACEMENTS REPLACE "<c>" WITH "<c1>"
```
which would cause subsequent references to the variable `<c>` to be replaced with references to the variable `<c1>`.

The string statement has a number of variations.  The word `DEFAULT` can be used in place of `REPLACE`.  In this case the string variable will only be set if it hasn't been set already.  The default form is often used within template files to assume a default value of a variable if it has not already been set within the calling file.
```arb
REPLACEMENTS DEFAULT "a string" WITH "another string" # 'DEFAULT' means only set this string if it isn't already set
```
Conversely there is the `SUBSTITUTE` form of the replacement statement which only sets a string value if the string has previously been set.  This form is particularly useful to manipulate the scope of the replacement strings, discussed below.  An example is
```arb
REPLACEMENTS REPLACE '<<string>>' WITH 'another string' # sets <<string>>
REPLACEMENTS SUBSTITUTE '<<string>>' WITH 'another other string' # redefines <<string>>, using its original scope
```
If the variable has not already been set then a `SUBSTITUTE` statement generates an error (as a safety feature related to variable scope).
To remove a string variable assignment the `CANCEL` keyword can be used, as in
```arb
REPLACEMENTS CANCEL "a string" # will cancel a previously set string replacement
```
If the variable has not already been set then a `CANCEL` statement issues a warning but is otherwise ignored.

## String variable scoping

The scope of a standard string replacement variable is all code within the remaining code block in which the variable is first defined (including any descendant blocks).  The precise definition of a [code block](#code-block) is detailed below, however most commonly a code block corresponds to the contents of an arb code file.  Hence, if an arb code file 'A.arb' includes a code file 'B.arb', and a standard string variable is defined in code file 'B.arb', then this string variable will not be defined back in (the remainder of) code file 'A.arb'.  In other words, the scope of a standard string variable is only the remainder of the code block in which the variable is defined and the block's children, and not the parents of the defining code block.  For this reason standard string variables are referred to as local string variables.

The alternative to a local string variable is a global string variable.  The scope of a global string variable is all code blocks (actually the grandparent or first code block, which spawns all other code blocks), so a global string variable defined in file 'B.arb' in the previous example will be used to replace strings in the remainder of file 'B.arb' as well as within the remainder of its calling file 'A.arb'.  Global variables are set using the `GLOBAL_REPLACEMENTS` keyword (or the legacy version of this, being `GENERAL_REPLACEMENTS`):
```arb
GLOBAL_REPLACEMENTS REPLACE "a string" WITH "a better string" # this replacement will occur in all code blocks subsequent to this statement
GENERAL_REPLACEMENTS REPLACE "a string" WITH "a better string" # GENERAL_REPLACEMENTS is a legacy and entirely equivalent form of the GLOBAL_REPLACEMENTS keyword
```

In terms of how these scoping rules are implemented, string variables are each associated with a specific code block, with replacements performed using any string variable that is defined within the current code block or its parents (see `@code_blocks[]{"string_variables"}` array hash in [StringCode.pm]), searched from the child (local) to its parents (global).  Global string variables are associated with the first code block so are available for replacement in all subsequently formed code blocks.

The different replacement string keywords and action statements also influence the scoping behaviour.  A normal `REPLACEMENTS REPLACE...` command will limit its actions to only the current (local) block.  Hence, 
```arb
REPLACEMENTS R "<<a string>>" W ""
```
will search through the strings within the local code block, and if `<<a string>>` is found, its value will be replaced by the empty string.  If this string is not found within the local code block then it will be set in the local code block and be available in the rest of that block and in all child blocks.  When the block is destroyed `<<a string>>` will also be destroyed.

In the case of a `GLOBAL_REPLACEMENTS REPLACE...` the process is analogous, except that the search and setting is only conducted within the global code block (ie, the grandparent).  As the global block remains throughout the parsing process, a global string will always remain available.  However, as strings are searched for in reverse order through the code blocks during the replacement process, the value of a local string variable takes precedence over a global string variable of the same name when replacements are done.

The `DEFAULT` action can be used to modify either the `GLOBAL_REPLACEMENTS` or `REPLACEMENTS` definitions.  In both cases all code blocks are first searched for the requested string.  Only if this string is not found is a new string variable created, with the code block that it is set in being either the global or local block, depending on the keyword.  Note that the blocks through which the search is conducted is not influenced by the choice of the `GLOBAL_REPLACMENTS` or `REPLACEMENTS` keyword.

The `SUBSTITUTE` action acts quite differently.  Under this action all code blocks are searched for the relevant string (in reverse order, from the local block to the global block), and when found, this string's value is set to the new value, but in its original code block.  Hence, the `SUBSTITUTE` action allows strings to be redefined in blocks other than the local or global ones.

`SUBSTITUTE` helps with traversing the scope of code blocks.  For instance, one way of making a local string available within a larger scope is to first define the string within the larger scope, but actually set its value within the smaller (sub) scope using `SUBSTITUTE`.  For example, in the previous example where a local variable was set within file 'B.arb', if this variable was first defined (to say an empty string) in file 'A.arb' using
```arb
REPLACEMENTS R "<<a string>>" W "" # the scope of a variable is defined by the block in which it is first defined
```
then its value can be set in file 'B.arb' using
```arb
REPLACEMENTS S "<<a string>>" W "a more useful value" # substitute retains the scope of the original definition
```
Now `<<a string>>` would be available back in file 'A.arb' as `a more useful value`.  A similar process can be used to set strings using `IF` statements, as in
```arb
REPLACEMENTS R '<<setme>>' W '' # define the scope of the string <<setme>>
IF <<condition variable>>
  REPLACEMENTS S '<<setme>>' W '1.d0' # a new block has been created, but S allows the scope of <<setme>> to escape from the IF block back to its parent block
ELSE
  REPLACEMENTS S '<<setme>>' W '2.d0'
END_IF
```
If a string name is not found during a `SUBSTITUTE` action, an error results.  This is a safety mechanism to prevent `SUBSTITUTE` setting variables in code blocks that the user does not intend.

Note that if a string is cancelled using (eg)
```arb
REPLACEMENTS CANCEL "<<a string>>" # completely removes string and defining scope
```
then all information about the string variable is lost, including its scope.  Hence any subsequent definitions will set the string's code block scope afresh.


Finally, there are short forms of the string setting statements illustrated by
```arb
REPLACEMENTS R "a string" W "another string" # R is a short form of REPLACE, and W is a short form of WITH
REPLACEMENTS C "a string" # C is a short form of CANCEL
REPLACEMENTS D "a string" W "another string" # and 'D' is short for 'DEFAULT'
REPLACEMENTS S "a string" W "another string" # and 'S' is short for 'SUBSTITUTE'
```
Also, multiple strings may be defined on the one line, illustrated by
```arb
GLOBAL_REPLACEMENTS R "string1" W "another string" D "string2" W "another string2" # multiple replacements can be specified on the one line, with difference action words
```

There are certain system generated global string variables that are set automatically but can also be changed by the user.  These include a number of variables that set the coordinate dimensions used by various variable operators.  Use the search hint `ref: string system variables` to find the list of these in `sub string_setup` within [StringCode.pm].

Also, for debugging purposes the perl subroutine `string_debug` can be used to print out all of the replacement strings at any point in the file, using (for example):
```arb
{{ print "DEBUGGING: string debug =\n".string_debug(); }}
```

## Code blocks

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

## Include statements

### Include statement overview

Include statements allow other input files to be included.   These files can be user written, or be from a library of template files within the [templates directory].  As an example, 
```arb
INCLUDE_TEMPLATE "navier_stokes/complete_equations"
```
will include the file `complete_equations.arb` from the `templates/navier_stokes` directory, replacing this include statement with the contents of the `complete_equations.arb` file.  Similarly 
```arb
INCLUDE "my_function" # this could be located in a variety of locations searched through
```
would include the arb file `my_function.arb` from the (eg) user's working directory.  Note that a trailing '.arb' file extension is implied within all include statements (and hence not required).

A new code block is started whenever a new file is included:
```arb
INCLUDE_LOCAL "some_arb_code" # a new block is created after the string "some_arb_code" has been read
```
This means that any string variables that are defined within the included file are not available outside of that file (except in its included children), unless these string variables are defined as global.

String replacements (or substitutions) that occur as the file is read in allow included files to be (effectively) used as functions.  More specifically, string code that follows an include statement is included as the first statement in the newly created block:
```arb
INCLUDE_LOCAL "another_arb_code" R "<mu_f>" W "<mu>" # so the replacement is done on the contents of another_arb_code.arb
```
This allows the template files to be used as functions or called repeatedly with different arguments.  An equivalent perl embedded statement can also be used, as in
```arb
INCLUDE_LOCAL "another_arb_code" {{ string_set("<mu_f>","<mu>"); }} # is equivalent to the last case
```
String replacements defined as part of `INCLUDE` statements are always local, meaning that their scope is defined by the code block of the included file.  Hence, once the entire file has been parsed these string replacements will be destroyed (and any previously defined ones of the same name will take over again).

### Include statement types

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


### Include statement examples

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

Partnering the include file capability is the ability to read in multiple definitions for the same variable or region.  The ultimate position of a variable or region's definition is that of the first definition for that variable or region.  The ultimate expression used for a variable/region is that given (read in) last. This functionality allows a variable/region's expression to be changed from what is used in (say) a template file by specifying a new definition lower in the file, after the template file include statement.  Options can also be added to previously specified options for a variable or region by including more definition statements (that may only contain options and not expressions) lower in the input file. Similarly for units for variables.

##If statements

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

##Simulation Info

The following strings can be used within an input file to help keep track of what the file contains. These and other automatically generated info strings are included as comments in most of the output files.

* `INFO_TITLE`: title for simulation
* `INFO_DESCRIPTION`: description
* `INFO_DATE`: interpreted as last modification date
* `INFO_AUTHOR`:
* `VERSION`: to be set as the arb version of the file, which is checked against the version of arb run

The `INFO_TITLE` and `INFO_DESCRIPTION` fields can accept an increment to their previously set value, using a `+` sign, as in
```arb
INFO_DESCRIPTION "a funky simulation"
INFO_DESCRIPTION+ ", now with extra funky bits" # giving a total description of "a funky simulation, now with extra funky bits"
```

##Embedded perl code {#embedded-perl-code}

###Embedded code syntax

Lying alongside solver code is embedded perl code, which is code that is embedded within the solver code and used to

*  set and manipulate string variables, and to
*  generated solver code directly.

Embedded perl code is more powerful in setting and manipulating string variables that solver code string statements.  It can also be used to produce 'bespoke' solver code.

Embedded perl code is delimited by {{ and }}.  The basic syntax rules of embedded code are:

*  string replacements are not performed on embedded code, but they are performed on whatever is returned from the embedded code
*  embedded code can be placed anywhere within the solver code, except in
     -  comments (where it is just a comment)
     -  solver code string statements (eg, `'REPLACE '<<a>>' WITH '1'`)
*  there is no limitation to the characters used within embedded code, EXCEPT for }}, which even when enclosed within a string will terminate the embedded code
*  needs to be valid perl

Embedded perl code is interpreted directly by perl, evaluated using the 'eval' (string) function (<http://perldoc.perl.org/functions/eval.html>).  For all intents and purposes, whatever is enclosed within {{ and }} is run as a perl subroutine, within the (scope of the) [StringCode.pm] module of [setup_equations.pl], and whatever is returned from that eval (think subroutine) is first subject to string and index replacements, and then substituted into the solver code in place of the embedded code.  So the following embedded code
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

### Using string variables within embedded perl code

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


There are some handy routines available to help with controlling code flows.  `string_test` compares the value of a string variable against a result (see ref: string_test variables in [StringCode.pm]), as in
```arb
REPLACEMENTS R "<<function>>" W "1"
IF {{ return (string_test('<<function>>','1')) }}
  # do something here if <<function>> = 1
ELSE_IF {{ return (!(string_test('<<function>>','0'))) }}
  # do something here if <<function>> /= 0
END_IF
```
Normally `string_test` simply compares the value of a single replacement variable against the desired result, however if replacement variables have a nested structure, the `text` option may be more useful.  With the `text` option whatever is passed to `string_eval` is regarded as a line of text that has all of the string replacement variables applied to it, as in
```arb
REPLACEMENTS R "_species" W "_mouse" # this replacement is above the species one, so is done after the species one during replacements (stack behaviour = last in, first out)
REPLACEMENTS R "<a>" W "<a_species>" # adds a suffix to variable <a>, causing <a> to become <a_species>
{{
  print (string_test('<a>','<a_species>')); # prints true (1)
  print (string_test('<a>','<a_mouse>')); # prints false (0)
  print (string_test('<a>','<a_species>','text')); # prints false (0) as <a> will be evaluated as <a_mouse>
  print (string_test('<a>','<a_mouse>','text')); # prints true (1)
  print (string_test('We would all like <a>','<a_mouse>','text')); # prints We would all like <a_mouse>
}}
```
Similar to `string_test`, `string_eval` (see ref: string_eval variables in [StringCode.pm]) evaluates the value of a string (or line of text, with `text` option), while `string_examine` returns whether the string has been defined or not (see ref: string_examine variables in [StringCode.pm]):
```arb
{{ print "The value of <<a string>> is currently ".string_eval('<<a string>>')."\n"; }}
{{ return string_eval('# do replacements on line of text: dimensions = <<dimensions>>','text') }}
{{ if (string_examine('<<a string>>')) { print "<<a string>> has been defined\n"; } }}
```
By default `string_test` and `string_eval` cause an error if the passed string variable is not defined. 

Also available is the `arb_defined` routine that reports whether a variable or region has already been defined.  This is useful for performing a definition depending on whether a definition action has occurred previously.
```arb
{{ return "# is <facenorm[l=1]> defined?: ".arb_defined('<facenorm[l=1]>') }} # prints VARIABLE
{{ return "# is <facenorm[l=1]> a region?: ".arb_defined('<facenorm[l=1]>','region') }} # prints 0
{{ return "# is <facenorm[l=1]> a variable?: ".arb_defined('<facenorm[l=1]>','variable') }} prints VARIABLE
NONE_CONSTANT <d[l=1,3]> "1.d0"
{{ return "# is <d[l=1,3]> defined?: ".arb_defined('<d[l=1,3]>') }} # prints VARIABLE
{{ return "# is <d[l=1,3]> a region?: ".arb_defined('<d[l=1,3]>','region') }} # prints 0
{{ return "# is <d[l=1,3]> a variable?: ".arb_defined('<d[l=1,3]>','variable') }} # prints VARIABLE
{{ return "# is <allcells> defined?: ".arb_defined('<allcells>') }} # prints REGION
{{ return "# is <newvar[r=0]> defined?: ".arb_defined('<newvar[r=0]>') }} # prints 0
NONE_CONSTANT <newvar> 1.d0
{{ return "# is <newvar[r=0]> defined?: ".arb_defined('<newvar[r=0]>') }} # prints VARIABLE
{{ return "# is <newvar[r=1]> defined?: ".arb_defined('<newvar[r=1]>') }} # prints 0
IF {{ return arb_defined('<newvar>') }}
  VARIABLE <newvar> 2.d0 # only alter value of <newvar> if it has already been defined
END_IF
```
Note that the value reported by `arb_defined` represents the value at the point in the file in which it is written - that is, it is evaluated as the file is being parsed.  This is also true of all the perl string subroutines.
  
###Embedded code purpose

Embedded perl code allows more flexibility in coding arb, but is (probably) for more advanced uses.  The most useful application is probably controlling code flow, using the complementary `IF` syntax and perl's conditional rules.  Also, looping is possible using embedded code, allowing flexibility in defining possibly multiple and repeating statements;  For example:
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
A possible use (largely superseded with index replacements, unless very complex) is to create a variable definition for a vector, as in 
```arb
# this demonstrates expanding a vector using native perl
{{ my $code; foreach my $l ( string_eval('<<dimensions>>','list') ) { $code .=
  "CELL_DERIVED <phi_normal_c[l=$l]> 'celldivgrad[l=$l](<phi_f>)' ON <allcells>\n";
}; return $code; }}
```
There are also functions to automate the loop process for expanding vector and tensor expressions, also based on the dimension range defined in the global string variable `<<dimensions>>` (again, look instead at index replacements now):
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
Note that template files are available to set commonly used coordinate systems in `templates/general`.  Embedded code is used extensively (for example) in the multiphase template files, where it is helpful to be able to loop over multiple versions of variables, specific to a number of species.

Variables defined within [setup_equations.pl] can also be accessed within the string code, as can generic perl subroutines.
```arb
{{
  print "$::transient_simulation\n"; # is a variable defined in main of setup_equations.pl which indicates whether this is a transient simulation or not
  if (!($::transient_simulation)) {
    print "WARNING $ENV{USERNAME}: did you want to run a transient simulation?\n"; # not tested, but should work...
  }
}}
```

With some perl hackery there's lots that can be done if required using embedded code - eg, arrays and hashes stored within the string variables (but you'll have to learn some perl, or create a timemachine to transport you back to a time when perl was the bees-knees and you would already have known it!).  


