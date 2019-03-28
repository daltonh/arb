---
title: 'version changes'
author: Dalton Harvie
date: 15/2/18
---

# Version changes

This section details changes to the arb syntax that have occurred for each version update.

## v0.60

* Vector, tensor and relstep index replacements have now been implemented, including the `kronecker` and `permutation` functions, as in
```arb
NONE_CONSTANT <b[l=<<i>>]> "2.d0**<<i>>" # defines all components of b listed in <<dimensions>>
NONE_CONSTANT <a[l=<<i>>,<<j>>]> "<a[l=<<i>>,<<j>>]> + permutation(<<i>>,<<j>>,<<k>>)*<b[<<j>>]>*<c[<<k>>]>" # calculates cross product of b and c
NONE_CONSTANT <i[l=<<i>>,<<j>>]> "kronecker(<<i>>,<<j>>)" # the identity matrix
NONE_TRANSIENT <d[r=<<r>>]> "" "<e[r=<<r>>]>" # defines all relstep versions of d listed in <<relsteps>>
```
The basic idea behind 'index replacements' is that the expression is repeated recursively for every unique index string found, over `<<dimensions>>` for l-type indices, and `<<relsteps>>` for r-type indicies.  Navier-stokes template files updated to use index replacement notation.  Watch out for missing `<<dimensions>>` definitions in non-3D problems.  Time to revise BSL vector algebra notation!  See the new examples file, `vector_tensor_algebra`.
* New 'colon' operators `cross` and `mag`, complementing `dot` and `ddot`
* Code communication 'touch' files now by default are looked for in the output directory, rather than the working directory, controlled by
```arb
GENERAL_OPTIONS outputdirtouchfiles
```
* Changes to variable names in advection limiter and species transport template files
* New templates in multifluid/multiphase, chemcial_species_within_species, and others, and many changes/improvements to multifluid
* Perl routine `arb_defined` to check on existence of previous variable or region definition
* Perl routines `string_test` and `string_eval` now accept `text` option, using full string replacements, rather than just reporting on the value of the replacement string (allows evaluation of nested string replacements)
* `context` centring is allowed instead of having to define the centring of an operator, which assumes that the centring is that of the (direct) context in which the operator appears
```arb
CELL_CONSTANT <a> "contextif(<d>,2,0)" # here 'context' becomes 'cell' when operator is used
FACE_CONSTANT <b> "contextif(<d>,2,0)" # here 'context' becomes 'face' when operator is used
NONE_CONSTANT <c> "nodesum(contextif(<d>,2,0),region=<allnodes>)" # here 'context' becomes 'node' when operator is used
```
* File utility in misc to transform variable names with spaces to underscores
* Within arb code replacement specifications, allow `SUFFIX` and `PREFIX` replacements
* Input files now copied over to output directory, within new subdirectory `input_files`

## v0.59

* More use of flag strings (eg `<<transientnavierstokes>>`) in template files, rather than confusing comment strings
* Update of pardiso libraries from version 500 to version 600.
* Rewriting boundary equations template files
* Ability to create subdirectories for output files
* Much work on multifluid
* Working on OSX 10.11

## v0.58

* Timesteprewinding has been implemented, with a whole new suite of `GENERAL_OPTIONS`
* `REPLACEMENTS` now accepts a `SUBSTITUTE` type (equivalent `S`), which only sets a string if it has already been set.  For example
```arb
REPLACEMENTS REPLACE '<<string>>' WITH 'another string' # sets <<string>>
REPLACEMENTS SUBSTITUTE '<<string>>' WITH 'another other string' # redefines <<string>>, using its original scope
REPLACEMENTS SUBSTITUTE '<<string2>>' WITH 'woops' # results in an error as <<string2>> has not previously been defined
```
* `GENERAL_OPTIONS` is now the preferred way to set simulation variables such as `timestepmax`, `timestepout`, `newtstepmax`, `newtrestol` etc
* `TRANSIENT_SIMULATION` should now be set using `GENERAL_OPTION transientsimulation`, and `STEADYSTATE_SIMULATION` by `GENERAL_OPTION notransientsimulation`
* Moving towards variable logicals being set via the `option`/`nooption` syntax.  So `staticmagnitude` becomes `nodynamicmagnitude`.
* Reaction files (`*.rxn`) files can now be included directly using include statements.  Just include the `rxn` extension, as in
```arb
INCLUDE_WORKING "my_reactions.rxn"
```
This just causes the `rxntoarb` script (ie, `arb_reactions`) to be run on the file before it is included in the solver code.

