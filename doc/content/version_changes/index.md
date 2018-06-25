---
title: 'version changes'
author: Dalton Harvie
date: 15/2/18
---

# Version changes

This section details changes to the arb syntax that have occurred for each version update.

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

