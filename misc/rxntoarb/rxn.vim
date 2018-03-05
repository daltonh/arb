" Language:     rxn
" Version:      2.12+
" Modified:     2018/02/23
" Maintainer:   Christian Biscombe

" For version 5.x: Clear all syntax items
" For version 6.x: Quit if a syntax file is already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case ignore
syn match rxnLabel "^[^ ].\{-}:\ze\s\+"
syn match rxnHeader "^\s*!.*$"
syn match rxnComment "#.*$" contains=rxnTodo
syn region rxnTodo start="\%(FIXME\|TODO\|XXX\)" end="$" contained
syn match rxnRegion "@\%(\w\+\|<[^>]*>\)"
syn keyword rxnStatement initial_species options surface_region[s] volume_region[s]
syn keyword rxnStatement exclude include_only nextgroup=rxnRegexp skipwhite
syn region rxnRegexp start="/" end="/\%(i\C\)\?" skip="\\/" contained oneline
syn match rxnOperator "\%(\s\+\zs+\ze\s\+\|->\|<=>\|\d\s*\zs\*\|\d\s*\zs\.\|;\)"
syn region rxnLet matchgroup=rxnStatement start="\<let\>" end="=" transparent
syn region rxnEnzyme matchgroup=rxnOperator start="{" end="}->" transparent
syn region rxnMetaspecies matchgroup=rxnOperator start="\[" end="\]" transparent
syn match rxnParameter "\%(k\|ka\|kf\|kon\|kd\|kr\|koff\|KM\|Km\|kcat\)\s*=\s*" nextgroup=rxnUnit
syn match rxnUnit "[A-Za-z0-9. ^*+-]*" contained
syn region rxnUnit start="'" end="'" contained
syn region rxnUnit start='"' end='"' contained

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_rxn_syn_inits")
  if version < 508
    let did_rxn_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  HiLink rxnComment Comment
  HiLink rxnHeader Special
  HiLink rxnLabel Type
  HiLink rxnOperator Operator
  HiLink rxnParameter Function
  HiLink rxnRegexp Special
  HiLink rxnRegion Special
  HiLink rxnStatement Statement
  HiLink rxnTodo Todo
  HiLink rxnUnit String
  delcommand HiLink
endif

let b:current_syntax = "rxn"
