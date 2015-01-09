" Vim settings file for arb finite volume solver
" Language:     arb
" Version:      0.30
" URL:          http://www.chemeng.unimelb.edu.au/people/staff/daltonh/downloads/arb
" Modified:     2012/06/24

" Only do these settings when not done yet for this buffer
if exists("b:did_ftplugin")
  finish
endif

" Don't do other file type settings for this buffer
let b:did_ftplugin = 1

setlocal comments=:#
setlocal cms=#%s
" Enable angle bracket highlighting and switching with '%'
setlocal mps+=<:>
