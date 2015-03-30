" Vim settings file for arb finite volume solver
" Language:     arb
" Version:      0.52
" Modified:     2015/03/22
" URL:          http://people.eng.unimelb.edu.au/daltonh/downloads/arb/
" Maintainer:   Christian Biscombe

" Only do these settings when not done yet for this buffer
if exists("b:did_ftplugin")
  finish
endif

" Don't do other file type settings for this buffer
let b:did_ftplugin = 1

setlocal comments=:#
setlocal cms=#%s
" Enable angle bracket highlighting and matching with '%'
setlocal mps+=<:>

" Extensions for matchit plugin
let b:match_ignorecase = 0
let b:match_words='\<\(BEGIN\|START\)_\(COMMENTS\=\|SKIP\)\>:\<\(END\|STOP\)_\(COMMENTS\=\|SKIP\)\>'

" Omni completion (insert mode keyword completion) with CTRL-X CTRL-O
if exists("+omnifunc")
  setlocal omnifunc=syntaxcomplete#Complete
  let g:omni_syntax_ignorecase = 0
  let g:omni_syntax_minimum_length = 2 " omni completion is pointless (i.e. uses more keystrokes) for anything shorter
  let g:omni_syntax_group_exclude_arb = 'arbDeprecated'
endif
