" Vim settings file for arb finite volume solver
" Language:     arb
" Version:      0.57
" Modified:     2017/12/20
" URL:          http://people.eng.unimelb.edu.au/daltonh/downloads/arb/

" Only do these settings when not done yet for this buffer
if exists("b:did_ftplugin")
  finish
endif

" Don't do other file type settings for this buffer
let b:did_ftplugin = 1

setlocal comments=:# cms=#%s mps+=<:>

" Extensions for matchit plugin
let b:match_ignorecase = 0
let b:match_words='\<IF\>:\<ELSE_IF\>:\<ELSE\>:\<END_IF\>,\<BLOCK\>:\<END_BLOCK\>,\<\%(MARKDOWN\|SKIP\)\>:\<END_\%(MARKDOWN\|SKIP\)\>'

" Omni completion (insert mode keyword completion) with CTRL-X CTRL-O
if exists("+omnifunc")
  setlocal omnifunc=syntaxcomplete#Complete
  let g:omni_syntax_ignorecase = 0
  let g:omni_syntax_minimum_length = 2 " omni completion is pointless (i.e. uses more keystrokes) for anything shorter
  let g:omni_syntax_group_exclude_arb = 'arbDeprecated'
endif
