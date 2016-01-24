" Vim settings file for arb finite volume solver
" Language:     arb
" Version:      0.55
" Modified:     2016/01/24
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
let b:match_words='\<\%(BEGIN\|START\)_\%(COMMENTS\=\|SKIP\)\>:\<\%(END\|STOP\)_\%(COMMENTS\=\|SKIP\)\>'

" Omni completion (insert mode keyword completion) with CTRL-X CTRL-O
if exists("+omnifunc")
  setlocal omnifunc=syntaxcomplete#Complete
  let g:omni_syntax_ignorecase = 0
  let g:omni_syntax_minimum_length = 2 " omni completion is pointless (i.e. uses more keystrokes) for anything shorter
  let g:omni_syntax_group_exclude_arb = 'arbDeprecated'
endif

if exists("*ArbIncludes")
  finish
endif

" Show contents of included files (with replacements if present) in new window
function ArbIncludes(...)
" Accepts one optional argument, which may be:
" 'a': show all included files and all other lines too ('all')
" 'i': show all included files but nothing else ('includes')
" 'o': show only included file nearest the cursor ('only') [default if no/unrecognised optional argument given]
" 't': show only the inclusion tree/hierarchy; like 'i' but only file names shown ('tree')

  " Sanity check
  let s:templates_dir = finddir('templates/','**;')
  if !isdirectory(s:templates_dir)
    echoe 'Could not find templates directory. Aborting.'
    return
  endif

  " Clear ArbIncludes window if it already exists
  if bufexists('ArbIncludes')
    bw! ArbIncludes
  endif

  " Determine mode ('a', 'i', 'o', 't') and set up accordingly
  let s:arg = a:0 ? a:1 : 'o' " default to 'o' if no argument given
  if s:arg =~ '\<\%(a\|i\|t\)\>'
    new ArbIncludes
    call append(1,getbufline('#',1,'$'))
    if s:arg == 'i'
      silent v/^\s*\%(<<.\{-}>>\s*\)*INCLUDE\%(_ROOT\|_FROM\|_WORKING\)\=\>/d
      call append(0,'')
    endif
  else " default case
    let position = winsaveview()
    call cursor(line('.'),1)
    let line = getline(search('^\s*\%(<<.\{-}>>\s*\)*INCLUDE\%(_WORKING\)\=\>','c'))
    if line =~ '^\s*\%(<<.\{-}>>\s*\)*INCLUDE_WORKING'
      let root_line = ''
    else
      let root_line = getline(search('^\s*\%(<<.\{-}>>\s*\)*INCLUDE_\%(ROOT\|FROM\)\>','bW'))
    endif
    call winrestview(position)
    new ArbIncludes
    call append(1, [root_line, line])
  endif

  " Define default general replacements
  let s:gen_rwlist = {}
  let s:gen_rwlist['<<dim1comment>>'] = ''
  let s:gen_rwlist['<<dim2comment>>'] = ''
  let s:gen_rwlist['<<dim3comment>>'] = ''
  let s:gen_rwlist['<<steadystatecomment>>'] = ''
  let s:gen_rwlist['<<transientcomment>>'] = '#'
  let s:gen_rwlist['<<cartesiancomment>>'] = ''
  let s:gen_rwlist['<<cylindricalcomment>>'] = '#'
  let s:gen_rwlist['<<steadystateflag>>'] = '1'
  let s:gen_rwlist['<<transientflag>>'] = '0'
  let s:gen_rwlist['<<cartesianflag>>'] = '1'
  let s:gen_rwlist['<<cylindricalflag>>'] = '0'
  let s:gen_rwlist['<<radius_c>>'] = '1.d0'
  let s:gen_rwlist['<<radius_f>>'] = '1.d0'
  let s:gen_rwlist['<<radius_n>>'] = '1.d0'
  let s:gen_rwlist['<<radiusdim1flag>>'] = '0'
  let s:gen_rwlist['<<radiusdim2flag>>'] = '0'
  let s:gen_rwlist['<<radiusdim3flag>>'] = '0'
  let s:gen_rwlist['<<radialdim>>'] = '0'
  let s:gen_rwlist['<<axialdim>>'] = '0'
  let s:gen_rwlist['<<reflect=1>>'] = ''
  let s:gen_rwlist['<<reflect=2>>'] = ''
  let s:gen_rwlist['<<reflect=3>>'] = ''

  " Read each line of current file, performing replacements and including contents of other files as needed
  let s:count_includes = 0
  let s:include_lines = []
  call cursor(1,1)
  while search('.*','W')
    let line = getline('.') " read line
    if line =~ '^\s*\%(GENERAL_\)\=REPLACEMENTS\>' " update general replacements list
      call s:ArbIncludes_GenRepl(line)
    elseif line !~ '^\s*#' " don't do replacements on comment lines
      for key in keys(s:gen_rwlist)
        let line = substitute(line, '\M' . key, s:gen_rwlist[key],'g')
      endfor
      call setline('.',line)
    endif
    if line =~ '^\s*INCLUDE\%(_WORKING\)\=\>' " insert contents of included file (with replacements)
      call s:ArbIncludes_Include(line)
    endif
  endwhile
      
  " Cover our tracks!
  call cursor(1,1)
  for i in range(0,len(s:include_lines)-1)
    call setline(search('^\s*INCLUDE\%(_WORKING\)\=\>','W'),s:include_lines[i])
  endfor
  silent g/^INCLUDE_ROOT\>.*# ArbIncludes$/d " remove dummy INCLUDE_ROOTs
   
  " Remove unwanted lines
  silent 1d
  if s:arg == 't'
    silent v/^\s*INCLUDE\%(_ROOT\|_FROM\|_WORKING\)\=\>/d
  endif
  if s:arg =~ '\%(i\|t\)'
    silent v/^\s*INCLUDE_\%(ROOT\|FROM\)\>/s/\(.*\)/  \1/
  endif

  " Adjust window settings
  call cursor(1,1)
  setlocal foldmethod=marker
  setlocal foldmarker={=====,}=====
  setlocal filetype=arb
  setlocal nomodifiable
  setlocal buftype=nofile
  setlocal bufhidden=wipe

endfunction
 
" Process general replacements
function s:ArbIncludes_GenRepl(line)

  let repl = substitute(a:line,'\s*\w*REPLACEMENTS\>\s*\(\%(\w\+\s*\(''\|"\).\{-}\2\s*\)\+\)\%(#.*\)\=','\1','') " string containing the list of replacements
  while len(repl) > 0
    if repl[0] == 'C' " CANCEL
      let r = substitute(repl,'[^''"]*\(''\|"\)\(.\{-}\)\1.*','\2','')
      if has_key(s:gen_rwlist,r) " remove DEFAULT/REPLACE/WITH pair from general replacements list
        unlet s:gen_rwlist[r]
      endif
      let repl = substitute(repl,'.\{-}\(''\|"\)\M' . r . '\m\1\s*','','') " remove CANCEL item from repl string
    else " DEFAULT or REPLACE
      let rwline = matchlist(repl,'\(\w*\)\s*\(''\|"\)\(.\{-}\)\2\s*\w*\s*\(''\|"\)\(.\{-}\)\4\s*') " [0] is next DEFAULT/REPLACE/WITH pair, [1] is DEFAULT or REPLACE, [3] is replacement, [5] is with
      if !has_key(s:gen_rwlist, rwline[3]) " earliest DEFAULT wins
        let s:gen_rwlist[rwline[3]] = rwline[5] " add DEFAULT/REPLACE/WITH pair to general replacements list
      else
        if rwline[1] =~ '^R' " latest REPLACE wins
          let s:gen_rwlist[rwline[3]] = rwline[5] " add DEFAULT/REPLACE/WITH pair to general replacements list
        endif
      endif
      let repl = substitute(repl,'\M' . rwline[0],'','') " remove DEFAULT/REPLACE/WITH pair from repl string
    endif
  endwhile

endfunction

" Process included files
function s:ArbIncludes_Include(line)

  " Locate and read included file
  let s:count_includes += 1
  let line = matchlist(a:line,'\(\s*\)\(INCLUDE\w*\)\s*\%(''\|"\)\(\S*\)\%(''\|"\)\s*\(\%(\w\+\s*\(''\|"\).\{-}\5\s*\)\+\)\=\%(#.*\)\=') " [0] is full line, [1] is indent, [2] is INCLUDE[_WORKING], [3] is filename, [4] is replacements
  if len(s:include_lines) < s:count_includes
    call add(s:include_lines,line[0])
  endif
  let file = line[3] =~ '.arb$' ? line[3] : line[3] . '.arb' " get name (basename + suffix) of included file
  let root_line = matchlist(getline(search('^\s*INCLUDE_\(ROOT\|FROM\)\>','bnW')),'\s*INCLUDE\w*\s*\%(\%(''\|"\)\(\S*\)\%(''\|"\)\)\=\s*\%(#.*\)\=') " [0] is full line, [1] is filename
  if len(root_line) == 0 " no INCLUDE_ROOT line
    let s:root = ''
  elseif root_line[1] == '' " INCLUDE_ROOT reset
    let s:root = getcwd() =~ '/templates/' ? substitute(getcwd(),'.*/templates/','','') : ''
  else " root specified (usual case)
    let s:root = root_line[1]
  endif
  if line[2] == 'INCLUDE_WORKING'
    let dir = ''
    let file = findfile(file,'**;')
  else
    let dir = substitute(globpath(s:templates_dir, s:root == '' ? s:root : '**/' . s:root),'\%(\n.*\)\=/*$','/','') " find directory containing the included file
  endif
  if filereadable(dir . file) " read included file
    let include_file = readfile(dir . file)
  else
    echohl WarningMsg
    echom 'WARNING: Could not find included file ' . dir . file 
    echom '  Skipping it and continuing.'
    echohl None
    return
  endif

  " Perform linewise operations on included file
  call map(include_file, '"  " . line[1] . v:val') " indent contents of included file
  let rwlist = []
  let repl = line[4]
  while len(repl) > 0 " need to make replacements in included file
    call add(rwlist,substitute(repl,'[^''"]*\(''\|"\)\(.\{-}\)\1.*','\2','')) " add item to replacements list (alternating REPLACE and WITH strings)
    let repl = substitute(repl,'.\{-}\(''\|"\)\M' . rwlist[-1] . '\m\1\s*','','') " remove item from repl string
  endwhile
  let count_nested = 0
  for i in range(0,len(include_file)-1)
    for j in range(0,len(rwlist)-1,2)
      let include_file[i] = substitute(include_file[i],'\M' . rwlist[j],rwlist[j+1],'g') " make replacements, treating special vim characters as literals (\M)
    endfor
    if include_file[i] =~ '^\s*\%(GENERAL_\)\=REPLACEMENTS\>' " nested general replacements
      call s:ArbIncludes_GenRepl(include_file[i])
    elseif include_file[i] !~ '^\s*#' " don't do replacements on comment lines
      for key in keys(s:gen_rwlist)
        let include_file[i] = substitute(include_file[i],'\M' . key,s:gen_rwlist[key],'g')
      endfor
    endif
    if include_file[i] =~ '^\s*INCLUDE\%(_WORKING\)\=\>' " nested includes
      call insert(s:include_lines,include_file[i],s:count_includes + count_nested)
      let count_nested += 1
      let include_file[i] = substitute(include_file[i],'\(\s*INCLUDE\%(_WORKING\)\=\s*\S*\)\(.*\)','\1 ' . line[4] . '\2','') " carry over upstream replacements
    endif
    if include_file[i] =~ '^\s*INCLUDE_\%(ROOT\|FROM\)\s*\%(#.*\)\=$' " INCLUDE_ROOT reset
      call insert(include_file,'INCLUDE_ROOT "' . s:root . '" # ArbIncludes',i+1)
    endif
  endfor
  call add(include_file,'INCLUDE_ROOT "' . s:root . '" # ArbIncludes') " reset root after include with a dummy INCLUDE_ROOT line
  
  " Insert contents of included file into ArbIncludes window
  call append(line('.'),include_file)
  if or(s:arg != 'o', line('.') > 2) " insert fold markers
    call append(line('.'), line[1] . '#{=====')
    call append(line('.')+len(include_file)+1, line[1] . '#}=====')
  endif

endfunction
