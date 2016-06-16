" Vim settings file for arb finite volume solver
" Language:     arb
" Version:      0.56
" Modified:     2016/02/01
" URL:          http://people.eng.unimelb.edu.au/daltonh/downloads/arb/
" Maintainer:   Christian Biscombe

" Only do these settings when not done yet for this buffer
if exists("b:did_ftplugin")
  finish
endif

" Don't do other file type settings for this buffer
let b:did_ftplugin = 1

setlocal comments=:# cms=#%s mps+=<:>

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
" 'a': show everything ('all') [default if no/unrecognised optional argument given]
" 't': show only the inclusion tree/hierarchy ('tree')

  " Sanity check
  let templates_dir = finddir('templates','**;') . '/'
  if !isdirectory(templates_dir)
    echoe 'Could not find templates directory. Aborting.'
    return
  endif
   
  " Determine mode (default to 'a')
  if a:0
    let arg = a:1 == 't' ? 't' : 'a'
  else
    let arg = 'a'
  endif

  " Create new ArbIncludes window
  if bufexists('ArbIncludes')
    bw ArbIncludes
  endif
  let lnum = line('.')
  new ArbIncludes
  setlocal nofoldenable
  call append(1,getbufline('#',1,'$'))
  call cursor(lnum+1,1)
  if arg == 't' " move cursor to next replacement or include line
    call search('^\s*\%(<<.\{-}>>\s*\)*\%(\%(GENERAL_\)\=REPLACEMENTS\|INCLUDE\%(_ROOT\|_FROM\|_WORKING\)\=\)\>','c')
  endif
  call setline('.', getline('.') . '# ArbIncludes start') " append dummy comment to save cursor position

  " Extract default replacements from setup_equations
  let s:gen_rwlist = [[],[]] " [0] is list of strings to be replaced, [1] is corresponding list of replacements
  let setup_equations = templates_dir . '../src/setup_equations.pl'
  if filereadable(setup_equations)
    let save_qflist = getqflist()
    silent execute 'vimgrep /%{\$general_replacements\[\$#general_replacements+1\]} = ( search => ".*", replace => ".*" );/j ' . setup_equations
    for item in getqflist()
      let rwpair = matchlist(item.text,'\(''\|"\)\(.*\)\1.*\(''\|"\)\(.*\)\3') " [2] is string to be replaced, [4] is replacement
      call add(s:gen_rwlist[0], rwpair[2])
      call add(s:gen_rwlist[1], rwpair[4])
    endfor
    call setqflist(save_qflist)
  else
    echoe 'Could not find setup_equations.pl. Default replacements not done.'
  endif

  " Read each line of current file, doing replacements and including contents of other files as needed
  let skip = 0
  call cursor(1,1)
  while search('^\s*[^#]','W') " skip comments and blank lines
    if skip " mark and skip block comments
      silent +,/^\s*\%(<<.\{-}>>\s*\)*\%(END\|STOP\)_\%(COMMENTS\=\|SKIP\)\>/-s/^/# ArbIncludes: /e
      call cursor(line('.')+1,1)
    endif
    let line = s:ArbDoRepl(getline('.'),s:gen_rwlist) " read line and do general replacements
    call setline('.',line)
    if line =~ '^\s*\%(GENERAL_\)\=REPLACEMENTS\>' " update general replacements list
      call s:ArbGetRepl(line,s:gen_rwlist)
    elseif line =~ '^\s*\%(BEGIN\|START\|END\|STOP\)_\%(COMMENTS\=\|SKIP\)\>' " set flag to skip block comment lines
      let skip = !skip
    elseif line =~ '^\s*TRANSIENT_SIMULATION\>' " overwrite default flags
      call s:ArbGetRepl('GENERAL_REPLACEMENTS R "<<steadystatecomment>>" W "#" R "<<transientcomment>>" W ""',s:gen_rwlist)
    endif
    if line !~ '^\s*INCLUDE\%(_WORKING\)\=\>'
      continue
    endif " else ...

    " Insert contents of included files (with replacements)
    let iline = matchlist(line,'\(\s*\)\(\w*\)\s*\%(''\|"\)\(\S*\)\%(''\|"\)\s*\(\%(\w\+\s*\(''\|"\).\{-}\5\s*\)\+\)\=\%(#.*\)\=') " [0] is full line, [1] is indent, [2] is INCLUDE[_WORKING], [3] is filename, [4] is replacements
    let file = iline[3] =~ '.arb$' ? iline[3] : iline[3] . '.arb' " get name (basename + suffix) of included file
    let root_line = matchlist(getline(search('^\s*INCLUDE_\(ROOT\|FROM\)\>','bnW')),'\s*\w*\s*\%(\%(''\|"\)\(\S*\)\%(''\|"\)\)\=') " [0] is full line (minus comments), [1] is filename
    if get(root_line,1,'') == '' " no INCLUDE_ROOT or root reset
      let path = expand('#:p:h')
      let root = path =~ '/templates/' ? substitute(path,'.*/templates/','','') : ''
    else " root specified
      let root = substitute(finddir(root_line[1], templates_dir . ',' . templates_dir . '**'),'.*templates/','','')
    endif
    if iline[2] == 'INCLUDE_WORKING'
      let file = templates_dir . '../' . file
    else
      let file = findfile(file, templates_dir . root . '/**')
    endif
    if !filereadable(file)
      echohl WarningMsg | echom 'WARNING: Could not find included file ' . iline[3] | echom '  Skipping it and continuing.' | echohl None
      continue
    endif
    let include_file = readfile(file) " create list containing all lines of included file

    " Perform linewise operations on included file
    let file_rwlist = [[],[]] " [0] is list of strings to be replaced, [1] is corresponding list of replacements
    call s:ArbGetRepl(iline[4],file_rwlist)
    call map(include_file, '"  " . iline[1] . v:val') " indent contents of included file
    for i in range(0,len(include_file)-1)
      if include_file[i] =~ '^\s*\%(#\|$\)' " skip comments and blank lines
        continue
      endif
      let include_file[i] = s:ArbDoRepl(include_file[i],file_rwlist) " do file-specific replacements
      if include_file[i] =~ '^\s*\%(<<.\{-}>>\s*\)*INCLUDE\%(_WORKING\)\=\>' " nested includes
        let include_file[i] = substitute(include_file[i],'\(\s*\%(<<.\{-}>>\s*\)*\w*\s*\S*\s*\%(\%(\w\+\s*\(''\|"\).\{-}\2\s*\)\+\)\=\).*','\1 ','') . iline[4] . '# ArbIncludes:' . include_file[i] " carry over upstream replacements
      endif
      if include_file[i] =~ '^\s*\%(<<.\{-}>>\s*\)*INCLUDE_\%(ROOT\|FROM\)\s*\%(#.*\)\=$' " INCLUDE_ROOT reset
        let include_file[i] = substitute(include_file[i],'\(\s*\%(<<.\{-}>>\s*\)*\w*\).*', '\1 "','') . root . '" # ArbIncludes:' . include_file[i] " explicitly set root
      endif
    endfor
    call insert(include_file, iline[1] . '#{=====', 0) " add opening fold marker
    call extend(include_file, [iline[1] . '#}=====', 'INCLUDE_ROOT "' . root . '" # ArbIncludes']) " add closing fold marker and reset root with a dummy INCLUDE_ROOT line
    call append(line('.'), include_file) " insert contents of included file into ArbIncludes window
  endwhile

  " Cover our tracks!
  silent 1d
  silent %s/INCLUDE.*# ArbIncludes:\s*\%(<<.\{-}>>\s*\)*//e " remove dummy INCLUDEs and INCLUDE_ROOTs
  silent g/# ArbIncludes$/d " remove dummy INCLUDE_ROOTs
  if arg == 't'
    silent v/^\s*\%(\%(GENERAL_\)\=REPLACEMENTS\|INCLUDE\%(_ROOT\|_FROM\|_WORKING\)\=\|\%(STEADY-\=STATE\|TRANSIENT\)_SIMULATION\)\>/d " only show inclusion tree in 't' mode
  else
    silent %s/^# ArbIncludes: //e " remove block comment markers
  endif
  silent %s/# ArbIncludes start//e " set cursor position
  setlocal nomodifiable buftype=nofile bufhidden=wipe foldenable foldmethod=marker foldmarker={=====,}===== filetype=arb " ArbIncludes window settings

endfunction

" Perform replacements
function s:ArbDoRepl(line,rwlist)
  let line = a:line
  for i in range(0,len(a:rwlist[0])-1) " perform replacements
    if line =~ '^\s*\%(<<.\{-}>>\s*\)*\%(\%(GENERAL_\)\=REPLACEMENTS\|INCLUDE\%(_ROOT\|_FROM\|_WORKING\)\=\)\>' " only do replacements on flags before GENERAL_REPLACEMENTS and INCLUDEs
      let split = matchlist(line,'\(\s*\%(<<.\{-}>>\s*\)*\)\(.*\)',) " [0] is full line, [1] is indent and flags, [2] is everything after that
      let line = substitute(split[1], '\M' . a:rwlist[0][i], a:rwlist[1][i],'g') . split[2]
    else
      let line = substitute(line,'\M' . a:rwlist[0][i], a:rwlist[1][i],'g')
    endif
  endfor
  return line
endfunction

" Process replacements
function s:ArbGetRepl(line,rwlist)
  let repl = substitute(a:line,'\s*\%(\w*REPLACEMENTS\|INCLUDE\w*\s*\S*\)\s*\(\%(\w\+\s*\(''\|"\).\{-}\2\s*\)\+\)\%(#.*\)\=','\1','') " string containing the list of replacements
  while len(repl) > 0
    if repl[0] == 'C' " CANCEL
      let cancel = substitute(repl,'[^''"]*\(''\|"\)\(.\{-}\)\1.*','\2','')
      let index = index(a:rwlist[0],cancel)
      if index >= 0 " remove DEFAULT/REPLACE/WITH pair from replacements list
        unlet a:rwlist[0][index]
        unlet a:rwlist[1][index]
      endif
      let repl = substitute(repl,'.\{-}\(''\|"\)\M' . cancel . '\m\1\s*','','') " remove CANCEL item from repl string
    else " DEFAULT or REPLACE
      let rwpair = matchlist(repl,'\(\w*\)\s*\(''\|"\)\(.\{-}\)\2\s*\w*\s*\(''\|"\)\(.\{-}\)\4\s*') " [0] is next DEFAULT/REPLACE/WITH pair, [1] is DEFAULT or REPLACE, [3] is string to be replaced, [5] is replacement
      let index = index(a:rwlist[0],rwpair[3])
      if index < 0 " add DEFAULT/REPLACE/WITH pair to replacements list
        call add(a:rwlist[0],rwpair[3])
        call add(a:rwlist[1],rwpair[5])
      elseif and(repl[0] == 'R', a:rwlist == s:gen_rwlist) " latest REPLACE wins
        let a:rwlist[1][index] = rwpair[5]
      endif
      let repl = substitute(repl,'\M' . rwpair[0],'','') " remove DEFAULT/REPLACE/WITH pair from repl string
    endif
  endwhile
endfunction
