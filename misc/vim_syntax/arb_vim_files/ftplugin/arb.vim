" Vim settings file for arb finite volume solver
" Language:     arb
" Version:      0.54
" Modified:     2016/01/17
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

" Show contents of included files (with replacements if present) in new window
if !exists("*ArbIncludes")
  function ArbIncludes(...)
  " Accepts one optional argument, which may be:
  " 'a': show all included files and all other lines too ('all')
  " 'i': show all included files but nothing else ('includes')
  " 'o': show only included file nearest the cursor ('only') [default if no/unrecognised optional argument given]
  " 't': show only the inclusion tree/hierarchy; like 'i' but only file names shown ('tree')

    " Clear ArbIncludes window if it already exists
    if bufexists('ArbIncludes')
      bw! ArbIncludes
    endif

    " Determine mode ('a', 'i', 'o', 't') and set up accordingly
    let arg = a:0 ? a:1 : 'o' " default to 'o' if no argument given
    if arg =~ '\<\(a\|i\|t\)\>'
      new ArbIncludes
      call append(0,getbufline('#',1,'$'))
      if arg == 'i'
        silent v/^[^#]*INCLUDE\(_ROOT\)\=\>/d
        call append(0,'')
      endif
    else " default case
      let position = winsaveview()
      call cursor(line('.'),1)
      let lnum_include = search('^[^#]*INCLUDE\>','c')
      let lnum_root = search('^[^#]*INCLUDE_ROOT\>','bW')
      call winrestview(position)
      new ArbIncludes
      call append(0,getbufline('#',lnum_root))
      call append(1,getbufline('#',lnum_include))
    endif
  
    " Read included files, performing replacements as needed, and add contents to new window
    let count_includes = 0
    let include_lines = []
    let root = ''
    call cursor(1,1)
    while search('^[^#]*INCLUDE\>','W') " scan through file for INCLUDE statements
      let count_includes += 1
      if len(include_lines) < count_includes
        call add(include_lines,getline('.'))
      endif
      let line = substitute(getline('.'),"\\(\\s*$\\|\\s*#[^'\"].*$\\)",'','') " strip trailing spaces and comments from INCLUDE lines (noting that '#' may appear in replacements)
      let file = substitute(line,".*INCLUDE\\s\\+\\('\\|\"\\)\\([^\.'\"]*\\)\\(\\.arb\\)\\=\\('\\|\"\\).*",'\2.arb','') " get name (basename + suffix) of included file
      let root_line = substitute(getline(search('^[^#]*INCLUDE_ROOT\>','bnW')),'#.*$','','') " strip comments from preceding INCLUDE_ROOT line (if present)
      if root_line == '' " no INCLUDE_ROOT line
        let root = ''
      elseif root_line !~ "\\('\\|\"\\)" " INCLUDE_ROOT reset; revert to previous root
        let root = previous_root
      else " root specified (usual case)
        let previous_root = root
        let root = substitute(root_line,".*INCLUDE_ROOT\\s\\+\\('\\|\"\\)\\([^'\"]*\\).*",'\2/','')
      endif
      let dir = finddir('templates/' . root,'**;') " find directory containing the included file
      if filereadable(dir . file)
        let include_file = readfile(dir . file) " read included file
      else " flags (<<>>) can make it difficult to identify root directory correctly; just throw a warning and continue
        echohl WarningMsg
        echom "WARNING: Could not find included file " . dir . file 
        echom "  Probably inferred incorrect root."
        echohl None
        continue
      endif
      let indent = repeat(' ',match(line,'INCLUDE')) " work out indentation level
      call map(include_file, 'indent . "  " . v:val') " add indent to all lines of included file
      let repl = substitute(line,'.*INCLUDE\s\+\S\+\s*','','') " get replacements (if present)
      if len(repl) != 0 " need to make replacements in included file
        " Create list containing REPLACE and WITH items in sequence
        let tmp = repl
        let rwlist = []
        while len(tmp) > 0
          if or(match(tmp,'"') == -1, and(match(tmp,"'") != -1, match(tmp,"'") < match(tmp,'"'))) " next string is single quoted
            call add(rwlist,substitute(tmp,"[^']*'\\([^']*\\).*",'\1','')) " add item to list
            let tmp = substitute(tmp,"\\M\\[^']\\*'" . rwlist[-1] . "'",'','') " remove item from tmp string, treating special vim characters as literals (\M)
          else " next string is double quoted
            call add(rwlist,substitute(tmp,'[^"]*"\([^"]*\).*','\1','')) " add item to list
            let tmp = substitute(tmp,'\M\[^"]\*"' . rwlist[-1] . '"','','') " remove item from tmp string, treating special vim characters as literals (\M)
          endif
        endwhile
        " Perform replacements
        for i in range(0,len(include_file)-1)
          for j in range(0,len(rwlist)-1,2)
            let include_file[i] = substitute(include_file[i],'\M' . rwlist[j],rwlist[j+1],'g') " make replacements, treating special vim characters as literals (\M)
          endfor
          if include_file[i] =~ '^[^#]*INCLUDE\>' " nested includes
            call add(include_lines,include_file[i])
            let include_file[i] = substitute(include_file[i],'\(INCLUDE\s\+\S\+\)\(.*\)','\1 ' . repl . '\2','') " carry over upstream replacements (will be removed below)
          endif
        endfor
      endif
      " Insert contents of included file
      call append(line('.'),include_file)
      if or(arg != 'o', line('.') > 2) " insert fold markers
        call append(line('.'), indent . '#{=====')
        call append(line('.')+len(include_file)+1, indent . '#}=====')
      endif
    endwhile

    " Remove any nested replacements and restore end-of-line comments in include lines
    call cursor(1,1)
    while search('^[^#]*INCLUDE\>','W')
      call setline('.',include_lines[0])
      call remove(include_lines,0)
    endwhile

    " Remove unwanted lines in 'i' and 't' modes
    if arg == 't'
      silent v/^[^#]*INCLUDE\(_ROOT\)\=\>/d
    endif
    if arg =~ '\(i\|t\)'
      silent v/^[^#]*INCLUDE_ROOT\>/s/\(.*\)/  \1/
    endif

    " Adjust window settings
    call cursor(1,1)
    setlocal foldmethod=marker
    setlocal foldmarker={=====,}=====
    setlocal filetype=arb
    setlocal nomodifiable
    setlocal readonly
    setlocal bufhidden=wipe

  endfunction
endif
