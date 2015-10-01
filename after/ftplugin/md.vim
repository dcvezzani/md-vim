function! MdMakeH1()
  echo ""
  call MdMakeHeader("=")
endfunction

function! MdMakeH2()
  echo ""
  call MdMakeHeader("-")
endfunction

function! MdMakeHeader(character)
  let line = substitute(getline("."), "\\s\\+$", "", "")
  let nextline = getline(line(".") + 1)
  let border = substitute(line, ".", a:character, "g")
  call setline(".", line)
  if nextline =~ "^" . a:character . "\\+\\s*$"
    call setline(line(".") + 1, border)
  else
    call append(".", border)
  endif
endfunction

function! MdSelectTerm()
  " mark word boundaries using 'surround' plugin
  " let @z = 'ysiwx' | normal @z
  " echo expand('<cWORD>')
  if col('.') > 1
    let @z = '?\(\s\|^\)\@<=[^[:space:]]*kjv' | normal @z
  else
    let @z = 'v' | normal @z
  endif
  let startCol = col('.')

  let @z = '/[^[:space:]]*\(\s\|$\)\@!kj' | normal @z
  let stopCol = col('.')

  echo ""
endfunction

function! MdMarkTerm(character, ...)
  let origPos = getpos('.')
  let characterEnd = a:character

  if a:0 > 0
    let characterEnd = a:1
  endif

  " mark word boundaries using 'surround' plugin
  call MdSelectTerm()

  call MdMarkTermExt(origPos[1], col("'<"), col("'>"), a:character, characterEnd)

  let posOffset = strlen(a:character)
  if a:0 > 1
    let posOffset = a:2
  endif

  " put cursor back where it was before (relatively)
  let newPos = [origPos[0], origPos[1], col("'<") + posOffset, origPos[3]]
  call setpos('.', newPos)
endfunction

function! MdCreateLinkPromptUri(character, ...)
  let origPos = getpos('.')
  
  let characterEnd = a:character

  if a:0 > 0
    let characterEnd = a:1
  endif

  let @z = '' | normal @z

  call MdMarkTermExt(origPos[1], col("'<"), col("'>"), a:character, characterEnd)

  let posOffset = strlen(a:character)
  if a:0 > 1
    let posOffset = a:2
  endif

  " put cursor back where it was before (relatively)
  let newPos = [origPos[0], origPos[1], col("'>") + posOffset + strlen(characterEnd), origPos[3]]
  call setpos('.', newPos)
endfunction

function! MdMarkTermExt(lineIndex, start, stop, characterStart, characterEnd)
  let lineContent = getline(a:lineIndex)

  " break up the line; segregate the term under the cursor
  let preTerm = strpart(lineContent, 0, a:start - 1)
  let term = strpart(lineContent, a:start - 1, a:stop - a:start + 1)
  let postTerm = strpart(lineContent, a:stop, strlen(lineContent) - a:stop)

  " surround term with character(s)
  call setline(a:lineIndex, preTerm . a:characterStart . term . a:characterEnd . postTerm)
endfunction

function! MdMarkInlineCode()
  echo ""
  call MdMarkTerm('`')
endfunction

function! MdMakeBold()
  echo ""
  call MdMarkTerm('**')
endfunction

function! MdMakeImg(mode, ...)
  let path = ''
  if a:0 > 0
    let path = a:1
  endif

  if a:mode == 'n'
    call MdMarkTerm('![](' . path, ')', 2)
    " let @z = '`>' . (strlen(path) + 5) . 'l' | normal @z
  elseif a:mode == 'i'
    call MdMarkTerm('![](' . path, ')', 2)
    startinsert
  endif

endfunction

function! MdMakeLink(mode)
  if a:mode == 'n'
    call MdMarkTerm('[](', ')', 1)
  elseif a:mode == 'v'
    if stridx('' . @+, 'http') == 0
      call MdCreateLinkPromptUri('[', '](' . @+ . ')', 3)
    else
      call MdCreateLinkPromptUri('[', ']()')
      startinsert
    endif
  endif
endfunction

function! MdMarkLine(character)
  let line = substitute(getline("."), "\\s\\+$", "", "")
  let newLine = substitute(line, ".*", a:character.'\0'.a:character, "")
  call setline(".", newLine)
endfunction

function! MdMakeCodeBlock()
  echo ""
  let line = line(".")
  let newLine = substitute(getline(line), ".*", '    \0', "")
  call setline(line, newLine)
endfunction

function! MdMakeUnorderedList()
  echo ""
  let line = line(".")
  while getline(line) !~ "^\\s*$"
    let newLine = substitute(getline(line), ".*", '* \0', "")
    call setline(line, newLine)

    let line = line + 1
  endwhile
endfunction

function! MdMakeOrderedList()
  echo ""
  let cnt = 1
  let line = line(".")
  while getline(line) !~ "^\\s*$"
    let newLine = substitute(getline(line), ".*", cnt.'. \0', "")
    call setline(line, newLine)

    let cnt = cnt + 1
    let line = line + 1
  endwhile
endfunction

function! MdFixOrderedList()
  echo ""
  let ltop = line(".")
  while getline(ltop) =~ "^\\s*[0-9]\\+\\." || 
      \ (getline(ltop) =~ "^\\s" && getline(ltop) !~ "^\\s*$")
    let ltop = ltop - 1
  endwhile

  let lbot = line(".")
  while getline(lbot) =~ "^\\s*[0-9]\\+\\." || 
      \ (getline(lbot) =~ "^\\s" && getline(lbot) !~ "^\\s*$")
    let lbot = lbot + 1
  endwhile

  let ltop = ltop + 1
  let lbot = lbot - 1
  if ltop > lbot
    return
  endif

  let i = 1
  let row = ltop
  while row <= lbot
    let line = getline(row)
    if line =~ "^\\s*[0-9]\\+\\."
      call setline(row, substitute(line, "[0-9]\\+", i, ""))
      let i = i + 1
    endif
    let row = row + 1
  endwhile
endfunction

function! MdFoldLevel(lnum)
  let line = getline(a:lnum)
  let nextline = getline(a:lnum + 1)
  if nextline =~ "^=\\+\\s*$"
    return '>1'
  elseif nextline =~ "^-\\+\\s*$"
    return '>2'
  elseif line =~ "^#"
    return '>' . strlen(matchstr(line, "^#*"))
  else
    let i = a:lnum
    while i > 0
      let line = getline(i)
      let nextline = getline(i + 1)
      if nextline =~ "^=\\+\\s*$"
        return '1'
      elseif nextline =~ "^-\\+\\s*$"
        return '2'
      elseif line =~ "^#"
        return strlen(matchstr(line, "^#*"))
      endif
      let i = i - 1
    endwhile
    return '0'
  endif
endfunction

function! MdFoldText()
  let line = getline(v:foldstart)
  let nextline = getline(v:foldstart + 1)
  if line !~ "^#"
    if nextline =~ "^="
      return ("# " . line)
    elseif nextline =~ "^-"
      return ("## " . line)
    endif
  endif
  return line
endfunction

function! MdFold()
  echo ""
  set foldenable
  set foldmethod=expr
  set foldexpr=MdFoldLevel(v:lnum)
  set foldtext=MdFoldText()
  set foldlevel=0
endfunction

" Shortcuts
vmap <buffer> qc :call MdMakeCodeBlock()<CR>
vmap <buffer> qk :call MdMakeLink('v')<CR>

"nunmap <buffer> qk
nmap <buffer> qi :call MdMakeImg('n', 'images/')<CR>
nmap <buffer> qI :call MdMakeImg('i', 'images/')<CR>
nmap <buffer> qk :call MdMakeLink('n')<CR>i
nmap <buffer> qc :call MdMarkInlineCode()<CR>
nmap <buffer> qlu :call MdMakeUnorderedList()<CR>
nmap <buffer> qlo :call MdMakeOrderedList()<CR>
nmap <buffer> qb :call MdMakeBold()<CR>
nmap <buffer> q= :call MdMakeH1()<CR>
nmap <buffer> q- :call MdMakeH2()<CR>
nmap <buffer> qlf :call MdFixOrderedList()<CR>
nmap <buffer> qz :call MdFold()<CR>
nmap <buffer> qp :!mdprev %<CR><CR>
nmap <buffer> qP :!mdprev --pdf %<CR><CR>
