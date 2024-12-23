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

" a:1 - re for additional term boundaries
" a:2 - leave term selected
function! MdSelectTerm(...)
  echo ""
  " mark word boundaries using 'surround' plugin
  " let @z = 'ysiwx' | normal @z
  " echo expand('<cWORD>')
  let reTermBoundaries = '[:space:]'

  if(a:0 > 0 && strlen(a:1) > 0)
    let reTermBoundaries = a:1
  endif

  let atWordBoundary = (match(strpart(getline('.'), col('.')-2, 1), '[' . reTermBoundaries . ']') == 0)
  if(col('.') > 1 && !atWordBoundary)
    call search('\([' . reTermBoundaries . ']\|^\)\@<=[^' . reTermBoundaries . ']*', 'bW')
  endif
  let startCol = col('.')
  let @z = 'v' | normal @z

  let atWordBoundary = (match(strpart(getline('.'), col('.'), 1), '[' . reTermBoundaries . ']') == 0)
  if(!atWordBoundary)
    call search('[^' . reTermBoundaries . ']*\([' . reTermBoundaries . ']\|$\)\@!', 'W')
  endif
  let stopCol = col('.')

  if(a:0 == 0 || (a:0 > 1 && a:2 != 'true'))
    let @z = '' | normal @z
  endif
  
  return [startCol, stopCol]
endfunction

function! MdMarkTerm(character, ...)
  let origPos = getpos('.')
  let characterEnd = a:character

  if a:0 > 0
    let characterEnd = a:1
  endif

  " mark word boundaries using 'surround' plugin
  let bounds = MdSelectTerm()

  call MdMarkTermExt(origPos[1], bounds[0], bounds[1], a:character, characterEnd)

  let posOffset = strlen(a:character)
  if a:0 > 1
    let posOffset = a:2
  endif

  " put cursor back where it was before (relatively)
  let newPos = [origPos[0], origPos[1], col("'<") + posOffset, origPos[3]]
  call setpos('.', newPos)
endfunction

function! MdCreateLinkPromptUri(...)
  call call("MdMarkTerms", a:000)
endfunction

function! MdMarkTerms(...)
  let origPos = getpos('.')
  
  if a:0 > 0
    let characterBegin = a:1
  endif

  let characterEnd = characterBegin

  if a:0 > 1
    let characterEnd = a:2
  endif

  let @z = '' | normal @z

  call MdMarkTermExt(origPos[1], col("'<"), col("'>"), characterBegin, characterEnd)

  let posOffset = strlen(characterBegin)
  if a:0 > 2
    let posOffset = a:3
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

" function! MdMakeBold()
"   echo ''
"   call MdMarkTerm('**')
" endfunction

function! MdMakeBold(mode)
  if a:mode == 'n'
    call MdMarkTerm('**', '**', 1)
  elseif a:mode == 'v'
    call MdMarkTerms('**', '**', 3)
  endif
endfunction

function! MdMakeHdr(mode, level)
  let curPos = getpos('.')
  let line = substitute(getline("."), "^#\\+\\s*\\(.*\\)$", "\\1", "")
  call setline(".", line)
  call setpos('.', [curPos[0], curPos[1], 0, curPos[3]])

  if a:mode == 'n'
    call MdMarkTerm(repeat('#', a:level).' ', '', 1)
  elseif a:mode == 'v'
    call MdMarkTerms(repeat('#', a:level).' ', '', 3)
  endif
  call setpos('.', [curPos[0], curPos[1], a:level+2, curPos[3]])
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
  let @+ = substitute(@+, "\n", '', "")
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

function! MdMakeLiDone(mode)
  let @+ = substitute(@+, "\n", '', "")
  if a:mode == 'n'
    call MdMarkLine('<li class="done">', '</li>', 1)
  elseif a:mode == 'v'
    call MdCreateLinkPromptUri('[', '](' . @+ . ')', 3)
  endif
endfunction

function! MdSplitOnBounds(strValue, start, stop)
  let preTerm = strpart(a:strValue, 0, a:start - 1)
  let term = strpart(a:strValue, a:start - 1, a:stop - a:start + 1)
  let postTerm = strpart(a:strValue, a:stop, strlen(a:strValue) - a:stop)
  return [preTerm, term, postTerm]
endfunction

function! MdMakeLinkUsingCurrentTerm02()
  let mode = 'v'
  let bounds = MdSelectTerm('[:space:]\/')
  let selectedTermInfo = MdGetSelectedTerm()

  let selectedTerm = selectedTermInfo[0]
  let startPos = selectedTermInfo[1]
  let endPos = selectedTermInfo[2]
  
  let reProtocol = '^[[:alnum:]]\+:\/\/'
  let @z = '' | normal @z
  let protocolExists = (match(selectedTerm, reProtocol) == 0)

  if protocolExists == 0
    let selectedTerm = 'http://' . selectedTerm
  else
    let bounds = MdSelectTerm()
  end
  
  " let @z = '' | normal @z
  let newTerm = MdTitlizeWords(mode)
  " let @z = 'gv' | normal @z

  let @+ = selectedTerm
  call MdMakeLink(mode)
  " return protocolExists
endfunction

function! MdMakeLinkUsingCurrentTerm(mode)
  let bounds = MdSelectTerm('[:space:]\/\.#')
  let strParts = MdSplitOnBounds(getline('.'), bounds[0], bounds[1])
  let newTerm = MdTitlizeWords('', strParts[1])

  call MdMakeLink(a:mode)
  let curPos = getpos('.')
  let @+ = newTerm
  let @z = 'h"+p' | normal @z
  call setpos('.', [curPos[0], curPos[1]+1, 0, curPos[3]])
endfunction

function! MdMarkTerm(character, ...)
  let origPos = getpos('.')
  let characterEnd = a:character

  if a:0 > 0
    let characterEnd = a:1
  endif

  " mark word boundaries using 'surround' plugin
  let bounds = MdSelectTerm()

  call MdMarkTermExt(origPos[1], bounds[0], bounds[1], a:character, characterEnd)

  let posOffset = strlen(a:character)
  if a:0 > 1
    let posOffset = a:2
  endif

  " put cursor back where it was before (relatively)
  let newPos = [origPos[0], origPos[1], col("'<") + posOffset, origPos[3]]
  call setpos('.', newPos)
endfunction


function! MdMakeCodeBlock(...) range
  echo ""
  let flush_left = 0

  let @g = string(a:1)
  " determine code block style
  " 0: indent
  " 1: flush left, use ``` block
  if a:0 > 0
    if a:1 == 'true'
      let flush_left = 1
    endif
  endif
  
  if flush_left == 0
    let line = line(".")
    let newLine = substitute(getline(line), ".*", '    \0', "")
    call setline(line, newLine)

  else
    " ensure normal mode
    let @z = '' | normal @z

    " remember row positions of highlighted text
    let start_row = line("'<")
    let stop_row = line("'>")

    call append(stop_row, '```')

    let origPos = getpos('.')
    call setpos('.', [origPos[0], start_row, 0, origPos[3]])
    let @z = 'O' | normal @z
    call setline('.', '```')

    " place cursor before code block
    let focus_row = 0
    if start_row > 0
      let focus_row = start_row-1
    endif

    call setpos('.', [origPos[0], focus_row, 0, origPos[3]])
  endif
endfunction

function! MdCopyFootnoteReference()
  let line = line(".")
  " http://vimdoc.sourceforge.net/htmldoc/pattern.html#/\zs
  let ref = matchstr(getline(line), '\"\zs\(\([^\"0-9]\+\)-\(\d\+\)\)\ze\"')
  let refNum = matchstr(getline(line), '\"\(\([^\"0-9]\+\)-\zs\(\d\+\)\ze\)\"')
  let @t = '<sup>[' . refNum . '](#' . ref .')</sup>'
  echo 'copied footnote reference: ' . @t
endfunction

function! MdPasteFootnoteReference()
  let curLine = line('.')
  if(match(@t, '^<sup>') == 0)
    let startStopPoints = MdSelectTerm()
    " echo startStopPoints[1]
    let curPos = getpos('.')
    call setpos('.', [curPos[0], curLine, startStopPoints[1], curPos[3]])
    let @z = '"tp' | normal @z
  endif
endfunction

function! MdMakeFootnotes(mode)
  echo ""
  if a:mode == 'v'
    call MdMakeFootnotesVis()
  else
    call MdMakeFootnotesNorm()
  endif
endfunction

function! MdMakeFootnotesNorm(...)
  let line_count = line('$')
  let line = line(".")
  let cnt = 1
  
  "let @b = '$bdir/'
  while ((getline(line) !~ '^\s*$') && (line < line_count))
    let curPos = getpos('.')
    let cnt = cnt + 1
    let line = line + 1
    call MdMakeFootnotesVis()
    call setpos('.', [curPos[0], curPos[1]+1, 0, curPos[3]])
  endwhile
endfunction

function! MdMakeFootnotesVis()
  echo ""
  let line = line(".")
  let newLine = substitute(getline(line), ".*", '* <a name="footnote-' . line . '">' . line . '</a>: \0', "")
  call setline(line, newLine)
endfunction

function! MdHideContent(mode) range
  if a:mode == 'v'
    call MdHideContentVis()
  else
    call MdHideContentNorm()
  endif
endfunction

function! MdHideContentNorm()
  let line_count = line('$')
  let line = line(".")
  let origPos = getpos('.')
  
  while ((getline(line) !~ '^\s*$') && (line < line_count))
    let line = line + 1
  endwhile

  call MdHideContentAux(origPos[1]-1, line)
endfunction

function! MdHideContentVis() range
  let start_row = line("'<")
  let stop_row = line("'>")

  call MdHideContentAux(start_row-1, stop_row+1)
endfunction

function! MdHideContentAux(start_row, stop_row) range
  call append(a:start_row, '<div style="display: none;">')
  call append(a:stop_row, '</div>')
endfunction

" Create lists.  Stops at the first empty line.  If bullets of the current type
" are encountered they are reformatted so that ul removes extra white space
" from the front or existing ol bullets are renumbered.
"
" a:1; prefix: prepend value after the bullet
" a:2; bullet: character or string representing the bullet; default value: '*'
" a:3; list_type: ol | ul; ol: ordered list (numbers); ul: unordered list
"
" " make an unordered list (*)
" call call("MdMakeList", [ '', '', 'ul' ])
"
" " make an ordered list (numbers)
" call call("MdMakeList", [ '', '', 'ol' ])
"
" " make a non-md list using a custom bullet
" call call("MdMakeList", [ '', '-' ])

function! MdMakeList(...)
  echo ""
  let cnt = 1

  if a:0 > 0
    let prefix = a:1
  else
    let prefix = ''
  endif
  
  if ((a:0 > 2) && (a:3 == 'ol'))
    let list_type = a:3
  else
    let list_type = 'ul'
  endif
  
  if ((a:0 > 1) && (strlen(a:2) > 0))
    let bullet = a:2
  else
    let bullet = '*'
  endif
  
  let line_count = line('$')
  let line = line(".")
  "let @b = '$bdir/'
  while ((getline(line) !~ '^\s*$') && (line < line_count))
    if (list_type == 'ol')
      let bullet = cnt.'.'
    endif

    " let reBulletLeeder = '^[^abcdefghijklmnopqrstuvwyzABCDEFGHIJKLMNOPQRSTUVWYZ0-9]*\(.*\)$'
    let reBulletLeeder = '^\([0-9]\. \|- \[[^\]*]\] \|- ✅ \|- \|* \)\+\(.*\)$'

    let newLine = getline(line)
    if(newLine =~ reBulletLeeder)
      let newLine = substitute(newLine, reBulletLeeder, '\2', "")
    endif
    

    if (list_type == 'ul')
      let newLine = substitute(newLine, ".*", bullet.' '.prefix.'\0', "")
      call setline(line, newLine)

    elseif (list_type == 'ol')
      let newLine = substitute(newLine, ".*", bullet.' '.prefix.'\0', "")
      call setline(line, newLine)
    endif

    let cnt = cnt + 1
    let line = line + 1
  endwhile
endfunction

function! MdMakeListDash(...)
  call call("MdMakeList", [ '', '-' ])
endfunction

function! MdMakeListCheckbox(...)
  let snippet = '- [ ]'
  
  if ((a:0 > 0) && (a:1 == 'checked'))
    let snippet = '- [x]'
    let snippet = '- ✅'
  endif
  
  call call("MdMakeList", [ '', snippet ])
endfunction

function! MdMakeListCheckboxChecked(...)
  call call("MdMakeList", [ '', '- [x]' ])
endfunction

function! MdMakeUnorderedList()
  call call("MdMakeList", [ '', '', 'ul' ])
endfunction

function! MdMakeOrderedList()
  call call("MdMakeList", [ '', '', 'ol' ])
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

function! MdReplaceSubstring(line, subString, startPos, endPos)
  let srcString = getline(a:line)
  if strlen(a:subString) > 0
    let curPos = getpos('.')
    let preString = strpart(srcString, 0, a:startPos-1)
    let postString = strpart(srcString, a:endPos, strlen(srcString))
    call setline(a:line, preString . a:subString . postString)
    call setpos('.', [curPos[0], a:line, strlen(preString) + strlen(a:subString), curPos[3]])
  endif
endfunction

function! MdResolveSnippet()
  echo ""

  let bounds = MdSelectTerm('[:space:]()\[\]<>\"', 'false')
  let term = strpart(getline('.'), bounds[0]-1, bounds[1]-(bounds[0]-1))

  let newTerm = ''
  if term == 'ds'
    let newTerm = substitute(term, '.*', strftime("%Y-%m-%d"), '')
  elseif term == 'dts'
    let newTerm = substitute(term, '.*', strftime("%Y-%m-%dT%H:%M:%S"), '')
  elseif term == 'tds'
    let newTerm = substitute(term, '.*', strftime("%Y-%m-%dT%H:%M:%S"), '')
  elseif term == 'dts2'
    let theDate = substitute(strftime("%A, %B %e %Y; %H:%M %Z"), '  *', ' ', 'g')
    let newTerm = substitute(term, '.*', theDate, '')
  elseif term == 'tds2'
    let theDate = substitute(strftime("%A, %B %e %Y; %H:%M %Z"), '  *', ' ', 'g')
    let newTerm = substitute(term, '.*', theDate, '')
  endif

  call MdReplaceSubstring(line('.'), newTerm, bounds[0], bounds[1])
endfunction

function! MdGetSelectedTerm()
  let selectedTerm = strpart(getline('.'), col("'<")-1, col("'>")-(col("'<")-1))
  let startPos = col("'<")
  let endPos = col("'>")
  return [selectedTerm, startPos, endPos]
endfunction

" ('n', '', [regex, replace, flags])               normal: apply to entire current line
" ('v', '', [regex, replace, flags])               visual: get term from current selection
" ('', selectedTerm, [regex, replace, flags])      apply to given term
function! MdModifyTerms(mode, ...)
  let regex = a:2[0]
  let replace = a:2[1]
  let flags = a:2[2]
  
  if(a:mode == 'v') "if applying to selection
    let selectedTermInfo = MdGetSelectedTerm()
    let selectedTerm = selectedTermInfo[0]
    let startPos = selectedTermInfo[1]
    let endPos = selectedTermInfo[2]

    let newSelectedTerm = substitute(selectedTerm, regex, replace, flags)
    call MdReplaceSubstring(line('.'), newSelectedTerm, startPos, endPos)

  elseif(a:mode == 'n') "if applying to current line
    let newLine = substitute(getline("."), regex, replace, flags)
    echo newLine
    call setline(line("."), newLine)

  else "if applying to provided term
    let selectedTerm = a:1 
    let newSelectedTerm = substitute(selectedTerm, regex, replace, flags)
    return newSelectedTerm
  endif
endfunction

function! MdTest(...)
  " echo MdModifyTerms('', "* Contribute to `core/wiki`; document", ['\<.', '\u\0', 'g'])
  " echo MdModifyTerms('v', '', ['\<.', '\u\0', 'g'])
  " echo MdModifyTerms('n', '', ['\<.', '\u\0', 'g'])
endfunction


function! MdTrimSpaces(term)
  let pattern2 = ['^\s\+', '', 'g']
  let pattern3 = ['\s\+$', '', 'g']

  let newTerm = MdModifyTerms('', a:term, pattern2)
  let newTerm = MdModifyTerms('', newTerm, pattern3)

  return newTerm
endfunction


" s/\<./\u&/g
function! MdCapitalizeFirstLetterInWords(...)
  let pattern = ['\<.', '\u\0', 'g']
  if(a:0 > 1 && strlen(a:2) > 0)
    return MdModifyTerms('', a:2, pattern)
  else
    if(a:0 > 0)
      let mode = a:1
    else
      let mode = 'n'
    end

    call MdModifyTerms(mode, '', pattern)
  endif
endfunction

function! MdSentence(...)
  let curString = getline('.')
  let startPos = 0
  let endPos = strlen(curString)
  let newString = substitute(getline('.'), '^.', '\u&', '')
  let newString = substitute(newString, '\([^\.\?\!]\)$', '\1\.', '')
  let endPos = strlen(newString)
  call MdReplaceSubstring(line('.'), newString, startPos, endPos)
endfunction

function! MdTitlizeWords(...)
  let pattern = ['[^0-9a-zA-Z]\+', ' ', 'g']
  if(a:0 > 1 && strlen(a:2) > 0)
    let newString = MdCapitalizeFirstLetterInWords('', a:2)
    let newString = MdModifyTerms('', newString, pattern)
    return MdTrimSpaces(newString)

  else
    if(a:1 == 'v')
      let curStringInfo = MdGetSelectedTerm()
      let curString = curStringInfo[0]
      let startPos = curStringInfo[1]
      let endPos = curStringInfo[2]
    else " == 'n'
      let curString = getline('.')
      let startPos = 0
      let endPos = strlen(curString)
    end

    let newString = MdCapitalizeFirstLetterInWords('', curString)
    let newString = MdModifyTerms('', newString, pattern)
    let newString = MdTrimSpaces(newString)

    call MdReplaceSubstring(line('.'), newString, startPos, endPos)
  endif
endfunction

function! MdSelectLine(...)
  let curPos = getpos('.')
  let curLen = len(getline('.'))
  let startCol = 1
  let stopCol = curLen

  call setpos('.', [curPos[0], curPos[1], startCol, 0])
  let @z = 'v' | normal @z
  call setpos('.', [curPos[0], curPos[1], stopCol, 0])
 
  if(a:0 > 0 && a:1 != 'true')
    let @z = '' | normal @z
  endif
 
  return [startCol, stopCol]
endfunction

function! MdMarkLine(character, ...)
  let origPos = getpos('.')
  let characterEnd = a:character

  if a:0 > 0
    let characterEnd = a:1
  endif

  let bounds = MdSelectLine('false')
  
  call MdMarkTermExt(origPos[1], bounds[0], bounds[1], a:character, characterEnd)

  let posOffset = strlen(a:character)
  if a:0 > 1
    let posOffset = a:2
  endif

  " put cursor back where it was before (relatively)
  let newPos = [origPos[0], origPos[1], col("'<") + posOffset, origPos[3]]
  call setpos('.', newPos)
endfunction


" Temp functions
" when managing unordered lists, easily mark the list item as 'done'
" requires a style to be defined for li and li.done
function! MdMakeLiDone(mode)
  "let @+ = substitute(@+, "\n", '', "")
  let line = line(".")
  let newLine = substitute(getline("."), "^\*\\s*\\(.*\\)$", "\\1", "")
  call setline(line, newLine)
  
  let startToken = '<li class="done">'
  let endToken = '</li>'
  let lenStartToken = strlen(startToken)
  if a:mode == 'n'
    call MdMarkLine(startToken, endToken, 1)

    " post work; position at beginning of line
    " and keep there when moving cursor
    let curPos = getpos('.')
    call setpos('.', [curPos[0], curPos[1]+1, 1, 0])
    let @z = '^' | normal @z
    
  elseif a:mode == 'v'
    call MdMarkTerms(startToken, endToken, lenStartToken)
  endif
endfunction

function! MdSelectBlock()
  let @z = 'V/\n\nkj' | normal @z
endfunction


let g:re_standup_bullet = "^[-\\*] \\(\\[[ x]\\] \\+\\)*[◐◑● ]*\\(.*\\)"
let g:re_work_log_entry = "^\\([^; ]\\+\\); *\\(.*\\)"
function! MdRemoveBullet()
  let line = substitute(getline("."), g:re_standup_bullet, "\\2", "")
  call setline(line("."), line)
endfunction

function! MdReplaceBulletYesterday()
  let line = substitute(getline("."), g:re_standup_bullet, "\\2", "")
  call setline(line("."), '- ◐ ' . line)
endfunction

function! MdReplaceBulletToday()
  let line = substitute(getline("."), g:re_standup_bullet, "\\2", "")
  call setline(line("."), '- ◑ ' . line)
endfunction

function! MdReplaceBulletYesterdayAndToday()
  let line = substitute(getline("."), g:re_standup_bullet, "\\2", "")
  call setline(line("."), '- ● ' . line)
endfunction

function! MdAddTimeSpent()
  let line = substitute(getline("."), g:re_work_log_entry, "\\2", "")
  let timeSpent = substitute(getline("."), g:re_work_log_entry, "\\1", "")

  if(strlen(timeSpent) > 0 && (match(timeSpent, "^[0-9\\.]\\+$") == 0))
    let timeSpent = str2float(timeSpent,10) + 0.5
    call setline(line("."), timeSpent . '; ' . line)
  else
    call setline(line("."), '0.5; ' . timeSpent . '; ' . line)
  endif
  
echo timeSpent
endfunction


" Shortcuts
vmap <buffer> qtf :call MdMakeFootnotes('v')<CR>
vmap <buffer> qc :call MdMakeCodeBlock('true')<CR>
vmap <buffer> qk :call MdMakeLink('v')<CR>
vmap <buffer> qb :call MdMakeBold('v')<CR>
vmap <buffer> qfc :call MdCapitalizeFirstLetterInWords('v')<CR>
vmap <buffer> qft :call MdTitlizeWords('v')<CR>
vmap <buffer> qhb :call MdHideContent('v')<CR>

"nunmap <buffer> qk
nmap <buffer> qft :call MdTitlizeWords('n')<CR>
nmap <buffer> qfs :call MdSentence('n')<CR>
nmap <buffer> qs :call MdSelectTerm('', 'true')<CR>
nmap <buffer> qfc :call MdCapitalizeFirstLetterInWords()<CR>
nmap <buffer> qj :call MdResolveSnippet()<CR>
nmap <buffer> qtt :call MdCopyFootnoteReference()<CR>
nmap <buffer> qtr :call MdPasteFootnoteReference()<CR>
nmap <buffer> <silent> qtf :call MdMakeFootnotes('n')<CR>
nmap <buffer> qi :call MdMakeImg('n', 'images/')<CR>
nmap <buffer> qI :!/Users/dcvezzani/scripts/rename-images.sh<CR>
"nmap <buffer> qi :call MdMakeImg('n', '')<CR>
"nmap <buffer> qI :call MdMakeImg('i', '')<CR>
nmap <buffer> qk :call MdMakeLink('n')<CR>i
nmap <buffer> qK :call MdMakeLinkUsingCurrentTerm('n')<CR>
" nmap <buffer> qK :call MdMakeLinkUsingCurrentTerm02()<CR>
nmap <buffer> qc :call MdMarkInlineCode()<CR>
nmap <buffer> qlu :call MdMakeUnorderedList()<CR>
" nmap <buffer> qll :call MdMakeUnorderedList(@b)<CR>
nmap <buffer> qlo :call MdMakeOrderedList()<CR>
" nmap <buffer> qld :call MdMakeLiDone('n')<CR>

nmap <buffer> qls :call MdSelectBlock()<CR>

nmap <buffer> qly :call MdReplaceBulletYesterday()<CR>
nmap <buffer> qlt :call MdReplaceBulletToday()<CR>
nmap <buffer> qlb :call MdReplaceBulletYesterdayAndToday()<CR>
nmap <buffer> qln :call MdRemoveBullet()<CR>
nmap <buffer> qlj :call MdAddTimeSpent()<CR>

vmap <buffer> qly :call MdReplaceBulletYesterday()<CR>
vmap <buffer> qlt :call MdReplaceBulletToday()<CR>
vmap <buffer> qlb :call MdReplaceBulletYesterdayAndToday()<CR>
vmap <buffer> qln :call MdRemoveBullet()<CR>
vmap <buffer> qlj :call MdAddTimeSpent()<CR>

nmap <buffer> qli :call MdMakeListDash()<CR>
nmap <buffer> qlx :call MdMakeListCheckbox()<CR>
nmap <buffer> qlX :call MdMakeListCheckbox('checked')<CR>
nmap <buffer> qb :call MdMakeBold('n')<CR>
nmap <buffer> qh2 :call MdMakeHdr('n', 2)<CR>
nmap <buffer> qh3 :call MdMakeHdr('n', 3)<CR>
nmap <buffer> qh4 :call MdMakeHdr('n', 4)<CR>
nmap <buffer> qh5 :call MdMakeHdr('n', 5)<CR>
nmap <buffer> qh6 :call MdMakeHdr('n', 6)<CR>
nmap <buffer> qhb :call MdHideContent('n')<CR>
" nmap <buffer> q= :call MdMakeH1()<CR>
" nmap <buffer> q- :call MdMakeH2()<CR>
nmap <buffer> qlf :call MdFixOrderedList()<CR>
nmap <buffer> qz :call MdFold()<CR>
" nunmap qp
" nunmap qP
" nmap <buffer> qp :!mdprev %<CR><CR>
" nmap <buffer> qP :!mdprev --pdf %<CR><CR>
