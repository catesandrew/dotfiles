" Vim ftplugin file
" Language:     Google Code Wiki, http://code.google.com/p/support/wiki/WikiSyntax
" Maintainer:   Silas Silva <silasdb@gmail.com>
" Home:         http://code.google.com/p/vimgcwsyntax/
" Other Home:   http://www.vim.org/scripts/script.php?script_id=3173
" Filenames:    *.wiki
" Version:      1.0

" Description
" ===========
"
" This is a ftplugin for Google Code Wiki files.  It has a powerful function
" for the formatexpr option. Just :set formatexpr=googlecodewiki#FormatExpr()
" and you will be able to format your texts without breaking link blocks or
" quote blocks. This features can be turned off with global variables.
"
"
" Customized Format expression:
"
" This file comes with the googlecodewiki#FormatExpr() function that implements
" a function to format Google Code Wiki files.  Sometimes it is desirable to
" use this function instead of default Vim format rules.  To use this function,
" just :set formatexpr=googlecodewiki#FormatExpr().
"
" One of the most useful features of this function is that it doesn't break
" links (all text surrounded by "[" and "]") and in-line code (all text
" surrounded by "`").  To change this behaviour, change the following variables:
"
" g:googlecodewiki_break_inside_brackets: if 1, break text surrounded by "["
" and "]".
"
" g:googlecodewiki_break_inside_graves: if 1, break text surrounded by "`"
"
"
" Install Details
" ===============
"
" Just put it in your local ftplugin directory.  In you .vimrc, include the
" following line to associate .wiki files to the googlecodewiki ftplugin:
"
"     autocmd BufNewFile,BufRead *.wiki set ft=googlecodewiki



" {{{1
" Global variables that change FormatExpr() behaviour.

if !exists("g:googlecodewiki_break_inside_brackets")
    let g:googlecodewiki_break_inside_brackets = 0
endif

if !exists("g:googlecodewiki_break_inside_graves")
    let g:googlecodewiki_break_inside_graves = 0
endif

" {{{1
" Format expression function to be set by the user, if he wants.  Just
" :set formatexpr=googlecodewiki#FormatExpr()
function! googlecodewiki#FormatExpr()
    if &textwidth == 0
        return
    endif

    if mode() == "i"
        call s:FormatInsertMode(v:lnum)
    else
        call s:FormatNormalMode(v:lnum, v:count)
    endif
endfunction


" Caveats of the functions above (should be fixed in future):
"
" TODO: Doesn't format correctly two or more neighboring blank lines.
" It lacks handling of formatoptions.  'M' option is specially important.


" {{{1
" Breaks a line according to the rules.
function! s:BreakLine(line, textwidth)
    let col = 1
    let i = 0
    let ls = -1
    let last = -1
    let in_brackets = 0
    let in_graves = 0
    let breaks = []
    while i < strlen(a:line)
        if a:line[i] =~ '\s' && !in_brackets && !in_graves
            " Store the last blank space where we want to break the line.
            let ls = i
        endif

        if !g:googlecodewiki_break_inside_brackets && a:line[i] == '['
            let in_brackets = 1
        endif
        if !g:googlecodewiki_break_inside_brackets && a:line[i] == ']'
            let in_brackets = 0
        endif

        if !g:googlecodewiki_break_inside_graves && a:line[i] == '`'
            let in_graves = !in_graves
        endif

        if col > a:textwidth && ls != last
            let breaks = add(breaks, ls)
            let in_brackets = 0
            let in_graves = 0
            let col = 1
            let last = ls
            let i = ls
        else
            let col += 1
        endif
        let i += 1
    endwhile

    " The line can be long, but anyway it can be continuous.  E.g.: [a long
    " text in brackets].  If the line don't break, return a one-element list:
    " the line itself.
    if empty(breaks)
        return [a:line]
    endif

    let lines = []
    let linestart = 0
    let breaks = add(breaks, strlen(a:line))
    for brk in breaks
        let lineend = brk
        let newline = strpart(a:line, linestart, lineend-linestart)

        " Chop out trailing spaces
        let newline = substitute(newline, '^\s\+', '', '')

        let lines = add(lines, newline)
        let linestart = lineend
    endfor

    return lines
endfunction

" {{{1
" Format expression for the insert mode (private function).
function! s:FormatInsertMode(lnum)
    let col = col('.')
    if col <= &textwidth
        return
    endif

    " We parse the entire line.
    let line = getline('.')
    let length = strlen(line)
    let col = col('.')

    let lines = s:BreakLine(line, &textwidth-1)

    if len(lines) == 1
        return
    endif

    " Append as new lines.
    call append((a:lnum-1), lines)

    " And delete old ones.
    " TODO: It echos "x fewer lines" in Vim prompt.  Is it possible to replace
    " append + exec for a better manner?  Maybe with setline()?
    exec ":.d"

    " offset from the end of the line
    let back = length - col

    " Set the cursor to the line below (created after break).
    call cursor(a:lnum+1, strlen(lines[1]) - back)
endfunction


" {{{1
" Format expression for insert mode (private function)
function! s:FormatNormalMode(lnum, count)
    let lines = getline(a:lnum, a:lnum + a:count - 1)

    " The following loop implements a state machine to detect paragraphs.  It
    " has three states:
    "
    " - "new": A new paragraph was detected
    " - "paragraph": the parser is yet on the paragraph detected before
    " - "end": the end of the paragraph was found.
    "
    " When the state machine reaches the "end" state, it is time to format the
    " current paragraph.  It joins all the lines of the paragraph and call
    " BreakLine() on it.
    let i = 0
    let state = 'new'
    let start_par = 0
    let end_par = 0
    while i < len(lines)
        " Was a new paragraph detected?
        if state == 'new'
            if (s:LineIsBlank(lines[i]))
                let i += 1
                continue
            else
                let state = 'paragraph'
                let start_par = i
            endif
        endif

        " Are we still in the last paragraph we detected?
        if state == 'paragraph'
            if (! s:LineIsBlank(lines[i]) && i < len(lines)-1)
                let i+= 1
                continue
            else
                let state = 'end'
                let end_par = i
            endif
        endif

        " Is it the end of the paragraph?
        if state == 'end'
            " Normally, this variable should be zero.  But for one special
            " case, when my range is just one line (in the case the user wants
            " to format only one line explicitally (in visual mode or passing a
            " range) or implicitally (one last at the end of the file), then I
            " should set it to one to sum it below.  If I don't do that, the
            " delete line triggers an error.
            let offset = 0
            if end_par - start_par == 0
                let offset = 1
            endif

            let all_lines = join(s:SubList(lines, start_par, end_par-start_par+offset), " ")
            let newlines = s:BreakLine(all_lines, &textwidth)

            " Delete old line
            " TODO: It echos "x fewer lines" in Vim prompt.  Is it possible to
            " replace append + exec for a better manner?  Maybe with setline()?
            exec ":" . (a:lnum+start_par) . "d " . (end_par - start_par + offset)

            " Append new one
            call append((a:lnum+start_par-1), newlines)
        endif
        let i += 1
    endwhile
endfunction

"{{{1
" Returns true or false if a line is blank (contains no characters or only
" spaces).
function! s:LineIsBlank(str)
    return (a:str =~ '^\s*$')
endfunction

"{{{1
" Given a 'list', returns a sublist beginning in 'start', with 'count' items.
function! s:SubList(list, start, count)
    let l = []
    let i = 0
    while i < a:count && i < len(a:list)
        let l = add(l, a:list[i+a:start])
        let i += 1
    endwhile
    return l
endfunction

" vim: set tw=0 et sw=4 sts=4 fdm=marker:
