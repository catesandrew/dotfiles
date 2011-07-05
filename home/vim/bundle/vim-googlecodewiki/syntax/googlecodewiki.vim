" Vim syntax file
" Language:     Google Code Wiki, http://code.google.com/p/support/wiki/WikiSyntax
" Maintainer:   Silas Silva <silasdb@gmail.com>
" Original:     FlexWiki, mantained by George V. Reilly
" Home:         http://code.google.com/p/vimgcwsyntax/
" Other Home:   http://www.vim.org/scripts/script.php?script_id=3173
" Filenames:    *.wiki
" Version:      1.0


" Description
" ===========
"
" This is a syntax file for Google Code Wiki, supporting highlighting of main
" features of Google Code Wiki, like:
"
" * Formating (bold, emphasis etc.)
" * Code text
" * Block links
" * Google Code Wiki pragmas
" * Titles
" * Folding between sections and in code block
"
" Check comments on the beginning of the ftplugin/googlecodewiki.vim for
" information about formating.
"
"
" Install Details
" ===============
"
" Just put it in your local syntax directory.  In you .vimrc, include the
" following line to associate .wiki files to the googlecodewiki syntax:
"
"     autocmd BufNewFile,BufRead *.wiki set ft=googlecodewiki


if exists("b:current_syntax")
    finish
endif

syntax region googlecodewikiPragmaRegion  start=/^\%1l.*$/ end=/^[^#]*$/ contains=googlecodewikiPragma
syntax match googlecodewikiPragma         /^#.*$/ contained

syntax keyword googlecodewikiTodo         TODO contained
syntax region googlecodewikiCommentRegion start='<wiki:comment>' end='</wiki:comment>' contains=googlecodewikiTodo

" TODO: check URL syntax against RFC
syntax match googlecodewikiLink           `\("[^"(]\+\((\([^)]\+\))\)\?":\)\?\(https\?\|ftp\|gopher\|telnet\|file\|notes\|ms-help\):\(\(\(//\)\|\(\\\\\)\)\+[A-Za-z0-9:#@%/;$~_?+-=.&\-\\\\]*\)` contains=@NoSpell
syntax region googlecodewikiLinkRegion    start=/\[/ end=/\]/ contains=googlecodewikiLink oneline

" TODO: The use of one of the typefaces bellow prevents the use of other.  How
" to combine them?

" text: *strong*
syntax match googlecodewikiBold            /\(^\|\W\)\zs\*\([^ ].\{-}\)\*/

" text: _emphasis_
syntax match googlecodewikiItalic          /\(^\|\W\)\zs_\([^ ].\{-}\)_/

" text: `code`
syntax match googlecodewikiCode            /`[^`]*`/ contains=@NoSpell

" text: {{{code}}}
syntax region googlecodewikiCodeRegion     start=/{{{/ end=/}}}/ contains=@NoSpell fold

"   text: ~~strike out~~
syntax region googlecodewikiStrikeoutText  start=/^\~\~/ end=/\(\~\~\|^$\)/
syntax region googlecodewikiStrikeoutText  start=/\W\~\~/ms=s+1 end=/\(\~\~\|^$\)/

"   text: +inserted text+ 
syntax match googlecodewikiInsText        /\(^\|\W\)\zs+\([^ ].\{-}\)+/

"   text: ^superscript^ 
syntax match googlecodewikiSuperScript    /\(^\|\W\)\zs^\([^ ].\{-}\)^/

"   text: ,,subscript,,
syntax region googlecodewikiSubScript  start=/^,,/ end=/\(,,\|^$\)/
syntax region googlecodewikiSubScript  start=/\W,,/ms=s+1 end=/\(,,\|^$\)/

" Aggregate all the regular text highlighting into googlecodewikiText
syntax cluster googlecodewikiText contains=googlecodewikiItalic,googlecodewikiBold,googlecodewikiCode,googlecodewikiCodeRegion,googlecodewikiStrikeoutText,googlecodewikiInsText,googlecodewikiSuperScript,googlecodewikiSubScript,googlecodewikiCitation,googlecodewikiLink,googlecodewikiWord,googlecodewikiEmoticons,googlecodewikiPragma

" Header levels, 1-6
syntax match googlecodewikiH1             /^=.*=$/
syntax match googlecodewikiH2             /^==.*==$/
syntax match googlecodewikiH3             /^===.*===$/
syntax match googlecodewikiH4             /^====.*====$/
syntax match googlecodewikiH5             /^=====.*=====$/
syntax match googlecodewikiH6             /^======.*======$/

" Define a region that represents text between title, that is used for folding.
" TODO:  should I make it hierarchically?
syntax region googlecodewikiTextBetweenTitle transparent 
            \ start=/^=.*=\s*$/ end=/^=.*=\s*$/me=s-1 fold

" <hr>, horizontal rule
syntax match googlecodewikiHR             /^----*$/

" Tables. Each line starts and ends with '||'; each cell is separated by '||'
syntax match googlecodewikiTable          /||/

" Bulleted list items start with space or tabs, then '*' or '#'
syntax match googlecodewikiList           /^\s*\(\*\|#\).*$/   contains=@googlecodewikiText


" Link GoogleWiki syntax items to colors
hi def link googlecodewikiH1                    Title
hi def link googlecodewikiH2                    googlecodewikiH1
hi def link googlecodewikiH3                    googlecodewikiH2
hi def link googlecodewikiH4                    googlecodewikiH3
hi def link googlecodewikiH5                    googlecodewikiH4
hi def link googlecodewikiH6                    googlecodewikiH5
hi def link googlecodewikiHR                    googlecodewikiH6

hi def googlecodewikiBold                       term=bold cterm=bold gui=bold
hi def googlecodewikiItalic                     term=italic cterm=italic gui=italic

hi def link googlecodewikiCode                  String
hi def link googlecodewikiCodeRegion            String
hi def link googlecodewikiWord                  Underlined

hi def link googlecodewikiEscape                Todo
hi def link googlecodewikiPragma                PreProc
hi def link googlecodewikiLink                  Underlined
hi def link googlecodewikiLinkRegion            Identifier
hi def link googlecodewikiCommentRegion         Comment
hi def link pragma                              Identifier
hi def link googlecodewikiList                  Type
hi def link googlecodewikiTable                 Type
hi def link googlecodewikiEmoticons             Constant
hi def link googlecodewikiStrikeoutText         Special
hi def link googlecodewikiInsText               Constant
hi def link googlecodewikiSuperScript           Special
hi def link googlecodewikiSubScript             Special
hi def link googlecodewikiCitation              Constant
hi def link googlecodewikiTodo                  Todo

hi def link googlecodewikiSingleLineProperty    Identifier

let b:current_syntax="GoogleCodeWiki"
