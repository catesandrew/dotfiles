" ~/.dotfiles/home/vim/sessions/parse-recipes.vim:
" Vim session script.
" Created by session.vim 2.4.9 on 05 February 2014 at 10:28:47.
" Open this file in Vim and run :source % to restore your session.

set guioptions=egm
silent! set guifont=Anonymous\ Pro:h14
if exists('g:syntax_on') != 1 | syntax on | endif
if exists('g:did_load_filetypes') != 1 | filetype on | endif
if exists('g:did_load_ftplugin') != 1 | filetype plugin on | endif
if exists('g:did_indent_on') != 1 | filetype indent on | endif
if &background != 'dark'
	set background=dark
endif
if !exists('g:colors_name') || g:colors_name != 'mustang' | colorscheme mustang | endif
call setqflist([])
let SessionLoad = 1
if &cp | set nocp | endif
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
silent only
cd /usr/local/src/js/serious-eats
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +291 test/test-cooks-illustrated.js
badd +761 test/data/cooks-illustrated.js
badd +1 test/data/serious-eats.js
badd +0 lib/recipe-parser.js
badd +0 lib/serious-eats-parser.js
badd +38 lib/cooks-illustrated-parser.js
badd +26 lib/food-network-parser.js
badd +0 test/test-serious-eats.js
badd +0 scrub-serious-eats.js
badd +283 lib/entities.js
" args lib/entities.js
edit test/test-serious-eats.js
set splitbelow splitright
wincmd t
set winheight=1 winwidth=1
" argglobal
setlocal fdm=marker
setlocal fde=0
setlocal fmr={,}
setlocal fdi=#
setlocal fdl=99
setlocal fml=1
setlocal fdn=20
setlocal fen
118
silent! normal! zo
119
silent! normal! zo
119
normal! zc
143
silent! normal! zo
167
silent! normal! zo
191
silent! normal! zo
220
silent! normal! zo
291
silent! normal! zo
let s:l = 108 - ((9 * winheight(0) + 39) / 78)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
108
normal! 05|
lcd /usr/local/src/js/serious-eats
tabedit /usr/local/src/js/serious-eats/test/data/serious-eats.js
set splitbelow splitright
wincmd t
set winheight=1 winwidth=1
" argglobal
setlocal fdm=marker
setlocal fde=0
setlocal fmr={,}
setlocal fdi=#
setlocal fdl=99
setlocal fml=1
setlocal fdn=20
setlocal fen
1000
silent! normal! zo
1016
silent! normal! zo
1023
silent! normal! zo
1024
silent! normal! zo
1041
silent! normal! zo
1049
silent! normal! zo
1057
silent! normal! zo
1065
silent! normal! zo
1023
silent! normal! zo
1030
silent! normal! zo
1038
silent! normal! zo
1039
silent! normal! zo
1054
silent! normal! zo
1062
silent! normal! zo
1053
silent! normal! zo
1054
silent! normal! zo
1067
silent! normal! zo
1068
silent! normal! zo
1075
silent! normal! zo
1080
silent! normal! zo
1087
silent! normal! zo
1095
silent! normal! zo
1102
silent! normal! zo
1109
silent! normal! zo
1115
silent! normal! zo
1122
silent! normal! zo
1130
silent! normal! zo
1138
silent! normal! zo
1144
silent! normal! zo
1150
silent! normal! zo
1157
silent! normal! zo
1164
silent! normal! zo
1171
silent! normal! zo
1180
silent! normal! zo
1187
silent! normal! zo
1194
silent! normal! zo
1080
silent! normal! zo
1080
normal! zc
1088
silent! normal! zo
1088
normal! zc
1095
silent! normal! zo
1102
silent! normal! zo
1109
silent! normal! zo
1117
silent! normal! zo
1125
silent! normal! zo
1131
silent! normal! zo
1137
silent! normal! zo
1144
silent! normal! zo
1151
silent! normal! zo
1158
silent! normal! zo
1167
silent! normal! zo
1174
silent! normal! zo
1181
silent! normal! zo
1102
silent! normal! zo
1102
normal! zc
1110
silent! normal! zo
1110
normal! zc
1118
silent! normal! zo
1118
normal! zc
1124
silent! normal! zo
1124
normal! zc
1130
silent! normal! zo
1130
normal! zc
1137
silent! normal! zo
1137
normal! zc
1144
silent! normal! zo
1144
normal! zc
1151
silent! normal! zo
1151
normal! zc
1160
silent! normal! zo
1160
normal! zc
1167
silent! normal! zo
1167
normal! zc
1174
silent! normal! zo
1174
normal! zc
let s:l = 1073 - ((22 * winheight(0) + 39) / 78)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
1073
normal! 058|
lcd /usr/local/src/js/serious-eats
tabedit /usr/local/src/js/serious-eats/lib/recipe-parser.js
set splitbelow splitright
wincmd t
set winheight=1 winwidth=1
" argglobal
setlocal fdm=marker
setlocal fde=0
setlocal fmr={,}
setlocal fdi=#
setlocal fdl=99
setlocal fml=1
setlocal fdn=20
setlocal fen
138
silent! normal! zo
150
silent! normal! zo
158
silent! normal! zo
177
silent! normal! zo
184
silent! normal! zo
197
silent! normal! zo
231
silent! normal! zo
244
silent! normal! zo
250
silent! normal! zo
251
silent! normal! zo
252
silent! normal! zo
291
silent! normal! zo
306
silent! normal! zo
309
silent! normal! zo
424
silent! normal! zo
478
silent! normal! zo
497
silent! normal! zo
let s:l = 232 - ((59 * winheight(0) + 39) / 78)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
232
normal! 05|
lcd /usr/local/src/js/serious-eats
tabedit /usr/local/src/js/serious-eats/lib/serious-eats-parser.js
set splitbelow splitright
wincmd t
set winheight=1 winwidth=1
" argglobal
setlocal fdm=marker
setlocal fde=0
setlocal fmr={,}
setlocal fdi=#
setlocal fdl=99
setlocal fml=1
setlocal fdn=20
setlocal fen
18
silent! normal! zo
47
silent! normal! zo
51
silent! normal! zo
51
normal! zc
77
silent! normal! zo
88
silent! normal! zo
105
silent! normal! zo
108
silent! normal! zo
120
silent! normal! zo
121
silent! normal! zo
128
silent! normal! zo
145
silent! normal! zo
164
silent! normal! zo
169
silent! normal! zo
175
silent! normal! zo
182
silent! normal! zo
190
silent! normal! zo
191
silent! normal! zo
197
silent! normal! zo
213
silent! normal! zo
214
silent! normal! zo
223
silent! normal! zo
224
silent! normal! zo
243
silent! normal! zo
244
silent! normal! zo
245
silent! normal! zo
105
silent! normal! zo
108
silent! normal! zo
120
silent! normal! zo
121
silent! normal! zo
128
silent! normal! zo
145
silent! normal! zo
164
silent! normal! zo
169
silent! normal! zo
175
silent! normal! zo
182
silent! normal! zo
190
silent! normal! zo
191
silent! normal! zo
197
silent! normal! zo
213
silent! normal! zo
214
silent! normal! zo
223
silent! normal! zo
224
silent! normal! zo
243
silent! normal! zo
244
silent! normal! zo
245
silent! normal! zo
let s:l = 97 - ((62 * winheight(0) + 39) / 78)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
97
normal! 09|
lcd /usr/local/src/js/serious-eats
tabedit /usr/local/src/js/serious-eats/scrub-serious-eats.js
set splitbelow splitright
wincmd t
set winheight=1 winwidth=1
" argglobal
setlocal fdm=marker
setlocal fde=0
setlocal fmr={,}
setlocal fdi=#
setlocal fdl=99
setlocal fml=1
setlocal fdn=20
setlocal fen
133
silent! normal! zo
146
silent! normal! zo
147
silent! normal! zo
let s:l = 149 - ((54 * winheight(0) + 39) / 78)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
149
normal! 0
lcd /usr/local/src/js/serious-eats
tabnext 2
if exists('s:wipebuf')
"   silent exe 'bwipe ' . s:wipebuf
endif
" unlet! s:wipebuf
set winheight=1 winwidth=20 shortmess=filnxtToO
let s:sx = expand("<sfile>:p:r")."x.vim"
if file_readable(s:sx)
  exe "source " . fnameescape(s:sx)
endif
let &so = s:so_save | let &siso = s:siso_save

" Support for special windows like quick-fix and plug-in windows.
" Everything down here is generated by vim-session (not supported
" by :mksession out of the box).

tabnext 2
1wincmd w
if exists('s:wipebuf')
  if empty(bufname(s:wipebuf))
if !getbufvar(s:wipebuf, '&modified')
  let s:wipebuflines = getbufline(s:wipebuf, 1, '$')
  if len(s:wipebuflines) <= 1 && empty(get(s:wipebuflines, 0, ''))
    silent execute 'bwipeout' s:wipebuf
  endif
endif
  endif
endif
doautoall SessionLoadPost
unlet SessionLoad
" vim: ft=vim ro nowrap smc=128
