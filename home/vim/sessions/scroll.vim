" ~/.dotfiles/home/vim/sessions/scroll.vim:
" Vim session script.
" Created by session.vim 2.4.9 on 29 October 2013 at 21:12:51.
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
call setqflist([{'lnum': 1, 'col': 101, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'components/thorax/src/collection.js', 'text': '/*global assignView, assignTemplate, createRegistryWrapper, dataObject, getEventCallback, getValue, modelCidAttributeName, viewCidAttributeName */'}, {'lnum': 146, 'col': 26, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'components/thorax/src/collection.js', 'text': '        itemElement.attr(modelCidAttributeName, model.cid);'}, {'lnum': 153, 'col': 39, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'components/thorax/src/collection.js', 'text': '        var last = $el.children(''['' + modelCidAttributeName + ''="'' + previousModel.cid + ''"]'').last();'}, {'lnum': 158, 'col': 25, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'components/thorax/src/collection.js', 'text': '        el.setAttribute(modelCidAttributeName, model.cid);'}, {'lnum': 175, 'col': 33, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'components/thorax/src/collection.js', 'text': '        viewEl = $el.find(''['' + modelCidAttributeName + ''="'' + model.cid + ''"]'');'}, {'lnum': 190, 'col': 31, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'components/thorax/src/collection.js', 'text': '      viewEl = $el.find(''['' + modelCidAttributeName + ''="'' + model.cid + ''"]'');'}, {'lnum': 362, 'col': 37, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'components/thorax/src/collection.js', 'text': '  this.itemFilter && $el.find(''['' + modelCidAttributeName + ''="'' + model.cid + ''"]'')[itemShouldBeVisible.call(this, model) ? ''show'' : ''hide'']();'}, {'lnum': 2, 'col': 5, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'components/thorax/src/model.js', 'text': 'var modelCidAttributeName = ''data-model-cid'';'}, {'lnum': 47, 'col': 16, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'components/thorax/src/model.js', 'text': '  cidAttrName: modelCidAttributeName'}, {'lnum': 79, 'col': 42, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'components/thorax/src/model.js', 'text': '      modelElement = $this.closest(''['' + modelCidAttributeName + '']''),'}, {'lnum': 80, 'col': 52, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'components/thorax/src/model.js', 'text': '      modelCid = modelElement && modelElement.attr(modelCidAttributeName);'}, {'lnum': 369, 'col': 26, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'js/views/shelf/shelf.js', 'text': '        itemElement.attr(modelCidAttributeName, model.cid);'}, {'lnum': 376, 'col': 39, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'js/views/shelf/shelf.js', 'text': '        var last = $el.children(''['' + modelCidAttributeName + ''="'' + previousModel.cid + ''"]'').last();'}, {'lnum': 381, 'col': 25, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'js/views/shelf/shelf.js', 'text': '        el.setAttribute(modelCidAttributeName, model.cid);'}])
let SessionLoad = 1
if &cp | set nocp | endif
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
silent only
cd /usr/local/src/walmart/us-mweb
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +0 components/phoenix/js/paged-collection.js
badd +0 js/views/shelf/shelf.js
badd +146 components/thorax/src/collection.js
badd +0 components/thorax/src/helpers/collection.js
badd +0 components/thorax/src/model.js
" args .
edit components/phoenix/js/paged-collection.js
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
3
silent! normal! zo
13
silent! normal! zo
let s:l = 39 - ((38 * winheight(0) + 39) / 78)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
39
normal! 0
tabedit js/views/shelf/shelf.js
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
8
silent! normal! zo
15
silent! normal! zo
16
silent! normal! zo
16
normal! zc
59
silent! normal! zo
59
normal! zc
65
silent! normal! zo
65
normal! zc
100
silent! normal! zo
100
normal! zc
135
silent! normal! zo
135
normal! zc
147
silent! normal! zo
147
normal! zc
156
silent! normal! zo
156
normal! zc
166
silent! normal! zo
166
normal! zc
192
silent! normal! zo
192
normal! zc
210
silent! normal! zo
210
normal! zc
219
silent! normal! zo
219
normal! zc
229
silent! normal! zo
229
normal! zc
249
silent! normal! zo
250
silent! normal! zo
250
normal! zc
249
normal! zc
262
silent! normal! zo
262
normal! zc
297
silent! normal! zo
297
normal! zc
330
silent! normal! zo
337
silent! normal! zo
343
silent! normal! zo
351
silent! normal! zo
372
silent! normal! zo
387
silent! normal! zo
398
silent! normal! zo
406
silent! normal! zo
422
silent! normal! zo
425
silent! normal! zo
430
silent! normal! zo
443
silent! normal! zo
453
silent! normal! zo
455
silent! normal! zo
470
silent! normal! zo
473
silent! normal! zo
478
silent! normal! zo
498
silent! normal! zo
499
silent! normal! zo
398
silent! normal! zo
405
silent! normal! zo
421
silent! normal! zo
424
silent! normal! zo
429
silent! normal! zo
442
silent! normal! zo
452
silent! normal! zo
454
silent! normal! zo
469
silent! normal! zo
472
silent! normal! zo
477
silent! normal! zo
497
silent! normal! zo
498
silent! normal! zo
508
silent! normal! zo
398
silent! normal! zo
405
silent! normal! zo
421
silent! normal! zo
424
silent! normal! zo
429
silent! normal! zo
442
silent! normal! zo
452
silent! normal! zo
454
silent! normal! zo
469
silent! normal! zo
472
silent! normal! zo
477
silent! normal! zo
497
silent! normal! zo
498
silent! normal! zo
508
silent! normal! zo
let s:l = 356 - ((95 * winheight(0) + 39) / 78)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
356
normal! 0
tabedit components/thorax/src/collection.js
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
77
silent! normal! zo
107
silent! normal! zo
120
silent! normal! zo
128
silent! normal! zo
173
silent! normal! zo
186
silent! normal! zo
244
silent! normal! zo
274
silent! normal! zo
294
silent! normal! zo
295
silent! normal! zo
298
silent! normal! zo
354
silent! normal! zo
let s:l = 355 - ((53 * winheight(0) + 39) / 78)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
355
normal! 012|
tabedit components/thorax/src/model.js
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
let s:l = 4 - ((3 * winheight(0) + 39) / 78)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
4
normal! 05|
tabedit components/thorax/src/helpers/collection.js
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
3
silent! normal! zo
18
silent! normal! zo
44
silent! normal! zo
50
silent! normal! zo
56
silent! normal! zo
74
silent! normal! zo
80
silent! normal! zo
81
silent! normal! zo
74
silent! normal! zo
80
silent! normal! zo
81
silent! normal! zo
86
silent! normal! zo
109
silent! normal! zo
124
silent! normal! zo
126
silent! normal! zo
134
silent! normal! zo
135
silent! normal! zo
142
silent! normal! zo
152
silent! normal! zo
109
silent! normal! zo
124
silent! normal! zo
126
silent! normal! zo
134
silent! normal! zo
135
silent! normal! zo
142
silent! normal! zo
152
silent! normal! zo
let s:l = 80 - ((35 * winheight(0) + 39) / 78)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
80
normal! 013|
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
