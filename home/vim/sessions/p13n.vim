" ~/.dotfiles/home/vim/sessions/p13n.vim:
" Vim session script.
" Created by session.vim 2.4.9 on 11 November 2013 at 14:08:34.
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
call setqflist([{'lnum': 26, 'col': 29, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'components/phoenix/js/init.js', 'text': '    return exports.trigger(''init-complete'');'}, {'lnum': 64, 'col': 22, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'components/phoenix/js/init.js', 'text': '    exports.trigger(''init-complete'');'}, {'lnum': 52, 'col': 15, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'components/phoenix/js/themes.js', 'text': 'exports.bind(''init-complete'', function() {'}, {'lnum': 180, 'col': 15, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'components/us-mweb-core/js/bridge.js', 'text': 'exports.once(''init-complete'', function() {'}, {'lnum': 40, 'col': 15, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'components/us-mweb-core/js/init.js', 'text': 'Phoenix.once(''init-complete'', function() {'}, {'lnum': 26, 'col': 4, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'docs/events.md', 'text': '- `init-complete`'}, {'lnum': 1461, 'col': 17, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'js/analytics.js', 'text': '  Phoenix.once(''init-complete'', function() {'}, {'lnum': 56, 'col': 17, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'js/util/tracking.js', 'text': '  Phoenix.once(''init-complete'', initMerchandising);'}])
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
badd +64 components/phoenix/js/init.js
badd +62 js/views/shelf/shelf-carousel.js
badd +54 components/phoenix/js/analytics/anivia.js
badd +22 components/phoenix/js/analytics/base.js
badd +2 js/session-tracking.js
badd +1 js/collections/rich-relevance.js
badd +105 components/phoenix/js/util/util.js
badd +143 js/views/bundle/bundle.js
badd +32 js/views/category-page.js
badd +58 js/views/home.js
badd +1 js/views/item/item.js
badd +41 js/views/rich-relevance.js
badd +39 lumbar.json
badd +1343 js/analytics.js
badd +1 components/phoenix/test/js/analytics/events.js
badd +206 test/js/analytics.js
badd +1 components/phoenix/test/js/analytics/omniture-test-setup.js
badd +1 components/phoenix/test/js/analytics/omniture.js
badd +1 test/js/collections/manual-shelf.js
badd +1 test/js/collections/manual-shelves.js
badd +1 test/js/collections/more-images.js
badd +1 test/js/collections/refinement-categories.js
badd +200 test/js/collections/shelf.js
badd +3 test/js/controllers/browse-search.js
badd +45 test/js/controllers/checkout.js
badd +22 test/js/controllers/home.js
badd +2 test/js/controllers/items.js
badd +1 test/js/collections/checkout/checkout-cart.js
badd +1 test/js/collections/checkout/gift-cards.js
badd +37 test/js/views/account/create.js
badd +20 test/js/views/checkout/payment/choose.js
badd +1 test/js/views/checkout/payment/cin-error.js
badd +5 test/js/views/checkout/payment/gift-cards.js
badd +1 test/js/collections/checkout/order-history.js
badd +123 test/js/models/checkout/complete-checkout.js
badd +127 test/js/models/checkout/order.js
badd +1 test/js/models/checkout/ship-to-home-address.js
badd +1 test/js/models/address.js
badd +1 test/js/models/item-helpers.js
badd +130 test/js/models/proto-order.js
badd +1 test/js/models/store.js
badd +1 test/js/models/user.js
badd +1 test/js/connection/error-handler.js
badd +1 test/js/connection/headers.js
badd +1 test/js/connection/translate-attributes.js
badd +1 test/js/collections/local-ad/promotions.js
badd +1 test/js/collections/photo/albums.js
badd +1 test/js/collections/photo/cart.js
badd +1 test/js/util/store.js
badd +1 test/js/util/tracking.js
badd +1 test/js/view-helpers.js
badd +1 test/js/validate.js
badd +72 test/js/views/cart/cart-section.js
badd +2 test/js/views/cart/threshold-shipping.js
badd +317 test/js/views/checkout/ship-to-home/exception.js
badd +7 test/js/views/checkout/address-form.js
badd +52 components/phoenix/js/themes.js
badd +197 components/us-mweb-core/js/bridge.js
badd +46 components/us-mweb-core/js/init.js
badd +16 test/js/platform/native-anivia.js
" args test/js/platform/native-anivia.js
edit components/phoenix/js/analytics/anivia.js
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
10
silent! normal! zo
26
silent! normal! zo
71
silent! normal! zo
73
silent! normal! zo
79
silent! normal! zo
94
silent! normal! zo
98
silent! normal! zo
132
silent! normal! zo
let s:l = 132 - ((56 * winheight(0) + 39) / 78)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
132
normal! 0
tabedit js/views/bundle/bundle.js
set splitbelow splitright
wincmd _ | wincmd |
vsplit
wincmd _ | wincmd |
vsplit
wincmd _ | wincmd |
vsplit
3wincmd h
wincmd w
wincmd w
wincmd w
wincmd t
set winheight=1 winwidth=1
exe 'vert 1resize ' . ((&columns * 25 + 98) / 196)
exe 'vert 2resize ' . ((&columns * 54 + 98) / 196)
exe 'vert 3resize ' . ((&columns * 20 + 98) / 196)
exe 'vert 4resize ' . ((&columns * 94 + 98) / 196)
" argglobal
setlocal fdm=marker
setlocal fde=0
setlocal fmr={,}
setlocal fdi=#
setlocal fdl=99
setlocal fml=1
setlocal fdn=20
setlocal fen
7
silent! normal! zo
24
silent! normal! zo
36
silent! normal! zo
38
silent! normal! zo
51
silent! normal! zo
let s:l = 38 - ((36 * winheight(0) + 39) / 78)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
38
let s:c = 43 - ((14 * winwidth(0) + 12) / 25)
if s:c > 0
  exe 'normal! ' . s:c . '|zs' . 43 . '|'
else
  normal! 043|
endif
wincmd w
" argglobal
edit js/views/category-page.js
setlocal fdm=marker
setlocal fde=0
setlocal fmr={,}
setlocal fdi=#
setlocal fdl=99
setlocal fml=1
setlocal fdn=20
setlocal fen
1
silent! normal! zo
13
silent! normal! zo
27
silent! normal! zo
50
silent! normal! zo
let s:l = 41 - ((33 * winheight(0) + 39) / 78)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
41
normal! 0
wincmd w
" argglobal
edit js/views/home.js
setlocal fdm=marker
setlocal fde=0
setlocal fmr={,}
setlocal fdi=#
setlocal fdl=99
setlocal fml=1
setlocal fdn=20
setlocal fen
2
silent! normal! zo
33
silent! normal! zo
42
silent! normal! zo
56
silent! normal! zo
113
silent! normal! zo
115
silent! normal! zo
124
silent! normal! zo
125
silent! normal! zo
126
silent! normal! zo
let s:l = 58 - ((39 * winheight(0) + 39) / 78)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
58
normal! 0
wincmd w
" argglobal
edit js/views/item/item.js
setlocal fdm=marker
setlocal fde=0
setlocal fmr={,}
setlocal fdi=#
setlocal fdl=99
setlocal fml=1
setlocal fdn=20
setlocal fen
7
silent! normal! zo
19
silent! normal! zo
65
silent! normal! zo
let s:l = 26 - ((8 * winheight(0) + 39) / 78)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
26
normal! 010|
wincmd w
exe 'vert 1resize ' . ((&columns * 25 + 98) / 196)
exe 'vert 2resize ' . ((&columns * 54 + 98) / 196)
exe 'vert 3resize ' . ((&columns * 20 + 98) / 196)
exe 'vert 4resize ' . ((&columns * 94 + 98) / 196)
tabedit js/collections/rich-relevance.js
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
16
silent! normal! zo
30
silent! normal! zo
31
silent! normal! zo
37
silent! normal! zo
53
silent! normal! zo
let s:l = 14 - ((13 * winheight(0) + 39) / 78)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
14
normal! 07|
tabedit js/views/rich-relevance.js
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
25
silent! normal! zo
38
silent! normal! zo
let s:l = 15 - ((14 * winheight(0) + 39) / 78)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
15
normal! 057|
tabedit js/views/shelf/shelf-carousel.js
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
12
silent! normal! zo
13
silent! normal! zo
18
silent! normal! zo
39
silent! normal! zo
45
silent! normal! zo
let s:l = 64 - ((47 * winheight(0) + 39) / 78)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
64
normal! 0110|
tabedit js/analytics.js
set splitbelow splitright
wincmd _ | wincmd |
vsplit
1wincmd h
wincmd w
wincmd t
set winheight=1 winwidth=1
exe 'vert 1resize ' . ((&columns * 100 + 98) / 196)
exe 'vert 2resize ' . ((&columns * 95 + 98) / 196)
" argglobal
setlocal fdm=marker
setlocal fde=0
setlocal fmr={,}
setlocal fdi=#
setlocal fdl=99
setlocal fml=1
setlocal fdn=20
setlocal fen
2
silent! normal! zo
293
silent! normal! zo
970
silent! normal! zo
972
silent! normal! zo
974
silent! normal! zo
975
silent! normal! zo
1101
silent! normal! zo
1233
silent! normal! zo
1246
silent! normal! zo
1254
silent! normal! zo
1253
silent! normal! zo
1259
silent! normal! zo
1270
silent! normal! zo
1283
silent! normal! zo
1296
silent! normal! zo
1298
silent! normal! zo
1301
silent! normal! zo
1308
silent! normal! zo
1312
silent! normal! zo
1270
silent! normal! zo
1283
silent! normal! zo
1296
silent! normal! zo
1298
silent! normal! zo
1301
silent! normal! zo
1308
silent! normal! zo
1312
silent! normal! zo
1325
silent! normal! zo
1341
silent! normal! zo
1368
silent! normal! zo
1374
silent! normal! zo
1375
silent! normal! zo
1392
silent! normal! zo
1408
silent! normal! zo
1425
silent! normal! zo
1440
silent! normal! zo
1451
silent! normal! zo
1484
silent! normal! zo
1801
silent! normal! zo
1802
silent! normal! zo
1804
silent! normal! zo
1816
silent! normal! zo
1818
silent! normal! zo
1818
silent! normal! zo
1819
silent! normal! zo
1821
silent! normal! zo
1827
silent! normal! zo
1828
silent! normal! zo
1325
silent! normal! zo
1341
silent! normal! zo
1378
silent! normal! zo
1695
silent! normal! zo
1696
silent! normal! zo
1698
silent! normal! zo
1710
silent! normal! zo
1712
silent! normal! zo
1712
silent! normal! zo
1713
silent! normal! zo
1715
silent! normal! zo
1721
silent! normal! zo
1722
silent! normal! zo
1745
silent! normal! zo
1745
silent! normal! zo
1745
silent! normal! zo
1745
silent! normal! zo
1745
silent! normal! zo
1745
silent! normal! zo
1745
silent! normal! zo
1745
silent! normal! zo
1745
silent! normal! zo
1745
silent! normal! zo
1745
silent! normal! zo
1745
silent! normal! zo
1745
silent! normal! zo
1745
silent! normal! zo
1745
silent! normal! zo
1745
silent! normal! zo
1745
silent! normal! zo
1745
silent! normal! zo
1745
silent! normal! zo
let s:l = 1363 - ((35 * winheight(0) + 39) / 78)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
1363
normal! 0
wincmd w
" argglobal
edit test/js/analytics.js
setlocal fdm=marker
setlocal fde=0
setlocal fmr={{{,}}}
setlocal fdi=#
setlocal fdl=99
setlocal fml=1
setlocal fdn=20
setlocal fen
let s:l = 3 - ((2 * winheight(0) + 39) / 78)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
3
normal! 0
wincmd w
exe 'vert 1resize ' . ((&columns * 100 + 98) / 196)
exe 'vert 2resize ' . ((&columns * 95 + 98) / 196)
tabedit components/phoenix/test/js/analytics/events.js
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
1
silent! normal! zo
25
silent! normal! zo
36
silent! normal! zo
let s:l = 37 - ((36 * winheight(0) + 39) / 78)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
37
normal! 024|
tabnext 6
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

tabnext 6
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
