" ~/.dotfiles/home/vim/sessions/mgd.vim:
" Vim session script.
" Created by session.vim 2.4.9 on 05 December 2013 at 16:25:39.
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
cd /usr/local/src/mp4/node-scripts-for-tvshows
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +7 package.json
badd +15 node_modules/node.io/package.json
badd +517 macgourmet.js
badd +3 .gitignore
badd +1 node_modules/request/tests/googledoodle.jpg
badd +7 node_modules/request/tests/run.js
badd +1 node_modules/request/tests/server.js
badd +37 node_modules/request/tests/squid.conf
badd +18 node_modules/request/tests/test-agentOptions.js
badd +163 node_modules/request/tests/test-basic-auth.js
badd +122 node_modules/request/tests/test-body.js
badd +32 node_modules/request/request.js
badd +3 node_modules/request/lib/debug.js
badd +1 node_modules/request/lib/copy.js
badd +1 node_modules/request/lib/getSafe.js
badd +48 node_modules/node.io/lib/node.io/utils.js
badd +67 node_modules/node.io/lib/node.io/spawn.js
badd +119 node_modules/node.io/lib/node.io/request.js
badd +72 node_modules/node.io/lib/node.io/dom.js
badd +431 node_modules/node.io/lib/node.io/processor.js
badd +126 node_modules/node.io/lib/node.io/process_worker.js
badd +1 node_modules/node.io/lib/node.io/process_slave.js
badd +0 node_modules/async/lib/async.js
" args .
edit macgourmet.js
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
27
silent! normal! zo
33
silent! normal! zo
42
silent! normal! zo
44
silent! normal! zo
48
silent! normal! zo
50
silent! normal! zo
56
silent! normal! zo
69
silent! normal! zo
92
silent! normal! zo
94
silent! normal! zo
102
silent! normal! zo
104
silent! normal! zo
116
silent! normal! zo
133
silent! normal! zo
150
silent! normal! zo
154
silent! normal! zo
158
silent! normal! zo
159
silent! normal! zo
165
silent! normal! zo
185
silent! normal! zo
198
silent! normal! zo
201
silent! normal! zo
205
silent! normal! zo
218
silent! normal! zo
234
silent! normal! zo
239
silent! normal! zo
240
silent! normal! zo
255
silent! normal! zo
271
silent! normal! zo
281
silent! normal! zo
292
silent! normal! zo
304
silent! normal! zo
320
silent! normal! zo
322
silent! normal! zo
349
silent! normal! zo
351
silent! normal! zo
352
silent! normal! zo
358
silent! normal! zo
374
silent! normal! zo
102
silent! normal! zo
104
silent! normal! zo
116
silent! normal! zo
133
silent! normal! zo
146
silent! normal! zo
150
silent! normal! zo
154
silent! normal! zo
158
silent! normal! zo
159
silent! normal! zo
165
silent! normal! zo
185
silent! normal! zo
198
silent! normal! zo
201
silent! normal! zo
205
silent! normal! zo
218
silent! normal! zo
234
silent! normal! zo
239
silent! normal! zo
240
silent! normal! zo
255
silent! normal! zo
271
silent! normal! zo
281
silent! normal! zo
292
silent! normal! zo
304
silent! normal! zo
320
silent! normal! zo
322
silent! normal! zo
349
silent! normal! zo
351
silent! normal! zo
352
silent! normal! zo
358
silent! normal! zo
374
silent! normal! zo
421
silent! normal! zo
465
silent! normal! zo
505
silent! normal! zo
506
silent! normal! zo
519
silent! normal! zo
521
silent! normal! zo
523
silent! normal! zo
524
silent! normal! zo
530
silent! normal! zo
532
silent! normal! zo
533
silent! normal! zo
535
silent! normal! zo
548
silent! normal! zo
529
silent! normal! zo
531
silent! normal! zo
532
silent! normal! zo
534
silent! normal! zo
542
silent! normal! zo
553
silent! normal! zo
549
silent! normal! zo
581
silent! normal! zo
585
silent! normal! zo
590
silent! normal! zo
581
silent! normal! zo
585
silent! normal! zo
589
silent! normal! zo
581
silent! normal! zo
585
silent! normal! zo
589
silent! normal! zo
let s:l = 518 - ((22 * winheight(0) + 39) / 78)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
518
normal! 05|
tabedit node_modules/async/lib/async.js
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
2
silent! normal! zo
39
silent! normal! zo
50
silent! normal! zo
52
silent! normal! zo
62
silent! normal! zo
72
silent! normal! zo
77
silent! normal! zo
88
silent! normal! zo
89
silent! normal! zo
96
silent! normal! zo
103
silent! normal! zo
113
silent! normal! zo
119
silent! normal! zo
120
silent! normal! zo
121
silent! normal! zo
125
silent! normal! zo
136
silent! normal! zo
142
silent! normal! zo
143
silent! normal! zo
144
silent! normal! zo
148
silent! normal! zo
169
silent! normal! zo
171
silent! normal! zo
180
silent! normal! zo
185
silent! normal! zo
188
silent! normal! zo
189
silent! normal! zo
193
silent! normal! zo
210
silent! normal! zo
216
silent! normal! zo
222
silent! normal! zo
230
silent! normal! zo
232
silent! normal! zo
235
silent! normal! zo
256
silent! normal! zo
257
silent! normal! zo
271
silent! normal! zo
280
silent! normal! zo
282
silent! normal! zo
285
silent! normal! zo
286
silent! normal! zo
293
silent! normal! zo
306
silent! normal! zo
308
silent! normal! zo
311
silent! normal! zo
312
silent! normal! zo
319
silent! normal! zo
329
silent! normal! zo
330
silent! normal! zo
331
silent! normal! zo
332
silent! normal! zo
347
silent! normal! zo
348
silent! normal! zo
349
silent! normal! zo
350
silent! normal! zo
363
silent! normal! zo
364
silent! normal! zo
365
silent! normal! zo
366
silent! normal! zo
379
silent! normal! zo
380
silent! normal! zo
381
silent! normal! zo
385
silent! normal! zo
393
silent! normal! zo
405
silent! normal! zo
418
silent! normal! zo
419
silent! normal! zo
426
silent! normal! zo
432
silent! normal! zo
433
silent! normal! zo
439
silent! normal! zo
441
silent! normal! zo
446
silent! normal! zo
462
silent! normal! zo
470
silent! normal! zo
471
silent! normal! zo
482
silent! normal! zo
491
silent! normal! zo
492
silent! normal! zo
493
silent! normal! zo
497
silent! normal! zo
515
silent! normal! zo
517
silent! normal! zo
518
silent! normal! zo
519
silent! normal! zo
520
silent! normal! zo
530
silent! normal! zo
532
silent! normal! zo
533
silent! normal! zo
547
silent! normal! zo
551
silent! normal! zo
555
silent! normal! zo
557
silent! normal! zo
558
silent! normal! zo
559
silent! normal! zo
560
silent! normal! zo
570
silent! normal! zo
572
silent! normal! zo
573
silent! normal! zo
587
silent! normal! zo
588
silent! normal! zo
589
silent! normal! zo
603
silent! normal! zo
612
silent! normal! zo
614
silent! normal! zo
626
silent! normal! zo
627
silent! normal! zo
628
silent! normal! zo
640
silent! normal! zo
641
silent! normal! zo
654
silent! normal! zo
655
silent! normal! zo
656
silent! normal! zo
668
silent! normal! zo
669
silent! normal! zo
682
silent! normal! zo
686
silent! normal! zo
690
silent! normal! zo
696
silent! normal! zo
710
silent! normal! zo
722
silent! normal! zo
723
silent! normal! zo
729
silent! normal! zo
753
silent! normal! zo
757
silent! normal! zo
763
silent! normal! zo
767
silent! normal! zo
778
silent! normal! zo
795
silent! normal! zo
799
silent! normal! zo
818
silent! normal! zo
819
silent! normal! zo
821
silent! normal! zo
823
silent! normal! zo
824
silent! normal! zo
829
silent! normal! zo
844
silent! normal! zo
850
silent! normal! zo
860
silent! normal! zo
862
silent! normal! zo
877
silent! normal! zo
883
silent! normal! zo
891
silent! normal! zo
899
silent! normal! zo
901
silent! normal! zo
905
silent! normal! zo
918
silent! normal! zo
919
silent! normal! zo
939
silent! normal! zo
940
silent! normal! zo
941
silent! normal! zo
953
silent! normal! zo
62
silent! normal! zo
72
silent! normal! zo
77
silent! normal! zo
88
silent! normal! zo
89
silent! normal! zo
96
silent! normal! zo
103
silent! normal! zo
113
silent! normal! zo
119
silent! normal! zo
120
silent! normal! zo
121
silent! normal! zo
125
silent! normal! zo
136
silent! normal! zo
142
silent! normal! zo
143
silent! normal! zo
144
silent! normal! zo
148
silent! normal! zo
169
silent! normal! zo
171
silent! normal! zo
180
silent! normal! zo
185
silent! normal! zo
188
silent! normal! zo
189
silent! normal! zo
193
silent! normal! zo
210
silent! normal! zo
216
silent! normal! zo
222
silent! normal! zo
230
silent! normal! zo
232
silent! normal! zo
235
silent! normal! zo
256
silent! normal! zo
257
silent! normal! zo
271
silent! normal! zo
280
silent! normal! zo
282
silent! normal! zo
285
silent! normal! zo
286
silent! normal! zo
293
silent! normal! zo
306
silent! normal! zo
308
silent! normal! zo
311
silent! normal! zo
312
silent! normal! zo
319
silent! normal! zo
329
silent! normal! zo
330
silent! normal! zo
331
silent! normal! zo
332
silent! normal! zo
347
silent! normal! zo
348
silent! normal! zo
349
silent! normal! zo
350
silent! normal! zo
363
silent! normal! zo
364
silent! normal! zo
365
silent! normal! zo
366
silent! normal! zo
379
silent! normal! zo
380
silent! normal! zo
381
silent! normal! zo
385
silent! normal! zo
393
silent! normal! zo
405
silent! normal! zo
418
silent! normal! zo
419
silent! normal! zo
426
silent! normal! zo
432
silent! normal! zo
433
silent! normal! zo
439
silent! normal! zo
441
silent! normal! zo
446
silent! normal! zo
462
silent! normal! zo
470
silent! normal! zo
471
silent! normal! zo
482
silent! normal! zo
491
silent! normal! zo
492
silent! normal! zo
493
silent! normal! zo
497
silent! normal! zo
515
silent! normal! zo
517
silent! normal! zo
518
silent! normal! zo
519
silent! normal! zo
520
silent! normal! zo
530
silent! normal! zo
532
silent! normal! zo
533
silent! normal! zo
547
silent! normal! zo
551
silent! normal! zo
555
silent! normal! zo
557
silent! normal! zo
558
silent! normal! zo
559
silent! normal! zo
560
silent! normal! zo
570
silent! normal! zo
572
silent! normal! zo
573
silent! normal! zo
587
silent! normal! zo
588
silent! normal! zo
589
silent! normal! zo
603
silent! normal! zo
612
silent! normal! zo
614
silent! normal! zo
626
silent! normal! zo
627
silent! normal! zo
628
silent! normal! zo
640
silent! normal! zo
641
silent! normal! zo
654
silent! normal! zo
655
silent! normal! zo
656
silent! normal! zo
668
silent! normal! zo
669
silent! normal! zo
682
silent! normal! zo
686
silent! normal! zo
690
silent! normal! zo
696
silent! normal! zo
710
silent! normal! zo
722
silent! normal! zo
723
silent! normal! zo
729
silent! normal! zo
753
silent! normal! zo
757
silent! normal! zo
763
silent! normal! zo
767
silent! normal! zo
778
silent! normal! zo
795
silent! normal! zo
799
silent! normal! zo
818
silent! normal! zo
819
silent! normal! zo
821
silent! normal! zo
823
silent! normal! zo
824
silent! normal! zo
829
silent! normal! zo
844
silent! normal! zo
850
silent! normal! zo
860
silent! normal! zo
862
silent! normal! zo
877
silent! normal! zo
883
silent! normal! zo
891
silent! normal! zo
899
silent! normal! zo
901
silent! normal! zo
905
silent! normal! zo
918
silent! normal! zo
919
silent! normal! zo
939
silent! normal! zo
940
silent! normal! zo
941
silent! normal! zo
953
silent! normal! zo
let s:l = 66 - ((5 * winheight(0) + 39) / 78)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
66
normal! 021|
tabnext 1
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

tabnext 1
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
