" ~/.dotfiles/home/vim/sessions/demindo.vim:
" Vim session script.
" Created by session.vim 2.4.9 on 05 February 2014 at 11:05:32.
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
call setqflist([{'lnum': 19, 'col': 121, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'public/codebase/ext/dhtmlxgrid_pgn.js', 'text': ' if (!this._pgn_templateA){this._pgn_templateA=this._pgn_template_compile("[prevpages:&lt:&nbsp] [currentpages:,&nbsp] [nextpages:&gt:&nbsp]");this._pgn_templateB=this._pgn_template_compile("Results <b>[from]-[to]</b> of <b>[total]</b>")}};var details=this.getStateOfView();this.pagingBlock.innerHTML = this._pgn_templateA.apply(this,details);this.recordInfoBlock.innerHTML = this._pgn_templateB.apply(this,details);this._pgn_template_active(this.pagingBlock);this._pgn_template_active(this.recordInfoBlock);this.callEvent("onPaging",[])};dhtmlXGridObject.prototype._pgn_block=function(sep){var start=Math.floor((this.currentPage-1)/this.pagesInGroup)*this.pagesInGroup;var max=Math.min(Math.ceil(this.rowsBuffer.length/this.rowsBufferOutSize),start+this.pagesInGroup);var str=[];for (var i=start+1;i<=max;i++)if (i==this.currentPage)str.push("<a class=''dhx_not_active''><b>"+i+"</b></a>");else'}, {'lnum': 21, 'col': 15, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'public/codebase/ext/dhtmlxgrid_pgn.js', 'text': ' };if (mode=="nextpages" || mode=="next"){if (this.rowsBuffer.length/this.rowsBufferOutSize <= this.currentPage )return ds;if (this.rowsBuffer.length/(this.rowsBufferOutSize*(mode=="next"?''1'':this.pagesInGroup)) <= 1 ) return ds;return ''<a onclick=\''this.grid.changePageRelative(''+(mode=="next"?''1'':''this.grid.pagesInGroup'')+'');return false;\''>''+ac+''</a>'''}, {'lnum': 30, 'col': 8, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'public/codebase/ext/dhtmlxgrid_pgn.js', 'text': ' case "nextpages":'}, {'lnum': 147, 'col': 53, 'valid': 1, 'vcol': 0, 'nr': -1, 'type': '', 'pattern': '', 'filename': 'source/class/swep/sfNavDocArrowBar.js', 'text': '					control = new qx.ui.form.Button(null, "buttons/nextpage.png");'}])
let SessionLoad = 1
if &cp | set nocp | endif
let s:so_save = &so | let s:siso_save = &siso | set so=0 siso=0
let v:this_session=expand("<sfile>:p")
silent only
cd /usr/local/src/ibt/client/demindo
if expand('%') == '' && !&modified && line('$') <= 1 && getline(1) == ''
  let s:wipebuf = bufnr('%')
endif
set shortmess=aoO
badd +30 public/codebase/ext/dhtmlxgrid_pgn.js
badd +147 source/class/swep/sfNavDocArrowBar.js
badd +3653 swep_pre.js
badd +27 source/class/swep/DockLayout.js
badd +1 components/qooxdoo/framework/source/class/q.js
badd +31 components/qooxdoo/framework/source/class/qx/ui/core/LayoutItem.js
badd +161 components/qooxdoo/framework/source/class/qx/ui/layout/Abstract.js
badd +155 components/qooxdoo/framework/source/class/qx/ui/layout/Canvas.js
badd +111 components/qooxdoo/framework/source/class/qx/ui/layout/Dock.js
badd +212 components/qooxdoo/framework/source/class/qx/ui/layout/Flow.js
badd +104 components/qooxdoo/framework/source/class/qx/ui/layout/VBox.js
badd +13 source/class/swep/Application.js
badd +20 source/class/swep/sfDiscrepencyItem.js
badd +105 source/class/swep/sfDispatchTool.js
badd +48 source/class/swep/sfOFDTool.js
badd +93 source/class/swep/sfPopupLookup.js
badd +54 source/class/swep/sfSlideTool.js
badd +5 public/codebase/dhtmlxmenu.js
badd +234 source/class/swep/sfDispatchMenu.js
badd +18 source/class/swep/sfDispatchNav.js
badd +8 mock-server.json
badd +1 mocks/paths.js
badd +7701 mocks/dispatch-refresh.js
badd +55 components/qooxdoo/framework/source/class/qx/ui/core/Widget.js
badd +24 source/class/swep/Debugger.js
badd +415 public/codebase/dhtmlxgrid.js
badd +1 source/class/swep/J/sfImageList.js
badd +80 source/class/swep/sfGrid.js
badd +116 source/class/swep/sfGrid_q.js
badd +33 public/Solumina.hth
badd +80 source/class/swep/gridNav.js
badd +468 source/class/swep/J/sfCheckBoxNew.js
badd +589 source/class/swep/J/sfEditCombo.js
badd +100 source/class/swep/newLabel.js
badd +76 source/class/swep/sfInputMatrix.js
badd +402 source/class/swep/Globals.js
badd +18 components/qooxdoo/framework/source/class/qx/lang/Object.js
badd +120 components/qooxdoo/framework/source/class/qx/core/Object.js
badd +40 source/class/swep/J/newLabel.js
badd +53 source/class/swep/J/newMemo.js
badd +30 source/class/swep/J/sfEdit.js
badd +313 source/class/swep/sfMasterNavBar.js
badd +89 source/class/swep/sfSlideNote.js
badd +74 public/codebase/excells/dhtmlxgrid_excell_swstatus.js
badd +16 source/class/swep/sfNavBarModes.js
badd +34 source/class/swep/sfDocumentBar.js
badd +276 source/class/swep/sfIMGridMenu.js
badd +1 source/script/swep.95b2df0cb7f1.js
badd +1 source/class/swep/methods.js
badd +71 conf/qooxdo-config.json
badd +1 source/class/swep/J/sfFilterButton.js
badd +100 source/class/swep/sfFilterButton.js
badd +22 source/class/swep/sfDispatchViewButtonBar.js
badd +58 source/class/swep/sfListBuilderBody.js
badd +49 source/class/swep/J/sfExtraButtonBar.js
badd +39 source/class/swep/sfExtraButtonBar.js
badd +90 source/class/swep/test/sfExtraButtonBar.js
badd +78 .jshintrc
badd +19 .jscs.json
" args .jscs.json
edit source/class/swep/Application.js
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
141
silent! normal! zo
150
silent! normal! zo
170
silent! normal! zo
252
silent! normal! zo
270
silent! normal! zo
334
silent! normal! zo
363
silent! normal! zo
365
silent! normal! zo
367
silent! normal! zo
376
silent! normal! zo
let s:l = 182 - ((82 * winheight(0) + 44) / 88)
if s:l < 1 | let s:l = 1 | endif
exe s:l
normal! zt
182
normal! 09|
lcd /usr/local/src/ibt/client/demindo
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
