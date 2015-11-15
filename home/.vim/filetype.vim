"
" Filetype detection
"
augroup filetypedetect
	" Detect .txt as 'text'
    autocmd! BufNewFile,BufRead *.txt setfiletype text
    " Tmux
	au BufNewFile,BufRead .tmux.conf*,tmux.conf* setf tmux
    " Pascal / Delphi
    if (1==1) "change to "1==0" to use original syntax
    au BufNewFile,BufRead *.pas,*.PAS set ft=delphi
    else
    au BufNewFile,BufRead *.pas,*.PAS set ft=pascal
    endif
    " Delphi project file
    au BufNewFile,BufRead *.dpr,*.DPR set ft=delphi
    " Delphi form file
    au BufNewFile,BufRead *.dfm,*.DFM set ft=delphi
    au BufNewFile,BufRead *.xfm,*.XFM set ft=delphi
    " Delphi package file
    au BufNewFile,BufRead *.dpk,*.DPK set ft=delphi
    " Delphi .DOF file = INI file for MSDOS
    au BufNewFile,BufRead *.dof,*.DOF set ft=dosini
    au BufNewFile,BufRead *.kof,*.KOF set ft=dosini
    au BufNewFile,BufRead *.dsk,*.DSK set ft=dosini
    au BufNewFile,BufRead *.desk,*.DESK set ft=dosini
    au BufNewFile,BufRead *.dti,*.DTI set ft=dosini
    " Delphi .BPG = Makefile
    au BufNewFile,BufRead *.bpg,*.BPG set ft=make|setlocal makeprg=make\ -f\ % 

augroup END
