"
"  ____                                 _
" |  _ \ ___ _ __ ___  ___  _ __   __ _| |
" | |_) / _ \ '__/ __|/ _ \| '_ \ / _` | |
" |  __/  __/ |  \__ \ (_) | | | | (_| | |
" |_|   \___|_|  |___/\___/|_| |_|\__,_|_|
"
"                  __
"  _ __  _ __ ___ / _| ___ _ __ ___ _ __   ___ ___
" | '_ \| '__/ _ \ |_ / _ \ '__/ _ \ '_ \ / __/ _ \
" | |_) | | |  __/  _|  __/ | |  __/ | | | (_|  __/
" | .__/|_|  \___|_|  \___|_|  \___|_| |_|\___\___|
" |_|
"        _                       __ _ _
" __   _(_)_ __ ___  _ __ ___   / _(_) | ___
" \ \ / / | '_ ` _ \| '__/ __| | |_| | |/ _ \
"  \ V /| | | | | | | | | (__  |  _| | |  __/
" (_)_/ |_|_| |_| |_|_|  \___| |_| |_|_|\___|
"
"
"
" My personally preferred version of vim is the one with the 'HUGE' feature
" set, in addition to the following configure options:
"
" ./configure --with-features=huge --enable-gui=macvim --enable-perlinterp
" --enable-pythoninterp --enable-rubyinterp --enable-cscope --enable-multibyte
"  --enable-luainterp --enable-cscope --with-lua-prefix=/usr/local
"  --with-luajit --with-tlib=ncurses --with-compiledby=Andrew
"
" To start vim without using this .vimrc file, use:
"     vim -u NORC
"
" To start vim without loading any .vimrc or plugins, use:
"     vim -u NONE
"
" Use vim settings, rather then vi settings (much better!)
" This must be first, because it changes other options as a side effect.

" Detect OS --------------------------------------------------------------- {{{

let s:is_windows = has('win32') || has('win64')
let s:is_cygwin = has('win32unix')
let s:is_macvim = has('gui_macvim')
let s:has_lua = has('lua')

" }}}
" Useful Functions -------------------------------------------------------- {{{

function! s:get_cache_dir(suffix) "{{{
  return resolve(expand(s:cache_dir . '/' . a:suffix))
endfunction "}}}

function! Source(begin, end) "{{{
  let lines = getline(a:begin, a:end)
  for line in lines
      execute line
  endfor
endfunction "}}}

function! Preserve(command) "{{{
  " preparation: save last search, and cursor position.
  let _s=@/
  let l = line(".")
  let c = col(".")
  " do the business:
  execute a:command
  " clean up: restore previous search history, and cursor position
  let @/=_s
  call cursor(l, c)
endfunction "}}}

function! StripTrailingWhitespace() "{{{
  call Preserve("%s/\\s\\+$//e")
endfunction "}}}

function! EnsureExists(path) "{{{
  let s:path = expand(a:path)
  if !isdirectory(s:path)
    silent execute '!mkdir ' s:path ' > /dev/null 2>&1'
  endif
endfunction "}}}

function! CloseWindowOrKillBuffer() "{{{
  let number_of_windows_to_this_buffer = len(filter(range(1, winnr('$')), "winbufnr(v:val) == bufnr('%')"))

  " never bdelete a nerd tree
  if matchstr(expand("%"), 'NERD') == 'NERD'
      wincmd c
      return
  endif

  if number_of_windows_to_this_buffer > 1
      wincmd c
  else
      bdelete
  endif
endfunction "}}}

" }}}
" Preamble -------------------------------------------------------------------- {{{
let s:cache_dir='~/.vim/tmp'

set nocompatible
filetype on
filetype off

" Utils {{{
source ~/.vim/functions/util.vim
" }}}

" Load external configuration before anything else {{{
if filereadable(expand("~/.vim/before.vimrc"))
  source ~/.vim/before.vimrc
endif
" }}}

" Local vimrc configuration {{{
let s:localrc = expand($HOME . '/.vim/local.vimrc')
if filereadable(s:localrc)
    exec ':so ' . s:localrc
endif
" }}}

" To disable a plugin, add it's bundle name to the following list
let g:pathogen_disabled = []

" for some reason the csscolor plugin is very slow when run on the terminal
" but not in GVim, so disable it if no GUI is running
if !has('gui_running')
  call add(g:pathogen_disabled, 'csscolor')
endif

if !s:has_lua
  call add(g:pathogen_disabled, 'neocomplete')
endif

" Gundo requires at least vim 7.3
if v:version < '703' || !has('python')
  call add(g:pathogen_disabled, 'gundo')
endif

if v:version < '702'
  call add(g:pathogen_disabled, 'autocomplpop')
  call add(g:pathogen_disabled, 'fuzzyfinder')
  call add(g:pathogen_disabled, 'l9')
endif

" Use pathogen to easily modify the runtime path to include all plugins under
" the ~/.vim/bundle directory
filetype off                                       " force reloading *after* pathogen loaded
silent! call pathogen#helptags()
silent! call pathogen#infect()
filetype plugin indent on                          " enable detection, plugins and indenting in one step

" }}}
" Basic options --------------------------------------------------------------- {{{

" Base configuration {{{
set timeoutlen=300      " mapping timeout
set ttimeoutlen=50      " keycode timeout
set mouse=a             " enable mouse
set mousehide           " hide when characters are typed
set ttyfast             " assume fast terminal connection
set viewoptions=folds,options,cursor,unix,slash " unix/windows compatibility
set encoding=utf-8
if s:is_macvim
    set macmeta             " Use option (alt) as meta key.  When on, option-key presses are not interpreted,
endif
if exists('$TMUX')
  set clipboard=
else
  set clipboard=unnamed " sync with OS clipboard
endif
set hidden              " hide buffers instead of closing them this means that the current buffer can be put to background without being written; and that marks and undo history are preserved
set autoread            " auto re-read when the file is written by other applications
set fileformats+=mac    " add mac to auto-detection of file format line endings
set nrformats-=octal    " always assume decimal numbers
set showcmd             " show (partial) command in the last line of the screen this also shows visual selection info
set tags=tags;/
set showfulltag
set modeline
set modelines=5

if s:is_windows && !s:is_cygwin
  " ensure correct shell in gvim
  set shell=c:\windows\system32\cmd.exe
endif

if $SHELL =~ '/fish$'
  " VIM expects to be run from a POSIX shell.
  set shell=sh
endif

set noshelltemp                                 " use pipes
" }}}
" Misc configuration {{{

if exists('+breakindent')
  set breakindent showbreak=\ +
endif
set modelines=0
set updatecount=50            " write swap file to disk after 50 keystrokes
set copyindent                " copy the previous indentation on autoindenting
set showmode                  " always show what mode we're currently editing in
set whichwrap=b,s,h,l,<,>,[,]
set cpoptions+=J
set wrapscan                  " search wrap around the end of the file
set notitle                   " don't rewrite title string

set gdefault                  " search/replace 'globally' (on a line) by default
set virtualedit=all           " allow the cursor to go in to 'invalid' places
set synmaxcol=512             " long lines slow down vim
set title                     " change the terminal's title
set nomodeline                " disable mode lines (security measure)
set dictionary=/usr/share/dict/words

" Insert only one space when joining lines that contain sentence-terminating
" punctuation like `.`.
set nojoinspaces

if has('cmdline_info')
  set ruler                   " show the ruler
  set rulerformat=%30(%=\:b%n%y%m%r%w\ %l,%c%V\ %P%) " a ruler on steroids
endif

" Remember things between sessions
"
" '20  - remember marks for 20 previous files
" \"100 - save 100 lines for each register
" :20  - remember 20 items in command-line history
" %    - remember the buffer list (if vim started without a file arg)
set viminfo='20,\"100,:20,%

" }}}
" Searching {{{

set hlsearch      " highlight search terms
set incsearch     " show search matches as you type
set ignorecase    " ignore case when searching
set smartcase     " ignore case if search pattern is all lowercase,
                  "    case-sensitive otherwise
nohlsearch        " avoid highlighting when reloading vimrc

if executable('ack')
  set grepprg=ack\ --nogroup\ --column\ --smart-case\ --nocolor\ --follow\ $*
  set grepformat=%f:%l:%c:%m
endif
if executable('ag')
  set grepprg=ag\ --nogroup\ --column\ --smart-case\ --nocolor\ --follow
  set grepformat=%f:%l:%c:%m
endif

" }}}
" Editor layout {{{

set showmatch          " when a bracket is inserted, breifly jump to a matching one
set matchtime=2        " jump to matching bracket for 2/10th of a second
set lazyredraw         " don't update the display while executing macros
set noshowmode
set termencoding=utf-8
set cmdheight=1        " use a status bar that is 1 rows high

" }}}
" Folding {{{

set foldenable             " enable folding
set foldcolumn=0           " add a fold column
set foldmethod=marker      " detect triple-{ style fold markers
set foldlevelstart=99      " start out with nothing folded
set foldopen=block,hor,insert,jump,mark,percent,quickfix,search,tag,undo " which commands trigger auto-unfold
let g:xml_syntax_folding=1 " enable xml folding

" }}}
" Tabs, whitespace, wrapping {{{

set backspace=indent,eol,start   " allow backspacing over everything in insert mode
set autoindent                   " always set autoindenting on
set expandtab                    " expand tabs by default (overloadable per file type later)
set smarttab                     " insert tabs on the start of a line according to
                                 " shiftwidth, not tabstop
set tabstop=4
set softtabstop=4
set shiftwidth=4                 " number of spaces to use for autoindenting
set smartindent                  " use smart indent
set nowrap                       " don't wrap lines
set textwidth=0                  " don't break line automatically
set formatoptions+=1             " When wrapping paragraphs, don't end lines
                                 " with 1-letter words (looks stupid)
set formatoptions+=m             " add multibyte support
                                 " set colorcolumn=+1
set nolinebreak                  " don't break line automatically
set iminsert=0                   " disable input method control in insert mode
set imsearch=0                   " disable input method control in search mode
set nolist                       " don't show invisible characters by default,
                                 " but it is enabled for some file types (see later)
set listchars=tab:▸\ ,eol:¬,extends:❯,precedes:❮,trail:␣
set shiftround                   " use multiple of shiftwidth when indenting with '<' and '>'
let &showbreak='↪ '

set scrolloff=8                  " keep 5 lines off the edges of the screen when scrolling
set scrolljump=8
set display+=lastline
let html_no_rendering=1 " Don't render italic, bold, links in HTML
set wildmenu                     " make tab completion for files/buffers act like bash
set wildmode=list:longest,full   " show a list when pressing tab and complete
                                 " first full match
set wildignore=.svn,CVS,.git,.hg,*.o,*.a,*.class,*.mo,*.la,*.so,*.obj,*.swp,*.jpg,*.png,*.xpm,*.gif,.DS_Store,*.aux,*.out,*.toc,tmp,*.scssc
" http://stackoverflow.com/questions/3686841/vim-case-insensitive-filename-completion/4780444#4780444
if exists("&wildignorecase")
    set wildignorecase
endif

set splitbelow
set splitright

if has('conceal')
  set conceallevel=1
  set listchars+=conceal:Δ
endif

" }}}
" Disable Sounds {{{

set noerrorbells                " don't beep
set novisualbell
set t_vb=

" }}}
" Vim behaviour {{{

set switchbuf=useopen           " reveal already opened files from the
                                " quickfix window instead of opening new buffers
set history=1000                " remember more commands and search history

" persistent undo
if has('persistent_undo')
  set undofile                    " keep a persistent backup file
  set undolevels=3000             " use many muchos levels of undo
  set undoreload=10000            " max number lines to save for undo on a buffer reload
  let &undodir = s:get_cache_dir('undo')
endif

" backups
let &backupdir = s:get_cache_dir('backup')
set backup
set nobackup                    " do not keep backup files, it's 70's style cluttering

" swap files
let &directory = s:get_cache_dir('swap')
set noswapfile                  " do not write annoying intermediate swap files,
                                "    who did ever restore from swap files anyway?
call EnsureExists(s:cache_dir)
call EnsureExists(&undodir)
call EnsureExists(&backupdir)
call EnsureExists(&directory)

" }}}
" Cursorline {{{

set cursorline                  " underline the current line, for quick orientation
" Only show cursorline in the current window and in normal mode.
augroup cline
    au!
    au WinLeave,InsertEnter,BufEnter,BufRead * setlocal nocursorline
    au WinEnter,InsertLeave,BufLeave * setlocal cursorline
augroup END

" }}}
" Trailing Whitespace {{{

" Only shown when not in insert mode so I don't go insane.
augroup trailing
    au!
    au InsertEnter * :set listchars-=trail:␣
    au InsertLeave * :set listchars+=trail:␣

    " With the following, the command will be applied to the first window, and
    " to any subsequent windows. The pattern * applies the highlight to all
    " files. Show leading whitespace that includes spaces, and trailing whitespace.
    au ColorScheme * highlight ExtraWhitespace ctermbg=darkgreen guibg=darkgreen
    au BufWinEnter,InsertLeave * match ExtraWhitespace /\s\+$/
    au BufWinLeave,InsertEnter * call clearmatches()
augroup END

" }}}
" Relative Number {{{

" Only show relative number in the current window
augroup rnumber
    au!
    au WinEnter,BufEnter,BufRead * setlocal relativenumber
    au WinLeave,BufLeave * setlocal norelativenumber
augroup END

" }}}
" Status line {{{

if has('statusline')
  set laststatus=2  " tell VIM to always put a status line in, even if there is only one window
endif

" }}}
" Leader {{{

" Change the mapleader from \ to ,
let mapleader=","
let maplocalleader = "\\"

" map Leader and LocalLeader to 0x00A0 (NO_BREAK_SPACE)
" let mapleader=' '
" let maplocalleader=' '
" inoremap <Leader> <Nop>
" inoremap <LocalLeader> <Nop>

" }}}
" Color scheme {{{

if &t_Co >= 256 || has("gui_running")
  set background=dark
  "colorscheme mustang
  colorscheme jellybeans
endif

if &t_Co > 2 || has("gui_running")
  syntax on " switch syntax highlighting on, when the terminal has colors
  " http://superuser.com/questions/430344/macvim-lags-while-vim-on-terminal-is-buttery-smooth
  " http://stackoverflow.com/questions/4775605/vim-syntax-highlight-improve-performance
  " syntax sync minlines=256
endif

" }}}

" }}}
" Useful functions ----------------------------------------------------------- {{{

command! CleanEmptyBuffers call CleanEmptyBuffers()
autocmd BufReadPost * call SetCursorPosition()

" }}}
" Searching and movement ------------------------------------------------------ {{{

"clearing highlighted search as well as redraw
nnoremap <silent> <leader>/ :nohlsearch<CR><C-L>
inoremap <silent> <leader>/ <C-O>:nohlsearch<CR>
nnoremap <BS> :set hlsearch! hlsearch?<cr>

"nmap <silent> <leader>hh :set invhlsearch<CR>
"nmap <silent> <leader>ll :set invlist<CR>
"nmap <silent> <leader>nn :set invnumber<CR>
"nmap <silent> <leader>pp :set invpaste<CR>
"nmap <silent> <leader>ii :set invrelativenumber<CR>

" Change Working Directory to that of the current file
cmap cwd lcd %:p:h
cmap cd. lcd %:p:h

" Thanks to Steve Losh for this liberating tip
" See http://stevelosh.com/blog/2010/09/coming-home-to-vim
nnoremap / /\v
"vnoremap / /\v
" nnoremap ? ?\v
" vnoremap ? ?\v
" nnoremap :s/ :s/\v

" command-line window
" nnoremap q: q:i
" nnoremap q/ q/i
" nnoremap q? q?i

" Speed up scrolling of the viewport slightly
nnoremap <C-e> 2<C-e>
nnoremap <C-y> 2<C-y>

" clear search matching
noremap <leader><space> :noh<cr>:call clearmatches()<cr>

" Don't jump when using * for search
nnoremap * *<c-o>

" Keep search matches in the middle of the window.
nnoremap <silent> n nzzzv
nnoremap <silent> N Nzzzv
nnoremap <silent> * *zz
nnoremap <silent> # #zz
nnoremap <silent> g* g*zz
nnoremap <silent> g# g#zz
nnoremap <silent> <C-o> <C-o>zz
nnoremap <silent> <C-i> <C-i>zz

" Same when jumping around
nnoremap g; g;zz
nnoremap g, g,zz

" Open a Quickfix window for the last search.
nnoremap <silent> <leader>? :execute 'vimgrep /'.@/.'/g %'<CR>:copen<CR>

" Highlight word

"nnoremap <silent> <leader>hh :execute 'match InterestingWord1 /\<<c-r><c-w>\>/'<cr>
"nnoremap <silent> <leader>h1 :execute 'match InterestingWord1 /\<<c-r><c-w>\>/'<cr>
"nnoremap <silent> <leader>h2 :execute '2match InterestingWord2 /\<<c-r><c-w>\>/'<cr>
"nnoremap <silent> <leader>h3 :execute '3match InterestingWord3 /\<<c-r><c-w>\>/'<cr>

" }}}
" Directional Keys ---------------------------------------------------------- {{{
"inoremap <up> <nop>
"inoremap <down> <nop>
"inoremap <left> <nop>
"inoremap <right> <nop>

" remap arrow keys
nnoremap <left> :bprev<CR>
nnoremap <right> :bnext<CR>
nnoremap <up> :tabnext<CR>
nnoremap <down> :tabprev<CR>

" smash escape
inoremap jk <esc>
inoremap kj <esc>

" better ESC
"inoremap <C-k> <Esc>

" " change cursor position in insert mode
" inoremap <C-h> <left>
" inoremap <C-l> <right>
"
" inoremap <C-u> <C-g>u<C-u>
"
" if mapcheck('<space>/') == ''
"     nnoremap <space>/ :vimgrep //gj **/*<left><left><left><left><left><left><left><left>
" endif

" Easier moving in tabs and windows
noremap <C-j>  <C-w>j
noremap <C-k>  <C-w>k
noremap <C-l>  <C-w>l
noremap <C-h>  <C-w>h

" Easy buffer navigation
noremap <leader>bp :bprevious<cr>
noremap <leader>bn :bnext<cr>

" Splits ,v and ,h to open new splits (vertical and horizontal)
nnoremap <leader>v <C-w>v<C-w>l
nnoremap <leader>h <C-w>s<C-w>j
nnoremap <leader>s <C-w>s
nnoremap <leader>vsa :vert sba<cr>

" c-j c-k pagedown/up, I find these more 'vimish' than c-d/c-u
nnoremap <c-j> <c-d>
nnoremap <c-k> <c-u>

" screen line scroll
" nnoremap <silent> j gj
" nnoremap <silent> k gk

" }}}
" Triggers -------------------------------------------------------------------- {{{

" Save when losing focus
"au FocusLost    * :silent! wall
"
" When vimrc is edited, reload it
"autocmd! BufWritePost vimrc source ~/.vimrc

" }}}
" Convenience mappings -------------------------------------------------------- {{{

" quick buffer open
" nnoremap gb :ls<cr>:e #
"
" if neobundle#is_sourced('vim-dispatch')
"     nnoremap <leader>tag :Dispatch ctags -R<cr>
" endif

" Fast saving and closing current buffer without closing windows displaying the buffer
nmap <leader>wq :w!<cr>:Bclose<cr>

" Seriously, guys. It's not like :W is bound to anything anyway.
command! W :w

" map <Alt-p> and <Alt-P> to paste below/above and reformat
nnoremap <M-P> P'[v']=
nnoremap <M-p> p'[v']=

" Clean whitespace
map <leader>W :%s/\s\+$//<cr>:let @/=''<CR>

" Formatting, TextMate-style
nnoremap <leader>q gqip
" good when you want to delete a paragraph?
"nnoremap <leader>q gqap

" Easier linewise reselection
nnoremap <leader>v V`]

" Better Completion
set completeopt=longest,menuone,preview

" This is a little tweak that is a time-saver while you’re building
" up your .vimrc. This effectively maps the ,ev and ,sv keys to
" edit/reload .vimrc. (I got this from Derek Wyatt’s .vimrc file.)
nnoremap <leader>ev <C-w>s<C-w>j:e $MYVIMRC<cr>
nnoremap <leader>sv <C-w>s<C-w>j:so $MYVIMRC<cr>
nnoremap <leader>es <C-w>s<C-w>j:e ~/.vim/snippets/<cr>
nnoremap <leader>eg <C-w>s<C-w>j:e ~/.gitconfig<cr>
nnoremap <leader>ez <C-w>s<C-w>j:e ~/.zshrc<cr>
nnoremap <leader>et <C-w>s<C-w>j:e ~/.tmux.conf<cr>

" reselect last paste
" nnoremap <expr> gp '`[' . strpart(getregtype(), 0, 1) . '`]'

" find current word in quickfix
" nnoremap <leader>fw :execute "vimgrep ".expand("<cword>")." %"<cr>:copen<cr>
" find last search in quickfix
" nnoremap <leader>ff :execute 'vimgrep /'.@/.'/g %'<cr>:copen<cr>

" Sudo to write
cmap w!! w !sudo tee % >/dev/null

" Quickly close the current window
"nnoremap <leader>q :q<CR>

" Use Q for formatting the current paragraph (or visual selection)
"vmap Q gq
"nmap Q gqap

" Use ,d (or ,dd or ,dj or 20,dd) to delete a line without adding it to the
" yanked stack (also, in visual mode)
nmap <silent> <leader>d "_d
vmap <silent> <leader>d "_d

" Yank from the cursor to the end of the line, to be consistent with C and D.
nnoremap Y y$
nnoremap D d$

" Yank/paste to the OS clipboard with ,y and ,p
nmap <leader>yo "+y
nmap <leader>Yo "+yy
nmap <leader>po "+p
nmap <leader>Po "+P

" Reselect visual block after indent/outdent
vnoremap < <gv
vnoremap > >gv

" Quick alignment of text
nmap <leader>al :left<CR>
nmap <leader>ar :right<CR>
nmap <leader>ac :center<CR>

" Pull word under cursor into LHS of a substitute (for quick search and replace)
nmap <leader>z :%s#\<<C-r>=expand("<cword>")<CR>\>#

" Scratch
nmap <leader><tab> :Sscratch<CR><C-W>x<C-J>

" Reselect text that was just pasted with <leader>v
nnoremap <leader>v V`]

" Replaste
nnoremap <D-p> "_ddPV`]

" }}}
" Folding --------------------------------------------------------------------- {{{

" Space to toggle folds.
nnoremap <Space> za
vnoremap <Space> za

" Make zO recursively open whatever top level fold we're in, no matter where the
" cursor happens to be.
nnoremap zO zCzO

" Use ,z to "focus" the current fold.
nnoremap <leader>z zMzvzz

" }}}
" Destroy infuriating keys ---------------------------------------------------- {{{

" Fuck you, help key.
if s:is_macvim
    set fuoptions=maxvert,maxhorz
    noremap <F1> :set invfullscreen<CR>
    inoremap <F1> <ESC>:set invfullscreen<CR>a
endif

" Fuck you too, manual key.
nnoremap K <nop>

" Stop it, hash key.
inoremap # X<BS>#

" Dreadful 'Ex' Mode
nnoremap <silent> Q ZZ

" }}}
" Various filetype-specific stuff --------------------------------------------- {{{

" api blueprint {{{

autocmd FileType apiblueprint setlocal tabstop=4
autocmd FileType apiblueprint setlocal softtabstop=4
autocmd FileType apiblueprint setlocal shiftwidth=4

" Use <localleader>1/2/3 to add headings.
au Filetype apiblueprint nnoremap <buffer> <localleader>1 yypVr=
au Filetype apiblueprint nnoremap <buffer> <localleader>2 yypVr-
au Filetype apiblueprint nnoremap <buffer> <localleader>3 I### <ESC>

autocmd FileType apiblueprint setlocal wrap
autocmd FileType apiblueprint setlocal linebreak
autocmd FileType apiblueprint setlocal nolist  " list disables linebreak
autocmd FileType apiblueprint setlocal breakindentopt=shift:10
autocmd FileType apiblueprint setlocal textwidth=0
autocmd FileType apiblueprint setlocal virtualedit=
autocmd FileType apiblueprint setlocal display+=lastline
autocmd FileType apiblueprint setlocal wrapmargin=10
autocmd FileType apiblueprint noremap  <buffer> <silent> k gk
autocmd FileType apiblueprint noremap  <buffer> <silent> j gj
autocmd FileType apiblueprint noremap  <buffer> <silent> 0 g0
autocmd FileType apiblueprint noremap  <buffer> <silent> $ g$

" }}}
" Markdown {{{

au BufNewFile,BufRead *.m*down setlocal filetype=markdown
au FileType markdown setlocal omnifunc=htmlcomplete#CompleteTags

" Use <localleader>1/2/3 to add headings.
au Filetype markdown nnoremap <buffer> <localleader>1 yypVr=
au Filetype markdown nnoremap <buffer> <localleader>2 yypVr-
au Filetype markdown nnoremap <buffer> <localleader>3 I### <ESC>

autocmd FileType markdown setlocal wrap
autocmd FileType markdown setlocal linebreak
autocmd FileType markdown setlocal nolist    " list disables linebreak
autocmd FileType markdown setlocal breakindentopt=shift:10
autocmd FileType markdown setlocal textwidth=0
autocmd FileType markdown setlocal virtualedit=
autocmd FileType markdown setlocal display+=lastline
autocmd FileType markdown setlocal wrapmargin=10
autocmd FileType markdown noremap  <buffer> <silent> k gk
autocmd FileType markdown noremap  <buffer> <silent> j gj
autocmd FileType markdown noremap  <buffer> <silent> 0 g0
autocmd FileType markdown noremap  <buffer> <silent> $ g$

" }}}
" SRT {{{

" Syntax highlighting for subtitle files in Subrip (srt) format
autocmd BufNewFile,BufRead *.srt setf srt

" }}}
" Ant {{{

autocmd FileType ant setlocal sw=2
autocmd FileType ant setlocal ts=2
autocmd FileType ant setlocal sts=2
autocmd FileType ant setlocal textwidth=0

" }}}
" Python {{{

" (tab width 4 chr, wrap at 79th char)
autocmd FileType python setlocal sw=4
autocmd FileType python setlocal ts=4
autocmd FileType python setlocal sts=4
autocmd FileType python setlocal textwidth=79
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete

" }}}
" Ruby {{{

"au FileType ruby setlocal expandtab shiftwidth=2 tabstop=2 softtabstop=2

" }}}
" C {{{

au FileType c setlocal foldmethod=syntax

" }}}
" HTML, Mustache, Handlebars, html template(s) {{{

" HTML (tab width 2 chr, no wrapping)
au BufRead,BufNewFile *.handlebars set filetype=handlebars

autocmd FileType mustache,handlebars let b:match_ignorecase = 1 |
    \ let b:match_words = '<:>,' .
    \ '<\@<=[ou]l\>[^>]*\%(>\|$\):<\@<=li\>:<\@<=/[ou]l>,' .
    \ '<\@<=dl\>[^>]*\%(>\|$\):<\@<=d[td]\>:<\@<=/dl>,' .
    \ '<\@<=\([^/][^ \t>]*\)[^>]*\%(>\|$\):<\@<=/\1>'

autocmd FileType html,mustache,handlebars setlocal sw=2
autocmd FileType html,mustache,handlebars setlocal ts=2
autocmd FileType html,mustache,handlebars setlocal sts=2
autocmd FileType html,mustache,handlebars setlocal textwidth=0
autocmd FileType html,mustache,handlebars setlocal omnifunc=htmlcomplete#CompleteTags

autocmd FileType handlebars setlocal foldmethod=marker
autocmd FileType handlebars setlocal commentstring={{!%s}}
autocmd FileType handlebars setlocal foldmarker=[[[,]]]

" XHTML (tab width 2 chr, no wrapping)
autocmd FileType xhtml setlocal sw=2
autocmd FileType xhtml setlocal ts=2
autocmd FileType xhtml setlocal sts=2
autocmd FileType xhtml setlocal textwidth=0

" }}}
" CSS, Stylus, and LessCSS {{{

au Filetype stylus let anyfold_activate=1

" (tab width 2 chr, wrap at 79th char)
autocmd FileType css,stylus setlocal sw=2
autocmd FileType css,stylus setlocal ts=2
autocmd FileType css,stylus setlocal sts=2
autocmd FileType css,stylus setlocal textwidth=0
autocmd FileType css,stylus setlocal omnifunc=csscomplete#CompleteCSS

au BufNewFile,BufRead *.less setlocal filetype=less

au BufNewFile,BufRead *.css  setlocal foldmethod=marker
au BufNewFile,BufRead *.less setlocal foldmethod=marker

au BufNewFile,BufRead *.css  setlocal foldmarker={,}
au BufNewFile,BufRead *.less setlocal foldmarker={,}

" Use cc to change lines without borking the indentation.
au BufNewFile,BufRead *.css  nnoremap <buffer> cc ddko
au BufNewFile,BufRead *.less nnoremap <buffer> cc ddko

" Use <leader>S to sort properties.  Turns this:
"
"     p {
"         width: 200px;
"         height: 100px;
"         background: red;
"
"         ...
"     }
"
" into this:

"     p {
"         background: red;
"         height: 100px;
"         width: 200px;
"
"         ...
"     }
"

au BufNewFile,BufRead *.css  nnoremap <buffer> <localleader>S ?{<CR>jV/\v^\s*\}?$<CR>k:sort<CR>:noh<CR>
au BufNewFile,BufRead *.less nnoremap <buffer> <localleader>S ?{<CR>jV/\v^\s*\}?$<CR>k:sort<CR>:noh<CR>

" }}}
" Haskell {{{

au BufEnter *.hs compiler ghc
let g:ghc = "/usr/local/bin/ghc"
let g:haddock_browser = "open"

" }}}
" Coffee Script {{{

au BufNewFile,BufReadPost *.coffee setl shiftwidth=2 tabstop=2 softtabstop=2 expandtab

" }}}
" Javascript {{{

" (tab width 2 chr, no-wrapping)
autocmd FileType javascript setlocal sw=2
autocmd FileType javascript setlocal ts=2
autocmd FileType javascript setlocal sts=2
autocmd FileType javascript setlocal textwidth=0
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS


au Filetype javascript let anyfold_activate=1
au FileType javascript nnoremap <buffer> <C-c>  :<C-u>call WriteJSDocComment()<CR>
au FileType javascript call JavaScriptFold()
au FileType javascript setl fen
au FileType javascript setl nocindent

au FileType javascript imap <C-t> console.log();<esc>hi
au FileType javascript imap <C-a> alert();<esc>hi

function! JavaScriptFold()
    "setl foldmethod=syntax
    "setl foldlevelstart=1
    "syn region foldBraces start="{" end="}" transparent fold keepend extend
    setl foldmethod=marker
    setl foldmarker={,}

    function! FoldText()
        return substitute(getline(v:foldstart), '{.*', '{...}', '')
    endfunction
    setl foldtext=FoldText()
endfunction

" }}}
" Vim {{{

au FileType vim setlocal foldmethod=marker
au FileType help setlocal textwidth=78
 " Bind <F1> to show the keyword under cursor
" general help can still be entered manually, with :h
au FileType vim noremap <buffer> <F1> <Esc>:help <C-r><C-w><CR>
au FileType vim noremap! <buffer> <F1> <Esc>:help <C-r><C-w><CR>

" }}}
" Firefox {{{

au BufRead,BufNewFile ~/Library/Caches/* setlocal buftype=nofile

" }}}
" XML {{{

autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

" }}}
" supervisor {{{

au BufRead,BufNewFile supervisor* set ft=supervisor

" }}}
" haproxy {{{

au BufRead,BufNewFile haproxy* set ft=haproxy

" }}}

" }}}
" Plugin settings ------------------------------------------------------------- {{{

" Rainbow Parentheses {{{

let g:rbpt_max = 16
let g:rbpt_loadcmd_toggle = 0
autocmd VimEnter * RainbowParenthesesToggle
autocmd Syntax * RainbowParenthesesLoadRound
autocmd Syntax * RainbowParenthesesLoadSquare
autocmd Syntax * RainbowParenthesesLoadBraces

" }}}
" Vim-Airline {{{

" Finally, you can add the convenience variable to your vimrc which will
" automatically populate the g:airline_symbols dictionary with the powerline
" symbols.

let g:airline_powerline_fonts = 1
" let g:airline#extensions#tabline#enabled = 1
" let g:airline#extensions#tabline#left_sep=' '
" let g:airline#extensions#tabline#left_alt_sep='¦'

" }}}
" Par {{{

" set formatprg=par\ -w72
:map <A-q> {v}!par -jw72<CR>
:vmap <A-q> !par -jw72<CR>

" }}}
" Color Highlight {{{

"let g:colorizer_auto_color = 1
let g:colorizer_auto_filetype='css,html,handlebars,stylus,javascript'
let g:colorizer_skip_comments = 1

" }}}
" Ack {{{

map <leader>a :Ack! 
if executable('ag')
    let g:ackprg = "ag --hidden --nogroup --column --smart-case --follow"
endif

" }}}
" Marked Support {{{
nnoremap <leader>M :silent !open -a Marked\ 2.app '%:p'<cr>

" }}}
" MultiMarkdown Support {{{
nnoremap <leader>MM :silent !open -a MultiMarkdown\ Composer.app '%:p'<cr>

" }}}
" Google Code Wiki {{{

autocmd BufNewFile,BufRead *.wiki set ft=googlecodewiki

" }}}
" tcomment_vim {{{

" yank visual before toggle comment
vmap gy ygvgc
" yank and paste visual before toggle comment
vmap gyy ygvgc'>gp'.
" yank line before toggle comment
nmap gy yygcc
" yank and paste line before toggle comment and remember position
" it works both in normal and insert mode
" Use :t-1 instead of yyP to preserve registers
nmap gyy mz:t-1<cr>gCc`zmz
imap gyy <esc>:t-1<cr>gCcgi

" And one more mapping for consistency: gcc toggle comment line but gc toggle
" comment visual, so let's make it more consistent:
vmap gcc gc

" }}}
" NERD Tree {{{

" Put focus to the NERD Tree with F3 (tricked by quickly closing it and
" immediately showing it again, since there is no :NERDTreeFocus command)
nmap <leader>fn :NERDTreeClose<CR>:NERDTreeToggle<CR>
nmap <leader>fN :NERDTreeClose<CR>
"map <F2> :NERDTreeToggle<CR>:NERDTreeMirror<CR>

let NERDTreeChDirMode=0

" Store the bookmarks file
call EnsureExists(s:get_cache_dir('NERDTreeBookmarks'))
let NERDTreeBookmarksFile=s:get_cache_dir('NERDTreeBookmarks')

" Show the bookmarks table on startup
let NERDTreeShowBookmarks=1

" Show hidden files, too
let NERDTreeShowFiles=1
let NERDTreeShowHidden=1

" Do not quit on opening files from the tree
let NERDTreeQuitOnOpen=0

" Highlight the selected entry in the tree
let NERDTreeHighlightCursorline=1

" Use a single click to fold/unfold directories and a double click to open files
let NERDTreeMouseMode=2

let NERDTreeKeepTreeInNewTab=1

" Don't display these kinds of files
let NERDTreeIgnore=[ '\.DS_Store', '\.pyc$', '\.pyo$', '\.py\$class$', '\.obj$', '\.o$', '\.so$', '\.egg$', '^\.git$' ]

au Filetype nerdtree setlocal nolist
au Filetype nerdtree setlocal relativenumber

" }}}
" HTML5 {{{

let g:event_handler_attributes_complete = 0
let g:rdfa_attributes_complete = 0
let g:microdata_attributes_complete = 0
let g:atia_attributes_complete = 0

" }}}
" Syntastic {{{

au BufEnter * call UpdateJsHintConf()
au BufEnter * call UpdateJscsConf()
au BufEnter * call UpdateEslintConf()
let g:syntastic_enable_signs=0
let g:syntastic_error_symbol='✗'
let g:syntastic_style_error_symbol = '✠'
let g:syntastic_warning_symbol = '∆'
let g:syntastic_style_warning_symbol = '≈'
let g:syntastic_always_populate_loc_list=1
let g:syntastic_javascript_checkers = ['eslint']
let g:syntastic_javascript_closurecompiler_path = '/usr/local/lib/closurecompiler/compiler.jar'
let g:syntastic_mode_map = { 'mode': 'active',
                            \ 'active_filetypes': ['ruby', 'php', 'javascript', 'html', 'handlebars', 'stylus', 'css'],
                            \ 'passive_filetypes': ['puppet', 'slim'] }


" }}}
" Har {{{

au BufRead,BufNewFile *.har set filetype=json
nmap <leader>jt <Esc>:%!python -m json.tool<CR><Esc>:set filetype=json<CR>

" }}}
" JSON {{{

au BufRead,BufNewFile *.json set filetype=json
nmap <leader>jt <Esc>:%!python -m json.tool<CR><Esc>:set filetype=json<CR>

" }}}
" Sessions {{{

let g:session_autosave = 'no'
let g:session_autoload = 'yes'
let g:session_directory = '~/.vim/tmp/sessions'
let g:session_persist_colors = 0
let g:session_menu = 0

" }}}
" vim-go {{{

let g:go_disable_autoinstall = 1

" }}}
" Rooter{{{

let g:rooter_patterns = ['package.json', 'bower.json', 'Jakefile.js', 'Rakefile', 'Gruntfile.js', '.git', '.git/', '.svn/']
let g:rooter_use_lcd = 1

" }}}
" Fugitive {{{

nnoremap <silent> <leader>gd :Gdiff<cr>
nnoremap <silent> <leader>gl :Glog<cr>
nnoremap <silent> <leader>gs :Gstatus<cr>
nnoremap <silent> <leader>gw :Gwrite<cr>
nnoremap <silent> <leader>gg :Ggrep<cr>
nnoremap <silent> <leader>gh :Gbrowse<cr>
nnoremap <silent> <leader>ga :Gadd<cr>
nnoremap <silent> <leader>gb :Gblame<cr>
nnoremap <silent> <leader>gp :Git push<CR>
nnoremap <silent> <leader>gc :Gcommit<cr>
nnoremap <silent> <leader>gm :Gmove<cr>
nnoremap <silent> <leader>gr :Gremove<cr>

autocmd BufReadPost fugitive://* set bufhidden=delete

" ,f for global git serach for word under the cursor (with highlight)
nmap <leader>gf :let @/="\\<<C-R><C-W>\\>"<CR>:set hls<CR>:silent Ggrep -w "<C-R><C-W>"<CR>:ccl<CR>:cw<CR><CR>
" same in visual mode
:vmap <leader>gf y:let @/=escape(@", '\\[]$^*.')<CR>:set hls<CR>:silent Ggrep -F "<C-R>=escape(@", '\\"#')<CR>"<CR>:ccl<CR>:cw<CR><CR>

"To use with GitHub FI, point g:fugitive_github_domains at a list of domains:
let g:fugitive_github_domains = ['http://git.ibaset.com']

augroup ft_fugitive
    au!
    au BufNewFile,BufRead .git/index setlocal nolist
augroup END

" }}}
" YankRing {{{

let g:yankring_replace_n_pkey = '<leader>['
let g:yankring_replace_n_nkey = '<leader>]'
let g:yankring_history_dir = '~/.vim/tmp/'
nmap <leader>y :YRShow<cr>

function! YRRunAfterMaps()
    nnoremap Y :<C-U>YRYankCount 'y$'<CR>
    omap <expr> L YRMapsExpression("", "$")
    omap <expr> H YRMapsExpression("", "^")
endfunction

" }}}
" vim-indent-object {{{

let g:indentobject_meaningful_indentation = ["haml", "sass", "python", "yaml", "markdown"]

" }}}
" easybuffer {{{

nmap <leader>be :EasyBufferToggle<enter>

" }}}
" ctrlp {{{

let g:ctrlp_clear_cache_on_exit=1
let g:ctrlp_max_height=40
let g:ctrlp_show_hidden=1
let g:ctrlp_follow_symlinks=1
let g:ctrlp_max_files=20000
call EnsureExists(s:get_cache_dir('ctrlp'))
let g:ctrlp_cache_dir=s:get_cache_dir('ctrlp')
let g:ctrlp_reuse_window='startify'
let g:ctrlp_extensions=['funky']
let g:ctrlp_root_markers = ['Rakefile', 'Makefile', 'Jakefile.js', 'bower.json', 'package.json', 'build.xml']
let g:ctrlp_custom_ignore = {
    \ 'dir': '\v[\/]((assets|node_modules|build|tmp|log|vendor\/(rails|gems|plugins)|bower_components|components)|(\.(git|hg|svn|idea|sass-cache)))$',
    \ 'file': '\v(\.(#.+|DS_Store|svn|png|jpe?g|gif|elc|rbc|pyc|swp|psd|ai|pdf|mov|aep|dmg|zip|gz|bmp)|(Thumbs\.db))$'
    \ }

nmap \ [ctrlp]
nnoremap [ctrlp] <nop>

nnoremap [ctrlp]t :CtrlPBufTag<cr>
nnoremap [ctrlp]T :CtrlPTag<cr>
nnoremap [ctrlp]l :CtrlPLine<cr>
nnoremap [ctrlp]o :CtrlPFunky<cr>
nnoremap [ctrlp]b :CtrlPBuffer<cr>

" }}}
" HTML Indent {{{

let g:html_indent_inctags = "html,body,head,tbody"
let g:html_indent_script1 = "inc"
let g:html_indent_style1 = "inc"
let g:html_indent_tags = 'li\|p'

" }}}
" neocomplete {{{

if s:has_lua
    call EnsureExists(s:get_cache_dir('neocomplete'))
    " Disable AutoComplPop.
    let g:acp_enableAtStartup=0
    let g:neocomplete#enable_at_startup=1
    let g:neocomplete#data_directory=s:get_cache_dir('neocomplete')
    let g:neocomplete#enable_smart_case=1
    " Set minimum syntax keyword length.
    let g:neocomplete#sources#syntax#min_keyword_length = 3
    let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'
endif

" }}}
" neosnippet {{{

let g:neosnippet#snippets_directory='~/.vim/bundle/vim-snippets/snippets,~/.vim/snippets'
let g:neosnippet#enable_snipmate_compatibility=1

imap <expr><TAB> neosnippet#expandable_or_jumpable() ? "\<Plug>(neosnippet_expand_or_jump)" : (pumvisible() ? "\<C-n>" : "\<TAB>")
smap <expr><TAB> neosnippet#expandable_or_jumpable() ? "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
imap <expr><S-TAB> pumvisible() ? "\<C-p>" : ""
smap <expr><S-TAB> pumvisible() ? "\<C-p>" : ""

" }}}
" unimpaired {{{

" Bubbling lines
nmap <C-Up> [e
nmap <C-Down> ]e
vmap <C-Up> [egv
vmap <C-Down> ]egv

" }}}
" emmet-vim {{{

function! s:zen_html_tab()
    let line = getline('.')
    if match(line, '<.*>') < 0
        return "\<c-y>,"
    endif
    return "\<c-y>n"
endfunction
autocmd FileType xml,xsl,xslt,xsd,css,sass,scss,less,mustache imap <buffer><tab> <c-y>,
autocmd FileType html imap <buffer><expr><tab> <sid>zen_html_tab()

" }}}
" JsBeautify {{{

nnoremap <leader>fjs :call JsBeautify()<cr>

" }}}
" tabular {{{

nmap <Leader>a& :Tabularize /&<CR>
vmap <Leader>a& :Tabularize /&<CR>
nmap <Leader>a= :Tabularize /=<CR>
vmap <Leader>a= :Tabularize /=<CR>
nmap <Leader>a: :Tabularize /:<CR>
vmap <Leader>a: :Tabularize /:<CR>
nmap <Leader>a:: :Tabularize /:\zs<CR>
vmap <Leader>a:: :Tabularize /:\zs<CR>
nmap <Leader>a, :Tabularize /,<CR>
vmap <Leader>a, :Tabularize /,<CR>
nmap <Leader>a<Bar> :Tabularize /<Bar><CR>
vmap <Leader>a<Bar> :Tabularize /<Bar><CR>

" }}}
" vim-sneak {{{

let g:sneak#streak = 1

" }}}
" undotree {{{

let g:undotree_WindowLayout='botright'
let g:undotree_SetFocusWhenToggle=1
nnoremap <silent> <F5> :UndotreeToggle<CR>

" }}}
" EasyGrep {{{

let g:EasyGrepRecursive=1
let g:EasyGrepAllOptionsInExplorer=1
let g:EasyGrepCommand=1
nnoremap <leader>vo :GrepOptions<cr>

" }}}

" }}}
" Text objects ---------------------------------------------------------------- {{{

" Shortcut for [] {{{

"onoremap id i[
"onoremap ad a[
"vnoremap id i[
"vnoremap ad a[

" }}}
" Next/Last () {{{
"vnoremap <silent> inb :<C-U>normal! f(vib<cr>
"onoremap <silent> inb :<C-U>normal! f(vib<cr>
"vnoremap <silent> anb :<C-U>normal! f(vab<cr>
"onoremap <silent> anb :<C-U>normal! f(vab<cr>
"vnoremap <silent> in( :<C-U>normal! f(vi(<cr>
"onoremap <silent> in( :<C-U>normal! f(vi(<cr>
"vnoremap <silent> an( :<C-U>normal! f(va(<cr>
"onoremap <silent> an( :<C-U>normal! f(va(<cr>

"vnoremap <silent> ilb :<C-U>normal! F)vib<cr>
"onoremap <silent> ilb :<C-U>normal! F)vib<cr>
"vnoremap <silent> alb :<C-U>normal! F)vab<cr>
"onoremap <silent> alb :<C-U>normal! F)vab<cr>
"vnoremap <silent> il( :<C-U>normal! F)vi(<cr>
"onoremap <silent> il( :<C-U>normal! F)vi(<cr>
"vnoremap <silent> al( :<C-U>normal! F)va(<cr>
"onoremap <silent> al( :<C-U>normal! F)va(<cr>
" }}}
" Next/Last {} {{{
"vnoremap <silent> inB :<C-U>normal! f{viB<cr>
"onoremap <silent> inB :<C-U>normal! f{viB<cr>
"vnoremap <silent> anB :<C-U>normal! f{vaB<cr>
"onoremap <silent> anB :<C-U>normal! f{vaB<cr>
"vnoremap <silent> in{ :<C-U>normal! f{vi{<cr>
"onoremap <silent> in{ :<C-U>normal! f{vi{<cr>
"vnoremap <silent> an{ :<C-U>normal! f{va{<cr>
"onoremap <silent> an{ :<C-U>normal! f{va{<cr>

"vnoremap <silent> ilB :<C-U>normal! F}viB<cr>
"onoremap <silent> ilB :<C-U>normal! F}viB<cr>
"vnoremap <silent> alB :<C-U>normal! F}vaB<cr>
"onoremap <silent> alB :<C-U>normal! F}vaB<cr>
"vnoremap <silent> il{ :<C-U>normal! F}vi{<cr>
"onoremap <silent> il{ :<C-U>normal! F}vi{<cr>
"vnoremap <silent> al{ :<C-U>normal! F}va{<cr>
"onoremap <silent> al{ :<C-U>normal! F}va{<cr>
" }}}
" Next/Last [] {{{
"vnoremap <silent> ind :<C-U>normal! f[vi[<cr>
"onoremap <silent> ind :<C-U>normal! f[vi[<cr>
"vnoremap <silent> and :<C-U>normal! f[va[<cr>
"onoremap <silent> and :<C-U>normal! f[va[<cr>
"vnoremap <silent> in[ :<C-U>normal! f[vi[<cr>
"onoremap <silent> in[ :<C-U>normal! f[vi[<cr>
"vnoremap <silent> an[ :<C-U>normal! f[va[<cr>
"onoremap <silent> an[ :<C-U>normal! f[va[<cr>

"vnoremap <silent> ild :<C-U>normal! F]vi[<cr>
"onoremap <silent> ild :<C-U>normal! F]vi[<cr>
"vnoremap <silent> ald :<C-U>normal! F]va[<cr>
"onoremap <silent> ald :<C-U>normal! F]va[<cr>
"vnoremap <silent> il[ :<C-U>normal! F]vi[<cr>
"onoremap <silent> il[ :<C-U>normal! F]vi[<cr>
"vnoremap <silent> al[ :<C-U>normal! F]va[<cr>
"onoremap <silent> al[ :<C-U>normal! F]va[<cr>
"" }}}
" Next/Last <> {{{
"vnoremap <silent> in< :<C-U>normal! f<vi<<cr>
"onoremap <silent> in< :<C-U>normal! f<vi<<cr>
"vnoremap <silent> an< :<C-U>normal! f<va<<cr>
"onoremap <silent> an< :<C-U>normal! f<va<<cr>

"vnoremap <silent> il< :<C-U>normal! f>vi<<cr>
"onoremap <silent> il< :<C-U>normal! f>vi<<cr>
"vnoremap <silent> al< :<C-U>normal! f>va<<cr>
"onoremap <silent> al< :<C-U>normal! f>va<<cr>
" }}}
" Next '' {{{
"vnoremap <silent> in' :<C-U>normal! f'vi'<cr>
"onoremap <silent> in' :<C-U>normal! f'vi'<cr>
"vnoremap <silent> an' :<C-U>normal! f'va'<cr>
"onoremap <silent> an' :<C-U>normal! f'va'<cr>

"vnoremap <silent> il' :<C-U>normal! F'vi'<cr>
"onoremap <silent> il' :<C-U>normal! F'vi'<cr>
"vnoremap <silent> al' :<C-U>normal! F'va'<cr>
"onoremap <silent> al' :<C-U>normal! F'va'<cr>
" }}}
" Next "" {{{
"vnoremap <silent> in" :<C-U>normal! f"vi"<cr>
"onoremap <silent> in" :<C-U>normal! f"vi"<cr>
"vnoremap <silent> an" :<C-U>normal! f"va"<cr>
"onoremap <silent> an" :<C-U>normal! f"va"<cr>

"vnoremap <silent> il" :<C-U>normal! F"vi"<cr>
"onoremap <silent> il" :<C-U>normal! F"vi"<cr>
"vnoremap <silent> al" :<C-U>normal! F"va"<cr>
"onoremap <silent> al" :<C-U>normal! F"va"<cr>
" }}}

" }}}
" Quickreturn ----------------------------------------------------------------- {{{

inoremap <c-cr> <esc>A<cr>
inoremap <s-cr> <esc>A:<cr>

" }}}
" Error toggles --------------------------------------------------------------- {{{

" Tame the quickfix window (open/close using f4)
nmap <silent> <f4> :QFixToggle<cr>
nmap <silent> <f3> :ErrorsToggle<cr>

" }}}
" Persistent echo ------------------------------------------------------------- {{{

" http://vim.wikia.com/wiki/Make_echo_seen_when_it_would_otherwise_disappear_and_go_unseen
"
" further improvement in restoration of the &updatetime. To make this
" usable in the plugins, we want it to be safe for the case when
" two plugins use same this same technique. Two independent
" restorations of &ut can run in unpredictable sequence. In order to
" make it safe, we add additional check in &ut restoration.
let s:Pecho=''
fu! s:Pecho(msg)
  let s:hold_ut=&ut | if &ut>1|let &ut=1|en
  let s:Pecho=a:msg
  aug Pecho
    au CursorHold * if s:Pecho!=''|echo s:Pecho
          \|let s:Pecho=''|if s:hold_ut > &ut |let &ut=s:hold_ut|en|en
          \|aug Pecho|exe 'au!'|aug END|aug! Pecho
  aug END
endf

" }}}
" Open quoted ----------------------------------------------------------------- {{{

nnoremap <silent> ø :OpenQuoted<cr>
command! OpenQuoted call OpenQuoted()

" Open the file in the current (or next) set of quotes.
function! OpenQuoted() " {{{
    let @r = ''

    exe 'normal! vi' . "'" . '"ry'

    if len(@r) == 0
        exe 'normal! i' . '"' . '"ry'
    endif

    if len(@r) == 0
        exe 'normal! "ry'
        let @r = ''
    endif

    exe "silent !open ." . @r
endfunction " }}}

" go back to previous position of cursor if any {{{
 autocmd BufReadPost *
   \ if line("'\"") > 0 && line("'\"") <= line("$") |
   \  exe 'normal! g`"zvzz' |
   \ endif

" }}}

" Common abbreviations / misspellings {{{
source ~/.vim/autocorrect.vim
" }}}

" Extra vi-compatibility {{{
" set extra vi-compatible options
set cpoptions+=$     " when changing a line, don't redisplay, but put a '$' at
                     " the end during the change
set formatoptions-=o " don't start new lines w/ comment leader on pressing 'o'
au filetype vim set formatoptions-=o
                     " somehow, during vim filetype detection, this gets set
                     " for vim files, so explicitly unset it again
" }}}

" Extra user or machine specific settings {{{
if filereadable(expand("~/.user.vim"))
  source ~/.user.vim
endif
" }}}

" Creating underline/overline headings for markup languages
" Inspired by http://sphinx.pocoo.org/rest.html#sections
nnoremap <leader>1 yyPVr=jyypVr=
nnoremap <leader>2 yyPVr*jyypVr*
nnoremap <leader>3 yypVr=
nnoremap <leader>4 yypVr-
nnoremap <leader>5 yypVr^
nnoremap <leader>6 yypVr"   

" }}}
" MacVim ---------------------------------------------------------------------- {{{

if has('gui_running')

    set guioptions-=T          	" remove the toolbar
    set guioptions+=t           " tear off menu items
    set guioptions-=l           " Remove all the UI cruft
    set guioptions-=L
    set guioptions-=r
    set guioptions-=R
    set lines=40               	" 40 lines of text instead of 24,
    set columns=80

    if s:is_windows
      autocmd GUIEnter * simalt ~x
    endif

    " <* <*> <+> <$> *** <|> !! || === ==> <<< >>> <> +++ <- -> => >> << >>= =<< .. ... :: -< >- -<< >>- ++ /= ==
    if s:is_macvim
      " set gfn=Anonymous\ Pro\ for\ Powerline:h14
      " set gfn=Hack:h12
      " set gfn=Pragmata\ Pro:h12
      set gfn=Hasklig:h12
      set macligatures
      set transparency=5          " Make the window slightly transparent
    endif

    if s:is_windows
      set gfn=Ubuntu_Mono:h10
    endif

    if has('gui_gtk')
      set gfn=Ubuntu\ Mono\ 11
    endif

    "winpos 0 0
    if ! &diff
        winsize 130 90
    else
        winsize 227 90
    endif

    highlight SpellBad term=underline gui=undercurl guisp=Orange

    " Use a line-drawing char for pretty vertical splits.
    set fillchars=vert:│

    " Set up the gui cursor to look nice
    set guicursor=n-c:block-Cursor-blinkon0
    set guicursor+=v:block-vCursor-blinkon0
    set guicursor+=ve:ver35-Cursor
    set guicursor+=o:hor50-Cursor
    set guicursor+=i-ci:ver25-Cursor
    set guicursor+=r-cr:hor20-Cursor
    set guicursor+=sm:block-Cursor-blinkwait175-blinkoff150-blinkon175

else
  if $COLORTERM == 'gnome-terminal'
    set t_Co=256 "why you no tell me correct colors?!?!
  endif
  if $TERM_PROGRAM == 'iTerm.app'
    " different cursors for insert vs normal mode
    if exists('$TMUX')
      let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
      let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
    else
      let &t_SI = "\<Esc>]50;CursorShape=1\x7"
      let &t_EI = "\<Esc>]50;CursorShape=0\x7"
    endif
  endif
endif

" }}}

" _ Vim {{{
augroup ft_vim
    au!
    au FileType vim setlocal foldmethod=marker
    au FileType help setlocal textwidth=78
    au BufWinEnter *.txt if &ft == 'help' | wincmd L | endif
augroup END
" }}}

" EXTENSIONS {{{

" _. Scratch {{{
source ~/.vim/functions/scratch_toggle.vim
" }}}

" _. Buffer Handling {{{
source ~/.vim/functions/buffer_handling.vim
" }}}

" _. Tab {{{
source ~/.vim/functions/insert_tab_wrapper.vim
" }}}

" _. Text Folding {{{
source ~/.vim/functions/my_fold_text.vim
" }}}

" }}}
" TEXT OBJECTS {{{

" Shortcut for [] motion
onoremap ir i[
onoremap ar a[
vnoremap ir i[
vnoremap ar a[

" }}}

" Load addidional configuration (ie to overwrite shorcuts) {{{
if filereadable(expand("~/.vim/after.vimrc"))
  source ~/.vim/after.vimrc
endif
" }}}
