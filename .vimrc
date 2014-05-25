" vim:set foldmethod=marker foldlevel=0:
" http://lambdalisue.hatenablog.com/entry/2013/06/23/071344

" Windows, unixでのruntimepathの違いを吸収するためのもの。 {{{
" $MY_VIMRUNTIMEはユーザーランタイムディレクトリを示す。
" :echo $MY_VIMRUNTIMEで実際のパスを確認できます。
if isdirectory($HOME . '/.vim')
  let $MY_VIMRUNTIME = $HOME.'/.vim'
"elseif isdirectory($HOME . '\vimfiles')
elseif filereadable($HOME . '\vimfiles')
  let $MY_VIMRUNTIME = $HOME.'\vimfiles'
elseif isdirectory($VIM . '\vimfiles')
  let $MY_VIMRUNTIME = $VIM.'\vimfiles'
endif
" }}}

" plugin neobundle {{{
" https://github.com/Shougo/neobundle.vim
" $ mkdir -p ~/.vim/bundle
" $ git clone git://github.com/Shougo/neobundle.vim ~/.vim/bundle/neobundle.vim
set nocompatible  " Be iMproved
filetype off

" httpsで取ってくるように指定する
let g:neobundle_default_git_protocol = 'https'

let s:noplugin = 0
let s:bundle_root = $MY_VIMRUNTIME . '/bundle'
let s:neobundle_root = s:bundle_root . '/neobundle.vim'
let s:bundle_log = $MY_VIMRUNTIME . '/info/neobundle/'

if !isdirectory(s:neobundle_root) || v:version < 702
  " NeoBundleが存在しない、もしくはVimのバージョンが古い場合はプラグインを一切読み込まない
  let s:noplugin = 1
else
  if !isdirectory(s:bundle_log)
    call mkdir(s:bundle_log, 'p')
  endif
  let g:neobundle#log_filename = s:bundle_log . strftime('%Y-%m-%d.log')

  " NeoBundleを'runtimepath'に追加し初期化を行う
  if has('vim_starting')
    execute "set runtimepath+=" . s:neobundle_root
  endif
  call neobundle#rc(s:bundle_root)

  " NeoBundle自身もNeoBundleで管理する
  NeoBundleFetch 'Shougo/neobundle.vim'

  " 非同期通信を可能にする
  " 'build'が指定されているのでインストール時に自動的に
  " 指定されたコマンドが実行され vimproc がコンパイルされる
  NeoBundle "Shougo/vimproc", {
    \ "build": {
    \   "windows"   : "make -f make_mingw32.mak",
    \   "cygwin"    : "make -f make_cygwin.mak",
    \   "mac"       : "make -f make_mac.mak",
    \   "unix"      : "make -f make_unix.mak",
    \ }}

  " vimshell
  NeoBundle 'Shougo/vimshell'

  " My Bundles here:
  "
  " Note: You don't set neobundle setting in .gvimrc!
  " Original repos on github
  "NeoBundle 'tpope/vim-fugitive'
  "NeoBundle 'Lokaltog/vim-easymotion'
  "NeoBundle 'rstacruz/sparkup', {'rtp': 'vim/'}
  " vim-scripts repos
  "NeoBundle 'L9'
  "NeoBundle 'FuzzyFinder'
  "NeoBundle 'rails.vim'
  " Non github repos
  "NeoBundle 'git://git.wincent.com/command-t.git'
  " gist repos
  "NeoBundle 'gist:Shougo/656148', {
  "      \ 'name': 'everything.vim',
  "      \ 'script_type': 'plugin'}
  " Non git repos
  "NeoBundle 'http://svn.macports.org/repository/macports/contrib/mpvim/'
  "NeoBundle 'https://bitbucket.org/ns9tks/vim-fuzzyfinder'

  " 補完
  " NeoBundle 'Shougo/neocomplcache.git'
  " NeoBundle 'Shougo/neocomplcache-clang'
  NeoBundle 'Shougo/neocomplete.vim'
  NeoBundle 'Shougo/neosnippet.git'
  NeoBundle 'Shougo/neosnippet-snippets.git'
  NeoBundle 'vim-scripts/snipmate-snippets'
  NeoBundle 'osyo-manga/vim-marching'
  NeoBundle 'osyo-manga/vim-reunions'

  " ファイル管理関係
  NeoBundle 'thinca/vim-template'

  " Unite
  NeoBundle 'Shougo/unite.vim'
  NeoBundle 'tsukkee/unite-help'
  NeoBundle 'Shougo/unite-outline'
  NeoBundle 'Shougo/unite-build'
  NeoBundle 'tsukkee/unite-tag'
  NeoBundle 'Sixeight/unite-grep'
  NeoBundle 'basyura/unite-rails'
  NeoBundle 'thinca/vim-unite-history'
  NeoBundle 'thinca/vim-openbuf'
  NeoBundle 'kshimo69/unite-vim_hacks'
  NeoBundle 'Shougo/vimfiler'
  NeoBundle 'kannokanno/unite-todo'
  NeoBundle 'osyo-manga/unite-quickrun_config'
  NeoBundle 'Shougo/neomru.vim'

  " quickrun
  NeoBundle 'thinca/vim-quickrun'
  NeoBundle 'osyo-manga/quickrun-hook-u-nya-'

  " git
  NeoBundle 'tpope/vim-fugitive'
  NeoBundleLazy "gregsexton/gitv", {
    \ "depends": ["tpope/vim-fugitive"],
    \ "autoload": {
    \   "commands": ["Gitv"],
    \ }}
  NeoBundleLazy 'mattn/gist-vim', {
    \ "depends": ["mattn/webapi-vim"],
    \ "autoload": {
    \   "commands": ["Gist"],
    \ }}
  NeoBundle 'mattn/webapi-vim'
  NeoBundle 'airblade/vim-gitgutter'

  " テキスト編集
  NeoBundle 'tpope/vim-surround'
  NeoBundle 'vim-scripts/Align'
  NeoBundle 'vim-scripts/YankRing.vim'
  NeoBundle 'kana/vim-smartchr'
  NeoBundle 'fuenor/qfixgrep.git'
  NeoBundle 'kana/vim-altr'  " いい感じにファイルを開くやつ
  NeoBundle 't9md/vim-quickhl'  " Highlight on the fly

  " コメント
  NeoBundle 'scrooloose/nerdcommenter'

  " 高機能なUndo
  " 'GundoToggle'が呼ばれるまでロードしない
  NeoBundleLazy 'sjl/gundo.vim', {
    \ "autoload": {"commands": ["GundoToggle"]}}
  " EclipseとかについてるTODOを有効化するやつ
  " '<Plug>TaskList'というマッピングが呼ばれるまでロードしない
  NeoBundleLazy 'vim-scripts/TaskList.vim', {
    \ "autoload": {"mappings": ['<Plug>TaskList']}}

  " Tags
  NeoBundle 'taglist.vim'  " require: http://ctags.sourceforge.net/
  NeoBundle 'wesleyche/SrcExpl'
  NeoBundle 'wesleyche/Trinity'
  NeoBundle 'scrooloose/nerdtree'
  NeoBundle 'gtags.vim'
  NeoBundleLazy 'majutsushi/tagbar', {
    \ "autload": {
    \   "commands": ["TagbarToggle"],
    \ },
    \ "build": {
    \   "mac": "brew install ctags",
    \ }}

  " Syntaxcheck
  NeoBundle 'scrooloose/syntastic.git'
  " NeoBundle "scrooloose/syntastic", {
    " \ "build": {
    " \   "mac": ["pip install flake8", "npm -g install coffeelint"],
    " \   "unix": ["pip install flake8", "npm -g install coffeelint"],
    " \ }}

  " StatusLine
  NeoBundle 'itchyny/lightline.vim'

  " カラー
  " NeoBundle 'desert256.vim'
  NeoBundle 'tomasr/molokai'
  " NeoBundle 'xoria256.vim'
  " NeoBundle 'altercation/vim-colors-solarized'

  " HTMLが開かれるまでロードしない
  NeoBundleLazy 'mattn/zencoding-vim', {
    \ "autoload": {"filetypes": ['html']}}

  " C++
  NeoBundleLazy 'vim-jp/cpp-vim', {
    \ 'autoload' : {'filetypes' : 'cpp'}
    \ }
  NeoBundle 'Mizuchi/STL-Syntax'

  " Python
  NeoBundleLazy 'davidhalter/jedi-vim', {
    \ "autoload": {
    \   "filetypes": ["python", "python3", "djangohtml"],
    \ },
    \ "build": {
    \   "mac": "pip install jedi",
    \   "unix": "pip install jedi",
    \ }}
  NeoBundle 'python_match.vim'
  " http://hashnote.net/2011/12/7/12/
  " NeoBundle 'lambdalisue/vim-python-virtualenv'
  " Djangoを正しくVimで読み込めるようにする
  NeoBundleLazy "lambdalisue/vim-django-support", {
    \ "autoload": {
    \   "filetypes": ["python", "python3", "djangohtml"]
    \ }}
  " Vimで正しくvirtualenvを処理できるようにする
  NeoBundleLazy "jmcantrell/vim-virtualenv", {
    \ "autoload": {
    \   "filetypes": ["python", "python3", "djangohtml"]
    \ }}
  NeoBundle 'mitechie/pyflakes-pathogen'
  NeoBundle 'reinh/vim-makegreen'
  NeoBundle 'lambdalisue/nose.vim'
  " NeoBundle 'peplin/ropevim'
  " $ pythonbrew off    # システムのPythonにインストールする
  " $ sudo pip install rope ropemode ropevim

  " Rails
  " NeoBundle 'tpope/vim-rails'

  " SQL
  NeoBundle 'SQLUtilities'

  " Javascript
  NeoBundle 'JavaScript-syntax'
  NeoBundle 'moll/vim-node'

  " JQuery
  NeoBundle 'jQuery'

  " Markdown
  NeoBundle 'rcmdnk/vim-markdown'

  " Tasks
  NeoBundle 'samsonw/vim-task'

  " Web
  NeoBundle 'mattn/mkdpreview-vim'
  NeoBundle 'tyru/open-browser.vim'

  " ドキュメント
  " NeoBundleLazy "vim-pandoc/vim-pandoc", {
    " \ "autoload": {
    " \   "filetypes": ["text", "pandoc", "markdown", "rst", "textile"],
    " \ }}
  NeoBundle 'Shougo/echodoc.git'
  NeoBundle 'thinca/vim-ref'
  NeoBundle 'taka84u9/vim-ref-ri.git'
  NeoBundle 'thoughtbot/vim-rspec.git'
  NeoBundle 'glidenote/memolist.vim'

  " Project
  NeoBundle 'vtreeexplorer'

  " Calendar
  NeoBundle 'itchyny/calendar.vim'


  " Game
  NeoBundle 'thinca/vim-threes'

  " Brief help
  " :NeoBundleList          - list configured bundles
  " :NeoBundleInstall(!)    - install(update) bundles
  " :NeoBundleClean(!)      - confirm(or auto-approve) removal of unused bundles

  " Installation check.
  NeoBundleCheck
endif

filetype plugin indent on  " Required!
" }}} plugin neobundle

" 基本設定 {{{
" autocmdの初期化 {{{
" release autogroup in MyAutoCmd
augroup MyAutoCmd
  autocmd!
augroup END
" }}} autocmdの初期化

" 検索 {{{
set ignorecase  " 大文字小文字を区別しない
set smartcase   " 検索文字に大文字がある場合は大文字小文字を区別
set incsearch   " インクリメンタルサーチ
set hlsearch    " 検索マッチテキストをハイライト
set wrapscan    " ファイル末尾までいったら最初から検索する

" バックスラッシュやクエスチョンを状況に合わせ自動的にエスケープ
cnoremap <expr> / getcmdtype() == '/' ? '\/' : '/'
cnoremap <expr> ? getcmdtype() == '?' ? '\?' : '?'

" grep
"set grepformat=%f:%l:%m,%f:%l%m,%f\ \ %l%m,%f
"set grepprg=grep\ -nh
" }}} 検索

" 編集 {{{
" タブ幅は4
set tabstop=4 shiftwidth=4 softtabstop=4
" タブはスペース
set expandtab
set smarttab
"set background=dark
"set autoindent
"set smartindent
"set cindent

scriptencoding utf-8

" CTRL-A, CTRL-Xで増減させる時の設定
set nrformats&
set nrformats-=octal  " 0で始まる数字を8進数として扱うか
set nrformats+=hex    " 0xで始まる数字を16進数として扱うか

set ffs=unix,dos,mac  " 想定される開業の種類
"if !(has("win32") || has("win95") || has("win64") || has("win16"))
  set encoding=utf-8  " default encoding
"endif
set fileencodings=utf-8,ucs-bom,iso-2022-jp-3,iso-2022-jp,eucjp-ms,euc-jisx0213,euc-jp,sjis,cp932
nmap ,ee :e ++enc=euc-jp<CR>
nmap ,es :e ++enc=cp932<CR>
nmap ,ej :e ++enc=iso-2022-jp<CR>
nmap ,eu :e ++enc=utf-8<CR>

set shiftround         " '<'や'>'でインデントする際に'shiftwidth'の倍数に丸める
set infercase          " 補完時に大文字小文字を区別しない
"set virtualedit=all    " カーソルを文字が存在しない部分でも動けるようにする
set virtualedit&
set virtualedit+=block
set hidden             " バッファを閉じる代わりに隠す（Undo履歴を残すため）
set switchbuf=useopen  " 新しく開く代わりにすでに開いてあるバッファを開く
set showmatch          " 対応する括弧などをハイライト表示する
"set matchtime=3        " 対応括弧のハイライト表示を3秒にする

" 対応括弧に'<'と'>'のペアを追加
set matchpairs& matchpairs+=<:>

" バックスペースで何でも消せるようにする
set backspace=indent,eol,start

" クリップボードをデフォルトのレジスタとして指定。後にYankRingを使うので
" 'unnamedplus'が存在しているかどうかで設定を分ける必要がある
if has('unnamedplus')
  set clipboard& clipboard+=unnamedplus,unnamed
else
  set clipboard& clipboard+=unnamed
endif

" Swapファイル？Backupファイル？前時代的すぎ
" なので全て無効化する
set nowritebackup
set nobackup
set noswapfile
set undodir=$HOME/.vim/undo

" 上書きされたファイルを自動的に読み込む
set autoread
" }}} 編集

" 表示 {{{
set list            " 不可視文字の可視化
set number          " 行番号の表示
set wrap            " 長いテキストの折り返し
set textwidth=0     " 自動的に改行が入るのを無効化
" set colorcolumn=80  " その代わり80文字目にラインを入れる
set modeline        " モードラインを表示
set showcmd         " 入力したコマンドを表示
set wildmenu        " コマンドライン補完
set wildchar=<tab>  " TABで補完
set wildmode=longest:full,full
set history=1000    " コマンドヒストリー1000件
set foldmethod=indent  " fold with indent
"set foldlevel=0  " all level folding
"set foldlevel=99  " all level not folding when open buffer
"set foldcolumn=4  " show folding line

" 新しいウィンドウを下に開く
set splitbelow
" 新しいウィンドウを右に開く
set splitright
set diffopt+=vertical  " 画面は縦分割

" 前時代的スクリーンベルを無効化
set t_vb=
"set novisualbell
set vb

" デフォルト不可視文字は美しくないのでUnicodeで綺麗に
if !(has("win32") || has("win95") || has("win64") || has("win16"))
  "set listchars=eol:$,tab:>-,trail:_
  "set listchars=eol:¬,tab:▸.
  "set listchars=tab:▸␣,trail:␣
  "set listchars=tab:»-,trail:-,extends:»,precedes:«,nbsp:%,eol:↲
  "set listchars=tab:»␣,trail:␣,extends:»,precedes:«,nbsp:%,eol:↲
  set listchars=tab:»␣,trail:␣,extends:»,precedes:«,nbsp:%,eol:\ 
else
  set listchars=eol:$,tab:>-,trail:_
endif

" 日本語入力をリセットする
au BufNewFile,BufRead * set iminsert=0
" モード変更時にIMEをoffにする設定
"set imdisable
" タブ幅のリセット
"au BufNewFile,BufRead * set tabstop=4 shiftwidth=4 softtabstop=4
" 全角スペースの表示
highlight ZenkakuSpace cterm=underline ctermfg=lightblue guibg=#666666
au BufNewFile,BufRead * match ZenkakuSpace /　/
" }}} 表示

" 色、GUI {{{
" ターミナルでも256色
set t_Co=256

if has('gui_running')
  " sidebar
  set guioptions& " initialize
  set guioptions-=m  "メニューバー
  set guioptions-=T  "ツールバー
  set guioptions-=r  "スクロールバー
  set guioptions-=R  "スクロールバー
  set guioptions-=l  "スクロールバー
  set guioptions-=L  "スクロールバー
  set guioptions-=b  "スクロールバー
  " clip board
  " http://vim-users.jp/2010/02/hack126/
  set guioptions+=a
  "set guifont=M+2VM+IPAG\ circle\ 14
  "set guifont=VL\ Gothic\ 14
  " window size
  "set lines=30
  "set columns=120
  gui
  if has('gui_macvim')
    set guioptions& " initialize
    set guioptions-=T
    set guioptions+=a
    set imdisable
    set antialias
    colorscheme macvim
    "set guifont=M+2VM+IPAG\ circle\ Regular:h14
    "set guifont=Monaco:h14
    "set guifont=Ricty\ Regular:h16
    set guifont=Ricty\ Discord\ Regular\ for\ Powerline:h14
    set transparency=20
    "set lines=40
    "set columns=120
    set fuoptions=maxvert,maxhorz
    " http://code.google.com/p/macvim-kaoriya/wiki/Readme
    " Lionのフルスクリーンじゃなくて従来のフルスクリーンを使う
    " % defaults write org.vim.MacVim MMNativeFullScreen 0
    set fullscreen
    "au GUIEnter * set fullscreen
  else
    set transparency=210
  endif
endif

" Statusline {{{
" set laststatus=2
" set statusline=%n\:%y%F\ %(\[%{GitBranch()}\]\ %)\|%{(&fenc!=''?&fenc:&enc).'\|'.&ff.'\|'}ascii\:\%03.3b\|hex\:\%02.2B\|%m%r%=<%v\:%l/%L:%p%%>
" highlight StatusLine term=NONE cterm=NONE ctermfg=black ctermbg=white
"highlight StatusLine gui=BOLD guifg=Black guibg=LightYellow
" Change status line's color when into insert mode
" augroup InsertHook
  " autocmd!
  " autocmd InsertEnter * highlight StatusLine guifg=White guibg=DarkCyan
  " autocmd InsertLeave * highlight StatusLine guifg=Black guibg=LightYellow
" augroup END
" }}} Statusline

augroup highlightIdegraphicSpace
  autocmd!
  autocmd ColorScheme * highlight IdeographicSpace term=underline ctermbg=Red guibg=Red
  autocmd VimEnter,WinEnter * match IdeographicSpace /　/
augroup END
syntax on

let g:solarized_termcolors=256
let g:solarized_termtrans=1
set background=dark
" colorscheme solarized
" colorscheme desert256
colorscheme molokai

" ターミナルの透過がそのまま見えるように
highlight Normal ctermbg=NONE
highlight NonText ctermbg=NONE

" highlight Folded ctermbg=grey ctermfg=blue guibg=grey guifg=blue
" highlight FoldColumn ctermfg=green guifg=green
set title
set ruler
set cursorline
" set cursorcolumn
" display line on current buffer
augroup cch
  autocmd! cch
  autocmd WinLeave * set nocursorline
  autocmd WinEnter,BufRead * set cursorline
augroup END
"hi clear CursorLine
hi CursorLine gui=underline term=underline cterm=underline
" hi CursorLine term=reverse cterm=reverse
"highlight CursorLine ctermbg=blue guibg=grey20
hi Visual term=reverse cterm=reverse
" Change cursor color when IME on/off
if has('multi_byte_ime') || has('xim')
  highlight CursorIM guibg=LightRed guifg=NONE
endif

" カーソルの変更
" コマンドモードは長方形、入力モードは点滅下線
let &t_ti .= "\e[3 q"
let &t_SI .= "\e[3 q"
let &t_EI .= "\e[1 q"
let &t_te .= "\e[1 q"
" }}} 色、GUI

" マクロ、キー設定 {{{
let mapleader = " "  "<Leader>をスペースに

" 入力モード中に素早くjjと入力した場合はESCとみなす
inoremap jj <Esc>

" ESCを二回押すことでハイライトを消す
nmap <silent> <Esc><Esc> :nohlsearch<CR>

" カーソル下の単語を * で検索
vnoremap <silent> * "vy/\V<C-r>=substitute(escape(@v, '\/'), "\n", '\\n', 'g')<CR><CR>

" カーソル下のキーワードでヘルプ
nnoremap <C-h> :<C-u>help<Space><C-r><C-w><Enter>

" 検索後にジャンプした際に検索単語を画面中央に持ってくる {{{
nnoremap n nzz
nnoremap N Nzz
nnoremap * *zz
nnoremap # #zz
nnoremap g* g*zz
nnoremap g# g#zz
nnoremap G Gzz
" }}}

nnoremap Y y$
nnoremap ¥ \
inoremap ¥ \

" j, k による移動を折り返されたテキストでも自然に振る舞うように変更 {{{
nnoremap j gj
nnoremap k gk
" }}}

" vを二回で行末まで選択
vnoremap v $h

" TABで対応ペアにジャンプ {{{
nnoremap <Tab> %
vnoremap <Tab> %
" }}}

" バッファ分割 {{{
" nmap <silent> <C-x><C-n> :bnext<CR>
" nmap <silent> <C-x><C-p> :bprevious<CR>
" nmap <silent> <C-x><C-l> :buffers<CR>
" split
nmap spj <SID>(split-to-j)
nmap spk <SID>(split-to-k)
nmap sph <SID>(split-to-h)
nmap spl <SID>(split-to-l)
nnoremap <SID>(split-to-j) : <C-u>belowright split<CR>
nnoremap <SID>(split-to-k) : <C-u>aboveleft split<CR>
nnoremap <SID>(split-to-h) : <C-u>topleft vsplit<CR>
nnoremap <SID>(split-to-l) : <C-u>botright vsplit<CR>
" }}} バッファ分割

" Ctrl + hjkl でウィンドウ間を移動 {{{
"nnoremap <C-h> <C-w>h
"nnoremap <C-j> <C-w>j
"nnoremap <C-k> <C-w>k
"nnoremap <C-l> <C-w>l
" scroll top window with key Ctrl+Shift+J
nnoremap <C-S-J> <C-W>k<C-E><C-W><C-W>
" }}}

" Shift + 矢印でウィンドウサイズを変更 {{{
nnoremap <S-Left>  <C-w><<CR>
nnoremap <S-Right> <C-w>><CR>
nnoremap <S-Up>    <C-w>-<CR>
nnoremap <S-Down>  <C-w>+<CR>
" }}}

" T + ? で各種設定をトグル {{{
nnoremap [toggle] <Nop>
nmap T [toggle]
nnoremap <silent> [toggle]s :setl spell!<CR>:setl spell?<CR>
nnoremap <silent> [toggle]l :setl list!<CR>:setl list?<CR>
nnoremap <silent> [toggle]t :setl expandtab!<CR>:setl expandtab?<CR>
nnoremap <silent> [toggle]w :setl wrap!<CR>:setl wrap?<CR>
" }}}

" w!! でスーパーユーザーとして保存（sudoが使える環境限定）
cmap w!! w !sudo tee > /dev/null %

" 存在しないフォルダを自動で作る {{{
" http://hashnote.net/2011/12/7/12/
augroup vimrc-auto-mkdir
  autocmd!
  autocmd BufWritePre * call s:auto_mkdir(expand('<afile>:p:h'), v:cmdbang)
  function! s:auto_mkdir(dir, force)
    if !isdirectory(a:dir) && (a:force ||
      \ input(printf('"%s" does not exist. Create? [y/N]', a:dir)) =~? '^y\%[es]$')
      call mkdir(iconv(a:dir, &encoding, &termencoding), 'p')
    endif
  endfunction
augroup END
" }}}

" vim 起動時のみカレントディレクトリを開いたファイルの親ディレクトリに指定 {{{
" autocmd MyAutoCmd VimEnter * call s:ChangeCurrentDir('', '')
"function! s:ChangeCurrentDir(directory, bang)
"  if a:directory == ''
"    lcd %:p:h
"  else
"    execute 'lcd' . a:directory
"  endif
"
"  if a:bang == ''
"    pwd
"  endif
"endfunction
" }}}
command! -nargs=0 CdCurrent cd %:p:h

" ~/.vimrc.localが存在する場合のみ設定を読み込む {{{
let s:local_vimrc = expand('~/.vimrc.local')
if filereadable(s:local_vimrc)
  execute 'source ' . s:local_vimrc
endif
" }}}

" Remember cursor potition {{{
autocmd BufReadPost *
  \ if line("'\"") > 0 && line ("'\"") <= line("$") |
  \     exe "normal! g'\"" |
  \ let b:posBufReadPost = getpos('.') |
  \ endif
autocmd BufWinEnter *
  \ if exists('b:posBufReadPost') |
  \     if b:posBufReadPost == getpos('.') |
  \     execute 'normal! zvzz' |
  \     endif |
  \ unlet b:posBufReadPost |
  \ endif
" }}} Remember cursor potition
" }}} マクロ、キー設定

" QuickFix {{{
" make, grep などのコマンド後に自動的にQuickFixを開く
autocmd MyAutoCmd QuickfixCmdPost make,grep,grepadd,vimgrep copen
" (l以外で始まる)QuickFixコマンドの実行が終わったらQuickFixウインドウを開く
"autocmd QuickFixCmdPost [^l]* copen

" QuickFixおよびHelpでは q でバッファを閉じる
autocmd MyAutoCmd FileType help,qf nnoremap <buffer> q <C-w>c

" QuickFixのウインドウだけになったら閉じる
augroup QfAutoCommands
  autocmd!
  autocmd WinEnter * if (winnr('$') == 1) && (getbufvar(winbufnr(0), '&buftype')) == 'quickfix' | quit | endif
augroup END
" }}} QuickFix

" Cscope {{{
if has("cscope") && filereadable("/usr/local/bin/cscope")
  set csprg=/usr/loca/bin/cscope
  set csto=0
  set cst
  set nocsverb
  " add any database in current directory
  if filereadable("cscope.out")
    cs add cscope.out
    " else add database pointed to by environment
  elseif $CSCOPE_DB != ""
    cs add $CSCOPE_DB
  endif
  set csverb
  " set cscopequickfix=s-,c-,d-,i-,t-,e-
endif
" }}} Cscope

" タブ {{{
nnoremap <silent> tt :<c-u>VimFilerTab<cr>
nnoremap <silent> tf :<c-u>tabfirst<cr>
nnoremap <silent> tl :<c-u>tablast<cr>
nnoremap <silent> tn :<c-u>tabnext<cr>
nnoremap <silent> tN :<c-u>tabNext<cr>
nnoremap <silent> tp :<c-u>tabprevious<cr>
nnoremap <silent> te :<c-u>tabedit<cr>
nnoremap <silent> tc :<c-u>tablast <bar> tabnew<cr>
nnoremap <silent> tx :<c-u>tabclose<cr>
nnoremap <silent> to :<c-u>tabonly<cr>
nnoremap <silent> ts :<c-u>tabs<cr>
" http://doruby.kbmj.com/aisi/20091218/Vim__
" 個別のタブの表示設定をします
function! GuiTabLabel()
  " タブで表示する文字列の初期化をします
  let l:label = ''
  " タブに含まれるバッファ(ウィンドウ)についての情報をとっておきます。
  let l:bufnrlist = tabpagebuflist(v:lnum)
  " 表示文字列にバッファ名を追加します
  " パスを全部表示させると長いのでファイル名だけを使います 詳しくは help fnamemodify()
  let l:bufname = fnamemodify(bufname(l:bufnrlist[tabpagewinnr(v:lnum) - 1]), ':t')
  " バッファ名がなければ No title としておきます。ここではマルチバイト文字を使わないほうが無難です
  let l:label .= l:bufname == '' ? 'No title' : l:bufname
  " タブ内にウィンドウが複数あるときにはその数を追加します(デフォルトで一応あるので)
  let l:wincount = tabpagewinnr(v:lnum, '$')
  if l:wincount > 1
    let l:label .= '[' . l:wincount . ']'
  endif
  " このタブページに変更のあるバッファがるときには '[+]' を追加します(デフォルトで一応あるので)
  for bufnr in l:bufnrlist
    if getbufvar(bufnr, "&modified")
      let l:label .= '[+]'
      break
    endif
  endfor

  " 表示文字列を返します
  return l:label
endfunction

" guitablabel に上の関数を設定します
" その表示の前に %N というところでタブ番号を表示させています
set guitablabel=%N:\ %{GuiTabLabel()}
" }}} タブ

" }}} 基本設定

" Allargs {{{
" http://d.hatena.ne.jp/secondlife/20060203/1138978661
" ex)
" :Allargs %s/foo/bar/ge|update
" 使う時。foo を bar に置換しまくる。
" :Allargs %s/foo/bar/ge|update
" eオプションをつけないと foo が無いというメッセージがのんびり表示されて、いつま
" でたっても置換が終わらないので気をつけよう(それに気づかずに密かにハマった)
" コマンドは | で連続で実行できて、update は変更のあったバッファだけを保存。と。
" カレントの *.cpp を置換する場合は予め、
" :ar *.cpp
" ってやっとくと全部読み込まれる。
" 確認するには
" :ar
function! Allargs(command)
  let i = 0
  while i < argc()
    if filereadable(argv(i))
      execute "e " . argv(i)
      execute a:command
    endif
    let i = i + 1
  endwhile
endfunction
command! -nargs=+ -complete=command Allargs call Allargs(<q-args>)
" }}} Allargs

" AllMaps {{{
" http://vim-users.jp/2011/02/hack203/
" 全てのマッピングを表示
" :AllMaps
" 現在のバッファで定義されたマッピングのみ表示
" :AllMaps <buffer>
" どのスクリプトで定義されたかの情報も含め表示
" :verbose AllMaps <buffer>
command!
\   -nargs=* -complete=mapping
\   AllMaps
\   map <args> | map! <args> | lmap <args>
" }}} AllMaps

" Open junk file {{{
" http://vim-users.jp/2010/11/hack181/
command! -nargs=0 JunkFile call s:open_junk_file()
function! s:open_junk_file()
  "let l:junk_dir = $HOME . '/junk'. strftime('/%Y/%m')
  let l:junk_dir = $HOME . '/junk'
  if !isdirectory(l:junk_dir)
    call mkdir(l:junk_dir, 'p')
  endif

  let l:filename = input('Junk Code: ', l:junk_dir.strftime('/%Y%m%d-%H%M%S.'))
  if l:filename != ''
    execute 'edit ' . l:filename
  endif
endfunction
" }}} Open junk file

" Change current directory {{{
command! -nargs=? -complete=dir -bang CD  call s:ChangeCurrentDir('<args>', '<bang>')
function! s:ChangeCurrentDir(directory, bang)
  if a:directory == ''
    lcd %:p:h
  else
    execute 'lcd' . a:directory
  endif

  if a:bang == ''
    pwd
  endif
endfunction

nnoremap <silent> <Space>cd :<C-u>CD<CR>
" }}} Change current directory

" :mesをクリップボードにコピー {{{
command! MessCopy call s:messcopy()
function! s:messcopy()
  redir @+>
  silent messages
  redir END
  " Copy to selection too.
  call setreg('*', getreg('+', 1), getregtype('+'))
endfunction
" }}} :mesをクリップボードにコピー

" ==== Programming ==== {{{

" ChangeLog {{{
let g:changelog_username = "Kimihiko Shimomura  <kshimo69@gmail.com>"
let g:changelog_dateformat = "%Y-%m-%d (%a)"
"nmap <C-m> :e ~/Dropbox/ChangeLog.txt<CR>
" }}} ChangeLog

" Ctags {{{
" タグファイルはカレントディレクトリから上向きに検索
set tags=./tags;
" grepは再帰、行番号表示、バイナリファイルは見ない、ファイル名表示
" .hgと.git、tagsは対象外
set grepprg=grep\ -rnIH\ --color\ --exclude=\.hg\ --exclude=\.git\ --exclude=\.svn\ --exclude=tags\ --exclude=GTAGS
" }}} Ctags

" IncludePATH {{{
" PATHにインクルードディレクトリを設定する
let $DEFAULT_INCLUDE_DIR = "/usr/include,/usr/local/include"
set path+=$DEFAULT_INCLUDE_DIR
" }}} IncludePATH

" matchit {{{
source $VIMRUNTIME/macros/matchit.vim
let b:match_words = &matchpairs . '\<if\>:\<fi\>,\<if\>:\<else\>,\<if\>:\<elif\>,\<begin\>:\<end\>'
" }}} matchit

" cpp {{{
function s:cpp()
  " インクルードパスの設定
  if (has("win32") || has("win95") || has("win64") || has("win16"))
    setlocal path+=D:/home/project/center
  endif
endfunction
augroup vimrc-cpp
  autocmd!
  autocmd FileType cpp call s:cpp()
augroup END

augroup cpp-namespace
  autocmd!
  autocmd FileType cpp inoremap <buffer><expr>; <SID>expand_namespace()
augroup END
function! s:expand_namespace()
  let s = getline('.')[0:col('.')-1]
  if s =~# '\<b;$'
    return "\<BS>oost::"
  elseif s =~# '\<s;$'
    return "\<BS>td::"
  elseif s =~# '\<d;$'
    return "\<BS>etail::"
  elseif s =~# '\<n;$'
    return "\<BS>n::"
  elseif s =~# '\<e;$'
    return "\<BS><BS>nex::"
  else
    return ';'
  endif
endfunction
" }}} cpp

" }}} ==== Programming ====

" ==== Plugins ==== {{{

" plugin vimshell {{{
command! VS :VimShell
" }}} plugin vimshel

" plugin neocomplete {{{
"Note: This option must set it in .vimrc(_vimrc).  NOT IN .gvimrc(_gvimrc)!
" Disable AutoComplPop.
let g:acp_enableAtStartup = 0
" Use neocomplete.
let g:neocomplete#enable_at_startup = 1
" Use smartcase.
let g:neocomplete#enable_smart_case = 1
" Set minimum syntax keyword length.
let g:neocomplete#sources#syntax#min_keyword_length = 3
let g:neocomplete#lock_buffer_name_pattern = '\*ku\*'

" Enable heavy features.
" Use camel case completion.
let g:neocomplete#enable_camel_case_completion = 1
" Use underbar completion.
let g:neocomplete#enable_underbar_completion = 1
" 補完に時間がかかってもスキップしない
" let g:neocomplete#skip_auto_completion_time = ""

" Define dictionary.
let g:neocomplete#sources#dictionary#dictionaries = {
  \ 'default' : '',
  \ 'vimshell' : $HOME.'/.vimshell_hist',
  \ 'scheme' : $HOME.'/.gosh_completions'
  \ }

" Define keyword.
if !exists('g:neocomplete#keyword_patterns')
  let g:neocomplete#keyword_patterns = {}
endif
let g:neocomplete#keyword_patterns['default'] = '\h\w*'

" 補完に辞書ファイルを追加
set complete+=k

" Plugin key-mappings.
inoremap <expr><C-g>     neocomplete#undo_completion()
inoremap <expr><C-l>     neocomplete#complete_common_string()
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)
xmap <C-k>     <Plug>(neosnippet_expand_target)

" スニペットを展開するキーマッピング
" <Tab> で選択されているスニペットの展開を行う
" 選択されている候補がスニペットであれば展開し、
" それ以外であれば次の候補を選択する
" また、既にスニペットが展開されている場合は次のマークへと移動する
imap <expr><Tab> neosnippet#expandable_or_jumpable() ?
  \ "\<Plug>(neosnippet_expand_or_jump)"
  \: pumvisible() ? "\<C-n>" : "\<TAB>"
smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
  \ "\<Plug>(neosnippet_expand_or_jump)"
  \: "\<TAB>"

" 現在の filetype のスニペットを編集する為のキーマッピング
" こうしておくことでサッと編集や追加などを行うことができる
" 以下の設定では新しいタブでスニペットファイルを開く
nnoremap <Space>ns :execute "tabnew\|:NeoSnippetEdit ".&filetype<CR>

" スニペットファイルの保存ディレクトリを設定
let g:neosnippet#snippets_directory = "~/.vim/neosnippet"

" For snippet_complete marker.
" if has('conceal')
  " set conceallevel=2 concealcursor=i
" endif

" snipmate-snnipets
let g:neosnippet#enable_snipmate_compatibility = 1
let g:neosnippet#snippets_directory = s:bundle_root . '/snipmate-snippets/snippets'

" Recommended key-mappings.
" <CR>: close popup and save indent.
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
  return neocomplete#close_popup() . "\<CR>"
  " For no inserting <CR> key.
  "return pumvisible() ? neocomplete#close_popup() : "\<CR>"
endfunction
" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"
inoremap <expr><C-y>  neocomplete#close_popup()
inoremap <expr><C-e>  neocomplete#cancel_popup()

" Enable omni completion.
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

" Enable heavy omni completion.
if !exists('g:neocomplete#sources#omni#input_patterns')
  let g:neocomplete#sources#omni#input_patterns = {}
endif
let g:neocomplete#sources#omni#input_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
let g:neocomplete#sources#omni#input_patterns.c = '[^.[:digit:] *\t]\%(\.\|->\)'
let g:neocomplete#sources#omni#input_patterns.cpp = '[^.[:digit:] *\t]\%(\.\|->\)\|\h\w*::'
" let g:neocomplete#sources#omni#input_patterns.ruby = '[^. *\t]\.\w*\|\h\w*::'
"autocmd FileType ruby setlocal omnifunc=rubycomplete#Complete

" For perlomni.vim setting.
" https://github.com/c9s/perlomni.vim
let g:neocomplete#sources#omni#input_patterns.perl = '\h\w*->\h\w*\|\h\w*::'

" 補完時にウインドウを表示しない
" set completeopt=menuone

" }}} plugin neocomplete

" plugin vim-marching {{{

if !(has("win32") || has("win95") || has("win64") || has("win16"))
  " clang コマンドの設定
  let g:marching_clang_command = "/usr/bin/clang"
  " インクルードディレクトリのパスを設定
  let g:marching_include_paths = [
    \   "/usr/include,/usr/local/include"
    \]
else
  " clang コマンドの設定
  let g:marching_clang_command = "C:/clang.exe"
  " インクルードディレクトリのパスを設定
  let g:marching_include_paths = [
    \   "C:/MinGW/lib/gcc/mingw32/4.6.2/include/c++"
    \   "C:/cpp/boost"
    \]
endif

" オプションを追加する場合
" let g:marching_clang_command_option="-std=c++1y"

" neocomplete.vim と併用して使用する場合
let g:marching_enable_neocomplete = 1

if !exists('g:neocomplete#force_omni_input_patterns')
  let g:neocomplete#force_omni_input_patterns = {}
endif

let g:neocomplete#force_omni_input_patterns.cpp =
  \ '[^.[:digit:] *\t]\%(\.\|->\)\w*\|\h\w*::\w*'

" 処理のタイミングを制御する
" 短いほうがより早く補完ウィンドウが表示される
set updatetime=200

" オムニ補完時に補完ワードを挿入したくない場合
imap <buffer> <C-x><C-o> <Plug>(marching_start_omni_complete)

" キャッシュを削除してからオムに補完を行う
imap <buffer> <C-x><C-x><C-o> <Plug>(marching_force_start_omni_complete)

" 非同期ではなくて、同期処理でコード補完を行う場合
" let g:marching_backend = "sync_clang_command"
" }}} plugin vim-marching

" plugin vim-template {{{
" テンプレート中に含まれる特定文字列を置き換える
autocmd MyAutoCmd User plugin-template-loaded call s:template_keywords()
function! s:template_keywords()
  silent! %s/<+DATE+>/\=strftime('%Y-%m-%d')/g
  silent! %s/<+FILENAME+>/\=expand('%:r')/g
endfunction
" テンプレート中に含まれる'<+CURSOR+>'にカーソルを移動
autocmd MyAutoCmd User plugin-template-loaded
  \   if search('<+CURSOR+>')
  \ |   silent! execute 'normal! "_da>'
  \ | endif
" }}} plugin vim-template

" plugin unite {{{
" http://d.hatena.ne.jp/ruedap/20110110/vim_unite_plugin
" http://d.hatena.ne.jp/Voluntas/20110823/1314031095
" 入力モードで開始する
let g:unite_enable_start_insert=1
" uniteのウインドウの高さ
let g:unite_winheight=15
" use vimfiler to open directory
call unite#custom_default_action("source/bookmark/directory", "vimfiler")
call unite#custom_default_action("directory", "vimfiler")
call unite#custom_default_action("directory_mru", "vimfiler")
"file_mruの表示フォーマットを指定。空にすると表示スピードが高速化される
let g:unite_source_file_mru_filename_format = ''
" カーソル行の色
let g:unite_cursor_line_highlight="CursorLine"
" バッファ一覧
nnoremap <silent> ,ub :<C-u>Unite buffer -direction=botright -auto-resize -toggle<CR>
" ファイル一覧
nnoremap <silent> ,uf :<C-u>UniteWithBufferDir -buffer-name=files file -direction=botright -auto-resize -toggle<CR>
" レジスタ一覧
nnoremap <silent> ,ur :<C-u>Unite register -buffer-name=register -direction=botright -auto-resize -toggle<CR>
" 最近使用したファイル一覧
nnoremap <silent> ,um :<C-u>Unite file_mru -direction=botright -auto-resize -toggle<CR>
" ブックマーク
nnoremap <silent> ,uc :<C-u>Unite bookmark -direction=botright -auto-resize -toggle<CR>
" ブックマークに追加
nnoremap <silent> ,ua :<C-u>UniteBookmarkAdd<CR>
" 常用セット
nnoremap <silent> ,uu :<C-u>Unite buffer file_mru file file/new -direction=botright -auto-preview -toggle<CR>
nnoremap <silent> ;; :<C-u>Unite buffer file_mru file file/new -direction=botright -auto-resize -toggle<CR>
" 全部乗せ
nnoremap <silent> ,uz :<C-u>UniteWithBufferDir -buffer-name=files buffer file_mru bookmark file file/new -direction=botright -auto-resize -toggle<CR>
" outline
" nnoremap <silent> ,uo :<C-u>Unite outline -buffer-name=outline -direction=topleft -auto-preview -auto-resize<CR>
nnoremap <silent> ,uo :<C-u>Unite outline -buffer-name=outline -direction=topleft<CR>
" tab
" nnoremap <silent> ,ut :<C-u>Unite tab -buffer-name=tab -direction=botright -auto-preview -auto-resize<CR>
" tag
nnoremap <silent> ,ut :<C-u>Unite tag -buffer-name=tag -direction=botright -auto-preview -auto-resize<CR>
" C-] の代わりに unite-tag を使う設定
" autocmd BufEnter *
      " \   if empty(&buftype)
      " \|    nnoremap <buffer> <C-]> :<C-u>UniteWithCursorWord -immidiately tag<CR>
      " \|  endif
" window
nnoremap <silent> ,uw :<C-u>Unite window -buffer-name=window -direction=botright -auto-preview -auto-resize<CR>
" snippets
imap <C-s> <Plug>(neocomplcache_start_unite_snippet)
" unite-todo
let g:unite_todo_note_suffix = 'md'
nnoremap <silent> ,ui :<C-u>UniteTodoAddSimple -tag -memo<CR>
nnoremap <silent> ,ul :<C-u>Unite todo:undone<CR>

" ウィンドウを分割して開く
au FileType unite nnoremap <silent> <buffer> <expr> <C-j> unite#do_action('split')
au FileType unite inoremap <silent> <buffer> <expr> <C-j> unite#do_action('split')
" ウィンドウを縦に分割して開く
au FileType unite nnoremap <silent> <buffer> <expr> <C-l> unite#do_action('vsplit')
au FileType unite inoremap <silent> <buffer> <expr> <C-l> unite#do_action('vsplit')

" http://ujihisa.blogspot.com/2010/12/investing-methods-of-object-on-unite.html
" Unite evalruby
let s:unite_source = {
  \ 'name': 'evalruby',
  \ 'is_volatile': 1,
  \ 'required_pattern_length': 1,
  \ 'max_candidates': 30,
  \ }
function! s:unite_source.gather_candidates(args, context)
  if a:context.input[-1:] == '.'
    let methods = split(
      \ unite#util#system(printf('ruby -e "puts %s.methods"', a:context.input[:-2])),
      \ "\n")
    call map(methods, printf("'%s' . v:val", a:context.input))
  else
    let methods = [a:context.input]
  endif
  return map(methods, '{
    \ "word": v:val,
    \ "source": "evalruby",
    \ "kind": "command",
    \ "action__command": printf("!ruby -e \"p %s\"", v:val),
    \ }')
endfunction
call unite#define_source(s:unite_source)

" http://d.hatena.ne.jp/ruedap/20110117/vim_unite_plugin_1_week
" unite.vim上でのキーマッピング
autocmd FileType unite call s:unite_my_settings()
function! s:unite_my_settings()
  " 単語単位からパス単位で削除するように変更
  "imap <buffer> <C-w> <Plug>(unite_delete_backward_path)
  " ESCキーを2回押すと終了する
  nmap <silent><buffer> <ESC><ESC> q
  imap <silent><buffer> <ESC><ESC> <ESC>q
  "入力モードのときjjでノーマルモードに移動
  imap <buffer> jj <Plug>(unite_insert_leave)
endfunction
" }}} plugin unite

" plugin vimfiler {{{
nnoremap <Leader>e :VimFilerExplorer<CR>
" 現在開いているバッファのディレクトリを開く
nnoremap <Leader>f :VimFilerBufferDir<CR>
" vimfilerだけになったら閉じる
" autocmd MyAutoCmd BufEnter * if (winnr('$') == 1 && &filetype ==# 'vimfiler') | q | endif
let g:vimfiler_as_default_explorer = 1
let g:vimfiler_enable_auto_cd = 1
" }}} plugin vimfiler

" plugin quickrun {{{
let g:quickrun_config = {}
" vimprocで実行する
let g:quickrun_config = {
  \ "*": {"runner": "remote/vimproc"},
  \ }
let g:quickrun_config = {
  \ "cpp/g++-preprocessor" : {
  \   "exec"    : "%c %o %s:p",
  \   "command" : "g++",
  \   "cmdopt"  : " -P -E -std=gnu++0x",
  \ },
  \}

" RSpec
let g:quickrun_config['ruby.rspec'] = {'command': 'spec'}
"let g:quickrun_config['ruby.rspec'] = {'command': "spec -l {line('.')}"}
augroup UjihisaRSpec
  autocmd!
  autocmd BufWinEnter,BufNewFile *_spec.rb set filetype=ruby.rspec
augroup END

" rst2html
let g:quickrun_config['rst'] = {
  \ 'command': 'rst2html',
  \ 'outputter': 'browser',
  \ }

" markdown
let g:quickrun_config['markdown'] = {
  \ 'type': 'markdown/pandoc',
  \ 'cmdopt': '-s ',
  \ 'outputter': 'browser',
  \ }

" u-nya-
" 使用しない場合は 0 にする
let g:quickrun_config._ = {
  \ "hook/u_nya_/enable" : 1,
\ }

" }}} plugin quickrun

" plugin vim-fugitive {{{
"nnoremap <Space>gd :<C-u>Gdiff<Enter>
"nnoremap <Space>gs :<C-u>Gstatus<Enter>
"nnoremap <Space>gl :<C-u>Glog<Enter>
"nnoremap <Space>ga :<C-u>Gwrite<Enter>
"nnoremap <Space>gc :<C-u>Gcommit<Enter>
"nnoremap <Space>gC :<C-u>Git commit --amend<Enter>
"nnoremap <Space>gb :<C-u>Gblame<Enter>
" }}} plugin vim-fugitive

" plugin gitv {{{
augroup gitvConfig
  autocmd!
  autocmd FileType gitv call s:my_gitv_settings()
  autocmd FileType git setlocal nofoldenable foldlevel=0
augroup END
" 現在のカーソル行の SHA1 ハッシュを取得
function! s:gitv_get_current_hash()
  return matchstr(getline('.'), '\[\zs.\{7\}\ze\]$')
endfunction
" 後で折りたたむ準備
function! s:toggle_git_folding()
  if &filetype ==# 'git'
    setlocal foldenable!
  endif
endfunction
function! s:my_gitv_settings()
  " 現在のカーソル位置にあるブランチ名を取得してログ上でブランチに checkout する
  setlocal iskeyword+=/,-,.
  nnoremap <silent><buffer> C :<C-u>Git checkout <C-r><C-w><CR>
  " ハッシュを使用したいろんなコマンド
  nnoremap <buffer> <Space>rb :<C-u>Git rebase <C-r>=gitv_get_current_hash()<CR><Space>
  nnoremap <buffer> <Space>R :<C-u>Git revert <C-r>=gitv_get_current_hash()<CR><CR>
  nnoremap <buffer> <Space>h :<C-u>Git cherry-pick <C-r>=gitv_get_current_hash()<CR><CR>
  nnoremap <buffer> <Space>rh :<C-u>Git reset --hard <C-r>=gitv_get_current_hash()<CR>
  " diffの折りたたみをトグル
  nnoremap <silent><buffer> t :<C-u>windo call <SID>toggle_git_folding()<CR>1<C-w>w
endfunction
" }}} plugin gitv

" plugin Align {{{
let g:Align_xstrlen = 3
let g:DrChipTopLvlMenu = 'Align'
" }}} plugin Align

" plugin yankring {{{
let g:yankring_manual_clipboard_check = 0
let g:yankring_history_dir = expand('$HOME')
let g:yankring_history_file = '.yankring_history'
nnoremap <silent> cy  ce<C-r>0<ESC>:let@/=@1<CR>:noh<CR>
vnoremap <silent> cy  c<C-r>0<ESC>:let@/=@1<CR>:noh<CR>
nnoremap <silent> ciy ciw<C-r>0<ESC>:let@/=@1<CR>:noh<CR>
" }}} plugin yankring

" plugin nerdcommenter {{{
let NERDSpaceDelims = 1
let NERDDefaultNesting = 0
" ,,でコメントをトグルする
nmap ,, <Plug>NERDCommenterToggle
vmap ,, <Plug>NERDCommenterToggle
" }}} plugin nerdcommenter

" plugin smartchr {{{
" inoremap <expr> = smartchr#loop('=', ' = ', ' == ')
" inoremap <expr> , smartchr#one_of(',', ', ')
" cnoremap <expr> / smartchr#loop('/', '~/', '//', {'ctype': ':'}
autocmd FileType c,cpp inoremap <buffer> <expr> . smartchr#loop('.', '->', '..')
" }}} plugin smartchr

" plugin vim-altr {{{
nmap <F2> <Plug>(altr-forward)
" 設定を追加する場合のサンプル
" call altr#define('autoload/%.vim', 'doc/%.txt', 'plugin/%.vim')
" }}} pluginvim-altr

" plugin vim-quickhl {{{
nmap <Leader><Space> <Plug>(quickhl-manual-this)
xmap <Leader><Space> <Plug>(quickhl-manual-this)
nmap <Leader>M <Plug>(quickhl-manual-reset)
xmap <Leader>M <Plug>(quickhl-manual-reset)

nmap <Leader>j <Plug>(quickhl-cword-toggle)
nmap <Leader>] <Plug>(quickhl-tag-toggle)
" map H <Plug>(operator-quickhl-manual-this-motion)
" }}} pluginvim-quickhl

" plugin Gundo {{{
nnoremap <Leader>u :GundoToggle<CR>
" }}} plugin Gundo

" plugin TaskList {{{
nmap <Leader>T <plug>TaskList
" }}} plugin TaskList

" plugin Trinity {{{
" Open and close all the three plugins on the same time
nmap <F8>  :TrinityToggleAll<CR>
" Open and close the Source Explorer separately
nmap <F9>  :TrinityToggleSourceExplorer<CR>
" Open and close the Taglist separately
nmap <F10> :TrinityToggleTagList<CR>
" Open and close the NERD Tree separately
nmap <F11> :TrinityToggleNERDTree<CR>
" }}} plugin Trinity

" plugin taglist {{{
" 現在編集中のファイルしか表示しない
" let Tlist_Show_One_File = 1
" 右側に表示する
" let Tlist_Use_Right_Window = 1
" 表示幅
" let Tlist_WinWidth = 45
" map <F9> :TlistToggle <CR>
" }}} plugin taglist

" plugin Source Explorer {{{
" 自動でプレビューを表示する。
" let g:SrcExpl_RefreshTime = 1
" プレビューウインドウの高さ
" let g:SrcExpl_WinHeight = 9
" tagsは自動で作成する
" let g:SrcExpl_UpdateTags = 1
" let g:SrcExpl_RefreshMapKey = "<Space>"
" let g:SrcExpl_GoBackMapKey = "<C-b>"

" // The switch of the Source Explorer
" map <F8> :SrcExplToggle <CR>
" // Set the height of Source Explorer window
" let g:SrcExpl_winHeight = 8
" // Set 100 ms for refreshing the Source Explorer
let g:SrcExpl_refreshTime = 100
" // Set "Enter" key to jump into the exact definition context
let g:SrcExpl_jumpKey = "<ENTER>"
" // Set "Space" key for back from the definition context
let g:SrcExpl_gobackKey = "<SPACE>"
" // Enable/Disable the local definition searching, and note that this is not
" // guaranteed to work, the Source Explorer doesn't check the syntax for now.
" // It only searches for a match with the keyword according to command 'gd'
" let g:SrcExpl_searchLocalDef = 1
" // Do not let the Source Explorer update the tags file when opening
let g:SrcExpl_isUpdateTags = 0
" // Use 'Exuberant Ctags' with '--sort=foldcase -R .' or '-L cscope.files' to
" // create/update the tags file
let g:SrcExpl_updateTagsCmd = "ctags --sort=foldcase -R ."
" // Set "<F12>" key for updating the tags file artificially
let g:SrcExpl_updateTagsKey = "<F5>"
" // Set "<F3>" key for displaying the previous definition in the jump list
let g:SrcExpl_prevDefKey = "<F3>"
" // Set "<F4>" key for displaying the next definition in the jump list
let g:SrcExpl_nextDefKey = "<F4>"
" }}} Source Explorer

" plugin nerdtree {{{
" map <F10> :NERDTreeToggle <CR>
" }}} plugin nerdtree

" plugin tagbar {{{
nmap <Leader>t :TagbarToggle<CR>
" }}} plugin tagbar

" plugin syntastic {{{
let g:syntastic_enable_signs=1
let g:syntastic_auto_loc_list=2
" }}} plugin syntastic

" plugin lightline {{{
set laststatus=2
let g:lightline = {
  \ 'colorscheme': 'wombat',
  \ 'mode_map': {'c': 'NORMAL'},
  \ 'active': {
  \   'left': [ [ 'mode', 'paste' ], [ 'fugitive', 'gitgutter', 'filename' ] ]
  \ },
  \ 'component_function': {
  \   'modified': 'MyModified',
  \   'readonly': 'MyReadonly',
  \   'fugitive': 'MyFugitive',
  \   'filename': 'MyFilename',
  \   'fileformat': 'MyFileformat',
  \   'filetype': 'MyFiletype',
  \   'fileencoding': 'MyFileencoding',
  \   'mode': 'MyMode',
  \   'gitgutter': 'MyGitGutter',
  \ }
  \ }

function! MyModified()
  return &ft =~ 'help\|vimfiler\|gundo' ? '' : &modified ? '+' : &modifiable ? '' : '-'
endfunction

function! MyReadonly()
  return &ft !~? 'help\|vimfiler\|gundo' && &readonly ? 'x' : ''
endfunction

function! MyFilename()
  return ('' != MyReadonly() ? MyReadonly() . ' ' : '') .
    \ (&ft == 'vimfiler' ? vimfiler#get_status_string() :
    \  &ft == 'unite' ? unite#get_status_string() :
    \  &ft == 'vimshell' ? vimshell#get_status_string() :
    \ '' != expand('%:t') ? expand('%:t') : '[No Name]') .
    \ ('' != MyModified() ? ' ' . MyModified() : '')
endfunction

function! MyFugitive()
  try
    if &ft !~? 'vimfiler\|gundo' && exists('*fugitive#head')
      return fugitive#head()
    endif
  catch
  endtry
  return ''
endfunction

function! MyFileformat()
  return winwidth(0) > 70 ? &fileformat : ''
endfunction

function! MyFiletype()
  return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype : 'no ft') : ''
endfunction

function! MyFileencoding()
  return winwidth(0) > 70 ? (strlen(&fenc) ? &fenc : &enc) : ''
endfunction

function! MyMode()
  return winwidth(0) > 60 ? lightline#mode() : ''
endfunction

function! MyGitGutter()
  if ! exists('*GitGutterGetHunkSummary')
    \ || ! get(g:, 'gitgutter_enabled', 0)
    \ || winwidth('.') <= 90
    return ''
  endif
  let symbols = [
    \ g:gitgutter_sign_modified . '',
    \ g:gitgutter_sign_added . '',
    \ g:gitgutter_sign_removed . ''
    \ ]
  let hunks = GitGutterGetHunkSummary()
  let ret = []
  for i in [0, 1, 2]
    if hunks[i] > 0
      call add(ret, symbols[i] . hunks[i])
    endif
  endfor
  return join(ret, ' ')
endfunction
" }}} plugin lightline

" plugin jedi {{{
let s:hooks = neobundle#get_hooks("jedi-vim")
function! s:hooks.on_source(bundle)
  " jediにvimの設定を任せると'completeopt+=preview'するので
  " 自動設定機能をOFFにし手動で設定を行う
  let g:jedi#auto_vim_configuration = 0
  " 補完の最初の項目が選択された状態だと使いにくいためオフにする
  let g:jedi#popup_select_first = 0
  " goto
  " let g:jedi#goto_command = "<leader>g"
  let g:jedi#goto_assignments_command = "<leader>g"
  " 定義元
  " let g:jedi#get_definition_command = "<leader>d"
  let g:jedi#get_definitions_command = "<leader>d"
  " pydoc
  " let g:jedi#pydoc = "K"
  let g:jedi#documentation_command = "K"
  " quickrunと被るため大文字に変更
  let g:jedi#rename_command = '<Leader>R'
endfunction
" }}} plugin jedi

" plugin pyflakes-pathgen {{{
" let pyflakes_use_quickfix = 0
" }}} plugin pyflakes-pathgen

" plugin vim-task {{{
"autocmd FileType taskedit inoremap <silent> <buffer> <CR> <ESC>:call Toggle_task_status()<CR>i
autocmd FileType taskedit noremap <silent> <buffer> <CR> :call Toggle_task_status()<CR>
" }}} plugin vim-task

" plugin open-browser {{{
" http://vim-users.jp/2011/08/hack225/
let g:netrw_nogx = 1 " disable netrw's gx mapping.
" カーソル下のURLをブラウザで開く
nmap gx <Plug>(openbrowser-smart-search)
vmap gx <Plug>(openbrowser-smart-search)
" カーソル下のキーワードをググる
nnoremap go :<C-u>OpenBrowserSearch<Space><C-r><C-w><Enter>
vnoremap go :<C-u>OpenBrowserSearch<Space><C-r><C-w><Enter>
" }}} plugin open-browser

" plugin echodoc {{{
" 自動的に有効
let g:echodoc_enable_at_startup = 1
" }}} plugin echodoc

" plugin memolist {{{
map <Leader>mn  :MemoNew<CR>
map <Leader>ml  :MemoList<CR>
map <Leader>mg  :MemoGrep<CR>
" let g:memolist_memo_suffix  =  "rst"
let g:memolist_memo_suffix  =  "md"
let g:memolist_memo_date  =  "%Y-%m-%d %H:%M"
let g:memolist_memo_date  =  "epoch"
let g:memolist_memo_date  =  "%D %T"
let g:memolist_prompt_tags  =  1
let g:memolist_prompt_categories  =  1
let g:memolist_qfixgrep  =  1
let g:memolist_vimfiler  =  1
let g:memolist_vimfiler_option = ''
" let g:memolist_template_dir_path  =  "path/to/dir"
" }}} plugin memolist

" plugin vtreeexplorer {{{
" map <F10> :VSTreeExplore <CR>
" let g:treeExplVertical = 1
" let g:treeExplWinSize = 30
" }}} plugin vtreeexplorer

" plugin Clendar {{{
" let g:calendar_google_calendar = 1
" let g:calendar_google_task = 1
" }}} plugin Calendar

" plugin HOGEHOGE {{{
" }}} plugin HOGEHOGE

" }}} ==== Plugins ====
