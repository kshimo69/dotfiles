" vim:set foldmethod=marker foldlevel=0:
" .vimrc

" vundle setting {{{
set nocompatible
filetype off

set rtp+=~/.vim/vundle/
call vundle#rc()

" repository samples
"" original repos on github
"Bundle 'tpope/vim-fugitive'
"" vim-scripts repos
"Bundle 'rails.vim'
"" non github repos
"Bundle 'git://git.wincent.com/command-t.git'

" Install
" :BundleInstall
" Update
" :BundleInstall!

" repositories
Bundle 'Shougo/neocomplcache'
Bundle 'Shougo/unite.vim'
Bundle 'tsukkee/unite-tag'
Bundle 'rails.vim'
Bundle 'tyru/open-browser.vim'
Bundle 'motemen/git-vim'
"hatena-edit
"vimfiler
"echodoc

filetype plugin indent on
" }}} vundle setting end

" ==== General ==== {{{

" Basic setting {{{
let $PATH = '/opt/local/bin:/opt/local/sbin:' . $PATH
" use modeline
set modeline
" no auto return when column has so long
set textwidth=0
" not create backup files
set nobackup
"" not create swap files
"set noswapfile
"" it can change buffer when buffer don't save
"set hidden
" auto read when file was overwrited
set autoread
" remove any character <BS> key
set backspace=indent,eol,start
" no beep
set vb
set t_vb=
" show command
set showcmd
" expand command
set wildmenu
" start expand with <TAB>
set wildchar=<tab>
" display type: list
set wildmode=list:full
" command history
set history=1000
"" fold with indent
"set foldmethod=indent
"" all level folding
"set foldlevel=0
"" all level not folding when open buffer
"set foldlevel=99
"" show folding line
"set foldcolumn=4
" tab set 4 spaces
set tabstop=4 shiftwidth=4 softtabstop=4
" TAB expand SPACE
set expandtab
set smarttab
" highlight bracket
set showmatch
" rect select
" http://vim-users.jp/2010/02/hack125/
set virtualedit&
set virtualedit+=block
"set background=dark
"set autoindent
"set smartindent
"set cindent
set nrformats&
set nrformats-=octal
set ffs=unix,dos,mac  " return code
if !(has("win32") || has("win95") || has("win64") || has("win16"))
  set encoding=utf-8  " default encoding
endif
set fileencodings=utf-8,ucs-bom,iso-2022-jp-3,iso-2022-jp,eucjp-ms,euc-jisx0213,euc-jp,sjis,cp932
nmap ,ee :e ++enc=euc-jp<CR>
nmap ,es :e ++enc=cp932<CR>
nmap ,ej :e ++enc=iso-2022-jp<CR>
nmap ,eu :e ++enc=utf-8<CR>
" Reset Japanese input
au BufNewFile,BufRead * set iminsert=0
" Tab reset
au BufNewFile,BufRead * set tabstop=4 shiftwidth=4 softtabstop=4
" Show zenkaku space
highlight ZenkakuSpace cterm=underline ctermfg=lightblue guibg=#666666
au BufNewFile,BufRead * match ZenkakuSpace /　/
" yank
let g:yankring_history_file = '.yankring_history_file'
" }}} Basic setting end

" View setting {{{
" display line number
set number
set diffopt+=vertical
" character when use 'set list'
set listchars=eol:$,tab:>\ ,extends:<
"set list  " display TAB and CR
colorscheme koehler
syntax on
highlight Folded ctermbg=grey ctermfg=blue guibg=grey guifg=blue
highlight FoldColumn ctermfg=green guifg=green
set title
set ruler
set cursorline
" display line on current buffer
augroup cch
    autocmd! cch
    autocmd WinLeave * set nocursorline
    autocmd WinEnter,BufRead * set cursorline
augroup END
:hi clear CursorLine
":hi CursorLine gui=underline
highlight CursorLine ctermbg=blue guibg=grey20
" Change cursor color when IME on/off
if has('multi_byte_ime') || has('xim')
    highlight CursorIM guibg=LightRed guifg=NONE
endif

" GUI {{{
if has('gui_running')
    " sidebar
    set guioptions& " initialize
    set guioptions+=b
    " clip board
    " http://vim-users.jp/2010/02/hack126/
    set clipboard=unnamed,autoselect
    set guioptions+=a
    "set guifont=M+2VM+IPAG\ circle\ 14
    set guifont=VL\ Gothic\ 14
    " window size
    set lines=30
    set columns=120
    gui
    "set transparency=20
endif
if has('gui_macvim')
    set guioptions& " initialize
    set guioptions-=T
    set imdisable
    set antialias
    "colorscheme macvim
    "set guifont=M+2VM+IPAG\ circle\ Regular:h14
    "set guifont=Monaco:h14
    set guifont=Ricty\ Regular:h16
    set transparency=20
    set lines=30
    set columns=120
    set fuoptions=maxvert,maxhorz
    set fullscreen
    "au GUIEnter * set fullscreen
endif
" GUI end }}}

" Status line setting {{{
set laststatus=2
set statusline=%n\:%y%F\ %(\[%{GitBranch()}\]\ %)\|%{(&fenc!=''?&fenc:&enc).'\|'.&ff.'\|'}ascii\:\%03.3b\|hex\:\%02.2B\|%m%r%=<%v\:%l/%L:%p%%>
highlight StatusLine term=NONE cterm=NONE ctermfg=black ctermbg=white
"highlight StatusLine gui=BOLD guifg=Black guibg=LightYellow
" Change status line's color when into insert mode
augroup InsertHook
    autocmd!
    autocmd InsertEnter * highlight StatusLine guifg=White guibg=DarkCyan
    autocmd InsertLeave * highlight StatusLine guifg=Black guibg=LightYellow
augroup END
" }}} Status line setting end
" }}} View setting end

" Search setting {{{
set ignorecase
set smartcase
set incsearch
set wrapscan
set hlsearch
" cancel highlight search
nmap <ESC><ESC> :nohlsearch<CR><ESC>
" search help under cursor keyword
nnoremap <C-h> :<C-u>help<Space><C-r><C-w><Enter>
" }}} Search setting end

" Keymap setting {{{
nnoremap j gj
nnoremap k gk
nnoremap <Space> jzz
nnoremap <S-Space> kzz
nnoremap Y y$
nnoremap ¥ \
inoremap ¥ \
" }}} Keymap setting end

" Tab setting {{{
nnoremap <silent> tt :<c-u>Texplore<cr>
nnoremap <silent> tf :<c-u>tabfirst<cr>
nnoremap <silent> tl :<c-u>tablast<cr>
nnoremap <silent> tn :<c-u>tabnext<cr>
nnoremap <silent> tN :<c-u>tabNext<cr>
nnoremap <silent> tp :<c-u>tabprevious<cr>
nnoremap <silent> te :<c-u>tabedit<cr>
nnoremap <silent> tc :<c-u>tabclose<cr>
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
" }}} Tab setting end

" Buffer setting {{{
nmap <silent> <C-x><C-n> :bnext<CR>
nmap <silent> <C-x><C-p> :bprevious<CR>
nmap <silent> <C-x><C-l> :buffers<CR>
" show serach result middle of buffer
nmap n nzz
nmap N Nzz
nmap * *zz
nmap # #zz
nmap g* g*zz
nmap g# g#zz
nmap G Gzz
" split
nmap spj <SID>(split-to-j)
nmap spk <SID>(split-to-k)
nmap sph <SID>(split-to-h)
nmap spl <SID>(split-to-l)
nnoremap <SID>(split-to-j) : <C-u>belowright split<CR>
nnoremap <SID>(split-to-k) : <C-u>aboveleft split<CR>
nnoremap <SID>(split-to-h) : <C-u>topleft vsplit<CR>
nnoremap <SID>(split-to-l) : <C-u>botright vsplit<CR>
" scroll top window with key Ctrl+Shift+J
nnoremap <C-S-J> <C-W>k<C-E><C-W><C-W>
" }}} Buffer setting end

" Remember cursor potition setting {{{
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
" }}} Remember cursor potition setting end

" Cscope setting {{{
if has("cscope") && filereadable("/usr/bin/cscope")
    set csprg=/usr/bin/cscope
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
endif
" }}} Cscope setting end

" }}} General end

" ==== Programming ==== {{{

" Execute editting script
function! s:ExecCode()
    exe "!" . &ft . " %"
:endfunction
command! Exec call <SID>ExecCode()

" Python setting {{{
" Configuration for python
autocmd FileType python setl autoindent
autocmd FileType python setl smartindent cinwords=if,elif,for,while,try,except,finally,def,class
autocmd FileType python setl expandtab tabstop=4 shiftwidth=4 softtabstop=4
"" Pydiction
"autocmd FileType python let g:pydiction_location = '~/.vim/dict/pydiction/complete-dict'
"autocmd FileType python inoremap . .<C-x><C-u><C-p>
" }}} Python setting end

" Xml setting {{{
" XML close tag
augroup MyXML
    autocmd!
    autocmd Filetype xml inoremap <buffer> </ </<C-x><C-o>
augroup END
" }}} Xml setting end

" }}} Programming end

" ==== Functions ==== {{{

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
" }}} Allargs end

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
" }}} AllMaps end

" }}} Functions end

" ==== Plugins ==== {{{

" neocomplcache setting {{{
" Setting examples:
" Disable AutoComplPop.
let g:acp_enableAtStartup = 0
" Use neocomplcache.
let g:neocomplcache_enable_at_startup = 1
" Use smartcase.
let g:neocomplcache_enable_smart_case = 1
" Use camel case completion.
let g:neocomplcache_enable_camel_case_completion = 1
" Use underbar completion.
let g:neocomplcache_enable_underbar_completion = 1
" Set minimum syntax keyword length.
let g:neocomplcache_min_syntax_length = 3
let g:neocomplcache_lock_buffer_name_pattern = '\*ku\*'

" Define dictionary.
let g:neocomplcache_dictionary_filetype_lists = {
    \ 'default' : '',
    \ 'vimshell' : $HOME.'/.vimshell_hist',
    \ 'scheme' : $HOME.'/.gosh_completions'
    \ }

" Define keyword.
if !exists('g:neocomplcache_keyword_patterns')
    let g:neocomplcache_keyword_patterns = {}
endif
let g:neocomplcache_keyword_patterns['default'] = '\h\w*'

" Plugin key-mappings.
imap <C-k>     <Plug>(neocomplcache_snippets_expand)
smap <C-k>     <Plug>(neocomplcache_snippets_expand)
inoremap <expr><C-g>     neocomplcache#undo_completion()
inoremap <expr><C-l>     neocomplcache#complete_common_string()

" SuperTab like snippets behavior.
"imap <expr><TAB> neocomplcache#sources#snippets_complete#expandable() ? "\<Plug>(neocomplcache_snippets_expand)" : pumvisible() ? "\<C-n>" : "\<TAB>"

" Recommended key-mappings.
" <CR>: close popup and save indent.
inoremap <expr><CR>  neocomplcache#smart_close_popup() . "\<CR>"
" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><C-h> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><C-y>  neocomplcache#close_popup()
inoremap <expr><C-e>  neocomplcache#cancel_popup()

" AutoComplPop like behavior.
"let g:neocomplcache_enable_auto_select = 1

" Shell like behavior(not recommended).
"set completeopt+=longest
"let g:neocomplcache_enable_auto_select = 1
"let g:neocomplcache_disable_auto_complete = 1
"inoremap <expr><TAB>  pumvisible() ? "\<Down>" : "\<TAB>"
"inoremap <expr><CR>  neocomplcache#smart_close_popup() . "\<CR>"

" Enable omni completion.
"autocmd FileType *
"\   if &l:omnifunc == ''
"\ |     setlocal omnifunc=syntaxcomplete#Complete
"\ | endif
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
"autocmd FileType c syntax clear cBlock |
"            \      syntax region myFold start='^{' end='^}' transparent fold |
"            \      syntax sync fromstart |
"            \      setlocal foldmethod=syntax
"autocmd FileType cpp syntax clear cBlock |
"            \        syntax region myFold start='^{' end='^}' transparent fold |
"            \        syntax sync fromstart |
"            \        setlocal foldmethod=syntax

" Enable heavy omni completion.
if !exists('g:neocomplcache_omni_patterns')
    let g:neocomplcache_omni_patterns = {}
endif
"let g:neocomplcache_omni_patterns.python = '[^. \t]\.\h\w*'
"let g:neocomplcache_omni_patterns.ruby = '[^. *\t]\.\h\w*\|\h\w*::'
let g:neocomplcache_omni_patterns.ruby = '[^. *\t]\.\w*\|\h\w*::'
autocmd FileType ruby setlocal omnifunc=rubycomplete#Complete
let g:neocomplcache_omni_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
let g:neocomplcache_omni_patterns.c = '\%(\.\|->\)\h\w*'
let g:neocomplcache_omni_patterns.cpp = '\h\w*\%(\.\|->\)\h\w*\|\h\w*::'
" }}} neocomplcache setting end

" unite setting {{{
" http://d.hatena.ne.jp/ruedap/20110110/vim_unite_plugin
" http://d.hatena.ne.jp/Voluntas/20110823/1314031095
" 入力モードで開始する
" let g:unite_enable_start_insert=1
" バッファ一覧
nnoremap <silent> ,ub :<C-u>Unite buffer -direction=botright -auto-resize -toggle<CR>
" ファイル一覧
nnoremap <silent> ,uf :<C-u>UniteWithBufferDir -buffer-name=files file -direction=botright -auto-resize -toggle<CR>
" レジスタ一覧
nnoremap <silent> ,ur :<C-u>Unite -buffer-name=register register -direction=botright -auto-resize -toggle<CR>
" 最近使用したファイル一覧
nnoremap <silent> ,um :<C-u>Unite file_mru -direction=botright -auto-resize -toggle<CR>
" 常用セット
nnoremap <silent> ,uu :<C-u>Unite buffer file_mru -direction=botright -auto-resize -toggle<CR>
" 全部乗せ
nnoremap <silent> ,ua :<C-u>UniteWithBufferDir -buffer-name=files buffer file_mru bookmark file -direction=botright -auto-resize -toggle<CR>

" ウィンドウを分割して開く
au FileType unite nnoremap <silent> <buffer> <expr> <C-j> unite#do_action('split')
au FileType unite inoremap <silent> <buffer> <expr> <C-j> unite#do_action('split')
" ウィンドウを縦に分割して開く
au FileType unite nnoremap <silent> <buffer> <expr> <C-l> unite#do_action('vsplit')
au FileType unite inoremap <silent> <buffer> <expr> <C-l> unite#do_action('vsplit')
" }}} unite setting end

" open-browser setting {{{
" http://vim-users.jp/2011/08/hack225/
let g:netrw_nogx = 1 " disable netrw's gx mapping.
nmap gx <Plug>(openbrowser-smart-search)
vmap gx <Plug>(openbrowser-smart-search)
" }}} open-browser setting end

" }}} Plugins end

" HOGE setting {{{
" }}} HOGE setting end
