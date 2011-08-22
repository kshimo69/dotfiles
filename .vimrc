" vim:set foldmethod=marker commentstring=//%s :
" .vimrc

" General {{{
let $PATH = '/opt/local/bin:/opt/local/sbin:' . $PATH
set nocompatible
set textwidth=0  " no auto return when column has so long
set nobackup  " not create backup files
"set noswapfile  " not craste swap files
"set hidden  " it can change buffer when buffer don't save
set autoread  " auto read when file was overwrited
set backspace=indent,eol,start  " remove any character <BS> key
set vb  " no beep
set t_vb=
set showcmd  " show command
set wildmenu  " expand command
set wildchar=<tab>  " start expand with <TAB>
set wildmode=list:full  " display type: list
set history=1000  " command history
set foldmethod=indent  " fold with indent
"set foldlevel=0  " all level folding
set foldlevel=99  " all level not folding when open buffer
"set foldcolumn=4  " show folding line
set tabstop=4 shiftwidth=4 softtabstop=4  " tab set 4 spaces
set expandtab  " TAB expand SPACE
set smarttab
set showmatch  " highlight bracket
" http://vim-users.jp/2010/02/hack125/
set virtualedit& " initialize
set virtualedit+=block
"set background=dark
"set autoindent
"set smartindent
"set cindent
set nrformats& " initialize
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
" }}}
" View {{{
set number  " display line number
set diffopt+=vertical
set listchars=eol:$,tab:>\ ,extends:<  " character when use 'set list'
"set list  " display TAB and CR
colorscheme koehler
" GUI
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
syntax on
filetype on
filetype plugin on
filetype indent on
highlight Folded ctermbg=grey ctermfg=blue guibg=grey guifg=blue
highlight FoldColumn ctermfg=green guifg=green
set title
set ruler
" }}}
" Search {{{
set ignorecase
set smartcase
set incsearch
set wrapscan
set hlsearch
" }}}
" Key map {{{
nnoremap j gj
nnoremap k gk
nnoremap <Space> jzz
nnoremap <S-Space> kzz
nnoremap Y y$
nnoremap ¥ \
inoremap ¥ \
" }}}
" tab {{{
nnoremap :t :Texplore
nnoremap >> :tabnext
nnoremap << :tabprevious
" }}}
" buffer {{{
nmap <silent> <C-x><C-n> :bnext<CR>
nmap <silent> <C-x><C-p> :bprevious<CR>
nmap <silent> <C-x><C-l> :buffers<CR>
" }}}
" show serach result middle of buffer {{{
nmap n nzz
nmap N Nzz
nmap * *zz
nmap # #zz
nmap g* g*zz
nmap g# g#zz
nmap G Gzz
" }}}
" split {{{
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
" }}}
" http://d.hatena.ne.jp/yuroyoro/20101104/1288879591 {{{
" highlight cursor line
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
" cancel highlight search
nmap <ESC><ESC> :nohlsearch<CR><ESC>
" search help under cursor keyword
nnoremap <C-h> :<C-u>help<Space><C-r><C-w><Enter>
" }}}
" Status line {{{
set laststatus=2
"set statusline=%n\:%y%F\ %(\[%{GitBranch()}\]\ %)\|%{(&fenc!=''?&fenc:&enc).'\|'.&ff.'\|'}ascii\:\%03.3b\|hex\:\%02.2B\|%m%r%=<%v\:%l/%L:%p%%>
set statusline=%n\:%y%F\\|%{(&fenc!=''?&fenc:&enc).'\|'.&ff.'\|'}ascii\:\%03.3b\|hex\:\%02.2B\|%m%r%=<%v\:%l/%L:%p%%>
highlight StatusLine term=NONE cterm=NONE ctermfg=black ctermbg=white
"highlight StatusLine gui=BOLD guifg=Black guibg=LightYellow
" Change status line's color when into insert mode
augroup InsertHook
    autocmd!
    autocmd InsertEnter * highlight StatusLine guifg=White guibg=DarkCyan
    autocmd InsertLeave * highlight StatusLine guifg=Black guibg=LightYellow
augroup END
" Change cursor color when IME on/off
if has('multi_byte_ime') || has('xim')
    highlight CursorIM guibg=LightRed guifg=NONE
endif
" }}}
" When editing a file, always jump to the last cursor position {{{
autocmd BufReadPost *
\ if line("'\"") > 0 && line ("'\"") <= line("$") |
\   exe "normal! g'\"" |
\ let b:posBufReadPost = getpos('.') |
\ endif
autocmd BufWinEnter *
\ if exists('b:posBufReadPost') |
\   if b:posBufReadPost == getpos('.') |
\   execute 'normal! zvzz' |
\   endif |
\ unlet b:posBufReadPost |
\ endif
" }}}
" http://www.slideshare.net/tsukkee/vim5-vimrc {{{
" load password file
if filereadable(expand('~/.vimrc.passwd'))
    source ~/.vimrc.passwd
endif

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
" }}}
" Complete {{{
autocmd FileType *
\   if &l:omnifunc == ''
\ |   setlocal omnifunc=syntaxcomplete#Complete
\ | endif
" }}}
" Configuration for c/c++ folding {{{
autocmd FileType c      syntax clear cBlock |
            \           syntax region myFold start='^{' end='^}' transparent fold |
            \           syntax sync fromstart |
            \           setlocal foldmethod=syntax
autocmd FileType cpp    syntax clear cBlock |
            \           syntax region myFold start='^{' end='^}' transparent fold |
            \           syntax sync fromstart |
            \           setlocal foldmethod=syntax
" }}}
" Configuration for python {{{
autocmd FileType python setl autoindent
autocmd FileType python setl smartindent cinwords=if,elif,for,while,try,except,finally,def,class
autocmd FileType python setl expandtab tabstop=4 shiftwidth=4 softtabstop=4
" Execute python script C-P
function! s:ExecPy()
    exe "!" . &ft . " %"
:endfunction
command! Exec call <SID>ExecPy()
autocmd FileType python map <silent> <C-P> :call <SID>ExecPy()<CR>
" Pydiction
autocmd FileType python let g:pydiction_location = '~/.vim/dict/pydiction/complete-dict'
"autocmd FileType python inoremap . .<C-x><C-u><C-p>
" }}}
" XML close tag {{{
augroup MyXML
    autocmd!
    autocmd Filetype xml inoremap <buffer> </ </<C-x><C-o>
augroup END
" }}}
" Reset Japanese input {{{
au BufNewFile,BufRead * set iminsert=0
" Tab reset
au BufNewFile,BufRead * set tabstop=4 shiftwidth=4 softtabstop=4
" Show zenkaku space
highlight ZenkakuSpace cterm=underline ctermfg=lightblue guibg=#666666
au BufNewFile,BufRead * match ZenkakuSpace /　/
" }}}
" yank {{{
let g:yankring_history_file = '.yankring_history_file'
" }}}
"" autocomplpop {{{
":set complete=.,w,b,u,t,i,k
"autocmd Filetype *    let g:AutoComplPop_CompleteOption='.,w,b,u,t,i,k'
"highlight Pmenu ctermbg=4
"highlight PmenuSel ctermbg=1
"highlight PMenuSbar ctermbg=4
"
"" http://vim.wikia.com/wiki/Autocomplete_with_TAB_when_typing_words
""Use TAB to complete when typing words, else inserts TABs as usual.
""Uses dictionary and source files to find matching words to complete.
""See help completion for source,
""Note: usual completion is on <C-n> but more trouble to press all the time.
""Never type the same word twice and maybe learn a new spellings!
""Use the Linux dictionary when spelling is in doubt.
""Window users can copy the file to their machine.
"function! Tab_Or_Complete()
"  if col('.')>1 && strpart( getline('.'), col('.')-2, 3 ) =~ '^\w'
"    return "\<C-N>"
"  else
"    return "\<Tab>"
"  endif
"endfunction
"":inoremap <Tab> <C-R>=Tab_Or_Complete()<CR>
""inoremap <expr> <CR> pumvisible()?"\<C-Y>":"\<CR>"
"":set dictionary="/usr/dict/words"
"" }}}
"" http://d.hatena.ne.jp/secondlife/20060203/1138978661 {{{
"" Allargs.vim
"" ex)
"" :Allargs %s/foo/bar/ge|update
"" 使う時。foo を bar に置換しまくる。
"" :Allargs %s/foo/bar/ge|update
"" eオプションをつけないと foo が無いというメッセージがのんびり表示されて、いつま
"" でたっても置換が終わらないので気をつけよう(それに気づかずに密かにハマった)
"" コマンドは | で連続で実行できて、update は変更のあったバッファだけを保存。と。
"" カレントの *.cpp を置換する場合は予め、
"" :ar *.cpp
"" ってやっとくと全部読み込まれる。
"" 確認するには
"" :ar
"function! Allargs(command)
"  let i = 0
"  while i < argc()
"    if filereadable(argv(i))
"      execute "e " . argv(i)
"      execute a:command
"    endif
"    let i = i + 1
"  endwhile
"endfunction
"command! -nargs=+ -complete=command Allargs call Allargs(<q-args>)
"" }}}
"" skk.vim http://fifnel.com/tag/mac/ {{{
"let skk_jisyo = '~/.vim/skk-jisyo'
"let skk_large_jisyo = '~/.vim/dict/skk/SKK-JISYO.L'
"let skk_auto_save_jisyo = 1
"let skk_keep_state = 0
"let skk_egg_like_newline = 1
"let skk_show_annotation = 1
"let skk_use_face = 1
"
"let g:skk_sticky_key = ';'
"let g:skk_kakutei_key = '.'
"let g:skk_use_color_cursor = 1
"let g:skk_cursor_hiragana_color = '#ff0000' " かなモード
"let g:skk_cursor_katakana_color = '#00ff00' " カナモード
"let g:skk_cursor_jisx0208_color = '#ffcc00' " 全英モード
"let g:skk_cursor_latin_color = '#000000' " アスキーモード
"let g:skk_cursor_abbrev_color = '#0000ff' " SKK abbrevモード
"let g:skk_latin_mode_string = 'SKK' " アスキーモード ( SKK:aA )
"let g:skk_hiragana_mode_string = 'かな' " かなモード ( SKK:あ )
"let g:skk_katakana_mode_string = 'カナ' " カナモード ( SKK:ア )
"let g:skk_jisx0208_latin_mode_string = '全英' " 全英モード ( SKK:Ａ )
"let g:skk_abbrev_mode_string = 'aあ' " SKK abbrevモード (SKK:aあ )
"" }}}
"" hatena.vim {{{
"let g:hatena_user = 'kshimo69'
"" }}}
"" outputz {{{
"let g:outputz_secret_key = 'Bby9m2635Q.C'
"" }}}
"" rtm-vim {{{
"let g:rtm_api_key = 'f5f8dd1b0f0fefbb8debe6d206868df2'
"let g:rtm_shared_secret = '0740d5f2e9c4f6b8'
"let g:rtm_token = '39d422c2f07a605614cf9593e22d1a0bbf117f96'
"" }}}
"" ChangeLog {{{
"let g:changelog_username = "Kimihiko Shimomura  <kshimo69@gmail.com>"
"let g:changelog_dateformat = "%Y-%m-%d (%a)"
"nmap <C-m> :e ~/clmemo/ChangeLog.txt<CR>
"" clmemogrep
"" http://d.hatena.ne.jp/ampmmn/20090219/1235042852
"let g:clmemogrep_changelogfilepath = '~/clmemo/ChangeLog.txt'
"let g:clmemogrep_itemseparator = '--------'
"nmap <C-g> :CLMemoGrep 
"" }}}
"" cursoroverdictionary {{{
"" http://d.hatena.ne.jp/ampmmn/20091002/1254474418
"" English to Japanese
"call cursoroverdictionary#add("gtj", "http://www.google.com/translate_t?langpair=auto|ja&text={word}", "utf-8", "sjis")
"call cursoroverdictionary#set_trim_pattern("gtj", '<span id=result_box.\{-}>', '</span>')
"call cursoroverdictionary#set_user_agent("gtj", "Mozilla/4.0 (ja)")
"" Japanese to English
"call cursoroverdictionary#add("gte", "http://www.google.com/translate_t?langpair=auto|en&text={word}", "utf-8", "sjis")
"call cursoroverdictionary#set_trim_pattern("gte", '<span id=result_box.\{-}>', '</span>')
"call cursoroverdictionary#set_user_agent("gte", "Mozilla/5.0 (ja)")
"" Alc
"call cursoroverdictionary#add("alc", "http://eow.alc.co.jp/{word}/UTF-8", "utf-8", "utf-8")
"call cursoroverdictionary#set_trim_pattern("alc", '<div id="resultList".\{-}>', '\t\t\t\t</div>')
"call cursoroverdictionary#set_user_agent("alc", 'Mozilla/5.0 (Windows; U; Windows NT 5.1; ja; rv:1.9.0.6) Gecko/2009011913 Firefox/3.0.6')
"" keymap
"nnoremap <silent> ,t :<c-u>CODToggle<cr>
"vnoremap <silent> ,gtj :<c-u>CODSelectedEx gtj<cr>
"vnoremap <silent> ,gte :<c-u>CODSelectedEx gte<cr>
"vnoremap <silent> ,alc :<c-u>CODSelectedEx alc<cr>
"" }}}
"" neocomplcache {{{
"" http://github.com/Shougo/neocomplcache/tarball/master
"" http://vim-users.jp/2009/07/hack-49/
"" update script
"" curl -L http://github.com/Shougo/neocomplcache/tarball/master | tar -xzC ~/.vim --strip-components=1 --exclude=presen
"" http://www.vim.org/scripts/script.php?script_id=2620
"" Setting examples:
"" Disable AutoComplPop.
"let g:acp_enableAtStartup = 0
"" Use neocomplcache.
"let g:neocomplcache_enable_at_startup = 1
"" Use smartcase.
"let g:neocomplcache_enable_smart_case = 1
"" Use camel case completion.
"let g:neocomplcache_enable_camel_case_completion = 1
"" Use underbar completion.
"let g:neocomplcache_enable_underbar_completion = 1
"" Set minimum syntax keyword length.
"let g:neocomplcache_min_syntax_length = 3
"let g:neocomplcache_lock_buffer_name_pattern = '\*ku\*'
"
"" Define dictionary.
"let g:neocomplcache_dictionary_filetype_lists = {
"    \ 'default' : '',
"    \ 'vimshell' : $HOME.'/.vimshell_hist',
"    \ 'scheme' : $HOME.'/.gosh_completions'
"    \ }
"
"" Define keyword.
"if !exists('g:neocomplcache_keyword_patterns')
"    let g:neocomplcache_keyword_patterns = {}
"endif
"let g:neocomplcache_keyword_patterns['default'] = '\h\w*'
"
"" Plugin key-mappings.
"imap <C-k>     <Plug>(neocomplcache_snippets_expand)
"smap <C-k>     <Plug>(neocomplcache_snippets_expand)
"inoremap <expr><C-g>     neocomplcache#undo_completion()
"inoremap <expr><C-l>     neocomplcache#complete_common_string()
"
"" SuperTab like snippets behavior.
""imap <expr><TAB> neocomplcache#sources#snippets_complete#expandable() ? "\<Plug>(neocomplcache_snippets_expand)" : pumvisible() ? "\<C-n>" : "\<TAB>"
"
"" Recommended key-mappings.
"" <CR>: close popup and save indent.
"inoremap <expr><CR>  neocomplcache#smart_close_popup() . "\<CR>"
"" <TAB>: completion.
"inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
"" <C-h>, <BS>: close popup and delete backword char.
"inoremap <expr><C-h> neocomplcache#smart_close_popup()."\<C-h>"
"inoremap <expr><BS> neocomplcache#smart_close_popup()."\<C-h>"
"inoremap <expr><C-y>  neocomplcache#close_popup()
"inoremap <expr><C-e>  neocomplcache#cancel_popup()
"
"" AutoComplPop like behavior.
""let g:neocomplcache_enable_auto_select = 1
"
"" Enable omni completion.
"autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
"autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
"autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
"autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
"autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
"
"" Enable heavy omni completion.
"if !exists('g:neocomplcache_omni_patterns')
"let g:neocomplcache_omni_patterns = {}
"endif
"let g:neocomplcache_omni_patterns.ruby = '[^. *\t]\.\w*\|\h\w*::'
"autocmd FileType ruby setlocal omnifunc=rubycomplete#Complete
"" }}}
" vimshell {{{
" http://github.com/Shougo/vimshell/tarball/master
" update script
" curl -L http://github.com/Shougo/vimshell/tarball/master | tar -xzC ~/.vim --strip-components=1 --exclude=presen
" }}}
"" Open junk file."{{{
"" http://vim-users.jp/2010/11/hack181/
"command! -nargs=0 JunkFile call s:open_junk_file()
"function! s:open_junk_file()
"    "let l:junk_dir = $HOME . '/junk'. strftime('/%Y/%m')
"    let l:junk_dir = $HOME . '/junk'
"    if !isdirectory(l:junk_dir)
"        call mkdir(l:junk_dir, 'p')
"    endif
"
"    let l:filename = input('Junk Code: ', l:junk_dir.strftime('/%Y%m%d-%H%M%S.'))
"    if l:filename != ''
"        execute 'edit ' . l:filename
"    endif
"endfunction
"" }}}
"" raimei setting {{{
"let raimei_api="google"
"let raimei_from="en"
"let raimei_to="ja"
"map <silent> <C-I> :pyfile ~/bin/raimei.py<CR>
"" }}}
" http://vim-users.jp/2011/02/hack203/ {{{
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
" }}}
