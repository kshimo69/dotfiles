" 行番号の表示
set number
" 長いテキストの折り返し
set wrap
set ambiwidth=double
" 1行の文字数が多い場合も表示する
set display=lastline
" 自動的に改行が入るのを無効化
set textwidth=0
" 入力したコマンドを表示
set showcmd
" コマンドライン補完
set wildmenu
" TABで補完
set wildchar=<tab>
set wildmode=longest:full,full
" コマンドヒストリー1000件
set history=1000
" fold with indent
set foldmethod=indent
" all level not folding when open buffer
set foldlevel=99

" タブ幅は4
set tabstop=4 shiftwidth=4 softtabstop=4
" タブはスペース
set expandtab
set smarttab

" SwapファイルとBackupファイルを無効化する
set nowritebackup
set nobackup
set noswapfile

" 大文字小文字を区別しない
set ignorecase
" 検索文字に大文字がある場合は大文字小文字を区別
set smartcase
" インクリメンタルサーチ
set incsearch
" 検索マッチテキストをハイライト
set hlsearch
" ファイル末尾までいったら最初から検索する
set wrapscan

" バックスラッシュやクエスチョンを状況に合わせ自動的にエスケープ
cnoremap <expr> / getcmdtype() == '/' ? '\/' : '/'
cnoremap <expr> ? getcmdtype() == '?' ? '\?' : '?'

" grepは再帰、行番号表示、バイナリファイルは見ない、ファイル名表示
" .hgと.git、tagsは対象外
set grepprg=grep\ -rnIH\ --color\ --exclude=\.hg\ --exclude=\.git\ --exclude=\.svn\ --exclude=tags\ --exclude=GTAGS

" CTRL-A, CTRL-Xで増減させる時の設定
set nrformats&
set nrformats-=octal  " 0で始まる数字を8進数として扱わない
set nrformats+=hex    " 0xで始まる数字を16進数として扱う

" 文字コードとか改行コードとか
scriptencoding utf-8
" 想定される改行の種類
set ffs=unix,dos,mac
" default encoding
set encoding=utf-8
set fileencodings=utf-8,ucs-bom,iso-2022-jp-3,iso-2022-jp,eucjp-ms,euc-jisx0213,euc-jp,sjis,cp932

" '<'や'>'でインデントする際に'shiftwidth'の倍数に丸める
set shiftround
" 補完時に大文字小文字を区別しない
set infercase
"set virtualedit=all    " カーソルを文字が存在しない部分でも動けるようにする
set virtualedit&
set virtualedit+=block
" バッファを閉じる代わりに隠す（Undo履歴を残すため）
set hidden
" 新しく開く代わりにすでに開いてあるバッファを開く
set switchbuf=useopen  " 新しく開く代わりにすでに開いてあるバッファを開く
" 対応する括弧などをハイライト表示する
set showmatch
" 対応括弧のハイライト表示を0.2秒にする
set matchtime=2
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

" undoファイルを使う
let s:undo_dir = expand("$HOME/.config/nvim/undo")
set undodir=s:undo_dir
if !isdirectory(s:undo_dir)
  call mkdir(s:undo_dir, 'p')
endif

" 上書きされたファイルを自動的に読み込む
set autoread

" 新しいウィンドウを下に開く
set splitbelow
" 新しいウィンドウを右に開く
set splitright
" 画面は縦分割
set diffopt+=vertical

" スクリーンベルを無効化
set t_vb=
"set novisualbell
set vb

" 不可視文字の表示
set listchars=eol:$,tab:>-,trail:_

" 日本語入力をリセットする
au MyAutoCmd BufNewFile,BufRead * set iminsert=0

colorscheme molokai

" 検索の色調整
au MyAutoCmd ColorScheme * hi Search term=reverse ctermfg=253 ctermbg=66 guifg=#FFFFFF guibg=#455354
au MyAutoCmd ColorScheme * hi TabLineSel term=reverse ctermfg=255 ctermbg=33 guifg=#FFFFFF guibg=#333333

" ターミナルの透過がそのまま見えるように
hi Normal ctermbg=NONE
hi NonText ctermbg=NONE

set title  " タイトルを表示
set ruler  " カーソルの行列を表示
set cursorline   " カーソル行の表示
" カレントバッファだけカーソルラインを表示する
au MyAutoCmd WinLeave * set nocursorline
au MyAutoCmd WinLeave * set nocursorcolumn
au MyAutoCmd WinEnter,BufRead * set cursorline
hi CursorLine gui=underline term=underline cterm=underline
hi Visual term=reverse cterm=reverse

" gitのコミットメッセージを書く時はspell check on
au MyAutoCmd FileType gitcommit setlocal spell

" Remember cursor potition
au MyAutoCmd BufReadPost *
\ if line("'\"") > 0 && line ("'\"") <= line("$") |
\     exe "normal! g'\"" |
\ let b:posBufReadPost = getpos('.') |
\ endif
au MyAutoCmd BufWinEnter *
\ if exists('b:posBufReadPost') |
\     if b:posBufReadPost == getpos('.') |
\     execute 'normal! zvzz' |
\     endif |
\ unlet b:posBufReadPost |
\ endif

" make, grep などのコマンド後に自動的にQuickFixを開く
au MyAutoCmd QuickfixCmdPost make,*grep* cwindow
" (l以外で始まる)QuickFixコマンドの実行が終わったらQuickFixウインドウを開く
"au MyAutoCmd QuickFixCmdPost [^l]* copen

" QuickFixおよびHelpでは q でバッファを閉じる
au MyAutoCmd FileType help,qf nnoremap <buffer> q <C-w>c

" p でpreview
au MyAutoCmd FileType qf nnoremap <buffer> p <CR>zz<C-w>p
au MyAutoCmd FileType qf nmap <silent> <buffer> j <Down>
au MyAutoCmd FileType qf nmap <silent> <buffer> k <Up>

" QuickFixのウインドウだけになったら閉じる
au MyAutoCmd WinEnter * if (winnr('$') == 1) && (getbufvar(winbufnr(0), '&buftype')) == 'quickfix' | quit | endif

" 個別のタブの表示設定をします
" http://doruby.kbmj.com/aisi/20091218/Vim__
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

" AllMaps
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

" タグファイルはカレントディレクトリから上向きに検索
set tags=./tags;

" 開いているファイルのディレクトリに移動
command! -nargs=? -complete=dir -bang CD call s:ChangeCurrentDir('<args>', '<bang>')
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
