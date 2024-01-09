-- [[ Setting options ]]
-- See `:help vim.o`
-- NOTE: You can change these options as you wish!

-- Set highlight on search
vim.o.hlsearch = true

-- Make line numbers default
vim.wo.number = false

-- Enable mouse mode
--vim.o.mouse = 'a'
vim.o.mouse = ''

-- Sync clipboard between OS and Neovim.
--  Remove this option if you want your OS clipboard to remain independent.
--  See `:help 'clipboard'`
vim.o.clipboard = 'unnamedplus'

-- Enable break indent
--vim.o.breakindent = true

-- Save undo history
--local undo_dir = vim.fn.expand('$HOME/.config/nvim/undo')
--if not vim.loop.fs_stat(undo_dir) then
--  vim.fn.system {
--    'mkdir',
--    '-p',
--    undo_dir,
--  }
--end
--vim.o.undodir = undo_dir
vim.o.undofile = true

-- Case-insensitive searching UNLESS \C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

vim.o.incsearch = true
vim.o.wrapscan = true

-- Keep signcolumn on by default
--vim.wo.signcolumn = 'yes'

-- Decrease update time
vim.o.updatetime = 100
vim.o.timeout = true
vim.o.timeoutlen = 300

-- Set completeopt to have a better completion experience
--vim.o.completeopt = 'menuone,noselect'

-- NOTE: You should make sure your terminal supports this
vim.o.termguicolors = true
vim.o.t_vb = ''
vim.o.vb = true

-- 長いテキストの折り返し
vim.o.wrap = true
vim.o.ambiwidth = 'double'
-- 1行の文字数が多い場合も表示する
vim.o.display = 'lastline'
-- 自動的に改行が入るのを無効化
vim.o.textwidth = 0
-- 入力したコマンドを表示
vim.o.showcmd = true
-- コマンドライン補完
vim.o.wildmenu = true
-- TABで補完
vim.o.wildchar = '<tab>'
vim.o.wildmode = 'longest:full,full'
-- コマンドヒストリー1000件
vim.o.history = 1000
-- fold with indent
vim.o.foldmethod = 'indent'
-- all level not folding when open buffer
vim.o.foldlevel = 99

-- タブ幅は4
vim.o.tabstop = 4
vim.o.shiftwidth = 4
vim.o.softtabstop = 4
-- タブはスペース
vim.o.expandtab = true
vim.o.smarttab = true

-- SwapファイルとBackupファイルを無効化する
vim.o.nowritebackup = true
vim.o.nobackup = true
vim.o.noswapfile = true

-- 文字列置換をインタラクティブに表示する
vim.o.inccommand = 'split'

-- バックスラッシュやクエスチョンを状況に合わせ自動的にエスケープ
vim.api.nvim_set_keymap('c', '/', [[getcmdtype() == '/' ? "\\/" : "/"]], { expr = true, noremap = true })
vim.api.nvim_set_keymap('c', '?', [[getcmdtype() == '?' ? "\\?" : "?"]], { expr = true, noremap = true })

if vim.fn.executable('rg') then
  vim.o.grepprg = 'rg --vimgrep --no-heading'
  vim.o.grepformat = '%f:%l:%c:%m,%f:%l:%m'
elseif vim.fn.executable('ag') then
  vim.o.grepprg = 'ag --vimgrep'
  vim.o.grepformat = '%f:%l:%c:%m'
else
  -- grepは再帰、行番号表示、バイナリファイルは見ない、ファイル名表示
  -- .hgと.git、tagsは対象外
  vim.o.grepprg = 'grep -rnIH --color --exclude=\\.hg --exclude=\\.git --exclude=\\.svn --exclude=tags --exclude=GTAGS'
end

-- CTRL-A, CTRL-Xで増減させる時の設定
if vim.o.nrformats == nil then
  vim.o.nrformats = {}
end
-- 0で始まる数字を8進数として扱わない
vim.opt.nrformats:remove({ 'octal' })
-- 0xで始まる数字を16進数として扱う
vim.opt.nrformats:append({ 'hex' })

-- 文字コードとか改行コードとか
-- 想定される改行の種類
vim.o.ffs = 'unix,dos,mac'
vim.o.fileencodings = 'utf-8,ucs-bom,iso-2022-jp-3,iso-2022-jp,eucjp-ms,euc-jisx0213,euc-jp,sjis,cp932'

-- '<'や'>'でインデントする際に'shiftwidth'の倍数に丸める
vim.o.shiftround = true
-- 補完時に大文字小文字を区別しない
vim.o.infercase = true
-- カーソルを文字が存在しない部分でも動けるようにする
--set virtualedit=all
if vim.o.virtualedit == nil then
  vim.o.virtualedit = {}
end
vim.opt.virtualedit:append({ 'block' })
-- バッファを閉じる代わりに隠す（Undo履歴を残すため）
vim.o.hidden = true
-- 新しく開く代わりにすでに開いてあるバッファを開く
vim.o.switchbuf = 'useopen'
-- 対応する括弧などをハイライト表示する
vim.o.showmatch = true
-- 対応括弧のハイライト表示を0.2秒にする
vim.o.matchtime = 2
-- 対応括弧に'<'と'>'のペアを追加
if vim.o.matchpairs == nil then
  vim.o.matchpairs = {}
end
vim.opt.matchpairs:append({ '<:>' })

-- バックスペースで何でも消せるようにする
vim.o.backspace = 'indent,eol,start'

-- 上書きされたファイルを自動的に読み込む
vim.o.autoread = true

-- 新しいウィンドウを下に開く
vim.o.splitbelow = true
-- 新しいウィンドウを右に開く
vim.o.splitright = true
-- 画面は縦分割
vim.opt.diffopt:append({ 'filler,vertical' })

-- 不可視文字の表示
vim.o.listchars = 'eol:$,tab:>-,trail:_'

-- 日本語入力をリセットする
vim.cmd[[
au MyAutoCmd BufNewFile,BufRead * set iminsert=0
]]

-- タイトルを表示
vim.o.title = true
-- カーソルの行列を表示
vim.o.ruler = true

-- カーソル行の表示
--set cursorline
-- カレントバッファだけカーソルラインを表示する
--au MyAutoCmd WinLeave * set nocursorline
--au MyAutoCmd WinLeave * set nocursorcolumn
--au MyAutoCmd WinEnter,BufRead * set cursorline
vim.cmd[[
hi CursorLine gui=underline term=underline cterm=underline
hi Visual term=reverse cterm=reverse
]]
-- カーソルラインが遅いので無効に
vim.o.nocursorline = true

-- syntax highlightが重い時があるので調整
vim.o.synmaxcol = 300
vim.cmd[[
au MyAutoCmd BufRead *.log setl syntax=off
]]

-- gitのコミットメッセージを書く時はspell check on
vim.cmd[[
au MyAutoCmd FileType gitcommit setlocal spell
au MyAutoCmd FileType gitcommit setlocal textwidth=0
]]

-- Remember cursor potition
vim.cmd[[
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
]]

-- make, grep などのコマンド後に自動的にQuickFixを開く
vim.cmd[[
au MyAutoCmd QuickfixCmdPost make,*grep* cwindow
]]
-- (l以外で始まる)QuickFixコマンドの実行が終わったらQuickFixウインドウを開く
--au MyAutoCmd QuickFixCmdPost [^l]* copen

-- :makeしたらファイルを自動的に保存する
vim.o.autowrite = true

-- QuickFixおよびHelpでは q でバッファを閉じる
vim.cmd[[
au MyAutoCmd FileType help,qf nnoremap <buffer> q <C-w>c
]]

-- p でpreview
vim.cmd[[
au MyAutoCmd FileType qf nnoremap <buffer> p <CR>zz<C-w>p
au MyAutoCmd FileType qf nmap <silent> <buffer> j <Down>
au MyAutoCmd FileType qf nmap <silent> <buffer> k <Up>
]]

-- QuickFixのウインドウだけになったら閉じる
vim.cmd[[
au MyAutoCmd WinEnter * if (winnr('$') == 1) && (getbufvar(winbufnr(0), '&buftype')) == 'quickfix' | quit | endif
]]

-- 個別のタブの表示設定をします
-- http://doruby.kbmj.com/aisi/20091218/Vim__
vim.cmd[[
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
]]

-- guitablabel に上の関数を設定します
-- その表示の前に %N というところでタブ番号を表示させています
vim.cmd[[
set guitablabel=%N:\ %{GuiTabLabel()}
]]

-- AllMaps
-- http://vim-users.jp/2011/02/hack203/
-- 全てのマッピングを表示
-- :AllMaps
-- 現在のバッファで定義されたマッピングのみ表示
-- :AllMaps <buffer>
-- どのスクリプトで定義されたかの情報も含め表示
-- :verbose AllMaps <buffer>
vim.cmd[[
command!
\   -nargs=* -complete=mapping
\   AllMaps
\   map <args> | map! <args> | lmap <args>
]]

-- タグファイルはカレントディレクトリから上向きに検索
vim.cmd[[
set tags=./tags;
]]

-- 開いているファイルのディレクトリに移動
vim.cmd[[
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
]]

-- 編集前のファイルとのdiff
vim.cmd[[
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
    \ | wincmd p | diffthis
endif
]]

-- tagbarの色調整
vim.cmd[[
hi TagbarSignature ctermfg=Yellow
]]

-- mark
vim.cmd[[
if !exists('g:markrement_char')
  let g:markrement_char = [
  \   'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
  \   'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'
  \ ]
endif
"nnoremap <silent>m :<C-u>call <SID>AutoMarkrement()<CR>
nnoremap <silent>m :<C-u>call s:AutoMarkrement()<CR>
function! s:AutoMarkrement()
  if !exists('b:markrement_pos')
    let b:markrement_pos = 0
  else
    let b:markrement_pos = (b:markrement_pos + 1) % len(g:markrement_char)
  endif
  execute 'mark' g:markrement_char[b:markrement_pos]
  echo 'marked' g:markrement_char[b:markrement_pos]
endfunction
]]
