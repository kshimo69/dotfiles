" 日本語入力をリセットする
au MyAutoCmd BufNewFile,BufRead * set iminsert=0

" syntax highlightが重い時があるので調整
set synmaxcol=300
au MyAutoCmd BufRead *.log setl syntax=off

" gitのコミットメッセージを書く時はspell check on
au MyAutoCmd FileType gitcommit setlocal spell
au MyAutoCmd FileType gitcommit setlocal textwidth=0

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

" 編集前のファイルとのdiff
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
    \ | wincmd p | diffthis
endif

" tagbarの色調整
hi TagbarSignature ctermfg=Yellow

" mark
if !exists('g:markrement_char')
  let g:markrement_char = [
  \   'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
  \   'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'
  \ ]
endif
nnoremap <silent>m :<C-u>call <SID>AutoMarkrement()<CR>
function! s:AutoMarkrement()
  if !exists('b:markrement_pos')
    let b:markrement_pos = 0
  else
    let b:markrement_pos = (b:markrement_pos + 1) % len(g:markrement_char)
  endif
  execute 'mark' g:markrement_char[b:markrement_pos]
  echo 'marked' g:markrement_char[b:markrement_pos]
endfunction
