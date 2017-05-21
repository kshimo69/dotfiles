au MyAutoCmd FileType gitv call s:my_gitv_settings()
au MyAutoCmd FileType git setlocal nofoldenable foldlevel=0
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
