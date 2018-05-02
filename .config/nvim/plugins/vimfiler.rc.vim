" vimfilerをデフォルトのexplorerに
let g:vimfiler_as_default_explorer = 1

" vimfilerだけになったら閉じる
" au MyAutoCmd BufEnter * if (winnr('$') == 1 && &filetype ==# 'vimfiler') | q | endif
let g:vimfiler_as_default_explorer = 1
let g:vimfiler_enable_auto_cd = 1

" VimFilerExplorerの幅を一時的に変える
au MyAutoCmd FileType vimfiler
  \ nmap <buffer> <SID>(vimfiler_redraw_screen) <Plug>(vimfiler_redraw_screen)|
  \ nnoremap <buffer><script> <C-W>> 30<C-W>><SID>(vimfiler_redraw_screen)|
  \ nnoremap <buffer><script> <C-W>< 30<C-W><<SID>(vimfiler_redraw_screen)|
  \ nnoremap <buffer><script> <C-W>\| <C-W>\|<SID>(vimfiler_redraw_screen)

" .pycを表示しない
let g:vimfiler_ignore_pattern = "\%(\.pyc$\)"

nnoremap <Leader>e :VimFilerExplorer<CR>
" 現在開いているバッファのディレクトリを開く
nnoremap <Leader>f :VimFilerBufferDir<CR>
