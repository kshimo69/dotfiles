let g:netrw_nogx = 1 " disable netrw's gx mapping.
" カーソル下のURLをブラウザで開く
nnoremap gx <Plug>(openbrowser-smart-search)
vnoremap gx <Plug>(openbrowser-smart-search)
" カーソル下のキーワードをググる
nnoremap go :<C-u>OpenBrowserSearch<Space><C-r><C-w><Enter>
vnoremap go <Plug>(openbrowser-open)
