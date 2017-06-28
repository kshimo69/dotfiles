let g:netrw_nogx = 1 " disable netrw's gx mapping.
" カーソル下のURLをブラウザで開く
nmap gx <Plug>(openbrowser-smart-search)
vmap gx <Plug>(openbrowser-smart-search)
" カーソル下のキーワードをググる
nnoremap go :<C-u>OpenBrowserSearch<Space><C-r><C-w><Enter>
vmap go <Plug>(openbrowser-open)
