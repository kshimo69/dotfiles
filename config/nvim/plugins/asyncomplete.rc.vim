"let g:asyncomplete_auto_popup = 0
" let g:asyncomplete_auto_completeopt = 0
let g:asyncomplete_popup_delay = 200

function! s:check_back_space() abort
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~ '\s'
endfunction

inoremap <silent><expr> <C-j>
  \ pumvisible() ? "\<C-n>" :
  \ <SID>check_back_space() ? "\<C-j>" :
  \ asyncomplete#force_refresh()
inoremap <expr><C-k> pumvisible() ? "\<C-p>" : "\<C-k>"
inoremap <expr><cr>  pumvisible() ? "\<C-y>" : "\<cr>"
imap <c-space> <Plug>(asyncomplete_force_refresh)

set completeopt+=menuone
"set completeopt+=menuone,noinsert,noselect,preview

"autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif
