let g:deoplete#enable_at_startup = 1
call deoplete#custom#option({
\ 'smart_case': v:true,
\ 'auto_complete_delay': 0,
\ 'auto_refresh_delay': 20,
\ })
" TabComplete
inoremap <expr><TAB> pumvisible() ? "\<C-n>" :
  \ neosnippet#expandable_or_jumpable() ?
  \ "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
snoremap <expr><TAB> neosnippet#expandable_or_jumpable() ?
  \ "\<Plug>(neosnippet_expand_or_jump)" : "\<TAB>"
" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><C-h> deoplete#smart_close_popup()."\<C-h>"
inoremap <expr><BS>  deoplete#smart_close_popup()."\<C-h>"
" <CR>: close popup and save indent.
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function() abort
  return deoplete#close_popup() . "\<CR>"
endfunction
" <C-g>: undo completion
inoremap <expr><C-g> deoplete#undo_completion()
" close the preview window after completion is done
au MyAutoCmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif
" do not show preview window when completion
set completeopt-=preview
