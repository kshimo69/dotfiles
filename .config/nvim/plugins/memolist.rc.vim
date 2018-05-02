let g:memolist_memo_suffix  =  "md"
let g:memolist_memo_date  =  "%Y-%m-%d %H:%M:%S"
let g:memolist_prompt_tags  =  0
let g:memolist_prompt_categories  =  0
" let g:memolist_qfixgrep  =  1
let g:memolist_vimfiler  =  1
" let g:memolist_unite = 1
" let g:memolist_unite_source = "file_rec"
" let g:memolist_unite_option = "-direction=botright -prompt-direction=top -auto-preview -start-insert"
" let g:memolist_template_dir_path  = $MY_VIMRUNTIME . '/template'
nnoremap <Leader>mn  :MemoNew<CR>
nnoremap <Leader>ml  :MemoList<CR>
nnoremap <Leader>mg  :MemoGrep<CR>
