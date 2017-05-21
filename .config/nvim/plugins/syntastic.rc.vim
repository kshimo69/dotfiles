let g:syntastic_python_checkers = ["flake8"]
let g:syntastic_python_flake8_args="--max-line-length=120 --ignore D1,D400,D401,E265"
let g:syntastic_enable_signs=1
" let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list=0
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0
" let g:syntastic_mode_map = { 'mode': 'passive',
                           " \ 'active_filetypes': ['python', 'cpp', 'ruby', 'javascript'],
                           " \ 'passive_filetypes': []}
let g:syntastic_mode_map = { 'mode': 'passive',
                           \ 'active_filetypes': ['python'],
                           \ 'passive_filetypes': []}
nnoremap <Leader>sc :SyntasticCheck<CR>
nnoremap <Leader>sr :SyntasticReset<CR>
