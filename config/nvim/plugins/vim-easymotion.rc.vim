let g:EasyMotion_do_mapping = 0
let g:EasyMotion_smartcase = 1

"nmap <Leader>s <Plug>(easymotion-s2)
"xmap <Leader>s <Plug>(easymotion-s2)
"omap <Leader>s <Plug>(easymotion-s2)
"もしくはこんな感じがオススメ
"map <Space> <Plug>(easymotion-s2)
"
map f <Plug>(easymotion-bd-fl)
map t <Plug>(easymotion-bd-tl)

map <Leader>w <Plug>(easymotion-bd-w)

let g:EasyMotion_keys = ';HKLYUIOPNM,QWERTASDGZXCVBJF'
" Show target key with upper case to improve readability
let g:EasyMotion_use_upper = 1
" Jump to first match with enter & space
let g:EasyMotion_enter_jump_first = 1
let g:EasyMotion_space_jump_first = 1

" Extend search motions with vital-over command line interface
" Incremental highlight of all the matches
" Now, you don't need to repetitively press `n` or `N` with EasyMotion feature
" `<Tab>` & `<S-Tab>` to scroll up/down a page of next match
" :h easymotion-command-line
"nmap g/ <Plug>(easymotion-sn)
"xmap g/ <Plug>(easymotion-sn)
"omap g/ <Plug>(easymotion-tn)
nmap <Leader>s <Plug>(easymotion-sn)
xmap <Leader>s <Plug>(easymotion-sn)
omap <Leader>s <Plug>(easymotion-tn)
" Support mappings feature
"EMCommandLineNoreMap <Space> <CR>
"EMCommandLineNoreMap ; <CR>
"EMCommandLineNoreMap <C-j> <Space>

"set nohlsearch
"map  / <Plug>(easymotion-sn)
"omap / <Plug>(easymotion-tn)
"map  n <Plug>(easymotion-next)
"map  N <Plug>(easymotion-prev)
" いらなくなる
" nmap <silent> <Esc><Esc> :<C-u>nohlsearch<CR>
