au MyAutoCmd FileType go nmap <Leader>b <Plug>(go-build)
au MyAutoCmd FileType go nmap <Leader>r <Plug>(go-run)
au MyAutoCmd FileType go nmap <Leader>d <Plug>(go-def-split)
au MyAutoCmd FileType go nmap <Leader>D <Plug>(go-def-vertical)

" ロケーションリストを使わずクイックフィックスだけ使う
"let g:go_list_type = "quickfix"

" 保存時に自動import
"let g:go_fmt_command = "goimports"

" 保存時にチェック(GoLint, GoVet, GoErrCheck)
"let g:go_metalinter_autosave = 1
" 構文チェックだけにする
"let g:go_metalinter_autosave_enabled = ['vet']

" GoRunやGoTestで水平分割
let g:go_term_mode = 'split'
