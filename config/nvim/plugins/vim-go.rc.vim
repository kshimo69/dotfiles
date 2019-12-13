au MyAutoCmd FileType go nmap <Leader>r <Plug>(go-run)
au MyAutoCmd FileType go nmap <Leader>d <Plug>(go-def-split)
au MyAutoCmd FileType go nmap <Leader>D <Plug>(go-def-vertical)
au MyAutoCmd FileType go nmap <leader>t <Plug>(go-test)
au MyAutoCmd FileType go nmap ,u <Plug>(go-test-func)

" run :GoBuild or :GoTestCompile based on the go file
function! s:build_go_files()
  let l:file = expand('%')
  if l:file =~# '^\f\+_test\.go$'
    call go#test#Test(0, 1)
  elseif l:file =~# '^\f\+\.go$'
    call go#cmd#Build(0)
  endif
endfunction
au MyAutoCmd FileType go nmap <leader>b :<C-u>call <SID>build_go_files()<CR>

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
