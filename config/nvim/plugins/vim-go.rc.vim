au MyAutoCmd FileType go nmap <Leader>r <Plug>(go-run)
au MyAutoCmd FileType go nmap <Leader>d <Plug>(go-def-split)
au MyAutoCmd FileType go nmap <Leader>D <Plug>(go-def-vertical)
au MyAutoCmd FileType go nmap <Leader>t <Plug>(go-test)
au MyAutoCmd FileType go nmap <Leader>u <Plug>(go-test-func)
au MyAutoCmd FileType go nmap <Leader>c <Plug>(go-coverage-toggle)
au MyAutoCmd FileType go nmap <Leader>p <Plug>(go-def-pop)

" run :GoBuild or :GoTestCompile based on the go file
function! s:build_go_files()
  let l:file = expand('%')
  if l:file =~# '^\f\+_test\.go$'
    call go#test#Test(0, 1)
  elseif l:file =~# '^\f\+\.go$'
    call go#cmd#Build(0)
  endif
endfunction
au MyAutoCmd FileType go nmap <Leader>b :<C-u>call <SID>build_go_files()<CR>

" ロケーションリストを使わずクイックフィックスだけ使う
let g:go_list_type = "quickfix"

" 保存時に自動import
let g:go_fmt_command = "goimports"

" 保存時にチェック(GoLint, GoVet, GoErrCheck)
let g:go_metalinter_autosave = 1
" 構文チェックだけにする
"let g:go_metalinter_autosave_enabled = ['vet']

" GoRunやGoTestで水平分割
let g:go_term_mode = 'split'
let g:go_term_height = 20

" ハイライト
"let g:go_highlight_types = 1
"let g:go_highlight_fields = 1
"let g:go_highlight_functions = 1
"let g:go_highlight_function_calls = 1

" 代替ファイルをどう開くか
au MyAutoCmd Filetype go command! -bang A call go#alternate#Switch(<bang>0, 'edit')
au MyAutoCmd Filetype go command! -bang AV call go#alternate#Switch(<bang>0, 'vsplit')
au MyAutoCmd Filetype go command! -bang AS call go#alternate#Switch(<bang>0, 'split')
au MyAutoCmd Filetype go command! -bang AT call go#alternate#Switch(<bang>0, 'tabe')

" importしていないパッケージも補完する
let g:go_gocode_unimported_packages = 1

" go-mod用
let g:go_def_mode = 'godef'
