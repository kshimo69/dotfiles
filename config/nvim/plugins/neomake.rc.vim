au MyAutoCmd BufEnter,BufWritePost * Neomake
"call neomake#configure#automake('rw', 500)
"let g:neomake_verbose=3
let g:neomake_python_enabled_makers = ['flake8']
let g:neomake_python_flake8_maker = {'args': ['--max-line-length=120', '--ignore=D1,D400,D401,E265,D203,W503'],}
let g:neomake_error_sign = {'text': '>', 'texthl': 'Error'}
let g:neomake_warning_sign = {'text': '>',  'texthl': 'ErrorMsg'}
" golang
let g:neomake_go_enabled_makers = ['golint', 'govet']
