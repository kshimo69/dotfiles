au MyAutoCmd BufEnter,BufWritePost * Neomake
let g:neomake_python_enabled_makers = ['flake8']
let g:neomake_python_flake8_maker = {'args': ['--max-line-length=120', '--ignore=D1,D400,D401,E265'],}
let g:neomake_error_sign = {'text': '>', 'texthl': 'Error'}
let g:neomake_warning_sign = {'text': '>',  'texthl': 'ErrorMsg'}
