" デフォルトキーマップを使用しない
let g:quickrun_no_default_key_mappings = 1

let g:quickrun_config = {'*': {'hook/time/enable': '1'},}
"let g:quickrun_config = {}
" C++
let g:quickrun_config['cpp/g++-preprocessor'] = {
  \   "exec"    : "%c %o %s:p",
  \   "command" : "g++",
  \   "cmdopt"  : " -P -E -std=c++11",
  \ }
" RSpec
let g:quickrun_config['ruby.rspec'] = {'command': 'spec'}
"let g:quickrun_config['ruby.rspec'] = {'command': "spec -l {line('.')}"}
au MyAutoCmd BufWinEnter,BufNewFile *_spec.rb set filetype=ruby.rspec
" rst2html
let g:quickrun_config['rst'] = {
  \ 'command': 'rst2html',
  \ 'outputter': 'browser',
  \ }
" markdown
let g:quickrun_config['markdown'] = {
  \ 'type': 'markdown/pandoc',
  \ 'cmdopt': '-s ',
  \ 'outputter': 'browser',
  \ }
" golang
let g:quickrun_config['go'] = {
  \ 'command': 'go',
  \ 'exec': ['%c run %s']
  \ }
nmap <silent> <Leader>rr <Plug>(quickrun)
