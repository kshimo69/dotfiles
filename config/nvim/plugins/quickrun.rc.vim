" デフォルトキーマップを使用しない
let g:quickrun_no_default_key_mappings = 1

" デフォルト設定
" 時刻を表示する
" vimprocを使用する
let g:quickrun_config = {
      \ '_': {
      \   'hook/time/enable': '1',
      \   'runner': 'vimproc',
      \ },
      \}

" QuickRun強制終了
nnoremap <expr><silent> <C-c> quickrun#is_running() ? quickrun#sweep_sessions() : "\<C-c>"

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
