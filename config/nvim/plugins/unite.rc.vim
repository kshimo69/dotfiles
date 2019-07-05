" 大文字小文字区別しない
let g:unite_enable_ignore_case = 1
let g:unite_enable_smart_case = 1
" uniteのウインドウの高さ
let g:unite_winheight=15
" カーソル行の色
let g:unite_cursor_line_highlight="CursorLine"
" unite grepにagを使う
if executable('ag')
  let g:unite_source_grep_command = 'ag'
  let g:unite_source_grep_default_opts = '--nogroup --nocolor --column'
  let g:unite_source_grep_recursive_opt = ''
endif
" unite.vim上でのキーマッピング
au MyAutoCmd FileType unite call s:unite_my_settings()
function! s:unite_my_settings()
  " 単語単位からパス単位で削除するように変更
  "imap <buffer> <C-w> <Plug>(unite_delete_backward_path)
  " ESCキーを2回押すと終了する
  nmap <silent><buffer> <ESC><ESC> q
  imap <silent><buffer> <ESC><ESC> <ESC>q
  "入力モードのときjjでノーマルモードに移動
  imap <buffer> jj <Plug>(unite_insert_leave)
endfunction
