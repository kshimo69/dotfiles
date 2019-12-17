let g:neoterm_size = 15
let g:neoterm_autoscroll = 1
"let g:neoterm_autoinsert = 1
nnoremap <silent> <Leader>rc :TREPLSendFile<CR>
nnoremap <silent> <Leader>rl :TREPLSendLine<CR>
vnoremap <silent> <Leader>rl :TREPLSendSelection<CR>
nnoremap <silent> vt :botright Ttoggle<CR>
nnoremap <silent> vs :botright Tnew<CR>
nnoremap <silent> vc :Tclose!<CR>
" terminal modeからcommand modeに
tnoremap <silent> <ESC> <C-\><C-n>
