function! s:on_lsp_buffer_enabled() abort
  setlocal omnifunc=lsp#complete
  setlocal signcolumn=yes
  nmap <buffer> gd <plug>(lsp-definition)
  nmap <buffer> <C-]> <plug>(lsp-definition)
  nmap <buffer> <f2> <plug>(lsp-rename)
  nmap <buffer> <Leader>d <plug>(lsp-type-definition)
  nmap <buffer> <Leader>a <plug>(lsp-peek-type-definition)
  nmap <buffer> <Leader>r <plug>(lsp-references)
  nmap <buffer> <Leader>i <plug>(lsp-implementation)
  nmap <buffer> gh <plug>(lsp-hover)
  inoremap <expr> <cr> pumvisible() ? "\<c-y>\<cr>" : "\<cr>"
endfunction

augroup lsp_install
  au!
  autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
augroup END
command! LspDebug let lsp_log_verbose=1 | let lsp_log_file = expand('~/lsp.log')

let g:lsp_diagnostics_enabled = 1
let g:lsp_diagnostics_echo_cursor = 1
let g:lsp_diagnostics_float_cursor = 0
let g:lsp_text_edit_enabled = 1
let g:lsp_preview_float = 1

" go
let g:lsp_settings_filetype_go = ['gopls', 'golangci-lint-langserver']
