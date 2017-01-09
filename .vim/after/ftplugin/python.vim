if exists('b:did_ftplugin_python')
  finish
endif
let b:did_ftplugin_python = 1

" PEP8 Indent rule
setl tabstop=4
setl softtabstop=4
setl shiftwidth=4
setl smarttab
setl expandtab
setl autoindent
setl nosmartindent
setl cindent
" setl textwidth=80
setl textwidth=0  "80文字で自動改行されるのを無効化
" setl colorcolumn=80
" setl commentstring=#%s

" Folding
" setl foldmethod=indent
" setl foldlevel=99

compiler nose

" http://hachibeechan.hateblo.jp/entry/vim-customize-for-python
" xmap <buffer> af <Plug>(textobj-python-function-a)
" omap <buffer> af <Plug>(textobj-python-function-a)
" xmap <buffer> if <Plug>(textobj-python-function-i)
" omap <buffer> if <Plug>(textobj-python-function-i)
" xmap <buffer> ac <Plug>(textobj-python-class-a)
" omap <buffer> ac <Plug>(textobj-python-class-a)
" xmap <buffer> ic <Plug>(textobj-python-class-i)
" omap <buffer> ic <Plug>(textobj-python-class-i)

" setlocal omnifunc=jedi#completions

if version < 600
  syntax clear
elseif exists('b:current_after_syntax')
  finish
endif

" We need nocompatible mode in order to continue lines with backslashes.
" Original setting will be restored.
let s:cpo_save = &cpo
set cpo&vim

syn match pythonOperator "\(+\|=\|-\|\^\|\*\)"
syn match pythonDelimiter "\(,\|\.\|:\)"
syn keyword pythonSpecialWord self

hi link pythonSpecialWord    Special
hi link pythonDelimiter      Special

let b:current_after_syntax = 'python'

let &cpo = s:cpo_save
unlet s:cpo_save
