" 上下をループする
" ジャンプ先のない項目をスキップする
" noremap <silent> <buffer> <expr> j <SID>jk(v:count1)
" noremap <silent> <buffer> <expr> k <SID>jk(-v:count1)

" function! s:jk(motion)
  " let max = line('$')
  " let list = getloclist(0)
  " if empty(list) || len(list) != max
    " let list = getqflist()
  " endif
  " let cur = line('.') - 1
  " let pos = g:V.modulo(cur + a:motion, max)
  " let m = 0 < a:motion ? 1 : -1
  " while cur != pos && list[pos].bufnr == 0
    " let pos = g:V.modulo(pos + m, max)
  " endwhile
  " return (pos + 1) . 'G'
" endfunction
