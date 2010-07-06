"=============================================================================
" FILE: iexe.vim
" AUTHOR: Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 31 May 2010
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

let s:last_interactive_bufnr = 1

function! vimshell#internal#iexe#execute(program, args, fd, other_info)"{{{
  " Interactive execute command.
  let [l:args, l:options] = vimshell#parser#getopt(a:args, 
        \{ 'arg=' : ['--encoding']
        \})
  if !has_key(l:options, '--encoding')
    let l:options['--encoding'] = &termencoding
  endif
  
  if empty(l:args)
    return 0
  endif

  if has_key(s:interactive_option, fnamemodify(l:args[0], ':r'))
    for l:arg in vimshell#parser#split_args(s:interactive_option[fnamemodify(l:args[0], ':r')])
      call add(l:args, l:arg)
    endfor
  endif

  if vimshell#iswin() && l:args[0] =~ 'cmd\%(\.exe\)\?'
    " Run cmdproxy.exe instead of cmd.exe.
    if !executable('cmdproxy.exe')
      call vimshell#error_line(a:fd, 'cmdproxy.exe is not found. Please install it.')
      return 0
    endif

    let l:args[0] = 'cmdproxy.exe'
  endif
  
  " Encoding conversion.
  if l:options['--encoding'] != '' && l:options['--encoding'] != &encoding
    call map(l:args, 'iconv(v:val, &encoding, l:options["--encoding"])')
  endif

  if exists('b:interactive') && b:interactive.process.is_valid
    " Delete zombee process.
    call vimshell#interactive#force_exit()
  endif

  " Initialize.
  try
    let l:sub = vimproc#ptyopen(l:args)
  catch 'list index out of range'
    let l:error = printf('File: "%s" is not found.', l:args[0])

    call vimshell#error_line(a:fd, l:error)

    return 0
  endtry

  call s:init_bg(l:sub, l:args, a:fd, a:other_info)

  " Set variables.
  let b:interactive = {
        \ 'process' : l:sub, 
        \ 'fd' : a:fd, 
        \ 'encoding' : l:options['--encoding'],
        \ 'is_secret': 0, 
        \ 'prompt_history' : {}, 
        \ 'command_history' : vimshell#interactive#load_history(), 
        \ 'is_pty' : (!vimshell#iswin() || (l:args[0] == 'fakecygpty')),
        \ 'is_background': 0, 
        \ 'args' : l:args,
        \}

  call vimshell#interactive#execute_pty_out(1)

  startinsert!

  return 1
endfunction"}}}

function! vimshell#internal#iexe#vimshell_iexe(args)"{{{
  call vimshell#internal#iexe#execute('iexe', vimshell#parser#split_args(a:args), {'stdin' : '', 'stdout' : '', 'stderr' : ''}, {'is_interactive' : 0})
endfunction"}}}

function! vimshell#internal#iexe#default_settings()"{{{
  setlocal buftype=nofile
  setlocal noswapfile
  setlocal wrap
  setlocal tabstop=8
  setlocal omnifunc=vimshell#complete#interactive_history_complete#omnifunc

  " Set syntax.
  syn region   InteractiveError   start=+!!!+ end=+!!!+ contains=InteractiveErrorHidden oneline
  syn match   InteractiveErrorHidden            '!!!' contained
  syn match   InteractivePrompt         '^->\s\|^\.\.\.$'
  syn match   InteractiveMessage   '\*\%(Exit\|Killed\)\*'
  
  hi def link InteractiveMessage WarningMsg
  hi def link InteractiveError Error
  hi def link InteractiveErrorHidden Ignore
  if has('gui_running')
    hi InteractivePrompt  gui=UNDERLINE guifg=#80ffff guibg=NONE
  else
    hi def link InteractivePrompt Identifier
  endif

  " Plugin key-mappings.
  inoremap <buffer><silent> <Plug>(vimshell_int_previous_history)  <ESC>:<C-u>call vimshell#int_mappings#previous_command()<CR>
  inoremap <buffer><silent> <Plug>(vimshell_int_next_history)  <ESC>:<C-u>call vimshell#int_mappings#next_command()<CR>
  inoremap <buffer><silent> <Plug>(vimshell_int_move_head)  <ESC>:<C-u>call vimshell#int_mappings#move_head()<CR>
  inoremap <buffer><silent> <Plug>(vimshell_int_delete_line)  <ESC>:<C-u>call vimshell#int_mappings#delete_line()<CR>
  inoremap <buffer><silent> <Plug>(vimshell_int_execute_line)       <ESC>:<C-u>call vimshell#int_mappings#execute_line(1)<CR>
  inoremap <buffer><silent> <Plug>(vimshell_int_interrupt)       <C-o>:<C-u>call <SID>on_interrupt(bufname('%'))<CR>
  inoremap <buffer><expr> <Plug>(vimshell_int_delete_backword_char)  vimshell#int_mappings#delete_backword_char(0)
  inoremap <buffer><expr> <Plug>(vimshell_int_another_delete_backword_char)  vimshell#int_mappings#delete_backword_char(1)
  inoremap <buffer><expr> <Plug>(vimshell_int_history_complete)  vimshell#complete#interactive_history_complete#complete()

  imap <buffer> <C-h>     <Plug>(vimshell_int_delete_backword_char)
  imap <buffer> <BS>     <Plug>(vimshell_int_delete_backword_char)
  imap <buffer><expr> <TAB>   pumvisible() ? "\<C-n>" : vimshell#complete#interactive_command_complete#complete()
  imap <buffer> <C-a>     <Plug>(vimshell_int_move_head)
  imap <buffer> <C-u>     <Plug>(vimshell_int_delete_line)
  inoremap <expr> <SID>(bs-ctrl-])    getline('.')[col('.') - 2] ==# "\<C-]>" ? "\<BS>" : ''
  imap <buffer> <C-]>               <C-]><SID>(bs-ctrl-])
  imap <buffer> <CR>      <C-]><Plug>(vimshell_int_execute_line)
  imap <buffer> <C-c>     <Plug>(vimshell_int_interrupt)
  imap <buffer> <C-k>  <Plug>(vimshell_int_history_complete)

  nnoremap <buffer><silent> <Plug>(vimshell_int_previous_prompt)  :<C-u>call vimshell#int_mappings#previous_prompt()<CR>
  nnoremap <buffer><silent> <Plug>(vimshell_int_next_prompt)  :<C-u>call vimshell#int_mappings#next_prompt()<CR>
  nnoremap <buffer><silent> <Plug>(vimshell_int_execute_line)  :<C-u>call vimshell#int_mappings#execute_line(0)<CR><ESC>
  nnoremap <buffer><silent> <Plug>(vimshell_int_paste_prompt)  :<C-u>call vimshell#int_mappings#paste_prompt()<CR>
  nnoremap <buffer><silent> <Plug>(vimshell_int_interrupt)       :<C-u>call <SID>on_interrupt(bufname('%'))<CR>
  nnoremap <buffer><silent> <Plug>(vimshell_int_exit)       :<C-u>call <SID>on_exit()<CR>
  nnoremap <buffer><silent> <Plug>(vimshell_int_restart_command)       :<C-u>call vimshell#int_mappings#restart_command()<CR>

  nmap <buffer> <C-p>     <Plug>(vimshell_int_previous_prompt)
  nmap <buffer> <C-n>     <Plug>(vimshell_int_next_prompt)
  nmap <buffer> <CR>      <Plug>(vimshell_int_execute_line)
  nmap <buffer> <C-y>     <Plug>(vimshell_int_paste_prompt)
  nmap <buffer> <C-r>     <Plug>(vimshell_int_restart_command)
  nmap <buffer> <C-c>     <Plug>(vimshell_int_interrupt)
  nmap <buffer> q         <Plug>(vimshell_int_exit)
endfunction"}}}

function! s:init_bg(sub, args, fd, other_info)"{{{
  " Save current directiory.
  let l:cwd = getcwd()

  " Init buffer.
  if a:other_info.is_interactive
    let l:context = a:other_info
    let l:context.fd = a:fd
    call vimshell#print_prompt(l:context)
  endif
  " Split nicely.
  call vimshell#split_nicely()

  edit `=fnamemodify(a:args[0], ':r').'@'.(bufnr('$')+1)`
  lcd `=l:cwd`

  call vimshell#internal#iexe#default_settings()
  
  execute 'set filetype=int-'.fnamemodify(a:args[0], ':r')

  " Set autocommands.
  augroup vimshell_iexe
    autocmd InsertEnter <buffer>       call s:insert_enter()
    autocmd InsertLeave <buffer>       call s:insert_leave()
    autocmd BufUnload <buffer>       call s:on_interrupt(expand('<afile>'))
    autocmd BufWinLeave,WinLeave <buffer>       let s:last_interactive_bufnr = expand('<afile>')
    autocmd CursorHoldI <buffer>  call s:on_hold_i()
  augroup END

  $

  startinsert!
endfunction"}}}

function! s:insert_enter()"{{{
  let s:save_updatetime = &updatetime
  let &updatetime = g:vimshell_interactive_update_time
endfunction"}}}
function! s:insert_leave()"{{{
  let &updatetime = s:save_updatetime
endfunction"}}}
function! s:on_hold_i()"{{{
  call vimshell#interactive#check_output(b:interactive, bufnr('%'), bufnr('%'))
  
  if b:interactive.process.is_valid
    if pumvisible()
      call feedkeys("\<C-e>\<C-r>\<ESC>", 'n')
    else
      call feedkeys("\<C-r>\<ESC>", 'n')
    endif
  endif
endfunction"}}}
function! s:on_interrupt(afile)"{{{
  call vimshell#interactive#hang_up(a:afile)
endfunction "}}}
function! s:on_exit()"{{{
  if !b:interactive.process.is_valid
    bdelete
  endif  
endfunction "}}}

" Interactive options."{{{
if vimshell#iswin()
  " Windows only.
  let s:interactive_option = {
        \ 'bash' : '-i', 'bc' : '-i', 'irb' : '--inf-ruby-mode', 
        \ 'gosh' : '-i', 'python' : '-i', 'zsh' : '-i', 
        \ 'powershell' : '-Command -', 
        \ 'termtter'   : '--monochrome', 
        \ 'scala'   : '--Xnojline', 'nyaos' : '-t',
        \}
else
  let s:interactive_option = {
        \'termtter' : '--monochrome', 
        \}
endif"}}}

" Command functions.
function! s:send_string(line1, line2, string)"{{{
  let l:winnr = bufwinnr(s:last_interactive_bufnr)
  if l:winnr <= 0
    return
  endif
  
  " Check alternate buffer.
  if getwinvar(l:winnr, '&filetype') =~ '^int-'
    if a:string != ''
      let l:string = a:string . "\<LF>"
    else
      let l:string = join(getline(a:line1, a:line2), "\<LF>") . "\<LF>"
    endif
    let l:line = split(l:string, "\<LF>")[0]
    
    execute winnr('#') 'wincmd w'

    " Save prompt.
    let l:prompt = vimshell#interactive#get_prompt(line('$'))
    let l:prompt_nr = line('$')
    
    " Send string.
    call vimshell#interactive#send_string(l:string)
    
    call setline(l:prompt_nr, l:prompt . l:line)
  endif
endfunction"}}}

command! -range -nargs=? VimShellSendString call s:send_string(<line1>, <line2>, <q-args>)

