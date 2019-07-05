" 色の設定
let g:quickhl_manual_colors = [
    \    'cterm=NONE gui=NONE ctermfg=Black guifg=Black ctermbg=LightBlue    guibg=LightBlue',
    \    'cterm=NONE gui=NONE ctermfg=Black guifg=Black ctermbg=LightMagenta guibg=LightMagenta',
    \    'cterm=NONE gui=NONE ctermfg=Black guifg=Black ctermbg=LightYellow  guibg=LightYellow',
    \    'cterm=NONE gui=NONE ctermfg=Black guifg=Black ctermbg=LightCyan    guibg=LightCyan',
    \    'cterm=NONE gui=NONE ctermfg=Black guifg=Black ctermbg=LightGreen   guibg=LightGreen',
    \    'cterm=NONE gui=NONE ctermfg=White guifg=White ctermbg=Blue         guibg=Blue',
    \    'cterm=NONE gui=NONE ctermfg=Black guifg=Black ctermbg=Magenta      guibg=Magenta',
    \    'cterm=NONE gui=NONE ctermfg=Black guifg=Black ctermbg=Yellow       guibg=Yellow',
    \    'cterm=NONE gui=NONE ctermfg=White guifg=White ctermbg=Red          guibg=Red',
    \    'cterm=NONE gui=NONE ctermfg=Black guifg=Black ctermbg=Cyan         guibg=Cyan',
    \    'cterm=NONE gui=NONE ctermfg=Black guifg=Black ctermbg=Green        guibg=Green',
    \    'cterm=NONE gui=NONE ctermfg=White guifg=White ctermbg=DarkBlue     guibg=DarkBlue',
    \    'cterm=NONE gui=NONE ctermfg=White guifg=White ctermbg=DarkMagenta  guibg=DarkMagenta',
    \    'cterm=NONE gui=NONE ctermfg=White guifg=White ctermbg=DarkRed      guibg=DarkRed',
    \    'cterm=NONE gui=NONE ctermfg=White guifg=White ctermbg=DarkCyan     guibg=DarkCyan',
    \    'cterm=NONE gui=NONE ctermfg=White guifg=White ctermbg=DarkGreen    guibg=DarkGreen',
    \    'cterm=NONE gui=NONE ctermfg=White guifg=White ctermbg=Brown        guibg=Brown',
    \    'cterm=NONE gui=NONE ctermfg=Black guifg=Black ctermbg=LightGray    guibg=LightGray',
    \    'cterm=NONE gui=NONE ctermfg=White guifg=White ctermbg=DarkGray     guibg=DarkGray',
    \]
nmap <Leader><Space> <Plug>(quickhl-manual-this)
xmap <Leader><Space> <Plug>(quickhl-manual-this)
nmap <Leader>M <Plug>(quickhl-manual-reset)
xmap <Leader>M <Plug>(quickhl-manual-reset)
nmap <Leader>j <Plug>(quickhl-cword-toggle)
nmap <Leader>] <Plug>(quickhl-tag-toggle)
