return {
  {
    'cocopon/iceberg.vim',
    config = function()
      vim.cmd.colorscheme "iceberg"
      vim.cmd [[
        " 検索の色調整
        "au MyAutoCmd ColorScheme * hi Search term=reverse ctermfg=253 ctermbg=66 guifg=#FFFFFF guibg=#455354
        "au MyAutoCmd ColorScheme * hi TabLineSel term=reverse ctermfg=255 ctermbg=33 guifg=#FFFFFF guibg=#333333
        " フォーカスしてないときの背景色
        let g:InactiveBackGround = 'ctermbg=235 guibg=#1e2132'
        " Neovim内でフォーカスしていないペインの背景色設定
        execute ('hi NormalNC '.g:InactiveBackGround)
        execute ('hi NontextNC '.g:InactiveBackGround)
        execute ('hi SpecialkeyNC '.g:InactiveBackGround)
        execute ('hi EndOfBufferNC '.g:InactiveBackGround)
        " フォーカスした時(colorscheme準拠に切替)
        au MyAutoCmd FocusGained * hi Normal ctermbg=234 guibg=#161821
        au MyAutoCmd FocusGained * hi NonText ctermbg=234 guibg=#161821
        au MyAutoCmd FocusGained * hi SpecialKey ctermbg=234 guibg=#161821
        au MyAutoCmd FocusGained * hi EndOfBuffer ctermbg=NONE guibg=NONE
        " フォーカスを外した時（フォーカスしていない時の背景色に切替)
        au MyAutoCmd FocusLost * execute('hi Normal '.g:InactiveBackGround)
        au MyAutoCmd FocusLost * execute('hi NonText '.g:InactiveBackGround)
        au MyAutoCmd FocusLost * execute('hi SpecialKey '.g:InactiveBackGround)
        au MyAutoCmd FocusLost * execute('hi EndOfBuffer '.g:InactiveBackGround)
      ]]
    end,
  }
}
