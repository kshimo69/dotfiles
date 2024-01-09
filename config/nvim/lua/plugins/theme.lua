return {
  {
    'cocopon/iceberg.vim',
    event = "VimEnter",
    config = function()
      vim.cmd.colorscheme "iceberg"
      vim.cmd[[
        " フォーカスしてないときの背景色
        let g:InactiveBackGround = 'ctermbg=235 guibg=#1e2132'
        " Neovim内でフォーカスしていないペインの背景色設定
        execute ('hi NormalNC '.g:InactiveBackGround)
        execute ('hi NontextNC '.g:InactiveBackGround)
        execute ('hi SpecialkeyNC '.g:InactiveBackGround)
        execute ('hi EndOfBufferNC '.g:InactiveBackGround)
      ]]
    end,
  }
}
