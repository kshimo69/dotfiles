return {
  --'nvim-lualine/lualine.nvim',
  'vim-airline/vim-airline',
  --event = { "BufReadPre", "BufNewFile" },
  event = "VimEnter",
  dependencies = {
    --'nvim-tree/nvim-web-devicons',
    'vim-airline-themes',
    'vim-airline/vim-airline-themes',
  },
  config = function()
    --[[
    require('lualine').setup {
      options = {
        theme = 'iceberg',
      },
    }
    ]]
    vim.cmd[[
      let g:airline_theme = 'iceberg'
      " タブにも適用
      let g:airline#extensions#tabline#enabled = 1
      set laststatus=2
    ]]
  end
}
