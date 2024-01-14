return {
  'nvim-lualine/lualine.nvim',
  event = { "BufReadPre", "BufNewFile" },
  dependencies = {
    'nvim-tree/nvim-web-devicons',
    'cocopon/iceberg.vim',
  },
  config = function()
    require('lualine').setup {
      options = {
        theme = 'iceberg',
        disabled_filetypes = {
          statusline = { 'NvimTree'},
        }
      },
    }
  end
}
