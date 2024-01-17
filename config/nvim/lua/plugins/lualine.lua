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
        theme = 'dracula',
        component_separators = '|',
        section_separators = '',
        disabled_filetypes = {
          statusline = { 'NvimTree' },
        }
      },
    }
  end
}
