return {
  'nvim-lualine/lualine.nvim',
  event = "VimEnter",
  dependencies = {
    'nvim-tree/nvim-web-devicons',
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
