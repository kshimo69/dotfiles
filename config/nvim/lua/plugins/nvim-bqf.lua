return {
  'kevinhwang91/nvim-bqf',
  tag = 'v1.1.0',
  ft = {
    'qf',
  },
  config = function()
    vim.keymap.set('n', '<C-q>', ':copen<CR>', { noremap = true, silent = true })
  end
}
