return {
  'nvim-telescope/telescope.nvim',
  tag = '0.1.5',
  event = { "BufReadPre", "BufNewFile" },
  keys = {
    { "<leader>m", "<cmd>Telescope marks<cr>", desc = "Jump to Mark" },
  },
  dependencies = {
    'nvim-lua/plenary.nvim',
  },
  config = function()
    require("telescope").setup {
      defaults = {
        mappings = {
          i = {
            ['<C-u>'] = false,
            ['<C-d>'] = false,
          },
        },
      },
    }

    local builtin = require('telescope.builtin')
    vim.keymap.set('n', '<leader>ff', builtin.find_files, { desc = 'search [F]iles' })
    vim.keymap.set('n', '<leader>fg', builtin.live_grep, { desc = 'search by [G]rep' })
    vim.keymap.set('n', '<leader>fh', builtin.help_tags, { desc = 'search [H]elp' })
    vim.keymap.set('n', '<leader>fb', builtin.buffers, { desc = 'Find existing [B]uffers' })
  end
}
