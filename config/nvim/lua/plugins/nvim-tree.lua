return {
  "nvim-tree/nvim-tree.lua",
  version = "*",
  lazy = false,
  dependencies = {
    "nvim-tree/nvim-web-devicons",
  },
  config = function()
    local function my_on_attach(bufnr)
      local api = require('nvim-tree.api')
      local function opts(desc)
        return { desc = 'nvim-tree: ' .. desc, buffer = bufnr, noremap = true, silent = true, nowait = true }
      end
      api.config.mappings.default_on_attach(bufnr)
      vim.keymap.set('n', '?', api.tree.toggle_help, opts('Help'))
    end
    require("nvim-tree").setup {
      on_attach = my_on_attach,
    }
    vim.keymap.set('n', '<leader>ee', function() vim.cmd("NvimTreeFindFile") end)
  end,
}
