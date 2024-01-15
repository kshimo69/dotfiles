return {
  "nvim-tree/nvim-tree.lua",
  version = "*",
  lazy = false,
  dependencies = {
    "nvim-tree/nvim-web-devicons",
  },
  config = function()
    vim.keymap.set('n', '<leader>ee', function() vim.cmd("NvimTreeFindFile") end)
    require("nvim-tree").setup {}
  end,
}
