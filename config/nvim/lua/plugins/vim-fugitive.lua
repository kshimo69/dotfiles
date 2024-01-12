return {
  'tpope/vim-fugitive',
  event = { "BufReadPre", "BufNewFile" },
  keys = {
    { "git", mode = "c", "<cmd>Git<cr>", desc = "OpenGit" },
  },
  dependencies = {
    'tpope/vim-rhubarb',
  },
}
