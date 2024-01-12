return {
  'numToStr/Comment.nvim',
  keys = {
    { ",,", mode = "n", "<Plug>(comment_toggle_linewise_current)", desc = "Comment toggle current line" },
    { ",,", mode = "x", "<Plug>(comment_toggle_linewise_visual)", desc = "Comment toggle linewise (visual)" },
    { "<C-_>", mode = "n", "<Plug>(comment_toggle_linewise_current)", desc = "Comment toggle current line" },
    { "<C-_>", mode = "x", "<Plug>(comment_toggle_linewise_visual)", desc = "Comment toggle linewise (visual)" },
  },
  config = function()
    require('Comment').setup()
  end
}

