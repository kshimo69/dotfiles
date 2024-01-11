return {
  'numToStr/Comment.nvim',
  keys = {
    { ",,", mode = "n", "<Plug>(comment_toggle_linewise_current)", desc = "Comment toggle current line" },
    { ",,", mode = "x", "<Plug>(comment_toggle_linewise_visual)", desc = "Comment toggle linewise (visual)" },
  },
  config = function()
    require('Comment').setup()
  end
}

