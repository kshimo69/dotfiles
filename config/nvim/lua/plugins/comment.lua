return {
  'numToStr/Comment.nvim',
  event = "VeryLazy",
  config = function()
    require('Comment').setup()
    vim.keymap.set('n', ',,', '<Plug>(comment_toggle_linewise_current)', { desc = 'Comment toggle current line' })
    vim.keymap.set('x', ',,', '<Plug>(comment_toggle_linewise_visual)', { desc = 'Comment toggle current line' })
  end
}

