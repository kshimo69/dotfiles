return {
  'lukas-reineke/indent-blankline.nvim',
  main = "ibl",
  event = { "BufReadPre", "BufNewFile" },
  opts = {},
  config = function()
    local hooks = require "ibl.hooks"
    hooks.register(
      hooks.type.WHITESPACE,
      hooks.builtin.hide_first_space_indent_level
    )
    require("ibl").setup {
      indent = { char = "|" },
      whitespace = { highlight = { "Whitespace", "NonText" } },
      scope = { enabled = true },
    }
  end
}

