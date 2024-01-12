return {
  -- LSP Configuration & Plugins
  'neovim/nvim-lspconfig',
  event = { 'BufReadPre', 'BufNewFile' },
  dependencies = {
    -- Automatically install LSPs to stdpath for neovim
    {
      'williamboman/mason.nvim',
      config = true,
    },
    'williamboman/mason-lspconfig.nvim',
    {
      'lukas-reineke/lsp-format.nvim',
      config = function()
        require('lsp-format').setup {}

        local on_attach = function(client)
          require('lsp-format').on_attach(client)
          -- ... custom code ...
        end
        require('lspconfig').gopls.setup { on_attach = on_attach }
        require('lspconfig').lua_ls.setup { on_attach = on_attach }
      end
    },

    -- Useful status updates for LSP
    -- NOTE: `opts = {}` is the same as calling `require('fidget').setup({})`
    {
      'j-hui/fidget.nvim',
      opts = {},
    },

    -- Additional lua configuration, makes nvim stuff amazing!
    'folke/neodev.nvim',
  },
  config = function()
    -- keyboard shortcut
    vim.keymap.set('n', 'gh', '<cmd>lua vim.lsp.buf.hover()<CR>')
    vim.keymap.set('n', 'gf', '<cmd>lua vim.lsp.buf.formatting()<CR>')
    vim.keymap.set('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>')
    vim.keymap.set('n', 'gD', '<cmd>lua vim.lsp.buf.definition()<CR>')
    vim.keymap.set('n', 'gd', '<cmd>lua vim.lsp.buf.declaration()<CR>')
    vim.keymap.set('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>')
    vim.keymap.set('n', 'gt', '<cmd>lua vim.lsp.buf.type_definition()<CR>')
    vim.keymap.set('n', '<f2>', '<cmd>lua vim.lsp.buf.rename()<CR>')
    vim.keymap.set('n', 'ge', '<cmd>lua vim.diagnostic.open_float()<CR>')
    -- vim.keymap.set('n', 'g]', '<cmd>lua vim.diagnostic.goto_next()<CR>')
    -- vim.keymap.set('n', 'g[', '<cmd>lua vim.diagnostic.goto_prev()<CR>')
  end,
}
