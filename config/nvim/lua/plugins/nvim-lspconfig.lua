return {
  -- LSP Configuration & Plugins
  'neovim/nvim-lspconfig',
  event = { 'BufReadPre', 'BufNewFile' },
  dependencies = {
    -- Automatically install LSPs to stdpath for neovim
    {
      'williamboman/mason.nvim',
      version = "^1.0.0",
      config = true,
    },
    {
      'williamboman/mason-lspconfig.nvim',
      version = "^1.0.0",
      config = function()
        local mason_lspconfig = require("mason-lspconfig")
        mason_lspconfig.setup({
          ensure_installed = { "lua_ls" },
        })
        local capabilities = vim.lsp.protocol.make_client_capabilities()
        capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)
        mason_lspconfig.setup_handlers({ function(server_name)
          local config = {
            capabilities = capabilities,
          }
          vim.lsp.config[server_name] = config
          vim.lsp.enable(server_name)
        end,
        })
      end,
    },
    {
      'lukas-reineke/lsp-format.nvim',
      config = function()
        require('lsp-format').setup {}

        local on_attach = function(client)
          require('lsp-format').on_attach(client)
          -- ... custom code ...
        end
        vim.lsp.config.gopls = { on_attach = on_attach }
        vim.lsp.enable('gopls')
        vim.lsp.config.lua_ls = { on_attach = on_attach }
        vim.lsp.enable('lua_ls')
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
    -- vim.keymap.set('n', 'gf', '<cmd>lua vim.lsp.buf.formatting()<CR>')
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
