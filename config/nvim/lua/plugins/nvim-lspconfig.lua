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
    {
      'williamboman/mason-lspconfig.nvim',
      config = function()
        -- v2: ensure_installed だけ指定すれば OK(自動で有効化されます)
        require('mason-lspconfig').setup({
          ensure_installed = { 'lua_ls', 'gopls' },
          -- 必要なら自動有効化を切る:
          -- automatic_enable = false,
        })
        -- nvim-cmp の capabilities を全 LSP に適用
        vim.lsp.config('*', {
          capabilities = require('cmp_nvim_lsp').default_capabilities(),
        })
        -- 個別設定: Lua
        vim.lsp.config('lua_ls', {
          settings = {
            Lua = {
              diagnostic = { globals = { 'vim' } },
            },
          },
        })
        -- 個別設定: Go
        vim.lsp.config('gopls', {
          -- ここに gopls の設定があれば追加
        })

        -- もし automatic_enable = false にした場合は手動で有効化
        -- vim.lsp.enable({ 'lua_ls', 'gopls' })
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
        -- 新 API で on_attach を紐付け
        vim.lsp.config('*', {
          on_attach = on_attach,
        })
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
