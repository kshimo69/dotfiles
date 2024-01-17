return {
  -- Autocompletion
  'hrsh7th/nvim-cmp',
  event = "InsertEnter",
  dependencies = {
    -- Snippet Engine & its associated nvim-cmp source
    'L3MON4D3/LuaSnip',
    'saadparwaiz1/cmp_luasnip',

    -- Adds LSP completion capabilities
    'hrsh7th/cmp-nvim-lsp',
    'hrsh7th/cmp-buffer',
    'hrsh7th/cmp-path',

    -- Adds a number of user-friendly snippets
    'rafamadriz/friendly-snippets',
  },
  config = function()
    local cmp = require('cmp')
    local luasnip = require 'luasnip'
    require('luasnip.loaders.from_vscode').lazy_load()
    luasnip.config.setup {}
    cmp.setup({
      enabled = true,
      completion = {
        completeopt = 'menu,menuone,noinsert',
      },
      snippet = {
        expand = function(args)
          luasnip.lsp_expand(args.body)
        end,
      },
      mapping = cmp.mapping.preset.insert({
        ['<C-n>'] = cmp.mapping.select_next_item(),
        ['<C-p>'] = cmp.mapping.select_prev_item(),
        ['<C-j>'] = cmp.mapping.select_next_item(),
        ['<C-k>'] = cmp.mapping.select_prev_item(),
        ['<C-d>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-Space>'] = cmp.mapping.complete {},
        ['<CR>'] = cmp.mapping.confirm {
          behavior = cmp.ConfirmBehavior.Replace,
          select = true,
        },
        ['<Tab>'] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_next_item()
          elseif luasnip.expand_or_locally_jumpable() then
            luasnip.expand_or_jump()
          else
            fallback()
          end
        end, { 'i', 's' }),
        ['<S-Tab>'] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_prev_item()
          elseif luasnip.locally_jumpable(-1) then
            luasnip.jump(-1)
          else
            fallback()
          end
        end, { 'i', 's' }),
      }),
      window = {
        completion = cmp.config.window.bordered(),
        documentation = cmp.config.window.bordered(),
      },
      sources = cmp.config.sources({
        { name = 'nvim_lsp' },
        { name = 'luasnip' },
        { name = 'buffer' },
        { name = 'path' },
      }),
      experimental = {
        ghost_text = true,
      },
    })

    vim.cmd [[
    highlight LspReferenceText  cterm=underline ctermfg=1 ctermbg=8 gui=underline guifg=grey guibg=lightyellow
    highlight LspReferenceRead  cterm=underline ctermfg=1 ctermbg=8 gui=underline guifg=grey guibg=lightyellow
    highlight LspReferenceWrite cterm=underline ctermfg=1 ctermbg=8 gui=underline guifg=grey guibg=lightyellow
    augroup lsp_document_highlight
      autocmd!
      autocmd CursorHold,CursorHoldI * lua vim.lsp.buf.document_highlight()
      autocmd CursorMoved,CursorMovedI * lua vim.lsp.buf.clear_references()
    augroup END
    ]]
    -- vim.api.nvim_create_autocmd({ 'CursorHold', 'CursorHoldI' }, {
    --   pattern = '*',
    --   group = 'MyAutoCmd',
    --   command = 'lua vim.lsp.buf.document_highlight()',
    -- })
    -- vim.api.nvim_create_autocmd({ 'CursorMoved', 'CursorMovedI' }, {
    --   pattern = '*',
    --   group = 'MyAutoCmd',
    --   command = 'lua vim.lsp.buf.clear_references()',
    -- })
  end,
}
