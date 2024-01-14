-- Set <space> as the leader key
-- See `:help mapleader`
--  NOTE: Must happen before plugins are required (otherwise wrong leader will be used)
local opts = { noremap = true, silent = true }
local keymap = vim.api.nvim_set_keymap
keymap('', '<Space>', '<Nop>', opts)
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- 入力モード中に素早くjjと入力した場合はESCとみなす
keymap('i', 'jj', '<Esc>', opts)

-- Insertモードで矢印で移動した時にUndo blockを途切れさせない
keymap('i', '<Left>', '<C-G>U<Left>', opts)
keymap('i', '<Right>', '<C-G>U<Right>', opts)

-- Insertモードを抜けたら日本語入力OFF
vim.cmd[[
if has('gui_running')
  inoremap <silent> <ESC> <ESC>:set iminsert=2<CR>
endif
]]

-- ESCを二回押すことでハイライトを無効に
keymap('n', '<Esc><Esc>', ':nohlsearch<CR>', opts)

-- カーソル下のキーワードでヘルプ
keymap('n', '<C-h>', ':<C-u>help<Space><C-r><C-w><Enter>', opts)

-- 検索後にジャンプした際に検索単語を画面中央に持ってくる
keymap('n', 'n', 'nzz', opts)
keymap('n', 'N', 'Nzz', opts)
keymap('n', '*', '*zz', opts)
keymap('n', '#', '#zz', opts)
keymap('n', 'g*', 'g*zz', opts)
keymap('n', 'g#', 'g#zz', opts)
--keymap('n', 'G', 'Gzz', opts)

-- Yでカーソル位置から行末までコピー
keymap('n', 'Y', 'y$', opts)

-- vvで行末まで選択
keymap('v', 'v', '^$h', opts)

-- j, k による移動を見た目の行に
keymap('n', 'j', 'gj', opts)
keymap('n', 'k', 'gk', opts)

-- T + ? で各種設定をトグル
keymap('n', '[toggle]', '<Nop>', opts)
keymap('n', 'T', '[toggle]', {})
keymap('n', '[toggle]s', ':setl spell!<CR>:setl spell?<CR>', opts)
keymap('n', '[toggle]l', ':setl list!<CR>:setl list?<CR>', opts)
keymap('n', '[toggle]n', ':setl nu!<CR>:setl nu?<CR>', opts)
keymap('n', '[toggle]t', ':setl expandtab!<CR>:setl expandtab?<CR>', opts)
keymap('n', '[toggle]w', ':setl wrap!<CR>:setl wrap?<CR>', opts)
keymap('n', '[toggle]c', ':setl cursorline!<CR>:setl cursorline?<CR>', opts)
keymap('n', '[toggle]h', ':if exists("syntax_on") <Bar> syntax off <Bar> else <Bar> syntax enable <Bar> endif<CR>', opts)
--nnoremap <expr> [toggle]a deoplete#toggle()
--keymap('n', '[toggle]i', ':IndentLinesToggle<CR>', opts)

-- w!! でスーパーユーザーとして保存（sudoが使える環境限定）
keymap('c', 'w!!', 'w !sudo tee > /dev/null %', opts)

-- QuickFixの操作
-- keymap('n', '<C-j>', ':<C-u>cnext<CR>', opts)
-- keymap('n', '<C-k>', ':<C-u>cprevious<CR>', opts)
-- keymap('n', '<C-q>', ':<C-u>cclose<CR>', opts)

-- タブ
--keymap('n', 'tt', ':<c-u>VimFilerTab<cr>', opts)
keymap('n', 'tf', ':<c-u>tabfirst<cr>', opts)
keymap('n', 'tl', ':<c-u>tablast<cr>', opts)
keymap('n', 'tn', ':<c-u>tabnext<cr>', opts)
keymap('n', 'tN', ':<c-u>tabNext<cr>', opts)
keymap('n', 'tp', ':<c-u>tabprevious<cr>', opts)
keymap('n', 'te', ':<c-u>tabedit<cr>', opts)
keymap('n', 'tc', ':<c-u>tablast <bar> tabnew<cr>', opts)
keymap('n', 'tx', ':<c-u>tabclose<cr>', opts)
keymap('n', 'to', ':<c-u>tabonly<cr>', opts)
keymap('n', 'ts', ':<c-u>tabs<cr>', opts)

-- コマンドラインモード
keymap('c', '<C-b>', '<Left>', { noremap = true })
keymap('c', '<C-f>', '<Right>', { noremap = true })
keymap('c', '<C-n>', '<Down>', { noremap = true })
keymap('c', '<C-p>', '<Up>', { noremap = true })
keymap('c', '<C-a>', '<Home>', { noremap = true })
keymap('c', '<C-e>', '<End>', { noremap = true })
keymap('c', '<C-d>', '<Del>', { noremap = true })
