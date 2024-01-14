-- 自分で書くautocmdはすべてMyAutoCmdグループに入れるので先頭で初期化する
-- release autogroup in MyAutoCmd
vim.api.nvim_create_augroup('MyAutoCmd', { clear = true })

require('keys')
require('lazyvim')
require('options')
