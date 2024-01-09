-- 自分で書くautocmdはすべてMyAutoCmdグループに入れるので先頭で初期化する
-- release autogroup in MyAutoCmd
vim.cmd[[
augroup MyAutoCmd
  autocmd!
augroup END
]]

require('keys')
require('lazyvim')
require('options')
