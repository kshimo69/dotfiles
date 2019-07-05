" jediにvimの設定を任せると'completeopt+=preview'するので
" 自動設定機能をOFFにし手動で設定を行う
let g:jedi#auto_vim_configuration = 0
" 補完の最初の項目が選択された状態だと使いにくいためオフにする
let g:jedi#popup_select_first = 0
" goto
let g:jedi#goto_assignments_command = "<leader>g"
" 定義元
let g:jedi#get_definitions_command = "<leader>d"
" pydoc
let g:jedi#documentation_command = "K"
" quickrunと被るため大文字に変更
let g:jedi#rename_command = '<Leader>R'
