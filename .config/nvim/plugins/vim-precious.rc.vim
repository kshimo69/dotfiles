" NORMALモードのカーソル移動中に頻繁に切り替わるとうざいのでデフォは無効化しておく(helpは例外)
"let g:precious_enable_switch_CursorMoved = { '*': 0, 'help': 1 }
" helpも
let g:precious_enable_switch_CursorMoved = { '*': 0 }
" INSERTモードのON／OFFに合わせてトグル
autocmd MyAutoCmd InsertEnter * :PreciousSwitch
autocmd MyAutoCmd InsertLeave * :PreciousReset
