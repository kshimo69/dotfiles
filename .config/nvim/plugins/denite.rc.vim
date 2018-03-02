" バッファ一覧
nnoremap <silent> ,b :<C-u>Denite buffer file_mru<CR>
" コマンド一覧
nnoremap <silent> ,c :<C-u>Denite command<CR>
" ファイル一覧
nnoremap <silent> ,f :<C-u>DeniteBufferDir file<CR>
" バッファ、ファイル一覧
nnoremap <silent> <Space>l :<C-u>DeniteBufferDir buffer file_mru file_rec<CR>
nnoremap <silent> <Space>L :<C-u>Denite file_rec<CR>
" アウトライン
nnoremap <silent> ,o :<C-u>Denite outline<CR>
" タグ
nnoremap <silent> ,e :<C-u>Denite tag<CR>
" grep
"nnoremap <silent> ,g :<C-u>DeniteCursorWord grep -buffer-name=search -no-empty -auto-resize<CR>
nnoremap <silent> ,g :<C-u>DeniteBufferDir grep -buffer-name=search -no-empty<CR>
nnoremap <silent> ,G :<C-u>Denite grep -buffer-name=search -no-empty<CR>
" resume
nnoremap <silent> ,r :<C-u>Denite -resume -buffer-name=search<CR>
" memolist
"nnoremap <Space>ml :<C-u>call denite#start([{'name': 'file_rec', 'args': [g:memolist_path]}])<CR>
" gtags
nnoremap <silent> ,t :<C-u>DeniteCursorWord gtags_context -buffer-name=gtags -no-empty<CR>
nnoremap <silent> ,T :<C-u>Denite -resume -buffer-name=gtags<CR>
" ヤンク履歴
nnoremap <silent> <C-p> :<C-u>Denite neoyank<CR>
