" バッファ一覧
nnoremap <silent> ,b :<C-u>Denite buffer -auto-resize<CR>
" コマンド一覧
nnoremap <silent> ,c :<C-u>Denite command -auto-resize<CR>
" ファイル一覧
nnoremap <silent> ,f :<C-u>Denite file -auto-resize<CR>
" バッファ、ファイル一覧
nnoremap <silent> <Space>l :<C-u>DeniteBufferDir buffer file_rec -auto-resize<CR>
nnoremap <silent> <Space>L :<C-u>Denite buffer file_rec -auto-resize<CR>
" アウトライン
nnoremap <silent> ,o :<C-u>Denite outline -auto-resize<CR>
" タグ
nnoremap <silent> ,e :<C-u>Denite tag -auto-resize<CR>
" grep
nnoremap <silent> ,g :<C-u>DeniteCursorWord grep -no-empty -auto-resize<CR>
nnoremap <silent> ,G :<C-u>Denite grep -no-empty -auto-resize<CR>
" resume
nnoremap <silent> ,r :<C-u>Denite -resume<CR>
" memolist
"nnoremap <Space>ml :<C-u>call denite#start([{'name': 'file_rec', 'args': [g:memolist_path]}])<CR>
