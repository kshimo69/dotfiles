"<Leader>をスペースに
let mapleader = " "

" 入力モード中に素早くjjと入力した場合はESCとみなす
inoremap jj <Esc>

" ESCを二回押すことでハイライトを無効に
nnoremap <silent><Esc><Esc> :nohlsearch<CR>

" カーソル下のキーワードでヘルプ
nnoremap <C-h> :<C-u>help<Space><C-r><C-w><Enter>

" 検索後にジャンプした際に検索単語を画面中央に持ってくる
nnoremap n nzz
nnoremap N Nzz
nnoremap * *zz
nnoremap # #zz
nnoremap g* g*zz
nnoremap g# g#zz
nnoremap G Gzz

" Yでカーソル位置から行末までコピー
nnoremap Y y$

" j, k による移動を見た目の行に
nnoremap j gj
nnoremap k gk

" T + ? で各種設定をトグル
nnoremap [toggle] <Nop>
nmap T [toggle]
nnoremap <silent> [toggle]s :setl spell!<CR>:setl spell?<CR>
nnoremap <silent> [toggle]l :setl list!<CR>:setl list?<CR>
nnoremap <silent> [toggle]t :setl expandtab!<CR>:setl expandtab?<CR>
nnoremap <silent> [toggle]w :setl wrap!<CR>:setl wrap?<CR>

" w!! でスーパーユーザーとして保存（sudoが使える環境限定）
cnoremap w!! w !sudo tee > /dev/null %

" QuickFixの操作
nnoremap <C-j> :<C-u>cnext<CR>
nnoremap <C-k> :<C-u>cprevious<CR>

" タブ
nnoremap <silent> tt :<c-u>VimFilerTab<cr>
nnoremap <silent> tf :<c-u>tabfirst<cr>
nnoremap <silent> tl :<c-u>tablast<cr>
nnoremap <silent> tn :<c-u>tabnext<cr>
nnoremap <silent> tN :<c-u>tabNext<cr>
nnoremap <silent> tp :<c-u>tabprevious<cr>
nnoremap <silent> te :<c-u>tabedit<cr>
nnoremap <silent> tc :<c-u>tablast <bar> tabnew<cr>
nnoremap <silent> tx :<c-u>tabclose<cr>
nnoremap <silent> to :<c-u>tabonly<cr>
nnoremap <silent> ts :<c-u>tabs<cr>

" nerdcommenter
" ,,でコメントをトグルする
nmap ,, <Plug>NERDCommenterToggle
vmap ,, <Plug>NERDCommenterToggle

" memolist
nnoremap <Leader>mn  :MemoNew<CR>
nnoremap <Leader>ml  :MemoList<CR>
nnoremap <Leader>mg  :MemoGrep<CR>

" tagbar
nnoremap <Leader>t :TagbarToggle<CR>

" vimfiler
nnoremap <Leader>e :VimFilerExplorer<CR>
" 現在開いているバッファのディレクトリを開く
nnoremap <Leader>f :VimFilerBufferDir<CR>