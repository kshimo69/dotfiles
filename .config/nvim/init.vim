let g:python_host_prog = expand('$HOME/.anyenv/envs/pyenv/versions/neovim2/bin/python')
let g:python3_host_prog = expand('$HOME/.anyenv/envs/pyenv/versions/neovim3/bin/python')

" ~/.cache がなければ作る
let $CACHE = expand('$HOME/.cache')
if !isdirectory(expand($CACHE))
  call mkdir(expand($CACHE), 'p')
endif

" プラグインが実際にインストールされるディレクトリ
let s:dein_dir = expand('$CACHE/dein')
" dein.vim 本体
let s:dein_repo_dir = s:dein_dir . '/repos/github.com/Shougo/dein.vim'

" dein.vim がなければ github から落としてくる
if &runtimepath !~# '/dein.vim'
  if !isdirectory(s:dein_repo_dir)
    execute '!git clone https://github.com/Shougo/dein.vim' s:dein_repo_dir
  endif
  execute 'set runtimepath^=' . fnamemodify(s:dein_repo_dir, ':p')
endif

if &compatible
  set nocompatible
endif

" 自分で書くautocmdはすべてMyAutoCmdグループに入れるので先頭で初期化する
" release autogroup in MyAutoCmd
augroup MyAutoCmd
  autocmd!
augroup END

if dein#load_state(s:dein_dir)
  call dein#begin(s:dein_dir)

  " TOMLファイルを読む
  call dein#load_toml('~/.config/nvim/dein.toml', {'lazy': 0})
  call dein#load_toml('~/.config/nvim/dein_lazy.toml', {'lazy': 1})

  call dein#end()
  call dein#save_state()
endif

" もし、未インストールものものがあったらインストール
if dein#check_install()
  call dein#install()
endif

filetype plugin indent on
syntax enable

runtime! options.rc.vim
runtime! keymap.rc.vim

" たぶんplugin onの後じゃないとダメ
source ~/.config/nvim/plugins/quickhl.rc.vim
