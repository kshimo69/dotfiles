let g:python_host_prog = expand('$HOME') . '/.anyenv/envs/pyenv/versions/neovim2/bin/python'
let g:python3_host_prog = expand('$HOME') . '/.anyenv/envs/pyenv/versions/neovim3/bin/python'

" https://github.com/lighttiger2505/.dotfiles/blob/master/vim/vimrc

" ~/.cache がなければ作る
let $CACHE = expand('~/.cache')

if !isdirectory(expand($CACHE))
  call mkdir(expand($CACHE), 'p')
endif


" dein

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

" TODO move to dein.rc.vim
if &compatible
  set nocompatible               " Be iMproved
endif

if dein#load_state(s:dein_dir)
  call dein#begin(s:dein_dir)

  " Let dein manage dein
  " Required:
  " call dein#add(expand('$CACHE/dein') . '/repos/github.com/Shougo/dein.vim')
  " call dein#add('Shougo/deoplete.nvim')

  " Add or remove your plugins here:
  " call dein#add('Shougo/neosnippet.vim')
  " call dein#add('Shougo/neosnippet-snippets')

  " You can specify revision/branch/tag.
  " call dein#add('Shougo/vimshell', { 'rev': '3787e5' })

  " TOMLファイルを読む
  let g:rc_dir = expand('~/.config/nvim/rc')
  let s:toml = g:rc_dir . '/dein.toml'
  " let s:lazy_toml = g:rc_dir . '/dein_lazy.toml'
  call dein#load_toml(s:toml, {'lazy': 0})
  " call dein#load_toml(s:lazy_toml, {'lazy': 1})

  " Required:
  call dein#end()
  call dein#save_state()
endif

" Required:
filetype plugin indent on
syntax enable

" もし、未インストールものものがあったらインストール
if dein#check_install()
  call dein#install()
endif
