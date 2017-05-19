#!/bin/bash

# brew

# zsh
brew install zsh

# anyenv

# pyenv-virtualenv
git clone https://github.com/yyuu/pyenv-virtualenv.git ~/.anyenv/envs/pyenv/plugins/pyenv-virtualenv

# reload zsh

# neovim
pyenv install 2.7.13
pyenv install 3.5.3
pyenv virtualenv 2.7.13 neovim2
pyenv virtualenv 3.5.3 neovim3
pyenv activate neovim2
pip install neovim
pyenv deactivate
pyenv activate neovim3
pip install neovim
pyenv deactivate
brew install neovim/neovim/neovim

# dein.vim
curl https://raw.githubusercontent.com/Shougo/dein.vim/master/bin/installer.sh > installer.sh
sh ./installer.sh ~/.cache/dein
rm -f install.sh
