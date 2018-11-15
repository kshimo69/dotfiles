#
# Defines environment variables.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

typeset -U PATH

# Ensure that a non-login, non-interactive shell has a defined environment.
if [[ ( "$SHLVL" -eq 1 && ! -o LOGIN || -z "${TMPDIR}" ) && -s "${ZDOTDIR:-$HOME}/.zprofile" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprofile"
fi

export XDG_CONFIG_HOME="$HOME/.config"

PATH=$PATH:$HOME/Library/Android/sdk/platform-tools
PATH=/usr/bin:/bin:/usr/sbin:/sbin:$PATH
PATH=/Developer/android-sdk-mac_x86/platform-tools:$PATH
PATH=/opt/local/bin:/opt/local/sbin:/usr/local/bin:/usr/local/sbin:$PATH
PATH=$HOME/jack_knives/bin:$PATH
PATH=$HOME/bin:$PATH
PATH=$HOME/local/bin:$PATH
PATH=$HOME/.cabal/bin:$PATH
PATH=$HOME/.cask/bin:$PATH

# go
#export GOROOT=/usr/local/go
#PATH=$PATH:$GOROOT/bin
export GOPATH=$HOME/go
PATH=$PATH:$HOME/go/bin

# http://qiita.com/luckypool/items/f1e756e9d3e9786ad9ea
# git clone https://github.com/riywo/anyenv ~/.anyenv
if [ -d $HOME/.anyenv/bin ]
then
    PATH=$HOME/.anyenv/bin:$PATH
    # for tmux
    for D in `ls $HOME/.anyenv/envs`
    do
        PATH="$HOME/.anyenv/envs/$D/shims:$PATH"
    done
fi

if [ -f $HOME/jack_knives/.nexenv.zsh ]
then
    source $HOME/jack_knives/.nexenv.zsh
fi

PATH=$HOME/.linuxbrew/bin:$PATH

export PATH
export MANPATH=$(brew --prefix)/share/man:$HOME/local/share/man:/opt/local/man:$MANPATH
export INFOPATH=$(brew --prefix)/share/info:$HOME/local/share/info:/opt/local/info:$INFOPATH
export WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'
export GTAGSLABEL=default
