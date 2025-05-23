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
PATH=$PATH:/mnt/c/Windows/System32

# go
export GOENV_DISABLE_GOPATH=1
export GOPATH=$HOME/work
PATH=$PATH:$GOPATH/bin
export GO111MODULE=auto

if [ -f $HOME/jack_knives/.nexenv.zsh ]
then
    source $HOME/jack_knives/.nexenv.zsh
fi

test -d /opt/homebrew && eval $(/opt/homebrew/bin/brew shellenv)
test -d /home/linuxbrew/.linuxbrew && eval $(/home/linuxbrew/.linuxbrew/bin/brew shellenv)

export PATH
#export MANPATH=$(brew --prefix)/share/man:$HOME/local/share/man:/opt/local/man:$MANPATH
#export INFOPATH=$(brew --prefix)/share/info:$HOME/local/share/info:/opt/local/info:$INFOPATH
export WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'
export GTAGSLABEL=default
