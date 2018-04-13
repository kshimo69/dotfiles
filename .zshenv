#
# Defines environment variables.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Ensure that a non-login, non-interactive shell has a defined environment.
if [[ ( "$SHLVL" -eq 1 && ! -o LOGIN || -z "${TMPDIR}" ) && -s "${ZDOTDIR:-$HOME}/.zprofile" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprofile"
fi

PATH=$HOME/.cask/bin:$HOME/.cabal/bin:$HOME/local/bin:$HOME/bin:$HOME/jack_knives/bin:/opt/local/bin:/opt/local/sbin:/usr/local/bin:/usr/local/bin/packer:/usr/local/sbin:/Developer/android-sdk-mac_x86/platform-tools:/usr/bin:/bin:/usr/sbin:/sbin:$PATH:$HOME/Library/Android/sdk/platform-tools

# go
export GOROOT=/usr/local/go
PATH=$PATH:$GOROOT/bin
export GOPATH=$HOME/go
PATH=$PATH:$HOME/go/bin

# http://qiita.com/luckypool/items/f1e756e9d3e9786ad9ea
# git clone https://github.com/riywo/anyenv ~/.anyenv
if [ -d $HOME/.anyenv/bin ]; then
    PATH=$HOME/.anyenv/bin:$PATH
    # for tmux
    for D in `ls $HOME/.anyenv/envs`
    do
        PATH="$HOME/.anyenv/envs/$D/shims:$PATH"
    done
fi

if [ -n "$OSDK_DIR" ]; then
    PATH=${CURRENT_ENV}/bin:${CURRENT_ENV}/Scripts:${CURRENT_ENV}/lib/site-packages/RendezVous/bin/Debug:${CURRENT_ENV}/lib/site-packages/RendezVous/bin/Release:${OSDK_DIR}/Build/Tools/CMake/Linux/bin_${BUILDER_ARCH}:$SWIG_PATH:$PATH
    alias python=${CURRENT_ENV}/bin/python
fi

typeset -U PATH
export PATH
export MANPATH=$HOME/local/share/man:/opt/local/man:$MANPATH
export WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'
export GTAGSLABEL=default
