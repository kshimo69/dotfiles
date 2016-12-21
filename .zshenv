PATH=$HOME/.cask/bin:$HOME/.cabal/bin:$HOME/local/bin:$HOME/bin:/opt/local/bin:/opt/local/sbin:/usr/local/bin:/usr/local/bin/packer:/usr/local/sbin:/Developer/android-sdk-mac_x86/platform-tools:/usr/bin:/bin:/usr/sbin:/sbin:$PATH:$HOME/Library/Android/sdk/platform-tools

# go
export GOROOT=/usr/local/go
PATH=$PATH:$GOROOT/bin
export GOPATH=$HOME/go
PATH=$PATH:$HOME/go/bin

# http://qiita.com/luckypool/items/f1e756e9d3e9786ad9ea
# git clone https://github.com/riywo/anyenv ~/.anyenv
if [ -d $HOME/.anyenv ]; then
    PATH=$HOME/.anyenv/bin:$PATH
    eval "$(anyenv init -)"
    # for tmux
    for D in `ls $HOME/.anyenv/envs`
    do
        PATH="$HOME/.anyenv/envs/$D/shims:$PATH"
    done
fi

if [ -n "$OSDK_DIR" ]; then
    PATH=${CURRENT_ENV}/bin:${CURRENT_ENV}/Scripts:${CURRENT_ENV}/lib/site-packages/RendezVous/bin/Debug:${CURRENT_ENV}/lib/site-packages/RendezVous/bin/Release:$OSDK_DIR/Build/Tools/CMake/Linux/bin_${BUILDER_ARCH}:$SWIG_PATH:$PATH
fi

typeset -U PATH
export PATH
export MANPATH=$HOME/local/share/man:/opt/local/man:$MANPATH
# export PYTHONPATH=/usr/local/lib/python2.7/site-packages:$PYTHONPATH
export WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'
export GTAGSLABEL=default
# [[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"  # This loads RVM into a shell session.
# [[ -s "$HOME/.pythonbrew/etc/bashrc" ]] && source "$HOME/.pythonbrew/etc/bashrc"  # pythonbrew

# docker
if [ "`uname`" = "Darwin" ]; then
    export DOCKER_HOST=tcp://192.168.59.103:2376
# else
    # export DOCKER_HOST=tcp://172.16.81.94:4243
fi

true
