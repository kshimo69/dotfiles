PATH=$HOME/.cabal/bin:$HOME/local/bin:$HOME/bin:/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:$PATH:/Applications/android-sdk-macosx/tools:/Applications/android-sdk-macosx/platform-tools

# http://qiita.com/luckypool/items/f1e756e9d3e9786ad9ea
# git clone https://github.com/riywo/anyenv ~/.anyenv
if [ -d $HOME/.anyenv ]; then
    PATH=$HOME/.anyenv/bin:$PATH
    eval "$(anyenv init -)"
    # for tmux
    for D in `find $HOME.anyenv/envs -type d -d 1`
    do
        PATH=$D/shims:$PATH
    done
fi

typeset -U PATH
export PATH
export MANPATH=$HOME/local/share/man:/opt/local/man:$MANPATH
export PYTHONPATH=/usr/local/lib/python2.7/site-packages:$PYTHONPATH
export WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'
export GTAGSLABEL=exuberant-ctags
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"  # This loads RVM into a shell session.
[[ -s "$HOME/.pythonbrew/etc/bashrc" ]] && source "$HOME/.pythonbrew/etc/bashrc"  # pythonbrew
