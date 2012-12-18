PATH=$HOME/.cabal/bin:$HOME/local/bin:$HOME/bin:/usr/local/bin/:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:$PATH:/Applications/android-sdk-macosx/tools:/Applications/android-sdk-macosx/platform-tools
typeset -U PATH
export PATH
export MANPATH=$HOME/local/share/man:/opt/local/man:$MANPATH
export PYTHONPATH=/usr/local/lib/python2.7/site-packages:$PYTHONPATH
export WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'
export GTAGSLABEL=exuberant-ctags
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"  # This loads RVM into a shell session.
[[ -s "$HOME/.pythonbrew/etc/bashrc" ]] && source "$HOME/.pythonbrew/etc/bashrc"  # pythonbrew
