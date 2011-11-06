PATH=$HOME/local/bin:$HOME/bin:/usr/local/bin/:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:$PATH
typeset -U PATH
export PATH
export MANPATH=$HOME/local/share/man:/opt/local/man:$MANPATH
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"  # This loads RVM into a shell session.
