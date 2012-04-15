PATH=$HOME/local/bin:$HOME/bin:/usr/local/bin/:/usr/local/sbin:/usr/bin:/bin:/usr/sbin:/sbin:$PATH:/Applications/android-sdk-macosx/tools:/Applications/android-sdk-macosx/platforms
typeset -U PATH
export PATH
export MANPATH=$HOME/local/share/man:/opt/local/man:$MANPATH
export PYTHONPATH=/usr/local/lib/python2.7/site-packages:$PYTHONPATH
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"  # This loads RVM into a shell session.
