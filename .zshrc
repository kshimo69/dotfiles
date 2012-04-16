autoload -U colors
colors
setopt COMPLETE_IN_WORD
bindkey -e
autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey '' history-beginning-search-backward-end
bindkey '' history-beginning-search-forward-end
bindkey '^r' history-incremental-search-backward
bindkey '^e' end-of-line
bindkey '^u' backward-kill-line
# http://qiita.com/items/1536
if zle -la | grep -q '^history-incremental-pattern-search'; then
    # zsh 4.3.10 以降でのみ有効
    bindkey '^R' history-incremental-pattern-search-backward
    bindkey '^S' history-incremental-pattern-search-forward
fi

# http://subtech.g.hatena.ne.jp/secondlife/20071003/1191394868
# % mkdir -p ~/.zsh/functions/completion
# % touch ~/.zsh/functions/completion/dummy
# % (mkdir ~/bin; cd ~/bin; wget http://www.rubyist.net/~rubikitch/archive/zshcomplete.txt; mv zshcomplete.txt zshcomplete.rb; chmod 755 zshcomplete.rb;
fpath=($HOME/.zsh/functions/completion $fpath)
autoload -U $HOME/.zsh/functions/completion/*(:t)
autoload -U compinit
compinit
generate-complete-function/ruby/optparse () {
    ruby -r $HOME/bin/zshcomplete $1 > $HOME/.zsh/completion/_`basename $1`
    reload-complete-functions
}
reload-complete-functions() {
    local f
    f=($HOME/.zsh/completion/*(.))
    unfunction $f:t 2> /dev/null
    autoload -U $f:t
}

alias ls='ls --color=auto'
alias rm='rm'
alias vi='TERM=xterm-256color vim'
alias vim='TERM=xterm-256color vim'
alias VIM='gvim'
#alias em='emacs -nw'
alias em='TERM=xterm-256color emacs -nw'
alias py='python'
alias r='rails'
alias pu='pushd'
alias po='popd'
#alias scp='noglob scp'
#alias wget='noglob wget'
alias chrome="/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome --user-data-dir=\"$HOME/.cbata\""
alias todoedit='todo.pl download;vim tasks.txt;todo.pl upload tasks.txt'
alias sukico='ssh kshimo69@49.212.62.39 -p 10022'
alias sukicodb1='ssh kshimo69@49.212.62.39 -p 10022'
alias sukicoweb1='ssh kshimo69@49.212.51.125 -p 10022'
alias sukicoweb2='ssh kshimo69@49.212.58.101 -p 10022'
alias sukicobatch1='ssh kshimo69@49.212.49.42 -p 10022'
alias sukicoec2='ssh -i $HOME/.ssh/sukico01.pem ec2-user@ec2-46-51-229-107.ap-northeast-1.compute.amazonaws.com'
alias clear_terminal='echo c'
alias fgrep='find . -type f -print0 | xargs -0 grep'
alias ngrep='grep --color=never'
NOTIFY=""
if [ -n "$DISPLAY" -a -x "`which notify-send`" ]; then
    NOTIFY='NOTIFY=notify-send'
fi
alias earthquake="$NOTIFY earthquake"
alias earthquake_sub="$NOTIFY earthquake ~/.earthquake_sub"

# clipboard
if which pbcopy >/dev/null 2>&1 ; then
    # Mac
    COPY='pbcopy'
elif which xsel >/dev/null 2>&1 ; then
    # Linux
    COPY='xsel --input --clipboard'
elif which putclip >/dev/null 2>&1 ; then
    # Cygwin
    COPY='putclip'
fi
copy-prev-cmd-to-clipboard () {
    tail -1 $HISTFILE | perl -e '<> =~  m/;(.+)\s?C?/; print $1;' | $COPY
}
zle -N copy-prev-cmd-to-clipboard
bindkey '^x^p' copy-prev-cmd-to-clipboard
alias -g C="|$COPY"

if [ "`uname`" = "Darwin" ]; then
    alias ls='ls -G'
    alias emacs='open -a Emacs'
    alias em='emacsclient -n'
    alias vi='env LANG=ja_JP.UTF-8 /Applications/MacVim.app/Contents/MacOS/Vim "$@"'
    alias vim='env LANG=ja_JP.UTF-8 /Applications/MacVim.app/Contents/MacOS/Vim "$@"'
    alias VIM='env LANG=ja_JP.UTF-8 open -a /Applications/MacVim.app/Contents/MacOS/MacVim'
    alias earthquake='earthquake'
elif [ "`uname`"  = "CYGWIN_NT-6.1-WOW64" ]; then
    alias vim='d:/vim/gvim.exe'
fi
if [ "${TERM}" = "eterm-color" ]; then
    alias ls='ls -F'
fi
DAY=`date +%Y%m%d%H%M%S`
if [ -f ~/.passwd ]; then
    . ~/.passwd
fi

export GREP_COLOR="01;35"
export GREP_OPTIONS=--color=auto
export LSCOLORS=gxfxcxdxbxegedabagacad
PATH=$HOME/local/bin:$HOME/bin:$PATH
#PATH=$HOME/local/bin:/opt/local/bin:/opt/local/sbin:/usr/local/bin/:/Developer/android-sdk-mac_x86/platform-tools/:$PATH:$HOME/bin
typeset -U PATH
export PATH
export MANPATH=$HOME/local/share/man:/opt/local/man:$MANPATH
export PERL5LIB=$HOME/local/lib/perl5:$HOME/local/lib/perl5/site_perl:$HOME/local/lib/perl:$HOME/local/share/perl
export PERL_AUTOINSTALL="--defaultdeps"
#export RUBYLIB=$HOME/local/lib/ruby/site_ruby/
#export GEM_HOME=$HOME/local/lib/rubygems/
export PYTHONSTARTUP=$HOME/.pythonrc.py
export WORKON_HOME=$HOME/.virtualenv
export PIP_DOWNLOAD_CACHE=$HOME/.pip_cache
export PIP_RESPECT_VIRTUALENV=true
#export PIP_REQUIRE_VIRTUALENV=true
# if virtualenvwrapper ver > 2.0
#if [ -x `which virtualenvwrapper.sh` ]; then
#    . `which virtualenvwrapper.sh`
#fi
which virtualenvwrapper.sh >/dev/null 2>&1 && . `which virtualenvwrapper.sh`
#echo '#!/usr/bin/zsh' > $WORKON_HOME/postmkvirtualenv
#echo '# This hook is run after a new virtualenv is activated.' >> $WORKON_HOME/postmkvirtualenv
#echo '' >> $WORKON_HOME/postmkvirtualenv
#echo '# virtualenv毎に pip をインストールする場合' >> $WORKON_HOME/postmkvirtualenv
#echo 'easy_install pip' >> $WORKON_HOME/postmkvirtualenv
#echo '' >> $WORKON_HOME/postmkvirtualenv
#echo 'pip install ipython' >> $WORKON_HOME/postmkvirtualenv
#echo '' >> $WORKON_HOME/postmkvirtualenv
#echo '# pudb も便利かもよ' >> $WORKON_HOME/postmkvirtualenv
#echo '#pip install pudb' >> $WORKON_HOME/postmkvirtualenv
export EDITOR=vim
export SVN_EDITOR=vim
export GISTY_DIR=$HOME/repos/gist
export GISTY_SSL_CA=$HOME/.rvm/usr/ssl/cert.pem
#export GISTY_SSL_CA=/etc/ssl/certs/ca-certificates.crt
export GISTY_SSL_VERIFY="none"
export GIT_EDITOR=vim
export LESS='--tabs=4 --no-init --LONG-PROMPT --ignore-case -R --jump-target=10'

zstyle ':completion:*:sudo:*' command-path /opt/local/sbin /opt/local/bin \
                                /usr/local/sbin /usr/local/bin /usr/sbin \
                                /usr/bin /sbin /bin /usr/X11R6/bin
zstyle ':completion:*:default' menu select=1
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
if [ "`uname`" = "Darwin" ]; then
    zstyle ':completion:*' list-colors $LSCOLORS
fi

setopt NO_beep
HISTFILE=~/.zsh-history
HISTSIZE=1000000
SAVEHIST=1000000
setopt append_history
setopt extended_history
setopt hist_ignore_dups
setopt hist_ignore_space
setopt share_history
setopt hist_no_store
setopt auto_list
setopt auto_cd
setopt auto_pushd
setopt auto_remove_slash
setopt print_eight_bit
setopt magic_equal_subst
setopt interactive_comments

STERM="screen"

# http://d.hatena.ne.jp/mollifier/20090814/p1
autoload -Uz vcs_info
zstyle ':vcs_info:*' formats '%r(%s):%b'
zstyle ':vcs_info:*' actionformats '%r(%s):%b|%a'
# http://d.hatena.ne.jp/umezo/20100508/1273332857
local COMMAND=""
local COMMAND_TIME=""
precmd () {
    psvar=()
    LANG=en_US.UTF-8 vcs_info
    [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
    if [ -x "`which growlnotify 2>/dev/null`" ]; then
        if [ "$COMMAND_TIME" -ne "0" ] ; then
            local d=`date +%s`
            d=`expr $d - $COMMAND_TIME`
            if [ "$d" -ge "30" ] ; then
                COMMAND="$COMMAND "
                growlnotify -t "${${(s: :)COMMAND}[1]} done." -m "$COMMAND"
            fi
        fi
        COMMAND="0"
        COMMAND_TIME="0"
    elif [ -n "$DISPLAY" -a -x "`which notify-send`" ]; then # for linux
        if [ "$COMMAND_TIME" -ne "0" ] ; then
            local d=`date +%s`
            d=`expr $d - $COMMAND_TIME`
            if [ "$d" -ge "30" ] ; then
                COMMAND="$COMMAND "
                notify-send -t 10000 "${${(s: :)COMMAND}[1]} done." "$COMMAND"
            fi
        fi
        COMMAND="0"
        COMMAND_TIME="0"
    fi
}
preexec () {
    COMMAND="${1}"
    COMMAND_TIME=`date +%s`
}

# http://d.hatena.ne.jp/dayflower/20081031/1225428086
if [ "$TERM" = "$STERM" ]; then
    #local -a shorthost

    #echo $TERMCAP | grep -q -i screen
    #if [ $? -eq 0 ]; then
    #    shorthost=""
    #else
    #    shorthost="${HOST%%.*}:"
    #fi

    #echo -ne "\ek$shorthost\e\\"

    preexec() {
        #echo -ne "\ek${shorthost}($1)\e\\"
        echo -ne "\ek($1)\e\\"
        echo -ne "\e_`dirs`\e\\"
        COMMAND="${1}"
        COMMAND_TIME=`date +%s`
    }

    precmd() {
        #echo -ne "\ek${shorthost}$(basename $(pwd))\e\\"
        echo -ne "\ek$(basename $(pwd))\e\\"
        screen -X title $(basename $(print -P "%~"))
        # vcs_info
        psvar=()
        LANG=en_US.UTF-8 vcs_info
        [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
        if [ -x "`which growlnotify 2>/dev/null`" ]; then
            if [ "$COMMAND_TIME" -ne "0" ] ; then
                local d=`date +%s`
                d=`expr $d - $COMMAND_TIME`
                if [ "$d" -ge "30" ] ; then
                    COMMAND="$COMMAND "
                    growlnotify -t "${${(s: :)COMMAND}[1]} done." -m "$COMMAND"
                fi
            fi
            COMMAND="0"
            COMMAND_TIME="0"
        elif [ -n "$DISPLAY" -a -x "`which notify-send`" ]; then # for linux
            if [ "$COMMAND_TIME" -ne "0" ] ; then
                local d=`date +%s`
                d=`expr $d - $COMMAND_TIME`
                if [ "$d" -ge "30" ] ; then
                    COMMAND="$COMMAND "
                    notify-send -t 10000 "${${(s: :)COMMAND}[1]} done." "$COMMAND"
                fi
            fi
            COMMAND="0"
            COMMAND_TIME="0"
        fi
    }
fi

function chpwd() {
    ls
    # if pwd | grep -q wire; then
    #     echo "change ruby version."
    #     rvm use ruby-1.8.7
    # fi
}
 
# SSHコマンドはscreenの新しい窓で
function ssh_screen(){
    eval server=\${$#}
    screen -t $server ssh "$@"
}
if [ "$TERM" = "$STERM" ]; then
    alias ssh=ssh_screen
fi

if [ ${TERM} != "$STERM" -a ${TERM} != "linux" -a ${TERM} != "eterm-color" -a ${TERM} != "vt100" ]; then
    exec screen -RR
fi

setopt prompt_subst

PROMPT='%{$fg[yellow]%}%n@%m%{$fg[yellow]%} %{$reset_color%}[%{$fg[cyan]%}%~%{$reset_color%}]
%#%{$reset_color%} '
RPROMPT="%1(v|[%F{green}%1v%f]|)"

#http://d.hatena.ne.jp/hitode909/20100211/1265879271
function u () {
    cd ./$(git rev-parse --show-cdup)
}

#http://d.hatena.ne.jp/tkng/20100712/1278896396

e_normal=`echo -e "\033[0;30m"`
e_RED=`echo -e "\033[1;31m"`
e_BLUE=`echo -e "\033[1;36m"`

#function make() {
#    LANG=C command make "$@" 2>&1 | sed -e "s@[Ee]rror:.*@$e_RED&$e_normal@g" -e "s@cannot\sfind.*@$e_RED&$e_normal@g" -e "s@[Ww]arning:.*@$e_BLUE&$e_normal@g"
#}
#function gcc() {
#    LANG=C command gcc "$@" 2>&1 | sed -e "s@[Ee]rror:.*@$e_RED&$e_normal@g" -e "s@cannot\sfind.*@$e_RED&$e_normal@g" -e "s@[Ww]arning:.*@$e_BLUE&$e_normal@g"
#}

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"  # This loads RVM into a shell session.
