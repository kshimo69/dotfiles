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
bindkey '^r' history-incremental-search-forward
bindkey '^e' end-of-line
bindkey '^u' backward-kill-line
# http://qiita.com/items/1536
if zle -la | grep -q '^history-incremental-pattern-search'; then
    # zsh 4.3.10 以降でのみ有効
    bindkey '^R' history-incremental-pattern-search-backward
    bindkey '^S' history-incremental-pattern-search-forward
fi
setopt no_flow_control

# http://subtech.g.hatena.ne.jp/secondlife/20071003/1191394868
# % mkdir -p ~/.zsh/functions/completion
# % touch ~/.zsh/functions/completion/dummy
# % (mkdir ~/bin; cd ~/bin; wget http://www.rubyist.net/~rubikitch/archive/zshcomplete.txt; mv zshcomplete.txt zshcomplete.rb; chmod 755 zshcomplete.rb;
if [ ! -f ~/.zsh/functions/completion/_dummy ]; then
    mkdir -p ~/.zsh/functions/completion
    touch ~/.zsh/functions/completion/dummy
fi
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
autoload -U add-zsh-hook

alias ls='ls --color=auto'
alias vi='vim'
alias VIM='gvim'
alias em='TERM=xterm-256color emacs -nw'
alias gst='git st && git stash list'
alias gch='git cherry -v'
alias g='git'
alias py='python'
alias r='rails'
alias pu='pushd'
alias po='popd'
alias chrome="/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome --user-data-dir=\"$HOME/.cbata\""
alias 32bitboot='sudo systemsetup -setkernelbootarchitecture i386'
alias 64bitboot='sudo systemsetup -setkernelbootarchitecture x86_64'
alias fgrep='find . -type f -print0 | xargs -0 grep'
alias ngrep='grep --color=never'
alias now='date +%Y%m%d%H%M%S'

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
    alias vi='env LANG=ja_JP.UTF-8 TERM=xterm-256color /Applications/MacVim.app/Contents/MacOS/Vim "$@"'
    alias vim='env LANG=ja_JP.UTF-8 TERM=xterm-256color /Applications/MacVim.app/Contents/MacOS/Vim "$@"'
    alias VIM='env LANG=ja_JP.UTF-8 open -a /Applications/MacVim.app/Contents/MacOS/MacVim'
elif [ "`uname`"  = "CYGWIN_NT-6.1-WOW64" ]; then
    alias gvim='d:/vim/gvim.exe'
fi
if [ "${TERM}" = "eterm-color" ]; then
    alias ls='ls -F'
fi

if [ -f ~/.passwd ]; then
    . ~/.passwd
fi

export LANG="ja_JP.UTF-8"
export GREP_COLOR="01;35"
export GREP_OPTIONS=--color=auto
export LSCOLORS=gxfxcxdxbxegedabagacad
export PERL5LIB=$HOME/local/lib/perl5:$HOME/local/lib/perl5/site_perl:$HOME/local/lib/perl:$HOME/local/share/perl
export PERL_AUTOINSTALL="--defaultdeps"
#export RUBYLIB=$HOME/local/lib/ruby/site_ruby/
#export GEM_HOME=$HOME/local/lib/rubygems/
export PYTHONSTARTUP=$HOME/.pythonrc.py
# export PYTHONPATH=/usr/local/lib/python2.7/site-packages:$PYTHONPATH
export WORKON_HOME=$HOME/.virtualenv
export PIP_DOWNLOAD_CACHE=$HOME/.pip_cache
export PIP_RESPECT_VIRTUALENV=true
#export PIP_REQUIRE_VIRTUALENV=true
# if virtualenvwrapper ver > 2.0
#if [ -x `which virtualenvwrapper.sh` ]; then
#    . `which virtualenvwrapper.sh`
#fi
# which virtualenvwrapper.sh >/dev/null 2>&1 && . `which virtualenvwrapper.sh`
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

# http://ymotongpoo.hatenablog.com/entry/20120516/1337123564
#PYTHON_VER=2.7
#export MACPORTS_PREFIX=/opt/local
#export VIRTUALENV_BIN=$MACPORTS_PREFIX/Library/Frameworks/Python.framework/Versions/$PYTHON_VER
#export PYTHONPATH=$MACPORTS_PREFIX/lib/python$PYTHON_VER/:$PYTHONPATH
#export WORKON_HOME=$HOME/.virtualenvs
#. $VIRTUALENV_BIN/bin/virtualenvwrapper.sh
#mkvenv () {
#    base_python=`which python$1` 
#    mkvirtualenv --distribute --python=$base_python $2
#}

if [ "`uname`" = "Darwin" ]; then
    export EDITOR="/Applications/MacVim.app/Contents/MacOS/Vim"
    export SVN_EDITOR="/Applications/MacVim.app/Contents/MacOS/Vim"
    export GIT_EDITOR="/Applications/MacVim.app/Contents/MacOS/Vim"
else
    export EDITOR=`which vim`
    export SVN_EDITOR=`which vim`
    export GIT_EDITOR=`which vim`
fi
export GISTY_DIR=$HOME/repos/gist
# export GISTY_SSL_CA=$HOME/.rvm/usr/ssl/cert.pem
#export GISTY_SSL_CA=/etc/ssl/certs/ca-certificates.crt
export GISTY_SSL_VERIFY="none"
export GISTY_ACCESS_TOKEN=2d84fc458cc532057f5aac4e96a09a8652dc79d9
export LESS='--tabs=4 --no-init --LONG-PROMPT --ignore-case -R'
export LESSCHARSET="utf-8"
# gtagsの設定
# % cd /usr/include
# % gtags ~/.inctags
#export GTAGSROOT=/usr/include
#export GTAGDBPATH=~/.inctags
export GTAGSLABEL=exuberant-ctags

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
function history-all { history -id 1 }  # 全履歴一覧
REPORTTIME=10
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

# http://qiita.com/mollifier/items/8d5a627d773758dd8078
autoload -Uz vcs_info
autoload -Uz is-at-least
# 以下の3つのメッセージをエクスポートする
#   $vcs_info_msg_0_ : 通常メッセージ用 (緑)
#   $vcs_info_msg_1_ : 警告メッセージ用 (黄色)
#   $vcs_info_msg_2_ : エラーメッセージ用 (赤)
zstyle ':vcs_info:*' max-exports 3

zstyle ':vcs_info:*' enable git svn hg bzr
# 標準のフォーマット(git 以外で使用)
# misc(%m) は通常は空文字列に置き換えられる
zstyle ':vcs_info:*' formats '%r(%s):%b'
zstyle ':vcs_info:*' actionformats '%r(%s):%b' '%m' '<!%a>'
zstyle ':vcs_info:(svn|bzr):*' branchformat '%b:r%r'
zstyle ':vcs_info:bzr:*' use-simple true

# zstyle ':vcs_info:*' formats '%r(%s):%b'
# zstyle ':vcs_info:*' actionformats '%r(%s):%b|%a'

if is-at-least 4.3.10; then
    # git 用のフォーマット
    # git のときはステージしているかどうかを表示
    zstyle ':vcs_info:git:*' formats '%r(%s):%b' '%c%u %m'
    zstyle ':vcs_info:git:*' actionformats '%r(%s):%b' '%c%u %m' '<!%a>'
    zstyle ':vcs_info:git:*' check-for-changes true
    zstyle ':vcs_info:git:*' stagedstr "+"    # %c で表示する文字列
    zstyle ':vcs_info:git:*' unstagedstr "-"  # %u で表示する文字列
fi

# hooks 設定
if is-at-least 4.3.11; then
    # git のときはフック関数を設定する

    # formats '(%s)-[%b]' '%c%u %m' , actionformats '(%s)-[%b]' '%c%u %m' '<!%a>'
    # のメッセージを設定する直前のフック関数
    # 今回の設定の場合はformat の時は2つ, actionformats の時は3つメッセージがあるので
    # 各関数が最大3回呼び出される。
    zstyle ':vcs_info:git+set-message:*' hooks \
                                            git-hook-begin \
                                            git-untracked \
                                            git-push-status \
                                            git-stash-count
                                            # git-nomerge-branch \

    # フックの最初の関数
    # git の作業コピーのあるディレクトリのみフック関数を呼び出すようにする
    # (.git ディレクトリ内にいるときは呼び出さない)
    # .git ディレクトリ内では git status --porcelain などがエラーになるため
    function +vi-git-hook-begin() {
        if [[ $(command git rev-parse --is-inside-work-tree 2> /dev/null) != 'true' ]]; then
            # 0以外を返すとそれ以降のフック関数は呼び出されない
            return 1
        fi

        return 0
    }

    # untracked ファイル表示
    #
    # untracked ファイル(バージョン管理されていないファイル)がある場合は
    # unstaged (%u) に ? を表示
    function +vi-git-untracked() {
        # zstyle formats, actionformats の2番目のメッセージのみ対象にする
        if [[ "$1" != "1" ]]; then
            return 0
        fi

        if command git status --porcelain 2> /dev/null \
            | awk '{print $1}' \
            | command grep -F '??' > /dev/null 2>&1 ; then

            # unstaged (%u) に追加
            hook_com[unstaged]+='?'
        fi
    }

    # push していないコミットの件数表示
    #
    # リモートリポジトリに push していないコミットの件数を
    # pN という形式で misc (%m) に表示する
    function +vi-git-push-status() {
        # zstyle formats, actionformats の2番目のメッセージのみ対象にする
        if [[ "$1" != "1" ]]; then
            return 0
        fi

        if [[ "${hook_com[branch]}" != "master" ]]; then
            # master ブランチでない場合は何もしない
            return 0
        fi

        # push していないコミット数を取得する
        local ahead
        ahead=$(command git rev-list origin/master..master 2>/dev/null \
            | wc -l \
            | tr -d ' ')

        if [[ "$ahead" -gt 0 ]]; then
            # misc (%m) に追加
            hook_com[misc]+="(p${ahead})"
        fi
    }

    # マージしていない件数表示
    #
    # master 以外のブランチにいる場合に、
    # 現在のブランチ上でまだ master にマージしていないコミットの件数を
    # (mN) という形式で misc (%m) に表示
    function +vi-git-nomerge-branch() {
        # zstyle formats, actionformats の2番目のメッセージのみ対象にする
        if [[ "$1" != "1" ]]; then
            return 0
        fi

        if [[ "${hook_com[branch]}" == "master" ]]; then
            # master ブランチの場合は何もしない
            return 0
        fi

        local nomerged
        nomerged=$(command git rev-list master..${hook_com[branch]} 2>/dev/null | wc -l | tr -d ' ')

        if [[ "$nomerged" -gt 0 ]] ; then
            # misc (%m) に追加
            hook_com[misc]+="(m${nomerged})"
        fi
    }


    # stash 件数表示
    #
    # stash している場合は :SN という形式で misc (%m) に表示
    function +vi-git-stash-count() {
        # zstyle formats, actionformats の2番目のメッセージのみ対象にする
        if [[ "$1" != "1" ]]; then
            return 0
        fi

        local stash
        stash=$(command git stash list 2>/dev/null | wc -l | tr -d ' ')
        if [[ "${stash}" -gt 0 ]]; then
            # misc (%m) に追加
            hook_com[misc]+=":S${stash}"
        fi
    }
fi

function _update_vcs_info_msg() {
    local -a messages
    local prompt

    LANG=en_US.UTF-8 vcs_info

    if [[ -z ${vcs_info_msg_0_} ]]; then
        # vcs_info で何も取得していない場合はプロンプトを表示しない
        prompt=""
    else
        # vcs_info で情報を取得した場合
        # $vcs_info_msg_0_ , $vcs_info_msg_1_ , $vcs_info_msg_2_ を
        # それぞれ緑、黄色、赤で表示する
        [[ -n "$vcs_info_msg_0_" ]] && messages+=( "%F{green}${vcs_info_msg_0_}%f" )
        [[ -n "$vcs_info_msg_1_" ]] && messages+=( "%F{yellow}${vcs_info_msg_1_}%f" )
        [[ -n "$vcs_info_msg_2_" ]] && messages+=( "%F{red}${vcs_info_msg_2_}%f" )

        # 間にスペースを入れて連結する
        # prompt="${(j: :)messages}"
        prompt="${(j::)messages}"
        # prompt="[${(j::)messages}]"
    fi

    RPROMPT="$prompt"
}
add-zsh-hook precmd _update_vcs_info_msg


function s() {
    $*
}

# http://d.hatena.ne.jp/umezo/20100508/1273332857
local COMMAND=""
local COMMAND_TIME=""
local TIMEOUT="30"
precmd () {
    if [ "q$TMUX" != "q" ]; then
        echo -ne "\ek$(basename $(pwd))\e\\"
    fi
    # psvar=()
    # LANG=en_US.UTF-8 vcs_info
    # [[ -n "$vcs_info_msg_0_" ]] && psvar[1]="$vcs_info_msg_0_"
    if [ -n "$SSH_CLIENT" -a -x "`which growl 2>/dev/null`" ]; then
        if [ "$COMMAND_TIME" -ne "0" ] ; then
            local d=`date +%s`
            d=`expr $d - $COMMAND_TIME`
            COMMAND="$COMMAND "
            if [ "${${(s: :)COMMAND}[1]}" = "s" ]; then
                growl -H `echo $SSH_CLIENT | awk '{ print $1 }'` -t "${${(s: :)COMMAND}[2]} done." -m "${${(s: :)COMMAND}[2,-1]}" -s -P "password" 2>/dev/null
            elif [ "$d" -ge "$TIMEOUT" ] ; then
                growl -H `echo $SSH_CLIENT | awk '{ print $1 }'` -t "${${(s: :)COMMAND}[1]} done." -m "$COMMAND" -P "password" 2>/dev/null
            fi
        fi
        COMMAND="0"
        COMMAND_TIME="0"
    elif [ -x "`which growlnotify 2>/dev/null`" ]; then
        if [ "$COMMAND_TIME" -ne "0" ] ; then
            local d=`date +%s`
            d=`expr $d - $COMMAND_TIME`
            COMMAND="$COMMAND "
            if [ "${${(s: :)COMMAND}[1]}" = "s" ]; then
                growlnotify -s -t "${${(s: :)COMMAND}[2]} done." -m "${${(s: :)COMMAND}[2,-1]}"
            elif [ "$d" -ge "$TIMEOUT" ] ; then
                growlnotify -t "${${(s: :)COMMAND}[1]} done." -m "$COMMAND"
            fi
        fi
        COMMAND="0"
        COMMAND_TIME="0"
    elif [ -n "$DISPLAY" -a -x "`which notify-send`" ]; then # for linux
        if [ "$COMMAND_TIME" -ne "0" ] ; then
            local d=`date +%s`
            d=`expr $d - $COMMAND_TIME`
            if [ "$d" -ge $TIMEOUT ] ; then
                COMMAND="$COMMAND "
                notify-send -t 10000 "${${(s: :)COMMAND}[1]} done." "$COMMAND"
            fi
        fi
        COMMAND="0"
        COMMAND_TIME="0"
    fi
}
preexec () {
    if [ "q$TMUX" != "q" ]; then
        echo -ne "\ek($1)\e\\"
    fi
    COMMAND="${1}"
    COMMAND_TIME=`date +%s`
}

function chpwd() {
    ls
}

setopt prompt_subst
# コマンド実行後は右プロンプトを消す
#setopt transient_rprompt

PROMPT='%{$reset_color%}%B%(?.%F{green}%n%f.%F{red}%n%f)%b%{$reset_color%}%{$fg[yellow]%}@%m %{$reset_color%}[%{$fg[cyan]%}%~%{$reset_color%}]
%#%{$reset_color%} '
# RPROMPT="%1(v|[%F{green}%1v%f]|)"

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

# [[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"  # This loads RVM into a shell session.
# [[ -s "$HOME/.pythonbrew/etc/bashrc" ]] && source "$HOME/.pythonbrew/etc/bashrc"  # pythonbrew
# [[ -d "$HOME/repos/powerline/powerline" ]] && which powerline >/dev/null 2>&1 && source ~/repos/powerline/powerline/bindings/zsh/powerline.zsh

# https://github.com/sstephenson/rbenv
# $ git clone https://github.com/sstephenson/rbenv.git ~/.rbenv
# [[ -d "$HOME/.rbenv" ]] && eval "$(rbenv init -)"

# note
# need install ruby-builder
#
# $ git clone https://github.com/sstephenson/ruby-build.git ~/.rbenv/plugins/ruby-build
# cd ~/.rbenv/plugins/ruby-build/
# sudo ./install.sh

true
