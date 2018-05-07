#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Customize to your needs...

# history
autoload -Uz history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end
bindkey '' history-beginning-search-backward-end
bindkey '' history-beginning-search-forward-end
bindkey '^U' backward-kill-line

# cdr
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs

function auto_ls() {
  ls
}
add-zsh-hook chpwd auto_ls

# environment
export LANG="ja_JP.UTF-8"
export TERM="xterm-256color"
export GREP=ag
export GREP_COLOR="01;35"
export LSCOLORS=gxfxcxdxbxegedabagacad
export LESS='--tabs=4 --no-init --LONG-PROMPT --ignore-case -R'
export LESSCHARSET="utf-8"
export EDITOR="nvim"
export SVN_EDITOR="nvim"
export GIT_EDITOR="nvim"
export HOMEBREW_CASK_OPTS="--appdir=/Applications"
if which fzf >/dev/null 2>&1
then
    export PERCOL=fzf
elif which peco >/dev/null 2>&1
then
    export PERCOL=peco
else
    export PERCOL=''
fi

# python
export PYTHONSTARTUP=$HOME/.pythonrc.py
export PIP_RESPECT_VIRTUALENV=true

# itermplot
#export MPLBACKEND="module://itermplot"

# gtags
# % cd /usr/include
# % gtags ~/.inctags
# % export GTAGSROOT=/usr/include
# % export GTAGDBPATH=~/.inctags
# % export GTAGSLABEL=exuberant-ctags

alias ls='ls --color=auto'
alias vi='nvim'
alias vim='nvim'
alias VIM='gvim'
alias em='TERM=xterm-256color emacs -nw'
alias g='git'
alias py='python'
alias r='rails'
alias pu='pushd'
alias po='popd'
alias chrome="/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome --user-data-dir=\"$HOME/.cbata\""
alias 32bitboot='sudo systemsetup -setkernelbootarchitecture i386'
alias 64bitboot='sudo systemsetup -setkernelbootarchitecture x86_64'
alias grep='grep --color=auto'
alias fgrep='find . -type f -print0 | xargs -0 grep'
alias ngrep='grep --color=never'
alias now='date +%Y%m%d%H%M%S'
alias ctags='`brew --prefix`/bin/ctags'
if [ "`uname`" = "Darwin" ]; then
    alias ls='ls -G'
    alias em='emacsclient -n "$@"'
    alias VIM='env LANG=ja_JP.UTF-8 /Applications/MacVim.app/Contents/bin/mvim'
elif [ "`uname`"  = "CYGWIN_NT-6.1-WOW64" ]; then
    alias gvim='d:/vim/gvim.exe'
fi
if [ "${TERM}" = "eterm-color" ]; then
    alias ls='ls -F'
fi

if [ -f ~/.passwd ]; then
    . ~/.passwd
fi

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
setopt magic_equal_subst

precmd () {
    if [ "q$TMUX" != "q" ]; then
        echo -ne "\ek$(basename $(pwd))\e\\"
    fi
}

preexec () {
    if [ "q$TMUX" != "q" ]; then
        echo -ne "\ek($1)\e\\"
    fi
}

# neotermの時はVIMRUNTIMEがある
if [ -n "$VIMRUNTIME" ]; then
    PROMPT='[%F{cyan}%~%f]
%#%{$reset_color%} '
fi
if [ -n "$OSDK_DIR" ]; then
    PROMPT='%B%(?.%F{green}%n%f.%F{red}%n%f)%b%F{yellow}@%f%F{red}OSDKBUILDER%f [%F{cyan}%~%f]
%#%{$reset_color%} '
fi

# peco
function peco-snippets() {
    BUFFER=$(cat ~/.snippets/* | grep -v "^#" | $PERCOL --query "$LBUFFER")
     #BUFFER=$(cat ~/.snippets/* | grep -v "^#" | $PERCOL --query "$LBUFFER" | $COPY)
    zle clear-screen
}
zle -N peco-snippets
bindkey '^x^s' peco-snippets

function peco-select-history() {
    local tac
    if which tac > /dev/null; then
        tac="tac"
    else
        tac="tail -r"
    fi
    BUFFER=$(\history -n 1 | \
        eval $tac | \
        $PERCOL --query "$LBUFFER")
    CURSOR=$#BUFFER
    zle clear-screen
}
zle -N peco-select-history
#bindkey '^r' peco-select-history

function peco-select-gitadd() {
    #local SELECTED_FILE_TO_ADD="$(git status --porcelain | \
    local SELECTED_FILE_TO_ADD="$(git status -s | \
                                  $PERCOL --query "$LBUFFER" | \
                                  awk -F ' ' '{print $NF}')"
    if [ -n "$SELECTED_FILE_TO_ADD" ]; then
      BUFFER="git add $(echo "$SELECTED_FILE_TO_ADD" | tr '\n' ' ')"
      CURSOR=$#BUFFER
    fi
    zle accept-line
    # zle clear-screen
}
zle -N peco-select-gitadd
bindkey "^g^a" peco-select-gitadd

# anyframe
fpath=($HOME/.zsh/anyframe(N-/) $fpath)
autoload -Uz anyframe-init
anyframe-init
bindkey '^x^b' anyframe-widget-cdr
bindkey '^x^r' anyframe-widget-put-history
bindkey '^g^b' anyframe-widget-insert-git-branch
bindkey '^x^f' anyframe-widget-insert-filename
bindkey '^x^a' anyframe-widget-select-widget

# anyenv
[ -f $HOME/.anyenv/bin/anyenv ] && eval "$(anyenv init - zsh)"

# fzf
# https://github.com/junegunn/fzf
export FZF_TMUX=1
export FZF_DEFAULT_OPTS='--reverse --ansi --select-1 --multi --extended --cycle'
alias f="fzf-tmux -u $FZF_DEFAULT_OPTS"
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

true
