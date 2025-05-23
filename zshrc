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

# environment
export LANG="ja_JP.UTF-8"
export TZ='Asia/Tokyo'
export TERM="xterm-256color"
export GREP=rg
export GREP_COLOR="01;35"
export GREP_COLORS="mt=$GREP_COLOR"
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

# asdf golang
export ASDF_GOLANG_MOD_VERSION_ENABLED=false

# itermplot
#export MPLBACKEND="module://itermplot"

# gtags
# % cd /usr/include
# % gtags ~/.inctags
# % export GTAGSROOT=/usr/include
# % export GTAGDBPATH=~/.inctags
# % export GTAGSLABEL=exuberant-ctags

# docker
COMPOSE_DOCKER_CLI_BUILD=1

alias ls='ls --color=auto'
alias vi='nvim'
alias vim='nvim'
alias VIM='gvim'
alias em='TERM=xterm-256color emacs -nw'
alias g='git'
alias k='kubectl'
alias d='docker'
alias t='go test ./... -count=1'
alias tv='go test ./... -test.v -count=1'
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
alias now="date '+%Y%m%d%H%M%S'"
alias brew="PATH=$HOMEBREW_PREFIX/bin:$HOMEBREW_PREFIX/sbin:/usr/local/bin:/usr/bin:/bin:/usr/local/sbin/:/usr/sbin:/sbin brew"
alias tmux='tmux -u'
if [ "`uname`" = "Darwin" ]; then
    alias ls='ls -G'
    alias em='emacsclient -n "$@"'
    alias VIM='env LANG=ja_JP.UTF-8 /Applications/MacVim.app/Contents/bin/mvim'
    alias ctags='`brew --prefix`/bin/ctags'
    alias goland='open /Applications/GoLand.app'
elif [ "`uname`"  = "CYGWIN_NT-6.1-WOW64" ]; then
    alias gvim='d:/vim/gvim.exe'
elif uname -a | grep -q microsoft; then
    alias open='/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe /c start'
    alias code="'/mnt/c/Users/${USER}/AppData/Local/Programs/Microsoft VS Code/bin/code'"
fi
if [ "${TERM}" = "eterm-color" ]; then
    alias ls='ls -F'
fi

if [ -f ~/.passwd ]; then
    . ~/.passwd
fi
if [ -f ~/.google_key ]; then
    alias google_key="oathtool --totp --base32 `cat ~/.google_key`"
fi

# clipboard
if which pbcopy >/dev/null 2>&1 ; then
    # Mac
    COPY='pbcopy'
elif which clip.exe >/dev/null 2>&1 ; then
    COPY='clip.exe'
    alias pbcopy='clip.exe'
    alias pbpaste='powershell.exe -Command Get-Clipboard'
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
setopt CLOBBER

function tmux_window_name_precmd () {
    if [ "q$TMUX" != "q" ]; then
        echo -ne "\ek$(basename $(pwd))\e\\"
    fi
}
add-zsh-hook precmd tmux_window_name_precmd

function tmux_command_exit_status_precmd () {
    result=$?
    if [ "q$TMUX" != "q" ]; then
        if [ $result -ne 0 ]; then
            echo -ne '\a'
        fi
    fi
}
add-zsh-hook precmd tmux_command_exit_status_precmd

function tmux_window_name_preexec () {
    # zshからGoLandを起動したときに統合Shellで$TMUXが見えてて余分にechoされてしまう対応
    if [ -n $TERMINAL_EMULATOR ]; then
        return 0
    fi
    if [ "q$TMUX" != "q" ]; then
        echo -ne "\ek$1\e\\"
    fi
}
#add-zsh-hook preexec tmux_window_name_preexec

# neotermの時はVIMRUNTIMEがある
if [ -n "$VIMRUNTIME" ]; then
    PROMPT='[%F{cyan}%~%f]
%#%{$reset_color%} '
fi
# GoLand
# TERMINAL_EMULATOR=JetBrains-JediTerm
if [ -n "$TERMINAL_EMULATOR" ]; then
    PROMPT='[%F{cyan}%~%f]
%#%{$reset_color%} '
fi
if [ -n "$OSDK_DIR" ]; then
    PROMPT='%B%(?.%F{green}%n%f.%F{red}%n%f)%b%F{yellow}@%f%F{red}OSDKBUILDER%f [%F{cyan}%~%f]
%#%{$reset_color%} '
fi

# hook関数precmd実行
__call_precmds() {
    type precmd > /dev/null 2>&1 && precmd
    for __pre_func in $precmd_functions; do $__pre_func; done
}

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

# anyenv
function enable-anyenv() {
    if [ -d $HOME/.anyenv/bin ]
    then
        [ -f $HOME/.anyenv/bin/anyenv ] || return
        eval "$($HOME/.anyenv/bin/anyenv init - zsh)"
        # http://qiita.com/luckypool/items/f1e756e9d3e9786ad9ea
        PATH=$HOME/.anyenv/bin:$PATH
        # for tmux
        for D in `ls $HOME/.anyenv/envs`
        do
            PATH="$HOME/.anyenv/envs/$D/shims:$PATH"
        done
    fi
}

# asdf
if [ -d ~/.asdf ]; then
    export PATH="${ASDF_DATA_DIR:-$HOME/.asdf}/shims:$PATH"
    # append completions to fpath
    fpath=(${ASDF_DATA_DIR:-$HOME/.asdf}/completions $fpath)
    # initialise completions with ZSH's compinit
    autoload -Uz compinit && compinit
fi

# direnv
if which direnv >/dev/null 2>&1
then
    eval "$(direnv hook zsh)"
fi

# fzf
# https://github.com/junegunn/fzf
export FZF_TMUX=0
export FZF_DEFAULT_OPTS='--reverse --border --ansi --select-1 --multi --extended --cycle'
export FZF_CTRL_T_COMMAND='rg --files --hidden --follow --glob "!.git/*"'
export FZF_CTRL_T_OPTS='--preview "$HOMEBREW_PREFIX/bin/bat --color=always --style=header,grid --line-range :100 {}"'
#alias f="fzf-tmux -d $FZF_DEFAULT_OPTS"
alias f="fzf -d $FZF_DEFAULT_OPTS"
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

function ga() {
    local selected
    selected=$(unbuffer git status -s | f -m --ansi --preview="echo {} | awk '{print \$2}' | xargs git diff --color" | awk -F ' ' '{print $NF}')
    if [[ -n "$selected" ]]; then
        git add $(echo "$selected" | tr '\n' ' ')
        echo git add $(echo "$selected" | tr '\n' ' ')
    fi
}

function ghq-fzf() {
    repo=`ghq list | f --preview "bat --color=always --style=header,grid --line-range :80 $(ghq root)/{}/README.*"`
    [[ ! -z ${repo} ]] && builtin cd $(ghq root)/${repo}
    __call_precmds
    zle reset-prompt
}
zle -N ghq-fzf
bindkey "^]" ghq-fzf

function fd() {
    local dir
    #dir=$(find ${1:-.} -type d 2> /dev/null | f +m) && cd "$dir"
    dir=$(find ${1:-.} -path '*/\.*' -prune -o -type d -print 2> /dev/null | f +m) && cd "$dir"
}

function githistory() {
    local h
    h=$(git log --pretty=oneline $* | f +m | awk '{print $1}')
    if zle; then
        LBUFFER+="$h"
        CURSOR=$#LBUFFER
        zle -R -c
    else
        print -z -f '%s' "$h"
    fi
}
zle -N githistory
bindkey '^g^h' githistory

function gb() {
    local branch
    branch=$(git --no-pager branch -vv | f +m | sed "s/\* *//" | awk '{print $1}' | sed "s/.* //")
    if zle; then
        LBUFFER+="$branch"
        CURSOR=$#LBUFFER
        zle -R -c
    else
        print -z -f '%s' "$branch"
    fi
}
zle -N gb
bindkey '^g^b' gb

function gba() {
    local branch
    branch=$(git --no-pager branch --all -vv | f +m | sed "s/\* *//" | awk '{print $1}' | sed "s/.* //")
    if zle; then
        LBUFFER+="$branch"
        CURSOR=$#LBUFFER
        zle -R -c
    else
        print -z -f '%s' "$branch"
    fi
}

function kx() {
    local context
    context=$(kubectl config get-contexts | f +m | sed "s/\* *//" | awk '{print $1}' | sed "s/.* //")
    if zle; then
        LBUFFER+="kubectl config use-context $context"
        CURSOR=$#LBUFFER
        zle -R -c
    else
        print -z -f "kubectl config use-context $context"
    fi
}

# spaceship
#[ -f $HOME/.spaceship.zsh ] && source $HOME/.spaceship.zsh

# The next line updates PATH for the Google Cloud SDK.
[ -f $HOME/.google-cloud-sdk/path.zsh.inc ] && source $HOME/.google-cloud-sdk/path.zsh.inc

# The next line enables shell command completion for gcloud.
[ -f $HOME/.google-cloud-sdk/completion.zsh.inc ] && source $HOME/.google-cloud-sdk/completion.zsh.inc

# starship
if which starship >/dev/null 2>&1
then
    eval "$(starship init zsh)"
fi

true
