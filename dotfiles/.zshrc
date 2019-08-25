ZSH=$HOME/.oh-my-zsh
COMPLETION_WAITING_DOTS="true"
plugins=(git git-hubflow osx xcode bundler rake rbenv ruby)
fpath=(/usr/local/share/zsh-completions $fpath)
source $ZSH/oh-my-zsh.sh
export LANG=en_US.UTF-8
export TERM=xterm-256color
PROMPT='$ '

# PATH
export PATH=$PATH:/bin:/usr/bin:/usr/local/bin
export PATH=$PATH:/sbin:/usr/sbin:/usr/local/sbin
export PATH="$PATH:$HOME/Library/Haskell/bin"
export PATH="$PATH:/Applications/Visual Studio Code.app/Contents/Resources/app/bin"
export GOPATH="$HOME/.go"
export PATH=$GOPATH/bin:$PATH

# Aliases
alias la='ls -a'
alias b='bundle exec'
alias e='code'

# Python
if which pyenv > /dev/null; then
	export PYENV_ROOT="${HOME}/.pyenv"
	export PATH=${PYENV_ROOT}/shims:${PATH}
	eval "$(pyenv init -)";
fi

# Ruby
if which rbenv > /dev/null; then
    eval "$(rbenv init -)"
else
    echo "rbenv not found"
fi

# Peco

## Git branches
alias -g B='`git branch -a | peco --prompt "GIT BRANCH>" | head -n 1 | sed -e "s/^\*\s*//g"`'

## Git repositories
alias repo='cd $(ghq list -p | peco)'

## Git changed files
function git-changed-files() {
    #git status --short | peco | awk -vFPAT='([^ ]+)|("[^"]+")' '{print $2}'
    git status --short | peco | awk '{print $2}'
}
alias -g F='$(git-changed-files)'

## History
function peco-select-history() {
    local tac
    if which tac > /dev/null; then
        tac="tac"
    else
        tac="tail -r"
    fi
    BUFFER=$(\history -n 1 | \
        eval $tac | \
        peco --query "$LBUFFER")
    CURSOR=$#BUFFER
    zle clear-screen
}
zle -N peco-select-history
bindkey '^r' peco-select-history

## Directories
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs
zstyle ':chpwd:*' recent-dirs-max 5000
zstyle ':chpwd:*' recent-dirs-default yes
zstyle ':completion:*' recent-dirs-insert both

function peco-cdr () {
    local selected_dir=$(cdr -l | awk '{ print $2 }' | peco)
    if [ -n "$selected_dir" ]; then
        BUFFER="cd ${selected_dir}"
        zle accept-line
    fi
    zle clear-screen
}
zle -N peco-cdr
bindkey '^x^f' peco-cdr

# Notify when command finishes
## https://gist.github.com/syui/7112389/raw/growl.zsh
## http://qiita.com/kazuph/items/3bfdfce6b7d02b43bf4d
alias pong='perl -nle '\''print "display notification \"$_\" with title \"Terminal\""'\'' | osascript'

preexec() {
  zsh_notify_cmd=$1
  zsh_notify_time=`date +%s`
}

precmd() {
  if (( $? == 0 )); then
    # message
    zsh_notify_status=done\!\!
  else
    zsh_notify_status=fail
  fi
  if [[ "${zsh_notify_cmd}" != "" ]]; then
    # time
    if (( `date +%s` - ${zsh_notify_time} > 3 )); then
      echo ${zsh_notify_cmd} ${zsh_notify_status}  | pong
    fi
  fi
  zsh_notify_cmd=
}

# Load local settings
_ZSH_LOCAL_SETTING="$HOME/.zshrc.local"
if [ -f $_ZSH_LOCAL_SETTING ]; then
    source $_ZSH_LOCAL_SETTING
fi
