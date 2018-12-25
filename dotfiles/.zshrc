# Common
ZSH=$HOME/.oh-my-zsh
ZSH_THEME="agnoster"
COMPLETION_WAITING_DOTS="true"
plugins=(brew git git-hubflow osx xcode bundler rails rake rbenv ruby zsh-syntax-highlighting)
source $ZSH/oh-my-zsh.sh
export LANG=en_US.UTF-8
export TERM=xterm-256color
alias la='ls -a'
alias b='bundle exec'
prompt_context() {}

# Postgre
export PGDATA=/usr/local/var/postgres

# PATH
export PATH=/usr/local/bin:/usr/sbin:/sbin:/usr/bin:/bin:$PATH
export PATH="$HOME/Library/Haskell/bin:$PATH"
export PATH="/usr/local/heroku/bin:$PATH"
export PATH="$PATH:/Applications/Visual Studio Code.app/Contents/Resources/app/bin"
export GOPATH="$HOME/.go"
export PATH=$GOPATH/bin:$PATH

# Emacs
alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs'
function emacsclient () {
    /Applications/Emacs.app/Contents/MacOS/bin/emacsclient -n $1 --alternate-editor /Applications/Emacs.app/Contents/MacOS/Emacs &
}

# VSCode
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

# Zsh
fpath=(/usr/local/share/zsh-completions $fpath)

cheat-sheet () { zle -M "`cat ~/zsh/cheat-sheet.conf`" }
zle -N cheat-sheet
bindkey "^[^h" cheat-sheet

git-cheat () { zle -M "`cat ~/zsh/git-cheat.conf`" }
zle -N git-cheat
bindkey "^[^g" git-cheat


# Peco
alias -g B='`git branch -a | peco --prompt "GIT BRANCH>" | head -n 1 | sed -e "s/^\*\s*//g"`'

function git-changed-files() {
    #git status --short | peco | awk -vFPAT='([^ ]+)|("[^"]+")' '{print $2}'
    git status --short | peco | awk '{print $2}'
}
alias -g F='$(git-changed-files)'
alias repo='cd $(ghq list -p | peco)'
alias repo-open='gh-open $(ghq list -p | peco)'

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

# Notify after command finished
## zsh integration: any command that takes longer than 3 seconds
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
