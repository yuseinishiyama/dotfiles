# plugins
fpath+=/usr/local/share/zsh-completions
source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# interface
PROMPT='%B%(?.%F{green}.%F{red})>%b%f '
export LANG=en_US.UTF-8
export TERM=xterm-256color
export CLICOLOR=1
export LSCOLORS='dxfxcxdxbxegedabagacad'
export LS_COLORS='di=33:ln=35:so=32:pi=33:ex=31:bd=34;46:cd=34;43:su=30;41:sg=30;46:tw=30;42:ow=30;43'
export GREP_COLOR='1;33'

# completion
zstyle ':completion:*:default' menu select=2 # focus selected
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}' # case-insensitive
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS} # color suggestions

# history
HISTSIZE=10000000
SAVEHIST=$HISTSIZE
setopt share_history
setopt histignorespace
setopt histignorealldups

# path
export GOPATH="$HOME/.go"
PATH=/usr/local/opt/coreutils/libexec/gnubin:$PATH #prioritize coreutils
PATH=$PATH:$GOPATH/bin
PATH=$PATH:$HOME/Library/Haskell/bin
PATH="$PATH:/Applications/Visual Studio Code.app/Contents/Resources/app/bin"
PATH=$PATH:$HOME/.ghq/github.com/yuseinishiyama/dotfiles/bin

# aliases
alias b='bundle exec'
alias cgr='cd `git rev-parse --show-toplevel`' # change to git root
alias g='git'
alias la='ls -a'
alias s='source $HOME/.zshrc'

# tmux
function ssh-then-tmux() {
  title=$2
  if [[ -z $title ]] ; then
    title='default'
  fi
  ssh -t "$1" "tmux -CC new -A -s yusei-${title}"
}
alias tsh='ssh-then-tmux'

# python
if which pyenv > /dev/null; then
	export PYENV_ROOT="${HOME}/.pyenv"
	export PATH=${PYENV_ROOT}/shims:${PATH}
	eval "$(pyenv init -)";
fi

# ruby
if which rbenv > /dev/null; then
    eval "$(rbenv init -)"
else
    echo "rbenv not found"
fi

# peco

## git branches
function select-branch() {
  git branch --format '%(refname:lstrip=2)' | peco
}
function change-branch() {
  select-branch | xargs git checkout
}
alias -g B='$(select-branch)'
zle -N change-branch
bindkey '^x^b' change-branch

## git repositories
function select-repo() {
  ghq list -p | peco
}
function change-repo() {
  local repo=$(select-repo)
  if [ -n "$repo" ]; then
      BUFFER="cd $repo"
      zle accept-line
  fi
}
zle -N change-repo
bindkey '^x^r' change-repo

## git dirty files
function git-dirty-files() {
  #git status --short | peco | awk -vFPAT='([^ ]+)|("[^"]+")' '{print $2}'
  git status --short | peco | awk '{print $2}'
}
alias -g F='$(git-dirty-files)'

## history
function select-history() {
  # Remove duplicated lines while keeping the order
  BUFFER=$(history -n 1 | tac | awk '!seen[$0]++' | peco)
  CURSOR=$#BUFFER
}
zle -N select-history
bindkey '^x^h' select-history

# notify when command finishes
## https://gist.github.com/syui/7112389/raw/growl.zsh
## http://qiita.com/kazuph/items/3bfdfce6b7d02b43bf4d
alias pong='perl -nle '\''print "display notification \"$_\" with title \"Shell\""'\'' | osascript'

preexec() {
  zsh_notify_cmd=`echo $1 | head -1 | awk '{ print $1 }'`
  zsh_notify_time=`date +%s`
}

precmd() {
  if [ $? -eq 0 ]; then
    zsh_notify_status=done
  else
    zsh_notify_status=fail
  fi
  if [[ "${zsh_notify_cmd}" != "" ]]; then
    if (( `date +%s` - ${zsh_notify_time} > 3 )); then
      echo ${zsh_notify_cmd} ${zsh_notify_status} | pong
    fi
  fi
  zsh_notify_cmd=
}

# vscode
function vscode-remote() {
  if [[ -z "$1" ]]; then
    echo "usage: $0 remote-host"
    return 1
  fi
  code --folder-uri "vscode-remote://ssh-remote+$1/"
}
alias e='code -n'
alias er='vscode-remote'

# local settings
_ZSH_LOCAL_SETTING="$HOME/.zshrc.local"
if [ -f $_ZSH_LOCAL_SETTING ]; then
    source $_ZSH_LOCAL_SETTING
fi
