export EDITOR='code -w'

[ -f /opt/homebrew/bin/brew ] && eval "$(/opt/homebrew/bin/brew shellenv)" # M1
[ -f /usr/local/bin/brew ] && eval "$(/usr/local/bin/brew shellenv)" # Intel

# plugins
fpath+=$HOMEBREW_PREFIX/share/zsh-completions
source $HOMEBREW_PREFIX/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

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
PATH=$HOMEBREW_PREFIX/opt/coreutils/libexec/gnubin:$PATH #prioritize coreutils
PATH=$PATH:$GOPATH/bin
PATH=$PATH:$HOME/Library/Haskell/bin
PATH="$PATH:/Applications/Visual Studio Code.app/Contents/Resources/app/bin"
PATH=$PATH:$HOME/ghq/github.com/yuseinishiyama/dotfiles/bin

# aliases
alias b='bundle exec'
alias cgr='cd `git rev-parse --show-toplevel`' # change to git root
alias g='git'
alias la='ls -a'
alias pbunbreak="pbpaste | tr '\n' ' ' | pbcopy"
alias s='source $HOME/.zshrc'
alias ls='ls --color=auto'

# edit command in editor
autoload -U edit-command-line
zle -N edit-command-line
bindkey '^x^e' edit-command-line

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
alias -g B='$(select-branch)'

## git repositories
function select-repo() {
  ghq list -p | peco
}
alias -g R='$(select-repo)'

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

# kubernetes
alias k=kubectl
ks() {
    kubectl config get-contexts -o name | peco --prompt "CONTEXT>" | \
        xargs kubectl config use-context
    kubectl get namespace --no-headers -o custom-columns=":metadata.name" | \
        peco --prompt "NAMESPACE>" | xargs kubectl config set-context --current --namespace
}

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh" || true
function iterm2_print_user_vars() {
  iterm2_set_user_var kubecontext $(kubectl config current-context):$(kubectl config view --minify --output 'jsonpath={..namespace}')
}

# local settings
_ZSH_LOCAL_SETTING="$HOME/.zshrc.local"
if [ -f $_ZSH_LOCAL_SETTING ]; then
    source $_ZSH_LOCAL_SETTING
fi
