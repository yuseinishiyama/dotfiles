# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

ZSH_THEME="powerline"

COMPLETION_WAITING_DOTS="true"

plugins=(brew git git-hubflow osx xcode bundler heroku rails rake rbenv ruby zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

export LANG=ja_JP.UTF-8

export TERM=xterm-256color

export PGDATA=/usr/local/var/postgres

# PATH
export PATH=$PATH:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin
export PATH="$HOME/Library/Haskell/bin:$PATH" 
export PATH="/usr/local/heroku/bin:$PATH"

# SVN 1.7 of Xcode
alias svn='/Applications/Xcode.app/Contents/Developer/usr/bin/svn'

# Emacs
alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs'

# virtualenv
VIRTUAL_ENV_WRAPPER=/usr/local/bin/virtualenvwrapper.sh
if [ -e $VIRTUAL_ENV_WRAPPER ]; then
    source $VIRTUAL_ENV_WRAPPER
else
    echo "virtualenvwrapper not found"
fi  

if which pyenv > /dev/null; then
	export PYENV_ROOT="${HOME}/.pyenv"
	export PATH=${PYENV_ROOT}/shims:${PATH}
	eval "$(pyenv init -)";
fi

# rbenv
eval "$(rbenv init -)"
export PATH="$HOME/.rbenv/shims:$PATH"

# zsh-completions
fpath=(/usr/local/share/zsh-completions $fpath)

# cheat-sheet
cheat-sheet () { zle -M "`cat ~/zsh/cheat-sheet.conf`" }
zle -N cheat-sheet
bindkey "^[^h" cheat-sheet

git-cheat () { zle -M "`cat ~/zsh/git-cheat.conf`" }
zle -N git-cheat
bindkey "^[^g" git-cheat
