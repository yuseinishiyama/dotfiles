#!/bin/bash

# create symlinks
symlink() {
    echo "Create symlinks..."
    DOT_FILES=( .emacs.d .zshrc .bash_profile .gitconfig .gitconfig.local .tmux.conf .peco)

    for file in ${DOT_FILES[@]}
    do
        ln -s $HOME/.dotfiles/$file $HOME/$file
    done
}

brew_setup() {
    echo "Setup homebrew..."
    # Install homebrew
    ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"
    brew bundle $HOME/.dotfiles/Brewfile
    brew bundle $HOME/.dotfiles/Caskfile
    brew cask alfred link # TODO: 失敗する
}

zsh_setup() {
    # Install oh-my-zsh
    # !!! oh-my-zsh makes zsh default shell automatically. !!!
    curl -L http://install.ohmyz.sh | sh
}

symlink
brew_setup
zsh_setup

echo "finish!"

# TODO: Settings for OS X

