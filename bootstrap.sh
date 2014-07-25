#!/bin/bash

# create symlinks
symlink() {
    echo "Create symlinks..."
    DOT_FILES=( .emacs.d .zshrc .bash_profile .gitconfig .tmux.conf .peco)

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
# TODO: oh-my-zsh
symlink
brew_setup
chsh -s `which zsh` # Set zsh as default shell.
echo "finish!"

# TODO: Settings for mac

