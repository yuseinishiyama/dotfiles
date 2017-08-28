#!/bin/sh

# How to get started.
# sh -c "`curl -fsSL https://raw.github.com/yuseinishiyama/dotfiles/master/install.sh`"

if [ ! -d "$HOME/.dotfiles" ]; then
    echo "Installing dotfiles..."
    git clone https://github.com/yuseinishiyama/dotfiles.git "$HOME/.dotfiles"
    cd $HOME/.dotfiles
    ./commands.sh install
else
    echo "dotfiles are already installed."
fi
