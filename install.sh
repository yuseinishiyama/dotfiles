#!/bin/bash

if [ ! -d "$HOME/.dotfiles" ]; then
    echo "Installing dotfiles..."
    git clone https://github.com/yuseinishiyama/dotfiles.git "$HOME/.dotfiles"
    cd "$HOME/.dotfiles"
    source bootstrap.sh
else
    echo "dotfiles are already installed."
fi
