#!/bin/sh

if [ ! -d "$HOME/.dotfiles" ]; then
    echo "Installing..."

    install_dir=$HOME/.dotfiles
    git clone https://github.com/yuseinishiyama/dotfiles.git $install_dir
    cd $install_dir
    ./bin/run install
else
    echo "dotfiles are already installed."
fi
