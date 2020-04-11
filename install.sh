#!/bin/sh

git clone https://github.com/yuseinishiyama/dotfiles.git --depth=1
./dotfiles/bin/bootstrap
rm -rf dotfiles
echo 'dotfile is installed successfully. Restart the terminal'
