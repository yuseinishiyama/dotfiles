#!/bin/sh

git clone https://github.com/yuseinishiyama/dotfiles.git --depth=1

source dotfiles/lib/brew
source dotfiles/lib/dev_tools

# Re-clone into designated place
install_commandline_tools
install_brew
brew install ghq
GHQ_ROOT=~/.ghq ghq get https://github.com/yuseinishiyama/dotfiles.git

# Main procedure
$(ghq root)/$(ghq list | grep yuseinishiyama/dotfiles)/bin/run import

# Clean up
rm -rf dotfiles
echo 'dotfile is installed successfully. Restart the terminal'
