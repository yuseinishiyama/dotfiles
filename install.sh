#!/bin/sh

install_commandline_tools() {
    touch /tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress;
    PROD=$(softwareupdate -l |
      grep "\*.*Command Line" |
      head -n 1 | awk -F"*" '{print $2}' |
      sed -e 's/^ *//' |
      tr -d '\n')
    softwareupdate -i "$PROD" --verbose;
}

install_commandline_tools

git clone https://github.com/yuseinishiyama/dotfiles.git --depth=1

source dotfiles/lib/brew

# Re-clone into designated place
install_brew
brew install ghq
GHQ_ROOT=~/.ghq ghq get https://github.com/yuseinishiyama/dotfiles.git

# Main procedure
$(ghq root)/$(ghq list | grep yuseinishiyama/dotfiles)/bin/run import

# Clean up
rm -rf dotfiles
echo 'dotfile is installed successfully. Restart the terminal'
