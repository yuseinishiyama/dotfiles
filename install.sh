#!/bin/sh

if [ ! -d "$HOME/.dotfiles" ]; then
    echo "Installing Xcode CLI tools..."
    touch /tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress;
    PROD=$(softwareupdate -l |
      grep "\*.*Command Line" |
      head -n 1 | awk -F"*" '{print $2}' |
      sed -e 's/^ *//' |
      tr -d '\n')
    softwareupdate -i "$PROD" --verbose;

    install_dir=.dotfiles
    cd $HOME
    git clone https://github.com/yuseinishiyama/dotfiles.git $install_dir
    cd $install_dir
    ./bin/run i
else
    echo "dotfiles are already installed"
fi
