#!/bin/sh

if ! xcode-select -p > /dev/null 2>&1; then
  echo "Xcode CLI tools are not avaiable. Run \`xcode-select --install\` to install them"
  exit 1
fi

git clone https://github.com/yuseinishiyama/dotfiles.git --depth=1

source dotfiles/lib/brew

install_brew
# Have brew and installed commands available in this session
[ -f /opt/homebrew/bin/brew ] && eval "$(/opt/homebrew/bin/brew shellenv)" # M1
[ -f /usr/local/bin/brew ] && eval "$(/usr/local/bin/brew shellenv)" # Intel

# Re-clone into designated place
brew install ghq
ghq get https://github.com/yuseinishiyama/dotfiles.git

# Main procedure
"$(ghq root)"/"$(ghq list | grep yuseinishiyama/dotfiles)"/bin/run import

# Clean up
rm -rf dotfiles
echo 'dotfile is installed successfully. Restart the terminal'
