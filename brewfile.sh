#!/bin/sh

# Make sure we're using the latest homebrew
brew update

# Upgrade any already-installed formular
brew upgrade

########################################
# Python
########################################
brew install python
brew install pyenv
brew install pyenv-virtualenv

########################################
# Ruby
########################################
brew install ruby
brew install rbenv
brew install ruby-build

########################################
# Emacs
########################################
#install --cocoa emacs || true #install emacs with brew-cask
brew install cask
brew install ispell

########################################
# zsh
########################################
brew install zsh
brew install zsh-completions

########################################
# misc
########################################
brew install tree
brew tap peco/peco
brew install peco
brew install autossh
brew install hub
brew install postgresql          # for 'pg'
brew install imagemagick         # for 'rmagick'
brew install graphviz
brew install libimobiledevice
brew install class-dump
brew install objc-codegenutils
brew install doxygen
brew install xctool
brew install jq
brew install gnu-sed
brew install cloc

# Remove outdated versions from the cellar.
brew cleanup
