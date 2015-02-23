#!/bin/sh

brew tap caskroom/cask
brew tap caskroom/versions
brew install brew-cask

brew cask install alfred
brew cask install atom
brew cask install appcleaner
brew cask install bettertouchtool
brew cask install clipmenu
brew cask install dash
brew cask install dropbox
brew cask install emacs
brew cask install evernote
brew cask install flux
brew cask install gimp
brew cask install google-chrome
brew cask install google-japanese-ime
brew cask install ifunbox
brew cask install ibrowse
brew cask install iterm2
brew cask install sequel-pro

# needs password
brew cask install karabiner

brew cask install keepassx
brew cask install skype
brew cask install slack
brew cask install spectacle
brew cask install sqwiggle
brew cask install teamviz
brew cask install yorufukurou

brew cask alfred link

brew cleanup
