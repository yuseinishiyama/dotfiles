# Make sure we're using the latest homebrew
update || true

# Upgrade any already-installed formular
upgrade || true

########################################
# Python
########################################
install python || true
install pyenv || true
install pyenv-virtualenv || true

########################################
# Ruby
########################################
install ruby || true
install rbenv || true
install ruby-build || true

########################################
# Emacs
########################################
#install --cocoa emacs || true #install emacs with brew-cask
install cask || true
install ispell || true

########################################
# zsh
########################################
install zsh || true
install zsh-completions || true

########################################
# misc
########################################
install tree || true
tap peco/peco || true
install peco || true
install autossh || true
install hub || true
install postgresql || true      # for 'pg'
install imagemagick || true     # for 'rmagick'
install graphviz || true
install libimobiledevice || true
install class-dump || true

# Remove outdated versions from the cellar.
cleanup
