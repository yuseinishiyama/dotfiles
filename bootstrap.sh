#!/bin/bash

DOT_FILES=( .emacs.d .zshrc .bash_profile .gitconfig .gitconfig.local .tmux.conf .peco )

link() {
    unlink
    echo "Create symlinks..."
    for file in ${DOT_FILES[@]}
    do
        ln -s $HOME/.dotfiles/$file $HOME/$file
    done
}

unlink() {
    echo "Remove symlinks..."
    for file in ${DOT_FILES[@]}
    do
        if [ -e $file ]; then
            # If symlinks are already exist, remove them first.
            # http://stackoverflow.com/questions/9104337/create-a-symbolic-link-of-directory-in-ubuntu
            rm $HOME/$file
        fi
    done
}

brew_install() {
    echo "Install homebrew..."
    ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"
    brew_update
    brew_update_cask
}

brew_update() {
    echo "Update homebrew..."
    brew bundle $HOME/.dotfiles/Brewfile
}

brew_update_cask() {
    echo "Update homebrew-cask..."
    brew bundle $HOME/.dotfiles/Caskfile
    brew cask alfred link # TODO: 失敗する
}
 
zsh_install() {
    # Install oh-my-zsh
    # !!! oh-my-zsh makes zsh default shell automatically. !!!
    echo "Install zsh..."
    curl -L http://install.ohmyz.sh | sh
}

mac_configure() {
    exit 1
}

CMDNAME=`basename $0`
USAGE="Usage: $CMDNAME [command ...]"

while [ $# -gt 0 ]
do
    case $1 in
        install)
            echo 'install'
            link
            brew_install
            zsh_install
            break
            ;;
        brew)
            echo 'brew'
            brew_update
            break
            ;;
        cask)
            echo 'cask'
            brew_update_cask
            break
            ;;
        link)
            echo 'link'
            link
            break
            ;;
        mac)
            echo 'mac'
            break;;
        *)
            echo "$USAGE" 1>&2
            exit 1
    esac
    shift
done

echo "finish!"

