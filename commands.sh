#!/bin/sh

DOT_FILES=( .emacs.d .zshrc .zshrc.local .bash_profile .gitconfig .gitconfig.local .gitignore.global .tmux.conf .peco .atom )
SCRIPT_PATH="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

link() {
    # http://stackoverflow.com/questions/9104337/create-a-symbolic-link-of-directory-in-ubuntu
    unlink

    echo "Create symlinks..."
    for file in ${DOT_FILES[@]}
    do
        ln -s $SCRIPT_PATH/$file $HOME/$file
    done
}

unlink() {
    echo "Remove symlinks..."
    for file in ${DOT_FILES[@]}
    do
        if [ -e $HOME/$file -o -h $HOME/$file ]; then
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
    echo "Update homebrew.."
    $SCRIPT_PATH/brewfile.sh
}

brew_update_cask() {
    echo "Update homebrew-cask..."
    $SCRIPT_PATH/caskfile.sh
}
 
zsh_install() {
    # Install oh-my-zsh
    # !!! oh-my-zsh makes zsh default shell automatically. !!!
    echo "Install zsh..."
    curl -L http://install.ohmyz.sh | sh
}

mac_configure() {
    echo "Configure settings for Mac OS X."
    $SCRIPT_PATH/defaults.sh -i
    ITERM_PLIST="$HOME/Library/Preferences/com.googlecode.iterm2.plist"
    if [ -e $ITERM_PLIST ]; then
        rm $ITERM_PLIST
    fi
    ln -s $SCRIPT_PATH/osx/com.googlecode.iterm2.plist $ITERM_PLIST
}

CMDNAME=`basename $0`
USAGE="Usage: $CMDNAME [install | brew | cask | link | mac]"
help() {
    echo $USAGE
    echo ""
    echo "Commands are:"
    echo "  install    Setup all configurations"
    echo "  brew       Install via Brewfile"
    echo "  cask       Install via Caskfile"
    echo "  link       Create symlinks for dotfiles"
    echo "  mac        Configure defaults and iTerm2."
}

if [ $# -eq 0 ]; then
    echo "$USAGE" 1>&2
    exit 1
fi

while [ $# -gt 0 ]
do
    case $1 in
        install)
            link
            brew_install
            zsh_install
            ;;
        brew)
            brew_update
            ;;
        cask)
            brew_update_cask
            ;;
        link)
            link
            ;;
        mac)
            mac_configure
            ;;
        -h)
            help
            exit 0
            ;;
        *)
            echo "$USAGE" 1>&2
            exit 1
    esac
    shift
done
