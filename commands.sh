#!/bin/sh

SCRIPT_PATH="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
DOT_FILES_PATH=$SCRIPT_PATH/dotfiles

DOT_FILE_NAMES=()
for file in $DOT_FILES_PATH/.*
do
    filename=`basename $file`

    if [ $filename != "." ] && [ $filename != ".." ] && [ $filename != ".DS_Store" ]; then
        DOT_FILE_NAMES+=($filename)
    fi
done

link() {
    echo "Creating symlinks..."

    for filename in ${DOT_FILE_NAMES[@]}
    do
        ln -s -v $DOT_FILES_PATH/$filename $HOME/$filename
    done
}

unlink() {
    echo "Removing symlinks..."

    for filename in ${DOT_FILE_NAMES[@]}
    do
        dotfile=$HOME/$filename

        if [ -e $dotfile ] && [ -h $dotfile ]; then
            rm -v $dotfile
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
            unlink
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
