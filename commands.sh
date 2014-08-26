#!/bin/sh

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
    echo "Configure settings for Mac OS X."
    $HOME/.dotfiles/osx/defaults.sh -i
    ITERM_PLIST="$HOME/Library/Preferences/com.googlecode.iterm2.plist"
    if [ -e $ITERM_PLIST ]; then
        rm $ITERM_PLIST
    fi
    ln -s $HOME/.dotfiles/osx/com.googlecode.iterm2.plist $ITERM_PLIST
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
            break
            ;;
        brew)
            brew_update
            break
            ;;
        cask)
            brew_update_cask
            break
            ;;
        link)
            link
            break
            ;;
        mac)
            mac_configure
            break
            ;;
        -h)
            help
            exit 0
            break
            ;;
        *)
            echo "$USAGE" 1>&2
            exit 1
    esac
    shift
done
