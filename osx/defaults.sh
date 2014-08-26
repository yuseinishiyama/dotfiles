CMD=`basename $0`

_set() {
    #1.デスクトップのアイコン非表示
    defaults write com.apple.finder CreateDesktop -bool FALSE; killall Finder
    #2.保存時、ディフォルトで詳細設定を開く
    defaults write -g NSNavPanelExpandedStateForSaveMode -boolean true
    #3.印刷時、ディフォルトで詳細設定を開く
    defaults write -g PMPrintingExpandedStateForPrint -boolean true
    #4.スクリーンショットの保存先を変更
    defaults write com.apple.screencapture location ~/Pictures/ScreenCaptures
    #5.スタックのリスト表示方法変更
    defaults write com.apple.dock use-new-list-stack -boolean true; killall Dock
    #6.スタックハイライト
    defaults write com.apple.dock mouse-over-hilite-stack -bool true
    #7.Finderにフルパス
    defaults write com.apple.finder _FXShowPosixPathInTitle -bool true
    #8.Dockのアニメーション禁止
    defaults write com.apple.dock no-bouncing -bool true
    #9.シングルアプリケーションモード
    #defaults write com.apple.dock single-app -bool true
}
    
_unset() {
    #1
    defaults delete com.apple.finder CreateDesktop;killall Finder
    #2
    defaults delete -g NSNavPanelExpandedStateForSaveMode
    #3
    defaults delete -g PMPrintingExpandedStateForPrint
    #4
    defaults delete com.apple.screencapture location
    #5
    defaults delete com.apple.dock use-new-list-stack; killall Dock
    #6
    defaults delete com.apple.finder _FXShowPosixPathInTitle
    #7
    defaults delete com.apple.dock mouse-over-hilite-stack
    #8
    defaults delete com.apple.dock no-bouncing
    #9
    #defaults delete com.apple.dock single-app
}

case $1 in
    -i)
            _set
            ;;
    -u)
            _unset
            ;;
    *)
            echo "Usage: $CMD [-i | -u]"
esac    
