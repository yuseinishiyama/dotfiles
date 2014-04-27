;; BackSpaceにする
(global-set-key "\C-h" 'delete-backward-char)

;; ウィンドウ切り替え
(define-key global-map (kbd "C-t") 'other-window)

;; 折り返しトグルコマンド
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)

;; 改行時にインデント
(global-set-key (kbd "C-m") 'newline-and-indent)

