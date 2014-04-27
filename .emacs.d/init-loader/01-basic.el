;;;;;;;;;;;;;;;;;;;; Common ;;;;;;;;;;;;;;;;;;;;

;; 時間表示
(display-time)

;; スタートアップページを表示しない
(setq inhibit-startup-message t)

;; ビープ音鳴らさない
(setq visible-bell t)

;; ツールバーを表示しないようにする（Official Emacs の場合は 0）
(tool-bar-mode 0)

;; 現在行をハイライト
(global-hl-line-mode)

;; 対応する括弧をハイライトする
(show-paren-mode t)

;; 行番号表示
(line-number-mode t)

;; 列番号表示
(column-number-mode t)

;; 行数表示
;; (setq linum-format "%2d ")
;; (global-linum-mode t)

;; タブを全てスペースに。
(setq-default indent-tabs-mode nil)

;; タイトルバーにフルパス
(setq frame-title-format "%f")

;; キーストロークをエコーエリアに早く表示する。
(setq echo-keystrokes 0.1)

;; GCを減らす
(setq gc-cons-threshold (* 10 gc-cons-threshold))

;;;;;;;;;;;;;;;;;;;; Emacs Client ;;;;;;;;;;;;;;;;;;;;

;; サーバーの起動
(when (require 'server nil t)
  (unless (server-running-p)
    (server-start)))

;; 終了せずに休止状態に
(global-set-key (kbd "C-x C-c") 'ns-do-hide-emacs)

;; M-x exitで終了
(defalias 'exit 'save-buffers-kill-emacs)

;;;;;;;;;;;;;;;;;;;; Windown ;;;;;;;;;;;;;;;;;;;;

;; 画面分割
(add-hook 'after-init-hook (lambda()
    (setq w (selected-window))
    (setq w2 (split-window-vertically (floor (* 0.7 (window-height)))))
    (select-window w2)
;;    (multi-term)
;;    (setq w3 (split-window-vertically (foor (* 0.2 (window-height)))))
    (run-scheme scheme-program-name)
    (select-window w)))

;;;;;;;;;;;;;;;;;;;; Encode ;;;;;;;;;;;;;;;;;;;;

(require 'ucs-normalize)
(setq prefer-coding-system 'utf-8-hfs)
;;(set-language-environment "Japanese") 
(set-language-environment 'utf-8) ;Japaneseだとmulti-termで日本語が入力できなかったため。
(setq locale-coding-system 'utf-8) ;;設定しないとシェルが文字化けする。

;;;;;;;;;;;;;;;;;;;; Auto Save and Backup ;;;;;;;;;;;;;;;;;;;;

;; バックアップファイルの保存先を変更。
(setq backup-directory-alist
  (cons (cons ".*" (expand-file-name "~/.emacs.d/backup"))
        backup-directory-alist))

;; 自動保存ファイルの保存先を変更。
(setq auto-save-file-name-transforms
  `((".*", (expand-file-name "~/.emacs.d/backup/") t)))
