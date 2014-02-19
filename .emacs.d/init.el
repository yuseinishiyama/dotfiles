;;;;;;;;;;;;;;;;;;;; Load Paths ;;;;;;;;;;;;;;;;;;;;

(when (< emacs-major-version 23)
  (defvar user-emacs-directory "~/.dotfiles/.emacs.d/"))

;; load-pathを再帰的に追加する関数
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
     (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
         (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
             (normal-top-level-add-subdirs-to-load-path))))))

;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "elpa" "elisp" "conf")

;;;;;;;;;;;;;;;;;;;; Auto Mode ;;;;;;;;;;;;;;;;;;;;

(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;;;;;;;;;;;;;;;;;;; Common ;;;;;;;;;;;;;;;;;;;;

;; 時間表示
(display-time)

;; スタートアップページを表示しない
(setq inhibit-startup-message t)

;; ビープ音鳴らさない
(setq visible-bell t)

;; 現在行を目立たせる
(global-hl-line-mode)

;; 行番号表示
(line-number-mode t)

;; 列番号表示
(column-number-mode t)

;; 行数表示
;; (setq linum-format "%2d ")
;; (global-linum-mode t)

;; 対応する括弧をハイライトする。
(show-paren-mode t)

;; タブを全てスペースに。
(setq-default indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;; Key-Map ;;;;;;;;;;;;;;;;;;;;

;; BackSpaceにする
(global-set-key "\C-h" 'delete-backward-char)
;; ウィンドウ切り替え
(define-key global-map (kbd "C-t") 'other-window)

;;;;;;;;;;;;;;;;;;;; Font ;;;;;;;;;;;;;;;;;;;;

;; (set-face-attribute 'default nil
;;             :family "Menlo" ;; font 
;;            :height 140)    ;; font size

;; (set-fontset-font
;;  nil 'japanese-jisx0208
;;  (font-spec :family "Hiragino Mincho Pro")) ;; font

;; (setq face-font-rescale-alist
;;       '((".*Hiragino_Mincho_pro.*" . 1.2)))

;;;;;;;;;;;;;;;;;;;; Window ;;;;;;;;;;;;;;;;;;;;

;; 画面分割
(add-hook 'after-init-hook (lambda()
    (setq w (selected-window))
    (setq w2 (split-window-vertically (floor (* 0.7 (window-height)))))
    (select-window w2)
;;    (multi-term)
;;    (setq w3 (split-window-vertically (floor (* 0.2 (window-height)))))
    (run-scheme scheme-program-name)
    (select-window w)))

;; ツールバーを表示しないようにする（Official Emacs の場合は 0）
(tool-bar-mode 0)

;; 透明化
(add-to-list 'default-frame-alist '(alpha . (0.85 0.85)))

;; 色
(custom-set-faces
 '(default ((t
             (:background "navy" :foreground "white");;"#55FF55")
             ))))
 '(cursor ((((class color)
             (background dark))
            (:background "black"));;"#00AA00"))
           (((class color)
             (background light))
            (:background "black"));"#999999"))
           (t ())
           ))

;;;;;;;;;;;;;;;;;;;; Scheme Mode ;;;;;;;;;;;;;;;;;;;;

(setq scheme-program-name "/usr/local/bin/gosh -i")
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)
(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))
(define-key global-map
  "\C-cS" 'scheme-other-window)

;;;;;;;;;;;;;;;;;;;; Encode ;;;;;;;;;;;;;;;;;;;;

(require 'ucs-normalize)
(setq prefer-coding-system 'utf-8-hfs)
;(set-language-environment "Japanese") 
(set-language-environment 'utf-8) ;Japaneseだとmulti-termで日本語が入力できなかったため。
(setq locale-coding-system 'utf-8) ;;設定しないとシェルが文字化けする。
;;(set-default-coding-systems 'utf-8)
;;(set-buffer-file-coding-system 'utf-8)
;;(set-terminal-coding-system 'utf-8)
;;(set-keyboard-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;; Package ;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives
         '("marmalade" . "http://marmalade-repo.org/packages/")
         '("melpa" . "http://melpa.milkbox.net/packages/"))

;;;;;;;;;;;;;;;;;;;; Auto Complete ;;;;;;;;;;;;;;;;;;;;

(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-1.4/dict")
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (ac-config-default))

;;;;;;;;;;;;;;;;;;;; Auto Save and Backup ;;;;;;;;;;;;;;;;;;;;

;; バックアップファイルの保存先を変更。
(setq backup-directory-alist
  (cons (cons ".*" (expand-file-name "~/.emacs.d/backup"))
        backup-directory-alist))

;; 自動保存ファイルの保存先を変更。
(setq auto-save-file-name-transforms
  `((".*", (expand-file-name "~/.emacs.d/backup/") t)))

;;;;;;;;;;;;;;;;;;;; Environment ;;;;;;;;;;;;;;;;;;;;

;; 実行環境の判別
(setq darwin-p  (eq system-type 'darwin)
      ns-p      (eq window-system 'ns)
      carbon-p  (eq window-system 'mac)
      linux-p   (eq system-type 'gnu/linux)
      cygwin-p  (eq system-type 'cygwin)
      nt-p      (eq system-type 'windows-nt)
      meadow-p  (featurep 'meadow)
      windows-p (or cygwin-p nt-p meadow-p))

;;;;;;;;;;;;;;;;;;;; Real Time Markdown Viewer ;;;;;;;;;;;;;;;;;;;;

(setq rtmv:lang 'ruby)

(require 'realtime-markdown-viewer)

;;;;;;;;;;;;;;;;;;;; Undo Tree ;;;;;;;;;;;;;;;;;;;;

(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;;;;;;;;;;;;;;;;;;;; Divided Settings ;;;;;;;;;;;;;;;;;;;;

(load "terminal")

(load "local")
