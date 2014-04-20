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
(add-to-load-path "elpa" "elisp" "conf" "public_repos")

;;;;;;;;;;;;;;;;;;;; Auto Install ;;;;;;;;;;;;;;;;;;;;

(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.dotfiles/.emacs.d/elisp/")
  (auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup))

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

;; タイトルバーにフルパス
(setq frame-title-format "%f")

;;;;;;;;;;;;;;;;;;;; Key-Map ;;;;;;;;;;;;;;;;;;;;

;; BackSpaceにする
(global-set-key "\C-h" 'delete-backward-char)

;; ウィンドウ切り替え
(define-key global-map (kbd "C-t") 'other-window)

;; 折り返しトグルコマンド
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)

;; 改行時にインデント
(global-set-key (kbd "C-m") 'newline-and-indent)

;;;;;;;;;;;;;;;;;;;; Font ;;;;;;;;;;;;;;;;;;;;

;; 英語
(set-face-attribute 'default nil
                    :family "Source Code Pro for Powerline" ;; font 
                    :height 140)

;; 日本語
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Hiragino Mincho Pro"))

;; フォントの横幅
(setq face-font-rescale-alist
      '((".*Menlo.*" . 1.0)
        (".*Hiragino_Mincho_pro.*" . 1.2)))

;;;;;;;;;;;;;;;;;;;; Window ;;;;;;;;;;;;;;;;;;;;

;; 画面分割
(add-hook 'after-init-hook (lambda()
    (setq w (selected-window))
    (setq w2 (split-window-vertically (floor (* 0.7 (window-height)))))
    (select-window w2)
;;    (multi-term)
;;    (setq w3 (split-window-vertically (foor (* 0.2 (window-height)))))
    (run-scheme scheme-program-name)
    (select-window w)))

;; ツールバーを表示しないようにする（Oficial Emacs の場合は 0）
(tool-bar-mode 0)

;; 透明化
(add-to-list 'default-frame-alist '(alpha . (0.85 0.85)))

;; 色
(when (require 'color-theme nil t)
  (color-theme-initialize)
  (color-theme-matrix))

;; フェイス設定例
;; (custom-set-faces
;;  '(default ((t
;;              (:background "navy" :foreground "white");;"#55FF55")
;;              ))))
;;  '(cursor ((((class color)
;;              (background dark))
;;             (:background "black"));;"#00AA00"))
;;            (((class color)
;;              (background light))
;;             (:background "black"));"#999999"))
;;            (t ())
;;            ))

;;;;;;;;;;;;;;;;;;;; Encode ;;;;;;;;;;;;;;;;;;;;

(require 'ucs-normalize)
(setq prefer-coding-system 'utf-8-hfs)
;;(set-language-environment "Japanese") 
(set-language-environment 'utf-8) ;Japaneseだとmulti-termで日本語が入力できなかったため。
(setq locale-coding-system 'utf-8) ;;設定しないとシェルが文字化けする。

;;;;;;;;;;;;;;;;;;;; Package ;;;;;;;;;;;;;;;;;;;;

(when (require 'package nil t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
  (package-initialize))

;;;;;;;;;;;;;;;;;;;; Auto Complete ;;;;;;;;;;;;;;;;;;;;

(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-1.4/dict")
  (global-auto-complete-mode t)
  (define-key ac-menu-map (kbd "C-n") 'ac-next)
  (define-key ac-menu-map (kbd "C-p") 'ac-previous)
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

;;;;;;;;;;;;;;;;;;;; Undo Tree ;;;;;;;;;;;;;;;;;;;;

(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;;;;;;;;;;;;;;;;;;;; Undo Hist ;;;;;;;;;;;;;;;;;;;;

(when (require 'undohist nil t)
  (undohist-initialize))

;;;;;;;;;;;;;;;;;;;; Anything ;;;;;;;;;;;;;;;;;;;;

(when (require 'anything-config nil t)
  (global-set-key (kbd "\C-x b") 'anything))

;;;;;;;;;;;;;;;;;;;; Ruby ;;;;;;;;;;;;;;;;;;;;

;; Ruby
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile$" . ruby-mode))

(when (require 'ruby-block nil t)
  (setq ruby-block-highlight-toggle t))

(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
(autoload 'inf-ruby-setup-keybindings "inf-ruby" "" t)
(eval-after-load 'ruby-mode
  '(add-hook 'ruby-mode-hook 'inf-ruby-setup-keybindings))

(require 'ruby-electric nil t)

(defun ruby-mode-hooks ()
  (ruby-block-mode t)
  (ruby-electric-mode t))
(add-hook 'ruby-mode-hook 'ruby-mode-hooks)

;; (require 'ido)
;; (ido-mode t)
(require 'rinari)

;;;;;;;;;;;;;;;;;;;; Haskell ;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))

;;;;;;;;;;;;;;;;;;;; Scheme ;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;; Markdown ;;;;;;;;;;;;;;;;;;;;

(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(setq rtmv:lang 'ruby)

(require 'realtime-markdown-viewer)

;;;;;;;;;;;;;;;;;;;; Divided Settings ;;;;;;;;;;;;;;;;;;;;

(load "terminal")

(load "local")
