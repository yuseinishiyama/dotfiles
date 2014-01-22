;;;;;;;;;;;;;;;;;;;;load-path;;;;;;;;;;;;;;;;;;;;

;; load-pathを再帰的に追加。
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
     (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
         (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
             (normal-top-level-add-subdirs-to-load-path))))))

;; elpa配下をロードパスに指定。
(add-to-load-path "elpa")

;; elisp配下をロードパスに指定。
(add-to-load-path "elisp")

;;;;;;;;;;;;;;;;;;;;Common;;;;;;;;;;;;;;;;;;;;

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
(setq linum-format "%2d ")
(global-linum-mode t)

;; 対応する括弧をハイライトする。
(show-paren-mode t)

;;;;;;;;;;;;;;;;;;;;Key;;;;;;;;;;;;;;;;;;;;

;;C-hをBackSpaceにする
(global-set-key "\C-h" 'delete-backward-char)

;;;;;;;;;;;;;;;;;;;;Font;;;;;;;;;;;;;;;;;;;;

;; (set-face-attribute 'default nil
;;             :family "Menlo" ;; font 
;;            :height 140)    ;; font size

;; (set-fontset-font
;;  nil 'japanese-jisx0208
;;  (font-spec :family "Hiragino Mincho Pro")) ;; font

;; (setq face-font-rescale-alist
;;       '((".*Hiragino_Mincho_pro.*" . 1.2)))

;;;;;;;;;;;;;;;;;;;;Window;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;Scheme;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;Encode;;;;;;;;;;;;;;;;;;;;

(require 'ucs-normalize)
(setq prefer-coding-system 'utf-8-hfs)
;(set-language-environment "Japanese") 
(set-language-environment 'utf-8) ;Japaneseだとmulti-termで日本語が入力できなかったため。
(setq locale-coding-system 'utf-8) ;;設定しないとシェルが文字化けする。
;;(set-default-coding-systems 'utf-8)
;;(set-buffer-file-coding-system 'utf-8)
;;(set-terminal-coding-system 'utf-8)
;;(set-keyboard-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;Shell;;;;;;;;;;;;;;;;;;;;

;; ;; より下に記述した物が PATH の先頭に追加されます
;; (dolist (dir (list
;;               "/sbin"
;;               "/usr/sbin"
;;               "/bin"
;;               "/usr/bin"
;;               "/opt/local/bin"
;;               "/sw/bin"
;;               "/usr/local/bin"
;;               (expand-file-name "~/bin")
;;               (expand-file-name "~/.emacs.d/bin")
;;               ))

;; ;; PATH と exec-path に同じ物を追加します
;;  (when (and (file-exists-p dir) (not (member dir exec-path)))
;;    (setenv "PATH" (concat dir ":" (getenv "PATH")))
;;    (setq exec-path (append (list dir) exec-path))))
;; (setenv "MANPATH" (concat "/usr/local/man:/usr/share/man:/Developer/usr/share/man:/sw/share/man" (getenv "MANPATH")))

;; ;; shell の存在を確認
;; (defun skt:shell ()
;;   (or (executable-find "zsh")
;;       (executable-find "bash")
;;       ;; (executable-find "f_zsh") ;; Emacs + Cygwin を利用する人は Zsh の代りにこれにしてください
;;       ;; (executable-find "f_bash") ;; Emacs + Cygwin を利用する人は Bash の代りにこれにしてください
;;       (executable-find "cmdproxy")
;;       (error "can't find 'shell' command in PATH!!")))

;; ;; shell 名の設定
;; (setq shell-file-name (skt:shell))
;; (setenv "SHELL" shell-file-name)
;; (setq explicit-shell-file-name shell-file-name)

;; multi-termのロード。
;; (setq multi-term-program "/bin/zsh")
;; (require 'multi-term)

;; multi-term内でのショートカットを設定する。
;; (when (require 'multi-term nil t)
;;    (add-hook 'term-mode-hook
;;          '(lambda ()
;;             ;; C-h を term 内文字削除にする
;;             (define-key term-raw-map (kbd "C-h") 'term-send-backspace)
;;             ;; C-y を term 内ペーストにする
;;             (define-key term-raw-map (kbd "C-y") 'term-paste)
;;             )))

;; ;; Emacs が保持する terminfo を利用する
;; (setq system-uses-terminfo nil)

;;;;;;;;;;;;;;;;;;;;Evernote;;;;;;;;;;;;;;;;;;;;

;; (add-hook 'shell-mode-hook
;;           (lambda ()
;;             (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)
;;             ))

;; (add-to-list 'load-path "~/.emacs.d/evernote-mode/")
;;   (setq evernote-ruby-command "/usr/local/Cellar/ruby/1.9.2-p290/bin/ruby")
;;   (require 'evernote-mode)
;;  ;; (setq evernote-username "u-say2480.d1515@m.evernote.com")
;;   (global-set-key "\C-cec" 'evernote-create-note)
;;   (global-set-key "\C-ceo" 'evernote-open-note)
;;   (global-set-key "\C-ces" 'evernote-search-notes)
;;   (global-set-key "\C-ceS" 'evernote-do-saved-search)
;;   (global-set-key "\C-cew" 'evernote-write-note)
;;   (global-set-key "\C-cep" 'evernote-post-region)
;;   (global-set-key "\C-ceb" 'evernote-browser)

;;;;;;;;;;;;;;;;;;;;Package;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives
         '("marmalade" . "http://marmalade-repo.org/packages/")
         '("melpa" . "http://melpa.milkbox.net/packages/"))

;;;;;;;;;;;;;;;;;;;;Auto-Complete;;;;;;;;;;;;;;;;;;;;

(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-1.4/dict")

;;;;;;;;;;;;;;;;;;;;Twitter;;;;;;;;;;;;;;;;;;;;

(require 'twittering-mode)

;; アイコン表示
(setq twittering-icon-mode t)

;; 認証
(setq twittering-account-authorization 'authorized)
(setq twittering-oauth-access-token-alist
      '(("oauth_token" . "239840891-7daJdZEXZlufjw2Vzocju5Dk9qFG1Fa2tE1lXX5z")
	("oauth_token_secret" . "pqJ5PnZJq2nLNpCsO0QTX6PBkePiNuTi6Nsm33aWuRpXt")
	("user_id" . "239840891")
	("screen_name" . "yuseinishiyama")))

;;;;;;;;;;;;;;;;;;;; Audo-Save and Backup ;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;; realtime-markdown-viewer ;;;;;;;;;;;;;;;;;;;;

(setq rtmv:lang 'ruby)

(require 'realtime-markdown-viewer)
