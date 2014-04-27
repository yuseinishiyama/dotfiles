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
(add-to-load-path ".cask")

;;;;;;;;;;;;;;;;;;;; Init Loader ;;;;;;;;;;;;;;;;;;;;

(when (require 'init-loader)
  (setq init-loader-show-log-after-init nil)
  (init-loader-load (concat user-emacs-directory "init-loader")))

;;;;;;;;;;;;;;;;;;;; Cask ;;;;;;;;;;;;;;;;;;;;

(when (require 'cask nil t)
  (cask-initialize))

(require 'pallet)

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
