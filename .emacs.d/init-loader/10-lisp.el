;; Common Lisp
(when (require 'slime-autoloads nil t)
  (setq inferior-lisp-program "/usr/local/bin/clisp")
  (setq slime-contribs '(slime-fancy)))

;; Scheme
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

;; Rainbow Delimiters
(when (require 'rainbow-delimiters nil t)
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'common-lisp-mode 'rainbow-delimiters-mode)
  (add-hook 'scheme-mode 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode 'rainbow-delimiters-mode))
