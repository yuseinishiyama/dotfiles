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
