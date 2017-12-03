(when (require 'flycheck nil t)
  (add-hook 'after-init-hook #'global-flycheck-mode))
  
