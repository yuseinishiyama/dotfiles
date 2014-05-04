(when (require 'helm-config nil t)
  (helm-mode 1)

  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)
  (global-set-key (kbd "C-;") 'helm-mini)
  (global-set-key (kbd "C-c d") 'helm-descbinds)

  ;; mini-bufferでC-hがprefix commandとして機能してしまうので上書き。
  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
  
  ;; mini-bufferでのデフォルトのTABの挙動がActionの表示なので、補完に上書き。
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action))
