(when (require 'helm-config nil t)
  (helm-mode 1)

  ;; mini-bufferでC-hがprefix commandとして機能してしまうので上書き。
  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
  
  ;; mini-bufferでのデフォルトのTABの挙動がActionの表示なので、補完に上書き。
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action))
