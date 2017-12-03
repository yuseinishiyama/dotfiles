(when (require 'auto-complete-config nil t)
  ;; デフォルトで登録されていない辞書ファイルの格納先。
  ;; (パッケージのディレクトリ内の辞書ファイルはデフォルトで登録済み)
  (add-to-list 'ac-dictionary-directories (concat user-emacs-directory "ac-dict"))

  (ac-config-default)

  (global-auto-complete-mode t)
  
  (setq ac-use-menu-map t) ;; 補完メニュー表示中のキーマップを有効に。
  ;; 以下デフォルトで設定済み。
  ;; (define-key ac-menu-map "\C-n" 'ac-next)
  ;; (define-key ac-menu-map "\C-p" 'ac-previous)

  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete))
