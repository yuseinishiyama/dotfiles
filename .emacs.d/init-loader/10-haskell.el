(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))

(autoload 'ghc-init "ghc" nil t)

(add-to-list 'ac-sources 'ac-source-ghc-mod)

(defun my/haskell-mode-hook ()
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indent))

(add-hook 'haskell-mode-hook 'my/haskell-mode-hook)



