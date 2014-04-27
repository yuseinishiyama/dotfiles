;; 英語
(set-face-attribute 'default nil
                    :family "Source Code Pro for Powerline"
                    :height 140)

;; 日本語
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Hiragino Mincho Pro"))

;; フォントの横幅
(setq face-font-rescale-alist
      '((".*Menlo.*" . 1.0)
        (".*Hiragino_Mincho_pro.*" . 1.2)))
