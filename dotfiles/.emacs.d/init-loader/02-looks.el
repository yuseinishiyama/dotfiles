;; 透明化
(add-to-list 'default-frame-alist '(alpha . (0.9 0.9)))

;; Powerline
(when (require 'powerline nil t)
  (powerline-default-theme))

;; 色
(when (require 'color-theme nil t)
  (color-theme-initialize)
  (color-theme-dark-laptop))

;; フェイス設定例
;; (custom-set-faces
;;  '(default ((t
;;              (:background "navy" :foreground "white");;"#55FF55")
;;              ))))
;;  '(cursor ((((class color)
;;              (background dark))
;;             (:background "black"));;"#00AA00"))
;;            (((class color)
;;              (background light))
;;             (:background "black"));"#999999"))
;;            (t ())
;;            ))
