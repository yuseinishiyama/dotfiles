(when (require 'popwin nil t)
  (popwin-mode 1)
  ;; open buffers of helm with popwin.
  (push '("^\*helm .+\*$" :regexp t) popwin:special-display-config))
