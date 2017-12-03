;;;;;;;;;;;;;;;;;;;; Undo Tree ;;;;;;;;;;;;;;;;;;;;

(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;;;;;;;;;;;;;;;;;;;; Undo Hist ;;;;;;;;;;;;;;;;;;;;

(when (require 'undohist nil t)
  (undohist-initialize))
