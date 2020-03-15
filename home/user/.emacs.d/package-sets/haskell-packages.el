;;; haskell-packages.el

;;; Commentary:
;;
;; My settings for working with haskell.
;;

;;; Code:

(use-package haskell-mode
  :mode "\\.hs\\'"
  :hook
  (haskell-mode . interactive-haskell-mode)
  :config
  (setq haskell-process-show-debug-tips nil)
  (setq haskell-process-suggest-remove-import-lines t)
  (setq haskell-process-auto-import-loaded-modules t)
  (setq haskell-process-log t))

(provide 'haskell-packages)
;;; haskell-packages.el ends here
