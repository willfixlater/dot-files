(setq haskell-process-show-debug-tips nil)

(setq haskell-process-suggest-remove-import-lines t)
(setq haskell-process-auto-import-loaded-modules t)
(setq haskell-process-log t)

;; (define-keys interactive-haskell-mode-map
;;   "M-." 'haskell-mode-goto-loc
;;   "C-c C-t" 'haskell-mode-show-type-at)

;; (define-keys haskell-mode-map
;;   "C-c C-l" 'haskell-process-load-or-reload
;;   "C-c C-z" 'haskell-interactive-bring
;;   "C-c C-t" 'haskell-process-do-type
;;   "C-c C-i" 'haskell-process-do-info
;;   "C-c C-c" 'haskell-process-cabal-build
;;   "C-c C-k" 'haskell-interactive-mode-clear)

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
