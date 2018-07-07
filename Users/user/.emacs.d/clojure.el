(add-hook 'cider-mode-hook #'eldoc-mode)

(setq cider-default-cljs-repl 'figwheel)
(setq cider-font-lock-dynamically '(macro core function var))
