(add-hook 'cider-mode-hook #'eldoc-mode)

(setq cider-font-lock-dynamically '(macro core function var))
