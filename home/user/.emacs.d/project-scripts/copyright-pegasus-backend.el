;; Load local elisp scripts
(dolist (module '("sql.el"
                  "markdown.el"
                  "web.el"
                  "python.el"))
  (load (expand-file-name (concat "~/.emacs.d/local-scripts/" module))))
