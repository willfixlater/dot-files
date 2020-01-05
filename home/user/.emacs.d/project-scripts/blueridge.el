;; Load local elisp scripts
(dolist (module '("lisp.el"
                  "sql.el"
                  "markdown.el"
                  "web.el"
                  "clojure.el"))
  (load (expand-file-name (concat "~/.emacs.d/local-scripts/" module))))
