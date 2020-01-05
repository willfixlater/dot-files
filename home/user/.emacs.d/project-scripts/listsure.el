;; Load local elisp scripts
(dolist (module '("lisp.el"
                  "sql.el"
                  "markdown.el"
                  "web.el"
                  "clojure.el"))
  (load (expand-file-name (concat "~/.emacs.d/local-scripts/" module))))

(setenv "AWS_PROFILE" "listsure")

(fset 'listsure-reset-project-ns
   [?\C-c ?\C-z ?\( ?d ?o ?  ?\( ?l ?i ?s ?t ?s ?u ?r ?e ?. ?d ?e ?v ?/ ?r ?e ?s ?e ?t ?\) ?  ?n ?i ?l ?\) return])

(define-keys cider-mode-map
  "C-c M-n R" 'listsure-reset-project-ns)
