(require 'org)

(define-keys org-mode-map
  "M-F" 'org-metaright
  "M-B" 'org-metaleft
  "M-N" 'org-metadown
  "M-P" 'org-metaup)

;; TODO: Setup babel for inline code eval
;; (org-babel-do-load-languages
;;   'org-babel-load-languages
;;   '((emacs-lisp . t)
;;     (shell . t)
;;     (clojure . t)
;;     (js . t)))
