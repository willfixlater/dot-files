(require 'org)

(defun my-org-mode-hook ()
  (setq fill-column 100)
  (auto-fill-mode t))

(add-hook 'org-mode-hook #'my-org-mode-hook)

(define-keys org-mode-map
  "M-F" 'org-metaright
  "M-B" 'org-metaleft
  "M-N" 'org-metadown
  "M-P" 'org-metaup)

(setq org-confirm-babel-evaluate nil)

;; TODO: Setup babel for inline code eval
;; (org-babel-do-load-languages
;;   'org-babel-load-languages
;;   '((emacs-lisp . t)
;;     (shell . t)
;;     (clojure . t)
;;     (js . t)))
