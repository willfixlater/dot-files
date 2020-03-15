;;; lisp-packages.el

;;; Commentary:
;;
;; My settings for working with lisp.
;;

;;; Code:

(use-package paredit
  :hook (((emacs-lisp-mode
            eval-expression-minibuffer-setup
            lisp-mode
            lisp-interaction-mode)
          . paredit-mode)
         ((emacs-lisp-mode
            eval-expression-minibuffer-setup
            lisp-mode
            lisp-interaction-mode)
          . company-mode)))

(provide 'lisp-packages)
;;; lisp-packages.el ends here
