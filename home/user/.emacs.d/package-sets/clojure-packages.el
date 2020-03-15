;;; clojure-packages.el

;;; Commentary:
;;
;; My settings for working with clojure.
;;

;;; Code:

(use-package yasnippet)

(use-package clojure-mode)

(use-package inf-clojure)

(use-package clj-refactor)

(use-package cider
  :init
  (defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1)
    (cljr-add-keybindings-with-prefix "C-c C-m"))
  :hook
  ((clojure-mode . paredit-mode)
   (clojure-mode . my-clojure-mode-hook)
   (cider-mode . eldoc-mode)
   (cider-mode . company-mode)
   (cider-repl-mode . company-mode))
  :config
  (setq cider-font-lock-dynamically '(macro core function var))
  (setq cider-repl-pop-to-buffer-on-connect nil))

(provide 'clojure-packages)
;;; clojure-packages.el ends here
