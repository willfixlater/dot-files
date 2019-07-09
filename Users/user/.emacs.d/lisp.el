(dolist (hook '(emacs-lisp-mode-hook
                eval-expression-minibuffer-setup-hook
                lisp-mode-hook
                lisp-interaction-mode-hook
                scheme-mode-hook
                clojure-mode-hook))
  (add-hook hook #'paredit-mode))
