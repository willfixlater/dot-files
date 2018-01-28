(load "~/.emacs.d/packaging.el")
(load "~/.emacs.d/mode-line.el")
(load-package-list
 '(("~/.emacs.d/elpa/packages" (seq queue spinner))
   ("~/.emacs.d/packages"      ((better-defaults "technomancy-better-defaults")
                                (color-theme-sanityinc-tomorrow "purcell-color-theme-sanityinc-tomorrow")
                                (paredit "campbell-paredit")
                                (clojure-mode "clojure-emacs-clojure-mode")
                                (cider "clojure-emacs-cider")
                                (haskell-mode-autoloads "haskell-haskell-mode")))))

(custom-set-variables
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))))

(aligned-mode-line " "
                   '(list "%b   " mode-line-modes)
                   '(list (buffer-state-*) "λ" (buffer-state-+))
                   "%p [%l,%c]"
                   "")

(dolist (hook '(emacs-lisp-mode-hook
                eval-expression-minibuffer-setup-hook
                lisp-mode-hook
                lisp-interaction-mode-hook
                scheme-mode-hook
                clojure-mode-hook))
  (add-hook hook #'paredit-mode))

(eval-after-load 'haskell-mode
  '(progn
     (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
     (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
     (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)))

(setq create-lockfiles nil)
(setq inhibit-startup-screen t)
(setq backup-directory-alist
      `((".*" . ,(expand-file-name "~/.emacs.d/backup/"))))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/auto-save/") t)))
(put 'dired-find-alternate-file 'disabled nil)
(add-to-list 'Info-default-directory-list "~/.emacs.d/packages/haskell-haskell-mode/")
(set-cursor-color "#ffffff")
(add-to-list 'default-frame-alist `(cursor-type . hbar))
(set-face-attribute 'default nil :font "Triplicate T3c-11:weight=semi-bold:antialias=true")
(set-face-attribute 'mode-line nil :font "Triplicate T3c-11:weight=semi-bold:antialias=true")
