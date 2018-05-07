(load "~/.emacs.d/packaging.el")

(load-package-list
 '(("~/.emacs.d/elpa/packages" (seq queue spinner js2-mode))
   ("~/.emacs.d/packages"      ((better-defaults "technomancy-better-defaults")
                                (shell-pop "kyagi-shell-pop")
                                (color-theme-sanityinc-tomorrow "purcell-color-theme-sanityinc-tomorrow")
                                (paredit "campbell-paredit")
                                (clojure-mode "clojure-emacs-clojure-mode")
                                (cider "clojure-emacs-cider")
                                (haskell-mode-autoloads "haskell-haskell-mode")
                                (geiser "jaor-geiser/elisp")
                                (rjsx-mode "felipeochoa-rjsx-mode")))))

(load "~/.emacs.d/lisp.el")
(load "~/.emacs.d/clojure.el")
(load "~/.emacs.d/haskell.el")
(load "~/.emacs.d/web.el")
(load "~/.emacs.d/mode-line.el")

(custom-set-variables
 '(shell-pop-universal-key "C-t")
 '(shell-pop-shell-type (quote ("ansi-term" "*term*" (lambda nil (ansi-term shell-pop-term-shell)))))
 '(shell-pop-term-shell "/bin/zsh")
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
     ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))))

(aligned-mode-line " "
                   '(list "%b   " mode-line-modes)
                   '(list (buffer-state-*) "λ" (buffer-state-+))
                   "%p [%l,%c]"
                   "")

(setq create-lockfiles nil)
(setq inhibit-startup-screen t)
(setq backup-directory-alist
      `((".*" . ,(expand-file-name "~/.emacs.d/backup/"))))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/auto-save/") t)))
(put 'dired-find-alternate-file 'disabled nil)
(set-cursor-color "#ffffff")
(add-to-list 'default-frame-alist `(cursor-type . hbar))
(set-face-attribute 'default nil :font "Triplicate T3c-11:weight=semi-bold:antialias=true")
(set-face-attribute 'mode-line nil :font "Triplicate T3c-11:weight=semi-bold:antialias=true")
