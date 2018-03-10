(load "~/.emacs.d/packaging.el")

(load-package-list
 '(("~/.emacs.d/elpa/packages" (seq queue spinner))
   ("~/.emacs.d/packages"      ((better-defaults "technomancy-better-defaults")
                                (color-theme-sanityinc-tomorrow "purcell-color-theme-sanityinc-tomorrow")
                                (paredit "campbell-paredit")
                                (clojure-mode "clojure-emacs-clojure-mode")
                                (cider "clojure-emacs-cider")
                                (haskell-mode-autoloads "haskell-haskell-mode")
                                (geiser "jaor-geiser/elisp")))))

(load "~/.emacs.d/clojure.el")
(load "~/.emacs.d/paredit-hooks.el")
(load "~/.emacs.d/haskell.el")
(load "~/.emacs.d/mode-line.el")
(load "~/.emacs.d/web.el")

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
