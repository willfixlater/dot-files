(load "~/.emacs.d/packaging.el")

(load-package-list
 '(("~/.emacs.d/elpa/packages" (seq queue spinner js2-mode))
   ("~/.emacs.d/packages"      ((better-defaults "technomancy-better-defaults")
                                (dash "magnars-dash")
                                (s "magnars-s")
                                (ag "wilfred-ag")
                                (avy "abo-abo-avy")
                                (ace-window "abo-abo-ace-window")
                                (shell-pop "kyagi-shell-pop")
                                (projectile "bbatsov-projectile")
                                (color-theme-sanityinc-tomorrow "purcell-color-theme-sanityinc-tomorrow")
                                (paredit "campbell-paredit")
                                (clojure-mode "clojure-emacs-clojure-mode")
                                (cider "clojure-emacs-cider")
                                (haskell-mode "haskell-haskell-mode")
                                (geiser "jaor-geiser/elisp")
                                (rjsx-mode "felipeochoa-rjsx-mode")))))

(load "~/.emacs.d/lisp.el")
(load "~/.emacs.d/clojure.el")
(load "~/.emacs.d/haskell.el")
(load "~/.emacs.d/web.el")
(load "~/.emacs.d/keybindings.el")
(load "~/.emacs.d/mode-line.el")

(custom-set-variables
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("8b9d07b01f2a9566969c2049faf982cab6a4b483dd43de7fd6a016bb861f7762" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(shell-pop-shell-type
   (quote
    ("ansi-term" "*term*"
     (lambda nil
       (ansi-term shell-pop-term-shell)))))
 '(shell-pop-term-shell "powershell")
 '(shell-pop-universal-key "C-t"))

(aligned-mode-line " "
                   '(list "%b   " mode-line-modes)
                   '(list (buffer-state-*) "λ" (buffer-state-+))
                   "%p [%l,%c]"
                   "")

(projectile-global-mode)
(setq create-lockfiles nil)
(setq inhibit-startup-screen t)
(setq backup-directory-alist
      `((".*" . ,(expand-file-name "~/.emacs.d/backup/"))))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/auto-save/") t)))
(put 'dired-find-alternate-file 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)
(set-cursor-color "#ffffff")
(add-to-list 'default-frame-alist `(cursor-type . hbar))
(set-face-attribute 'default nil :font "Fira Code:antialias=true")
(set-face-attribute 'mode-line nil :font "Fira Code:antialias=true")
