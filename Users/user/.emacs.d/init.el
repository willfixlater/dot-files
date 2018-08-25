(load "~/.emacs.d/packaging.el")

(load-package-list
 '(("~/.emacs.d/elpa/packages" (seq queue spinner js2-mode))
   ("~/.emacs.d/packages"      ((better-defaults "technomancy-better-defaults")
                                (dash "magnars-dash")
                                (s "magnars-s")
                                (ag "wilfred-ag")
                                (avy "abo-abo-avy")
                                (sesman "vspinu-sesman")
                                (magit-popup "magit-magit-popup")
                                (ghub "magit-ghub")
                                (with-editor "magit-with-editor")
                                (magit "magit-magit/lisp")
                                (magit-gitflow "jtatarik-magit-gitflow")
                                (ace-window "abo-abo-ace-window")
                                (projectile "bbatsov-projectile")
                                (color-theme-sanityinc-tomorrow "purcell-color-theme-sanityinc-tomorrow")
                                (paredit "campbell-paredit")
                                (clojure-mode "clojure-emacs-clojure-mode")
                                (cider "clojure-emacs-cider")
                                (haskell-mode "haskell-haskell-mode")
                                (geiser "jaor-geiser/elisp")
                                (rjsx-mode "felipeochoa-rjsx-mode")
                                (markdown-mode "jrblevin-markdown-mode")
                                (haml-mode "nex3-haml-mode")
                                (sass-mode "nex3-sass-mode")))))

(load "~/.emacs.d/lisp.el")
(load "~/.emacs.d/clojure.el")
(load "~/.emacs.d/haskell.el")
(load "~/.emacs.d/web.el")
(load "~/.emacs.d/markdown.el")
(load "~/.emacs.d/keybindings.el")
(load "~/.emacs.d/prettify.el")
(load "~/.emacs.d/mode-line.el")

(custom-set-variables
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("8b9d07b01f2a9566969c2049faf982cab6a4b483dd43de7fd6a016bb861f7762" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))))

(aligned-mode-line " "
                   '(list "%b   " mode-name)
                   '(list (buffer-state-*) (if window-system "λ" "|") (buffer-state-+))
                   "%p [%l,%c]"
                   "")

(projectile-global-mode)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default truncate-lines t)
(set-face-attribute 'fringe nil :background "#1d1f21")
(setq backup-directory-alist `((".*" . ,(expand-file-name "~/.emacs.d/backup/"))))
(setq auto-save-file-name-transforms `((".*" ,(expand-file-name "~/.emacs.d/auto-save/") t)))

(setq default-frame-alist
      '((font . "Fira Code-11:antialias=true")
        (cursor-type . hbar)
        (cursor-color . "#ffffff")
        (vertical-scroll-bars . nil)
        (create-lockfiles . nil)
        (ag-highlight-search . t)))
