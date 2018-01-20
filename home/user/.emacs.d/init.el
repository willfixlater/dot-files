(load "~/.emacs.d/packaging.el")
(load-package-list
 '(("~/.emacs.d/elpa/packages" (seq queue spinner))
   ("~/.emacs.d/packages"      ((better-defaults "technomancy-better-defaults")
                                (color-theme-sanityinc-tomorrow "purcell-color-theme-sanityinc-tomorrow")
                                (clojure-mode "clojure-emacs-clojure-mode")
                                (cider "clojure-emacs-cider")))))

(custom-set-variables
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))))

(setq inhibit-startup-screen t)
(setq create-lockfiles nil)
(setq backup-directory-alist
      `((".*" . ,(expand-file-name "~/.emacs.d/backup/"))))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/auto-save/") t)))
(set-cursor-color "#ffffff")
(put 'dired-find-alternate-file 'disabled nil)
(add-to-list 'default-frame-alist
             `(cursor-type . hbar))
(add-to-list 'default-frame-alist
	     `(font . ,(concat "Triplicate T3c-11"
		       	       ":weight=semi-bold"
			       ":antialias=true")))
