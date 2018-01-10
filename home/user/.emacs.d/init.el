(defconst elpa-dir "~/.emacs.d/elpa/packages")
(defconst package-dir "~/.emacs.d/packages")

(add-to-list 'load-path
	     (format "%s/%s" elpa-dir "seq"))

(require 'seq)

(add-to-list 'load-path
	     (format "%s/%s" elpa-dir "queue"))

(require 'queue)

(add-to-list 'load-path
	     (format "%s/%s" elpa-dir "spinner"))

(require 'spinner)

(add-to-list 'load-path
	     (format "%s/%s" package-dir "technomancy-better-defaults"))

(require 'better-defaults)

(add-to-list 'load-path
	     (format "%s/%s" package-dir "purcell-color-theme-sanityinc-tomorrow"))

(require 'color-theme-sanityinc-tomorrow)

(add-to-list 'load-path
	     (format "%s/%s" package-dir "clojure-emacs-clojure-mode"))

(require 'clojure-mode)

(add-to-list 'load-path
	     (format "%s/%s" package-dir "clojure-emacs-cider"))

(require 'cider)

(setq inhibit-startup-screen t)
(setq create-lockfiles nil)
(setq backup-directory-alist
  `((".*" . ,(expand-file-name "~/.emacs.d/backup/"))))
(setq auto-save-file-name-transforms
  `((".*" ,(expand-file-name "~/.emacs.d/auto-save/") t)))

(add-to-list 'default-frame-alist
	     `(font . ,(concat "Triplicate T3c-11"
		       	       ":weight=semi-bold"
			       ":antialias=true")))
(put 'dired-find-alternate-file 'disabled nil)

(custom-set-variables
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))))

