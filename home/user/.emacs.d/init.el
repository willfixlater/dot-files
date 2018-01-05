(defconst elpa-dir "~/.emacs.d/elpa")
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
