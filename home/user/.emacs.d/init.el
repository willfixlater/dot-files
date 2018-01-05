(add-to-list 'load-path
	     "~/.emacs.d/packages/technomancy-better-defaults")

(require 'better-defaults)

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
