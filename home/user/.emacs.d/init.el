; Initialize packages
; (packages listed in /etc/nixos/configuration.nix)
(require 'package)
(setq package-archives nil)
(setq package-enable-at-startup nil)
(package-initialize)

; Load elisp modules
(load "~/.emacs.d/lisp.el")
(load "~/.emacs.d/clojure.el")
(load "~/.emacs.d/haskell.el")
(load "~/.emacs.d/rust.el")
(load "~/.emacs.d/sql.el")
(load "~/.emacs.d/web.el")
(load "~/.emacs.d/markdown.el")
(load "~/.emacs.d/keybindings.el")
(load "~/.emacs.d/prettify.el")
(load "~/.emacs.d/mode-line.el")

; Set mode line
(aligned-mode-line '(list " %b   " mode-name)
                   '(list (buffer-state-*) (if window-system "λ" "|") (buffer-state-+))
                   "[%l,%c] %p%%")

; Set theme
(load-theme 'sanityinc-tomorrow-night t)

; Set frame defaults
(setq-default default-frame-alist
	      '((font . "Fira Code-11:bold:antialias=true")
		(cursor-type . hbar)
		(cursor-color . "#ffffff")
		(vertical-scroll-bars . nil)))

; Set misc. settings
(setq-default truncate-lines t)
(setq-default ag-highlight-search t)
(setq-default create-lockfiles nil)
(setq-default comint-prompt-read-only t)
(setq-default backup-directory-alist `((".*" . ,(expand-file-name "~/.emacs.d/backup/"))))
(setq-default auto-save-file-name-transforms `((".*" ,(expand-file-name "~/.emacs.d/auto-save/") t)))
(setq-default header-line-format "")
(fringe-mode '(18 . 18))
(fset 'yes-or-no-p 'y-or-n-p)
(set-face-attribute 'mode-line nil :font "Fira Code-11:bold:antialias=true")
(set-face-attribute 'mode-line-inactive nil :font "Fira Code-11:bold:antialias=true")
(set-face-attribute 'header-line nil :background "#1d1f21" :box "#1d1f21")
(set-face-attribute 'fringe nil :background "#1d1f21")
(put 'dired-find-alternate-file 'disabled nil)
;; TODO: Add hook once magit-gitflow can be installed in nixos
;(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

