; Initialize packages
; (packages listed in ~/.config/nixpkgs/config.nix)
(require 'package)
(setq package-archives nil)
(setq package-enable-at-startup nil)
(package-initialize)

; Load elisp modules
(load "~/.emacs.d/utils.el")
(load "~/.emacs.d/lisp.el")
(load "~/.emacs.d/clojure.el")
(load "~/.emacs.d/haskell.el")
(load "~/.emacs.d/rust.el")
(load "~/.emacs.d/python.el")
(load "~/.emacs.d/sql.el")
(load "~/.emacs.d/web.el")
(load "~/.emacs.d/markdown.el")
(load "~/.emacs.d/restclient.el")
(load "~/.emacs.d/keybindings.el")
(load "~/.emacs.d/prettify.el")
(load "~/.emacs.d/mode-line.el")
(load "~/.emacs.d/desktop.el")

; Set theme
(load-theme 'sanityinc-tomorrow-night t)

; Set mode line
(aligned-mode-line '(list " %b   " mode-name)
                   '(list (buffer-state-*) (if window-system "λ" "|") (buffer-state-+))
                   "[%l,%c] %p%%")

; Set and create auto-save and backup dirs
(defconst backup-dir-path (expand-file-name "~/.emacs.d/backup/"))
(defconst auto-save-dir-path (expand-file-name "~/.emacs.d/auto-save/"))
(make-new-directory backup-dir-path)
(make-new-directory auto-save-dir-path)

; Set frame defaults
(setq-default default-frame-alist
	      '((font . "Fira Code-11:bold:antialias=true")
		(cursor-type . hbar)
		(cursor-color . "#ffffff")
		(vertical-scroll-bars . nil)
                (left-fringe . 20)
                (right-fringe . 20)))

; Projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

; Set misc. settings
;; TODO: Add hook once magit-gitflow can be installed in nixos
;(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
(setq-default truncate-lines t)
(setq-default ag-highlight-search t)
(setq-default create-lockfiles nil)
(setq-default backup-directory-alist `((".*" . ,backup-dir-path)))
(setq-default auto-save-file-name-transforms `((".*" ,auto-save-dir-path t)))
(setq-default comint-prompt-read-only t)
(setq-default header-line-format "")
(fringe-mode '(20 . 20))
(fset 'yes-or-no-p 'y-or-n-p)
(set-face-attribute 'mode-line nil :font "Fira Code-11:bold:antialias=true")
(set-face-attribute 'mode-line-inactive nil :font "Fira Code-11:bold:antialias=true")
(set-face-attribute 'header-line nil :background "#1d1f21" :box "#1d1f21")
(set-face-attribute 'fringe nil :background "#1d1f21")
(put 'dired-find-alternate-file 'disabled nil)

;; NOTE: Run this after creating a frame, mainly to work around emacsclient not
;; respecting these settings.
(defun face-attributes-new-frame-hook (_)
  (set-face-attribute 'mode-line nil :font "Fira Code-11:bold:antialias=true")
  (set-face-attribute 'mode-line-inactive nil :font "Fira Code-11:bold:antialias=true")
  (set-face-attribute 'header-line nil :background "#1d1f21" :box "#1d1f21")
  (set-face-attribute 'fringe nil :background "#1d1f21"))

(add-hook 'after-make-frame-functions #'face-attributes-new-frame-hook)
