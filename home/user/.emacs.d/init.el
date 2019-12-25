;; Initialize packages
;; (packages listed in ~/.config/nixpkgs/config.nix)
(require 'package)
(setq package-archives nil)
(setq package-enable-at-startup nil)
(package-initialize)

;; Add local packages to load path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/local-packages"))

;; Load local elisp scripts
(dolist (module '("utils.el" "keybindings.el" "mode-line.el" "desktop.el"
                  "git.el" "org.el" "restclient.el" "note-taking.el"
                  "eglot.el" "lisp.el" "web.el"
                  "clojure.el"
                  "haskell.el"
                  "rust.el"
                  "python.el"
                  "dart.el"
                  "scala.el"
                  "sql.el"
                  "markdown.el"))
  (load (expand-file-name (concat "~/.emacs.d/local-scripts/" module))))

;; Load project specific scripts
(when-let ((daemon-name (daemonp))
           (project-file-name (concat "~/.emacs.d/project-scripts/" daemon-name ".el"))
           (expanded-file-name (expand-existing-file-name project-file-name)))
  (load expanded-file-name))

;; Set theme
(load-theme 'sanityinc-tomorrow-night t)

;; Set mode line
(aligned-mode-line '(list " %b   " mode-name)
                   '(list (buffer-state-*) (if window-system "λ" "|") (buffer-state-+))
                   "[%l,%c] %p%%")

;; Set and create auto-save and backup dirs
(defconst backup-dir-path (expand-file-name "~/.emacs.d/backup/"))
(defconst auto-save-dir-path (expand-file-name "~/.emacs.d/auto-save/"))
(make-new-directory backup-dir-path)
(make-new-directory auto-save-dir-path)

;; Set frame defaults
(setq-default default-frame-alist
	      '((font . "Fira Code-11:bold:antialias=true")
		(cursor-type . hbar)
		(cursor-color . "#ffffff")
		(vertical-scroll-bars . nil)
                (left-fringe . 20)
                (right-fringe . 20)))

;; Set Emacs faces
(set-face-attribute 'mode-line nil :font "Fira Code-11:bold:antialias=true")
(set-face-attribute 'mode-line-inactive nil :font "Fira Code-11:bold:antialias=true")
(set-face-attribute 'header-line nil :background "#1d1f21" :foreground "#373b41" :box nil)
(set-face-attribute 'fringe nil :background "#1d1f21")

;; NOTE: Run this after creating a frame, mainly to work around emacsclient not
;; respecting these settings.
(defun face-attributes-new-frame-hook (_)
  (set-face-attribute 'mode-line nil :font "Fira Code-11:bold:antialias=true")
  (set-face-attribute 'mode-line-inactive nil :font "Fira Code-11:bold:antialias=true")
  (set-face-attribute 'header-line nil :background "#1d1f21" :foreground "#373b41" :box nil)
  (set-face-attribute 'fringe nil :background "#1d1f21"))

(add-hook 'after-make-frame-functions #'face-attributes-new-frame-hook)

;; Projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Company
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-backends (quote (company-capf company-elisp)))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(package-selected-packages
   (quote
    (company-lsp lsp-ui lsp-mode sbt-mode scala-mode yaml-mode use-package sass-mode rust-mode rjsx-mode restclient projectile nix-mode markdown-mode magit haskell-mode geiser flutter floobits emojify elpy eglot dart-mode color-theme-sanityinc-tomorrow clj-refactor better-defaults ag ace-window))))

;; Set misc. settings
(setq-default truncate-lines t)
(setq-default ag-highlight-search t)
(setq-default create-lockfiles nil)
(setq-default backup-directory-alist `((".*" . ,backup-dir-path)))
(setq-default auto-save-file-name-transforms `((".*" ,auto-save-dir-path t)))
(setq-default comint-prompt-read-only t)
(setq-default header-line-format "▏")
(fringe-mode '(20 . 20))
(fset 'yes-or-no-p 'y-or-n-p)
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
