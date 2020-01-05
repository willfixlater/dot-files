;; Initialize packages
;; (base packages listed in ~/.config/nixpkgs/config.nix)
(require 'package)
(setq-default package-archives nil)
(setq-default package-enable-at-startup nil)
(package-initialize)

;; Add local packages to load path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/local-packages"))

(require 'org)
(require 'magit-gitflow) ; Package currently does not build from nixpkgs
(require 'voice-recording)
(require 'my-utils)
(require 'my-mode-line)

;; Set theme
(load-theme 'sanityinc-tomorrow-night t)

;; General aesthetic settings
(setq-default default-frame-alist
	      '((font . "Fira Code-11:bold:antialias=true")
		(cursor-type . hbar)
		(cursor-color . "#ffffff")
		(vertical-scroll-bars . nil)
                (left-fringe . 20)
                (right-fringe . 20)))
(fringe-mode '(20 . 20))
;; Header line gives vertical space between buffer and title bar, vertical bar
;; prevents gap in window borders
(when window-system
  (setq-default header-line-format "▏"))
;; ---
(set-face-attribute 'mode-line nil :font "Fira Code-11:bold:antialias=true")
(set-face-attribute 'mode-line-inactive nil :font "Fira Code-11:bold:antialias=true")
(set-face-attribute 'header-line nil :background "#1d1f21" :foreground "#373b41" :box nil)
(set-face-attribute 'fringe nil :background "#1d1f21")
;; Sets same attributes as above when emacs is run as a client to a daemon
(defun face-attributes-new-frame-hook (_)
  (set-face-attribute 'mode-line nil :font "Fira Code-11:bold:antialias=true")
  (set-face-attribute 'mode-line-inactive nil :font "Fira Code-11:bold:antialias=true")
  (set-face-attribute 'header-line nil :background "#1d1f21" :foreground "#373b41" :box nil)
  (set-face-attribute 'fringe nil :background "#1d1f21"))
(add-hook 'after-make-frame-functions #'face-attributes-new-frame-hook)

;; Set mode line
(aligned-mode-line '(list "%b   " mode-name)
                   '(list (buffer-state-*) (if window-system "λ" "|") (buffer-state-+))
                   "[%l,%c] %p%%")

;; Auto save and backup settings
(defconst backup-dir-path (expand-file-name "~/.emacs.d/backup/"))
(defconst auto-save-dir-path (expand-file-name "~/.emacs.d/auto-save/"))
(make-new-directory backup-dir-path)
(make-new-directory auto-save-dir-path)
(setq-default backup-directory-alist `((".*" . ,backup-dir-path)))
(setq-default auto-save-file-name-transforms `((".*" ,auto-save-dir-path t)))
(setq-default create-lockfiles nil)

;; Desktop hooks
(defun desktop-read-on-init ()
  (let* ((desktop-dir (daemon->desktop (daemonp)))
         (lock-file (concat desktop-dir "/.emacs.desktop.lock")))
    (make-new-directory desktop-dir)
    (when (and (file-exists-p lock-file)
               (file-writable-p lock-file))
      (delete-file lock-file))
    (desktop-read desktop-dir)))
(add-hook 'after-init-hook 'desktop-read-on-init)

(defun desktop-save-on-autosave ()
  (desktop-save (daemon->desktop (daemonp))))
(add-hook 'auto-save-hook 'desktop-save-on-autosave)

(defun desktop-save-on-kill ()
  (let* ((desktop-dir (daemon->desktop (daemonp))))
    (desktop-save desktop-dir)
    (desktop-release-lock desktop-dir)))
(add-hook 'kill-emacs-hook 'desktop-save-on-kill)

;; Set directory for voice recordings
(setq-default voice-recordings-dir "~/Documents/VoiceRecordings")

;; Dired settings
(put 'dired-find-alternate-file 'disabled nil)

;; Ag settings
(setq-default ag-highlight-search t)

;; Org mode settings
(setq-default org-confirm-babel-evaluate nil)

(defun my-org-mode-hook ()
  (setq fill-column 100)
  (auto-fill-mode t))
(add-hook 'org-mode-hook #'my-org-mode-hook)

;; Projectile settings
(projectile-mode +1)

;; Magit settings
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

;; Restclient settings
(add-to-list 'auto-mode-alist '("\\.restclient\\'" . restclient-mode))

;; Misc. settings
(setq-default truncate-lines t) ; Allow lines to overflow and not wrap
(setq-default comint-prompt-read-only t) ; Make shell prompt read only
(fset 'yes-or-no-p 'y-or-n-p) ; Replace all yes/no prompts with y/n prompts

;; Custom keybindings
(define-keys (current-global-map)
  "C-z" 'undo
  "C-/" 'comment-or-uncomment-region
  "C->" 'paredit-forward-slurp-sexp
  "C-<" 'paredit-backward-slurp-sexp
  "M-p" 'scroll-down-line
  "M-n" 'scroll-up-line
  "C-," 'scroll-right
  "C-." 'scroll-left
  "M-Z" 'zap-to-char
  "C-c r" 'point-to-register
  "C-c j" 'jump-to-register
  "C-x o" 'ace-window
  "C-c g" 'magit-status)

(define-keys projectile-mode-map
  "C-c p" 'projectile-command-map)

(define-keys org-mode-map
  "M-F" 'org-metaright
  "M-B" 'org-metaleft
  "M-N" 'org-metadown
  "M-P" 'org-metaup)

;; Load project specific scripts
(when-let ((daemon-name (daemonp))
           (project-file-name (concat "~/.emacs.d/project-scripts/" daemon-name ".el"))
           (expanded-file-name (expand-existing-file-name project-file-name)))
  (load expanded-file-name))
