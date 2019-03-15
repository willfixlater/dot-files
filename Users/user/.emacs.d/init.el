;;; init.el --- Initialization file for Emacs

;;; Commentary:

;;; Emacs Startup File

;;; Code:

;; Front matter

; Declarations

(defvar ag-highlight-search)
(defvar comint-prompt-read-only)

(declare-function load-package-list "~/.emacs.d/packaging.el")
(declare-function aligned-mode-line "~/.emacs.d/mode-line.el")
(declare-function projectile-global-mode "ext:projectile")
(declare-function global-flycheck-mode "ext:flycheck")

; Packages

(load "~/.emacs.d/packaging.el")

(load "~/.emacs.d/packages/ellerh-peg/peg.el")
(load "~/.emacs.d/packages/cask-epl/epl.el")

(load-package-list
 '(("~/.emacs.d/elpa/packages" (seq queue spinner js2-mode csv-mode))
   ("~/.emacs.d/packages"      ((better-defaults "technomancy-better-defaults")
                                (dash "magnars-dash")
                                (s "magnars-s")
                                (ag "wilfred-ag")
                                (avy "abo-abo-avy")
                                (sesman "vspinu-sesman")
                                (yasnippet "joaotavora-yasnippet")
                                (multiple-cursors "magnars-multiple-cursors")
                                (edn "expez-edn")
                                (inflections "eschulte-jump")
                                (hydra "abo-abo-hydra")
                                (pkg-info "lunaryorn-pkg-info")
                                (magit-popup "magit-magit-popup")
                                (ghub "magit-ghub")
                                (with-editor "magit-with-editor")
                                (magit "magit-magit/lisp")
                                (magit-gitflow "jtatarik-magit-gitflow")
                                (ace-window "abo-abo-ace-window")
                                (projectile "bbatsov-projectile")
                                (flycheck "flycheck-flycheck")
                                (company "company-mode-company-mode")
                                (color-theme-sanityinc-tomorrow "purcell-color-theme-sanityinc-tomorrow")
                                (paredit "campbell-paredit")
                                (clojure-mode "clojure-emacs-clojure-mode")
                                (cider "clojure-emacs-cider")
                                (clj-refactor "clojure-emacs-clj-refactor")
                                (haskell-mode "haskell-haskell-mode")
                                (rust-mode "rust-lang-rust-mode")
                                (geiser "jaor-geiser/elisp")
                                (rjsx-mode "felipeochoa-rjsx-mode")
                                (typescript-mode "ananthakumaran-typescript")
                                (tide "ananthakumaran-tide")
                                (markdown-mode "jrblevin-markdown-mode")
                                (haml-mode "nex3-haml-mode")
                                (sass-mode "nex3-sass-mode")
                                (dart-mode "bradyt-dart-mode")
                                (flutter "amake-flutter")))))

(load "~/.emacs.d/lisp.el")
(load "~/.emacs.d/clojure.el")
(load "~/.emacs.d/haskell.el")
(load "~/.emacs.d/rust.el")
(load "~/.emacs.d/typescript.el")
(load "~/.emacs.d/dart.el")
(load "~/.emacs.d/sql.el")
(load "~/.emacs.d/web.el")
(load "~/.emacs.d/markdown.el")
(load "~/.emacs.d/keybindings.el")
(load "~/.emacs.d/prettify.el")
(load "~/.emacs.d/mode-line.el")
(load "~/.emacs.d/desktop.el")
(load "~/.emacs.d/wsl.el")

(load "~/.emacs.d/packages/floobits-floobits-emacs/floobits.el")

;; Initialisation

; Theme

(load-theme 'sanityinc-tomorrow-night t)

; Mode Line

(aligned-mode-line '(list "%b   " mode-name)
                   '(list (buffer-state-*) (if window-system "λ" "|") (buffer-state-+))
                   "[%l,%c] %p%%")

; Misc. settings

(projectile-global-mode)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
(add-hook 'after-init-hook #'global-flycheck-mode)
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default truncate-lines t)
(set-face-attribute 'fringe nil :background "#1d1f21")
(setq ag-highlight-search t)
(setq create-lockfiles nil)
(setq comint-prompt-read-only t)
(setq backup-directory-alist `((".*" . ,(expand-file-name "~/.emacs.d/backup/"))))
(setq auto-save-file-name-transforms `((".*" ,(expand-file-name "~/.emacs.d/auto-save/") t)))
(put 'dired-find-alternate-file 'disabled nil)
(put 'scroll-left 'disabled nil)

(setq default-frame-alist
      '((font . "Fira Code-11:antialias=true")
        (cursor-type . hbar)
        (cursor-color . "#ffffff")
        (vertical-scroll-bars . nil)
        (left-fringe . 20)
        (right-fringe . 20)))

;; Back matter

(provide 'init)
;;; init.el ends here
