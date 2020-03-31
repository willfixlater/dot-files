;;; base-packages.el

;;; Commentary:
;;
;; My base package set, to be used regardless of language or project.
;;

;;; Code:

(straight-use-package 'use-package)

;; Fundamental packages

(use-package better-defaults)

;; Packages that affect the presentation and aesthetics of emacs

(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-night t))

(use-package emojify)

;; Packages that augment the base functionality of emacs but that do
;; not relate to any particular language or ecosystem

(use-package ag
  :config
  (setq ag-highlight-search t))

(use-package ace-window)

(use-package projectile
  :config
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package magit
  :bind
  ("C-c g" . magit))

(use-package magit-gitflow
  :hook (magit-mode-hook . turn-on-magit-gitflow))

(use-package flycheck)

(use-package company)

(use-package yasnippet)

(use-package org
  :config
  (setq org-confirm-babel-evaluate nil)
  (defun my-org-mode-hook ()
    (setq fill-column 100)
    (auto-fill-mode t))
  (add-hook 'org-mode-hook #'my-org-mode-hook))

(use-package restclient)

(provide 'base-packages)
;;; base-packages.el ends here
