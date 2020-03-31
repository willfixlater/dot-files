;; Add local-features and package-sets to the load path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/package-sets"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/local-features"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/project-features"))

;; Require local-features used to configure emacs
(require 'misc-utils)
(require 'daemon-per-project)

;; Load presentational settings
(load-from-root "presentational-settings.el")

;; Load functional settings
(load-from-root "functional-settings.el")

;; Load custom keybindings
(load-from-root "custom-keybindings.el")

;; Set variables before any packages are initialised
(setq straight-use-package-by-default t)

;; Bootstrap straight.el
(load-from-root "bootstrap-straight.el")

;; Load project settings if they exist
(load-project-settings)
