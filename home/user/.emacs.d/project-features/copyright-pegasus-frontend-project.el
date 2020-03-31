;;; copyright-pegasus-frontend-project.el

;;; Commentary:
;;
;; Settings for my default project.
;;

;;; Code:

(require 'base-packages)
(require 'markdown-packages)

(use-package vue-mode
  :mode "\\.vue\\'"
  :config
  (setq mmm-submode-decoration-level 0)
  (setq js-indent-level 2))

(provide 'copyright-pegasus-frontend-project)
;;; copyright-pegasus-frontend-project.el ends here
