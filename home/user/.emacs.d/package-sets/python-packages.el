;;; python-packages.el

;;; Commentary:
;;
;; My settings for working with python.
;;

;;; Code:

(use-package elpy
  :hook
  (elpy-mode . flycheck-mode)
  :init
  (elpy-enable)
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))

(provide 'python-packages)
;;; python-packages.el ends here
