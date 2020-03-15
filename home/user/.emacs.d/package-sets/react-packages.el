;;; react-packages.el

;;; Commentary:
;;
;; My settings for working with react.
;;

;;; Code:

(use-package js2-mode
  :config
  (setq js2-strict-missing-semi-warning nil)
  (setq js-indent-level 2)
  (setq css-indent-offset 2))

(use-package rjsx-mode
  :mode "\\.js\\'")

(provide 'react-packages)
;;; react-packages.el ends here
