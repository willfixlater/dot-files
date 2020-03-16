;;; markdown-packages.el

;;; Commentary:
;;
;; My settings for working with markdown.
;;

;;; Code:

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

(provide 'markdown-packages)
;;; markdown-packages.el ends here
