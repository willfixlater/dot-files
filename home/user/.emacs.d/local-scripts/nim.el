(add-to-list 'auto-mode-alist '("\\.nim\\'" . nim-mode))
(add-to-list 'auto-mode-alist '("\\.nimble\\'" . nim-mode))

(defun my-nim-hook ()
  (nim-mode)
  (auto-fill-mode 0)
  (electric-indent-local-mode 0))
(add-hook 'nim-mode-hook 'my-nim-hook)
