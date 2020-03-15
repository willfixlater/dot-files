(use-package lsp-mode :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)

(use-package company-lsp
  :commands company-lsp
  :config (push 'company-lsp company-backends)) 

(use-package ccls
  :config
  (setq ccls-executable "ccls")
  (setq lsp-prefer-flymake nil)
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))
