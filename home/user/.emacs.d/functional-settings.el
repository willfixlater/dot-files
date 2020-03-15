;; Don't create lockfiles
(setq create-lockfiles nil)

;; Navigate between dired directories without leaving behind buffers
(put 'dired-find-alternate-file 'disabled nil)

;; Allow lines to overflow and not wrap
(setq-default truncate-lines t)

;; Make shell prompt read only
(setq-default comint-prompt-read-only t)

;; Replace all yes/no prompts with y/n prompts
(fset 'yes-or-no-p 'y-or-n-p)
