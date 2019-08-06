(define-keys (current-global-map)
  "C-z" 'undo
  "C-/" 'comment-or-uncomment-region
  "C->" 'paredit-forward-slurp-sexp
  "C-<" 'paredit-backward-slurp-sexp
  "M-p" 'scroll-down-line
  "M-n" 'scroll-up-line
  "C-," 'scroll-right
  "C-." 'scroll-left
  "M-Z" 'zap-to-char
  "C-c r" 'point-to-register
  "C-c j" 'jump-to-register
  "C-x o" 'ace-window
  "C-c g" 'magit-status)
