(defun define-keys (keymap &rest mappings*)
  "Call 'define-key' on KEYMAP for all supplied MAPPINGS*.

   Example: (define-keys my-keymap
              \"C-c C-m 1\" 'my-func
              \"C-c C-m 2\" 'my-other-func)"
  (let ((mappings (seq-partition mappings* 2)))
    (seq-do (lambda (mapping)
              (seq-let [key def] mapping
                (define-key keymap (kbd key) def)))
            mappings)))

(define-keys (current-global-map)
  "C-z" 'undo
  "C-/" 'comment-or-uncomment-region
  "C->" 'paredit-forward-slurp-sexp
  "C-<" 'paredit-backward-slurp-sexp
  "M-p" 'scroll-down-line
  "M-n" 'scroll-up-line
  "C-," 'scroll-right
  "C-." 'scroll-left
  "C-c r" 'point-to-register
  "C-c j" 'jump-to-register
  "C-x o" 'ace-window
  "C-c g" 'magit-status)
