;;; my-mode-line.el

;;; Commentary:
;;
;; Mode line helper functions, in particular aligned-mode-line.
;;

;;; Code:

(defun buffer-state-* ()
  (cond (buffer-read-only "·")
        ((buffer-modified-p) "*")
        (t "-")))

(defun buffer-state-+ ()
  (cond ((buffer-modified-p) "*")
        (buffer-read-only "·")
        (t "-")))

(defun aligned-mode-line (left center right)
  (let* ((seq->str        `(lambda (sequence) (mapconcat 'identity sequence "")))
         (left-f          `(format-mode-line ,left))
         (center-f        `(format-mode-line ,center))
         (right-f         `(format-mode-line ,right))
         (total-available `(- (window-width) (length ,center-f)))
         (odd-split       `(cl-oddp ,total-available))
         (available       `(/ ,total-available 2))
         (raw-left-space  `(- ,available (length ,left-f)))
         (raw-right-space `(- ,available (length ,right-f)))
         (left-space      `(cond
                            ((and window-system ,odd-split) (+ ,raw-left-space 1))
                            (window-system                  (+ ,raw-left-space 1))
                            (,odd-split                     (- ,raw-left-space 1))
                            (t                              (- ,raw-left-space 2))))
         (right-space     `(cond
                            ((and window-system ,odd-split) (+ ,raw-right-space 2))
                            (window-system                  (+ ,raw-right-space 1))
                            (,odd-split                     (- ,raw-right-space 1))
                            (t                              (- ,raw-right-space 2))))
         (left-overflow   `(>= (length ,left-f) ,available))
         (right-overflow  `(>= (length ,right-f) ,available))
         (left            `(if ,left-overflow
                             (concat (seq-take ,left-f (- ,available 4)) "... ")
                             (concat ,left-f (,seq->str (make-list ,left-space " ")))))
         (right           `(if ,right-overflow
                             (concat " ..." (seq-drop ,right-f (+ 4 (- (length ,right-f) ,available))))
                             (concat (,seq->str (make-list ,right-space " ")) ,right-f))))
    (setq-default mode-line-format
                  `((if window-system "  ")
                    (:eval ,left)
                    (:eval ,center)
                    (:eval ,right)))))

(provide 'my-mode-line)
;;; my-mode-line.el ends here
