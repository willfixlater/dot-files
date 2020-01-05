;;; my-utils.el

;;; Commentary:
;;
;; My utils for use in configuring emacs.
;;

;;; Code:

(defun make-new-directory (dir)
 (unless (file-exists-p dir)
   (make-directory dir t)))

(defun define-keys (keymap &rest mappings*)
  "Call 'define-key' on KEYMAP for all supplied MAPPINGS*.

   Example: (define-keys my-keymap
              \"C-c C-m 1\" 'my-func
              \"C-c C-m 2\" 'my-other-func)"
  (let* ((mappings (seq-partition mappings* 2)))
    (seq-do (lambda (mapping)
              (seq-let [key def] mapping
                (define-key keymap (kbd key) def)))
            mappings)))

(defun expand-existing-file-name (name)
  (let* ((file-name (expand-file-name name)))
    (when (file-exists-p file-name)
      file-name)))

(defun daemon->desktop (daemon-name)
  "Map DAEMON-NAME to a a destination directory for saving the desktop."
  (concat "~/.emacs.d/desktops/" (or daemon-name "other")))

(provide 'my-utils)
;;; my-utils.el ends here
