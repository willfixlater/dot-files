;;; misc-utils.el

;;; Commentary:
;;
;; My utils for use in configuring emacs.
;;

;;; Code:

(defun make-dir-path (&rest dirs)
  (seq-reduce (lambda (acc dir)
                (concat acc (file-name-as-directory dir)))
              (cdr dirs)
              (file-name-as-directory (car dirs))))

(defun make-file-path (&rest dirs-&-filename)
  (let* ((dirs (butlast dirs-&-filename))
         (dir-path
          (seq-reduce (lambda (acc dir)
                        (concat acc (file-name-as-directory dir)))
                      (cdr dirs)
                      (file-name-as-directory (car dirs))))
         (filename (car (last dirs-&-filename))))
    (concat dir-path filename)))

(defun load-script (dir file-name)
  "Loads FILE-NAME from DIR, first resolving the absolute file path."
  (let* ((absolute-file-path (expand-file-name file-name dir)))
    (load absolute-file-path)))

(defun load-from-root (file-name)
  "Loads FILE-NAME from the root of the user's emacs directory."
  (load-script user-emacs-directory file-name))

(defun define-keys (keymap &rest mappings)
  "Call 'define-key' on KEYMAP for all supplied MAPPINGS.

   Example: (define-keys my-keymap
              \"C-c C-m 1\" 'my-func
              \"C-c C-m 2\" 'my-other-func)"
  (let* ((mappings* (seq-partition mappings 2)))
    (seq-do (lambda (mapping)
              (seq-let [key def] mapping
                (define-key keymap (kbd key) def)))
            mappings*)))

(defun expand-existing-file-name (name)
  (let* ((file-name (expand-file-name name)))
    (when (file-exists-p file-name)
      file-name)))

;; Stolen from: https://emacsredux.com/blog/2013/06/21/eval-and-replace/
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(provide 'misc-utils)
;;; misc-utils.el ends here
