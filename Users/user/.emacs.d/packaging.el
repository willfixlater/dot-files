(defun load-and-require (dir sym &rest path)
  (add-to-list 'load-path (mapconcat 'identity (cons dir path) "/"))
  (require sym))

(defun load-package (dir sym-path)
  (if (symbolp sym-path)
    (load-and-require dir sym-path (symbol-name sym-path))
    (apply 'load-and-require dir sym-path)))

(defun load-packages (packs)
  (let ((dir  (nth 0 packs))
        (syms (nth 1 packs)))
    (mapc (apply-partially 'load-package dir) syms)))

(defun load-package-list (packages)
  (mapc 'load-packages packages))
