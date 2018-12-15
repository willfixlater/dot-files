(defun make-new-directory (dir)
 (unless (file-exists-p dir)
   (make-directory dir)))
