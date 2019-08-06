(defun daemon->desktop (daemon-name)
  "Map DAEMON-NAME to a a destination directory for saving the desktop."
  (concat "~/.emacs.d/desktops/" (or daemon-name "other")))

(defun desktop-autosave ()
  (desktop-save (daemon->desktop (daemonp))))

(defun desktop-save-on-kill ()
  (let* ((desktop-dir (daemon->desktop (daemonp))))
    (desktop-save desktop-dir)
    (desktop-release-lock desktop-dir)))

(defun desktop-read-on-init ()
  (let* ((desktop-dir (daemon->desktop (daemonp)))
         (lock-file (concat desktop-dir "/.emacs.desktop.lock")))
    (make-new-directory desktop-dir)
    (when (and (file-exists-p lock-file)
               (file-writable-p lock-file))
      (delete-file lock-file))
    (desktop-read desktop-dir)))

(add-hook 'auto-save-hook 'desktop-autosave)
(add-hook 'kill-emacs-hook 'desktop-save-on-kill)
(add-hook 'after-init-hook 'desktop-read-on-init)
