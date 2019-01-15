(defun wsl-shell ()
  "Start a WSL Bash shell."
  (interactive)
  (let ((buffer-name (read-string "Buffer name (default *wsl*): "
                                  nil nil "*wsl*"))
        (explicit-shell-file-name "C:/Windows/System32/bash.exe")
        (default-directory (expand-file-name "~")))
    (shell buffer-name)))
