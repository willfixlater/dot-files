;;; voice-recording.el

;;; Commentary:
;;
;; Originally stolen from: https://gnu.support/gnu-emacs/emacs-lisp/Emacs-Lisp-Record-voice-notes-within-GNU-Emacs.html
;;

;;; Code:

(defvar voice-recordings-dir "/tmp"
  "This is your voice recording directory")

(defvar voice-recording-extension ".wav"
  "This is your sound file extension")

(defun voice-record-command (filename)
  "Returns voice recording command"
  (format "arecord \"%s\"" filename))

(defun voice-record-on-termination (process current-group)
  "Returns voice recording command termination signal, nil implies
no explicit signal"
  (interrupt-process process current-group))

(defun record-voice-note ()
  "This function uses SOX sound tools to record voice notes. The
concept is more important than which tools are used. It starts
recording the sound file within emacs. It can be your sound
note. Once you press `q` it will stop recording, and open up the
directory with sound files"
  (interactive)
  (let* ((dir (concat (file-name-as-directory voice-recordings-dir)
                      (file-name-as-directory
                       (format-time-string "%Y/%m/%Y-%m-%d/"))))
         (file-name (concat (format-time-string "%Y-%m-%d-%H:%M:%S")
                            voice-recording-extension))
         (file-path (expand-file-name file-name dir))
         (command (voice-record-command file-path))
         (buffer "*Voice Recording*")
         (exit-event "exited abnormally with code 1\n"))
    (make-directory dir t)
    (switch-to-buffer buffer)
    (erase-buffer)
    (setq-local header-line-format
                "➜ Terminate the recording with 'C-g'")
    (let* ((process
            (start-process-shell-command buffer buffer command)))
      (set-process-sentinel process
                            (lambda (process event)
                              (message event)
                              (when (string= event exit-event)
                                (kill-current-buffer)
                                (find-file dir)
                                (revert-buffer))))
      (local-set-key "\C-g"
                     (lambda ()
                       (interactive)
                       (voice-record-on-termination process nil)))
      (recursive-edit))))

(provide 'voice-recording)
;;; voice-recording.el ends here
