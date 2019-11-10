;;; voice-recording.el

;;; Commentary:
;;
;; Originally stolen from: https://gnu.support/gnu-emacs/emacs-lisp/Emacs-Lisp-Record-voice-notes-within-GNU-Emacs.html
;;

;;; Code:

(defvar voice-recordings-dir "/tmp"
  "This is your voice recording directory. Ensure it does not end in a slash.")

(defvar voice-recording-extension ".wav"
  "This is your sound file extension")

(defun record-voice-note ()
  "This function uses SOX sound tools to record voice notes. The
concept is more important than which tools are used. It starts
recording the sound file within emacs. It can be your sound
note. Once you press `q` it will stop recording, and open up the
directory with sound files"
  (interactive)
  (let* ((filepath (concat voice-recordings-dir "/"
                           (format-time-string "%Y/%m/%Y-%m-%d/")))
         (filename (concat filepath
                           (format-time-string "%Y-%m-%d-%H:%M:%S")
                           voice-recording-extension))
         (command (voice-record-command filename))
         (buffer "*Voice Recording*"))
    (make-directory filepath t)
    (switch-to-buffer buffer)
    (erase-buffer)
    (setq-local header-line-format "➜ Finish recording with 'q'")
    (let* ((process (start-process-shell-command buffer buffer command)))
      (local-set-key "q" (lambda ()
                           (interactive)
                           (voice-record-on-termination process nil)
                           (local-set-key "q" 'kill-current-buffer)
                           (find-file filepath)
                           (revert-buffer)))
      (recursive-edit))))

(defun voice-record-command (filename)
  "Returns voice recording command"
  (format "arecord \"%s\"" filename))

(defun voice-record-on-termination (process current-group)
  "Returns voice recording command termination signal, nil implies
no explicit signal"
  (interrupt-process process current-group))

(provide 'voice-recording)
;;; voice-recording.el ends here
