;;; systemctl.el --- Emacs interface to Systemd     -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@delysid.org>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides a front end to Systemd.
;;
;; Use `M-x systemctl-list-units RET' to see a list of all known
;; Systemd units and their status on localhost.  With a prefix
;; argument (`C-u M-x systemctl-list-units RET') you will be prompted
;; for a remote host to connect to.
;;
;; In systemctl-list-units-mode, `RET' will visit all relevant
;; configuration fragments for the unit at point (the equivalent of
;; "systemctl cat some.service").  With a `C-u' prefix argument, it
;; will prompt for a new override.conf file to create (somewhat
;; equivalent to "systemctl edit some.service").  Contrary to the
;; command-line "systemctl" tool, systemctl.el allows viewing and
;; editing of remote unit files thanks to TRAMP.
;;
;; Key bindings `s t a r t' and `s t o p' can be used to start and stop
;; services.  Similarily, `e n a b l e' and `d i s a b l e' can be used to
;; permanently enable and disable unit files.

;;; Granting access to non-root users:

;; Some operations are obviously not allowed when executed from within a
;; non-root Emacs session.  If you want to explicitly grant access to certain
;; users, you can create a polkit localauthority configuration file.
;; Below is an example.  You might want to change the group name, or use
;; "unix-user" instead.
;;
;; /etc/polkit-1/localauthority/50-local.d/10-systemd.pkla:
;;
;; [Normal Staff Permissions]
;; Identity=unix-group:sudo
;; Action=org.freedesktop.systemd1.*
;; ResultAny=no
;; ResultInactive=no
;; ResultActive=yes

;;; Todo:

;; * Have someone with window/frame-fu see if there is a better way to
;;   visit N files in a frame, each in a separate window.  The current approach
;;   feels a bit crude, see `systemctl-edit-unit-files'.
;; * Optionally automatically reload the Systemd daemon when a unit buffer is
;;   saved.
;; * Detect if we are not root, and use the sudo method to edit
;;   system files on localhost.
;; * Add support for local and remote systemd user sessions.
;; * Figure out what's necessary to support local and remote containers.
;; * Menu entries for `systemctl-list-units-mode'.

;;; Code:

(require 'systemd)
(require 'tabulated-list)
(require 'tramp)

(defgroup systemctl nil
  "Interface to Systemd.")

(defcustom systemctl-default-override-file-name "override.conf"
  "Default file name for new override.conf files."
  :group 'systemctl
  :type 'string)

(defcustom systemctl-list-units-format
  (vector (list "Unit" 22 t)
          (list "Loaded" 9 t)
          (list "Active" 8 t)
          (list "State" 8 t)
          (list "Description" 50 nil))
  "Column format specification for the `systemctl-list-units' command."
  :group 'systemctl
  :type '(vector (list :tag "Unit"
                       (string :tag "Title")
                       (number :tag "Width")
                       (boolean :tag "Sortable"))
                 (list :tag "Loaded"
                       (string :tag "Title")
                       (number :tag "Width")
                       (boolean :tag "Sortable"))
                 (list :tag "Active"
                       (string :tag "Title")
                       (number :tag "Width")
                       (boolean :tag "Sortable"))
                 (list :tag "State"
                       (string :tag "Title")
                       (number :tag "Width")
                       (boolean :tag "Sortable"))
                 (list :tag "Description"
                       (string :tag "Title")
                       (number :tag "Width")
                       (boolean :tag "Sortable"))))

(defcustom systemctl-tramp-method "scpx"
  "The TRAMP method to use when remotely accessing Systemd Unit files."
  :group 'systemctl
  :type (cons 'choice
	      (mapcar (lambda (method)
			(list 'const (car method)))
		      tramp-methods)))

(defvar-local systemctl-bus :system
  "Default D-Bus bus to use when accessing Systemd.
You should use the function `systemctl-bus' to retrieve the value of this
variable to make sure the bus is properly initialized in case it is pointing
to a remote machine.")

(defvar systemctl-list-units-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" #'systemctl-edit-unit-files)
    (define-key map "f" #'systemctl-find-fragment)
    (define-key map "start" #'systemctl-start)
    (define-key map "stop"  #'systemctl-stop)
    (define-key map "enable" #'systemctl-enable)
    (define-key map "disable" #'systemctl-disable)
    map)
  "Keymap for `systemctl-list-units-mode'.")

(defun systemctl-bus ()
  (when (stringp systemctl-bus)
    (dbus-init-bus systemctl-bus))
  systemctl-bus)
      
(defun systemctl-list-units-entries ()
  "Retrieve a list of units known to Systemd.
See `systemctl-list-units-format' and `tabulated-list-entries'."
  (mapcar (lambda (desc)
            (list (nth 6 desc)
                  (vector (nth 0 desc)
                          (nth 2 desc)
                          (nth 3 desc)
                          (nth 4 desc)
                          (nth 1 desc))))
          (systemd-ListUnits (systemctl-bus))))

(defun systemctl-unescape-unit-name (string)
  (while (string-match "\\\\x\\([0-9a-f]\\{2\\}\\)" string)
    (setq string
          (replace-match (string (string-to-number (match-string 1 string) 16))
                         t t string)))
  string)

(defun systemctl-list-units-print-entry (id cols)
  "Insert a Systemd Units List entry at point.
See `tabulated-list-printer'."
  (let ((beg (point))
        (x (max tabulated-list-padding 0))
        (inhibit-read-only t))
    (when (> x 0) (insert (make-string x ?\s)))
    (dotimes (n (length tabulated-list-format))
      (let ((desc (aref cols n)))
        (when (= n 0)
          (setq desc (systemctl-unescape-unit-name desc)))
        (setq x (tabulated-list-print-col n desc x))))
    (insert ?\n)
    (put-text-property beg (point) 'tabulated-list-id id)
    (put-text-property beg (point) 'tabulated-list-entry cols)))

(define-derived-mode systemctl-list-units-mode tabulated-list-mode
  "Systemd-Units"
  "Major mode for displaying a list of Systemd Units."
  (setq tabulated-list-entries #'systemctl-list-units-entries
        tabulated-list-format    systemctl-list-units-format
        tabulated-list-printer #'systemctl-list-units-print-entry)
  (tabulated-list-init-header))
  
;;;###autoload
(defun systemctl-list-units (&optional host)
  "Display a list of all Systemd Units."
  (interactive
   (list (when (equal current-prefix-arg '(4))
           (read-string "Remote host: "))))
  
  (with-current-buffer (let ((buffer-name (if host
					      (format "*Systemd Units (%s)*"
						      host)
					    "*Systemd Units*")))
			 (get-buffer-create buffer-name))
    (systemctl-list-units-mode)
    (when host
      (setq systemctl-bus (systemd-remote-bus host)
	    default-directory (systemctl-file-name "/etc/systemd/")))
    (tabulated-list-print)
    (pop-to-buffer (current-buffer))))

(defun systemctl-list-units-get-unit ()
  (when (eq major-mode 'systemctl-list-units-mode)
    (let ((entry (tabulated-list-get-entry)))
      (when entry
	(aref entry 0)))))

(defun systemctl-start (unit)
  "Start Systemd UNIT."
  (interactive (list (or (systemctl-list-units-get-unit)
                         (read-string "Unit: "))))
  (systemd-StartUnit (systemctl-bus) unit "replace")
  (when (eq major-mode 'systemctl-list-units-mode)
    (tabulated-list-revert)))

(defun systemctl-stop (unit)
  (interactive (list (or (systemctl-list-units-get-unit)
                         (read-string "Unit: "))))
  (systemd-StopUnit (systemctl-bus) unit "replace")
  (when (eq major-mode 'systemctl-list-units-mode)
    (tabulated-list-revert)))

(defun systemctl-enable (unit)
  "Enable Systemd UNIT."
  (interactive (list (or (systemctl-list-units-get-unit)
                         (read-string "Unit: "))))
  (pcase (systemd-EnableUnitFiles (systemctl-bus) (list unit) nil nil)
    (`(,carries-install-info ,changes)
     (if changes
	 (pcase-dolist (`(,type ,from ,to) changes)
	   (message "%s %s -> %s" type from to))
       (message "No changes")))))

(defun systemctl-disable (unit)
  "Disable Systemd UNIT."
  (interactive (list (or (systemctl-list-units-get-unit)
                         (read-string "Unit: "))))
  (let ((changes (systemd-DisableUnitFiles (systemctl-bus) (list unit) nil)))
    (if changes
	(pcase-dolist (`(,type ,from ,to) changes)
	  (message "%s %s -> %s" type from to))
      (message "No changes"))))

(defun systemctl-reload ()
  "Reload all unit files."
  (interactive)
  (systemd-Reload (systemctl-bus)))

(defun systemctl-file-name (file-name)
  (if (and (stringp systemctl-bus)
	   (string-match "unixexec:path=ssh,.*argv2=\\([^,]*\\),"
			 systemctl-bus))
      (let ((host (systemd-unescape-dbus-address
		   (match-string 1 systemctl-bus))))
	(concat "/" systemctl-tramp-method ":" host ":" file-name))
    file-name))

(defun systemctl-find-fragment (unit)
  (interactive
   (list (or (and (eq major-mode 'systemctl-list-units-mode)
		  (tabulated-list-get-id))
	     (systemd-GetUnit (systemctl-bus) (read-string "Unit: ")))))
  (let ((fragment-path (systemd-unit-FragmentPath (systemctl-bus) unit)))
    (when fragment-path
      (find-file (systemctl-file-name fragment-path)))))

(defun systemctl-edit-unit-files (unit &optional override-file)
  "Visit all configuration files related to UNIT simultaneously.
If optional OVERRIDE-FILE is specified, or if a prefix argument has been
given interactively, open a (new) override file."
  (interactive
   (let* ((unit (if (tabulated-list-get-entry)
		    (systemctl-unescape-unit-name (aref (tabulated-list-get-entry) 0))
		  (read-string "Unit: ")))
	  (unit-path (or (tabulated-list-get-id)
			 (systemd-GetUnit (systemctl-bus) unit)))
	  (override-file
	   (when (equal current-prefix-arg '(4))
	     (read-file-name "Override file: "
			     (systemctl-file-name
			      (concat "/etc/systemd/system/" unit ".d/"))
			     nil nil
			     systemctl-default-override-file-name))))
     (list unit-path override-file)))
  (let ((files (mapcar #'systemctl-file-name
		       (systemd-unit-DropInPaths (systemctl-bus) unit))))
    (when override-file
      (push override-file files))
    (let ((path (systemd-unit-FragmentPath (systemctl-bus) unit)))
      (when (not (string= path ""))
	(setq files (nconc files
			   (list (systemctl-file-name path))))))
    (let ((path (systemd-unit-SourcePath (systemctl-bus) unit)))
      (when (not (string= path ""))
	(setq files (nconc files
			   (list (systemctl-file-name path))))))
    (if files
	(let ((buffers (mapcar #'find-file-noselect files)))
	  (pop-to-buffer (pop buffers))
	  (when buffers
	    (delete-other-windows)
	    (dolist (buffer buffers)
	      (let ((window (split-window (car (last (window-list))))))
		(shrink-window-if-larger-than-buffer)
		(set-window-buffer window buffer)))
	    (dolist (window (window-list))
	      (shrink-window-if-larger-than-buffer window))))
      (when (called-interactively-p 'interactive)
	(message "No configuration files associated with `%s'." unit)))))

(provide 'systemctl)
;;; systemctl.el ends here
