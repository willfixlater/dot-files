;;; systemd-codegen.el --- D-Bus Introspection      -*- lexical-binding: t; -*-

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

;; This library is used to automatically generate D-Bus bindings for systemd
;; and related components via introspection.
;;
;; The macro `systemd-codegen-define' can be used to generate Lisp code according
;; to the currently available introspection data.  This can be useful for
;; development.
;;
;; To avoid a dependency on systemd at compile time, `systemd-codegen-to-string'
;; is provided to statically generate all the Lisp code for the currently running
;; version of systemd.
;;
;; `systemd-codegen-to-string' is used to generate the bulk of the content of
;; systemd.el.

;;; Code:

(require 'cl-lib)
(require 'dbus)

(defvar systemd-codegen-interfaces
  '(("org.freedesktop.systemd1.Manager"
     :prefix "systemd"
     :interface systemd-dbus-interface-manager)
    ("org.freedesktop.systemd1.Automount"
     :prefix "systemd-automount"
     :interface systemd-dbus-interface-automount)
    ("org.freedesktop.systemd1.BusName"
     :prefix "systemd-bus-name"
     :interface systemd-dbus-interface-bus-name)
    ("org.freedesktop.systemd1.Device"
     :prefix "systemd-device"
     :interface systemd-dbus-interface-device)
    ("org.freedesktop.systemd1.Mount"
     :prefix "systemd-mount"
     :interface systemd-dbus-interface-mount)
    ("org.freedesktop.systemd1.Path"
     :prefix "systemd-path"
     :interface systemd-dbus-interface-path)
    ("org.freedesktop.systemd1.Service"
     :prefix "systemd-service"
     :interface systemd-dbus-interface-service)
    ("org.freedesktop.systemd1.Scope"
     :prefix "systemd-scope"
     :interface systemd-dbus-interface-scope)
    ("org.freedesktop.systemd1.Slice"
     :prefix "systemd-slice"
     :interface systemd-dbus-interface-slice)
    ("org.freedesktop.systemd1.Socket"
     :prefix "systemd-socket"
     :interface systemd-dbus-interface-socket)
    ("org.freedesktop.systemd1.Swap"
     :prefix "systemd-swap"
     :interface systemd-dbus-interface-swap)
    ("org.freedesktop.systemd1.Target"
     :prefix "systemd-target"
     :interface systemd-dbus-interface-target)
    ("org.freedesktop.systemd1.Timer"
     :prefix "systemd-timer"
     :interface systemd-dbus-interface-timer)
    ("org.freedesktop.systemd1.Unit"
     :prefix "systemd-unit"
     :interface systemd-dbus-interface-unit)
    ("org.freedesktop.login1.Manager"
     :prefix "systemd-logind"
     :interface systemd-dbus-interface-logind-mamanger)
    ("org.freedesktop.login1.Seat"
     :prefix "systemd-logind-seat"
     :interface systemd-dbus-interface-logind-seat)
    ("org.freedesktop.login1.Session"
     :prefix "systemd-logind-session"
     :interface systemd-dbus-interface-logind-session)
    ("org.freedesktop.login1.User"
     :prefix "systemd-logind-user"
     :interface systemd-dbus-interface-logind-user)
    ("org.freedesktop.network1.Manager"
     :prefix "systemd-networkd"
     :interface systemd-dbus-interface-networkd-manager)
    ("org.freedesktop.network1.Link"
     :prefix "systemd-networkd-link"
     :interface systemd-dbus-interface-networkd-link)
    ("org.freedesktop.network1.Network"
     :prefix "systemd-networkd-network"
     :interface systemd-dbus-interface-networkd-network)
    ("org.freedesktop.resolve1.Manager"
     :prefix "systemd-resolved"
     :interface systemd-dbus-interface-resolved-manager)
    ("org.freedesktop.resolve1.Link"
     :prefix "systemd-resolved-link"
     :interface systemd-dbus-interface-resolved-link)
    ("org.freedesktop.hostname1"
     :prefix "systemd-hostnamed"
     :interface systemd-dbus-interface-hostnamed)
    ("org.freedesktop.locale1"
     :prefix "systemd-localed"
     :interface systemd-dbus-interface-localed)
    ("org.freedesktop.timedate1"
     :prefix "systemd-timedated"
     :interface systemd-dbus-interface-timedated)
    ("org.freedesktop.machine1.Manager"
     :prefix "systemd-machined"
     :interface systemd-dbus-interface-machined-mananger)
    ("org.freedesktop.machine1.Image"
     :prefix "systemd-machined-image"
     :interface systemd-dbus-interface-machined-image)
    ("org.freedesktop.machine1.Machine"
     :prefix "systemd-machined-machine"
     :interface systemd-dbus-interface-machined-machine)))

(defun systemd-codegen-introspect (service path &optional interfaces)
  (let ((xml (dbus-introspect-xml :system service path)))
    (dolist (item
	     (and (eq (car-safe xml) 'node)
		  (xml-node-children xml))
	     (sort interfaces (lambda (a b) (string-lessp (car a) (car b)))))
      (cond
       ((and (listp item) (eq 'interface (car-safe item)))
	(let* ((interface (xml-get-attribute-or-nil item 'name))
	       (interface-info (cdr (assoc interface systemd-codegen-interfaces)))
	       (prefix (plist-get interface-info :prefix))
	       (object-interface (not (string-match "\\(\\.Manager\\|1\\)$" interface)))
	       (service (pcase service
			  ("org.freedesktop.systemd1" 'systemd-dbus-service)
			  (_ service)))
	       (path (pcase path
		       ("/org/freedesktop/systemd1" 'systemd-dbus-path)
		       (_ path)))
	       forms)
	  (when (and prefix (not (assoc interface interfaces)))
	    (push `(defconst ,(plist-get interface-info :interface) ,interface) forms)
	    (setq
	     interfaces
	     (append
	      interfaces
	      (list
	       (cons
		interface
		(let ((interface (plist-get interface-info :interface)))
		  (dolist (interface-item (cddr item) (nreverse forms))
		    (cond
		     ((eq 'property (car-safe interface-item))
		      (let* ((property (xml-get-attribute interface-item 'name))
			     (name (intern (concat prefix "-" property)))
			     (readwrite
			      (string-equal
			       "readwrite"
			       (xml-get-attribute interface-item 'access)))
			     (arglist `(bus
					,@(when object-interface
					    '(path)))))
			(push `(defun ,name ,arglist
				 ,(if readwrite
				      "Use `setf' to set the value of this property."
				    "Read only property.")
				 (dbus-get-property
				  bus ,service
				  ,(if object-interface 'path path)
				  ,interface ,property))
			      forms)
			(when readwrite
			  (push (list 'gv-define-setter name (cons 'value arglist)
				      (list '\`
					    (list 'dbus-set-property
						  '(\, bus)
						  service
						  (if object-interface
						      '(\, path)
						    path)
						  interface property
						  '(\, value))))
				forms))))

		     ((eq 'method (car-safe interface-item))
		      (let* ((method (xml-get-attribute interface-item 'name))
			     (name (intern (concat prefix "-" method)))
			     (args (cl-remove-if-not
				    (lambda (arg)
				      (string= "in"
					       (xml-get-attribute
						arg 'direction)))
				    (xml-get-children interface-item 'arg)))
			     (arglist `(bus ,@(when object-interface '(path))
					    ,@(when args '(&rest args)))))
			(push `(defun ,name ,arglist
				 (,@(if args
					'(apply #'dbus-call-method)
				      '(dbus-call-method))
				  bus ,service
				  ,(if object-interface 'path path)
				  ,interface ,method
				  ,@(when args '(args))))
			      forms)))))))))))))
       ((and (listp item) (eq 'node (xml-node-name item)))
	(let ((name (xml-get-attribute-or-nil item 'name)))
	  (setq interfaces (systemd-codegen-introspect
			    service (concat path "/" name) interfaces))))))))

(defmacro systemd-codegen-define (suffix)
  (cons 'progn (cl-mapcan #'cdr (systemd-codegen-introspect
				 (concat "org.freedesktop." suffix)
				 (concat "/org/freedesktop/" suffix)))))

(defun systemd-codegen-to-string (suffix)
  (with-temp-buffer
    (pcase-dolist (`(,interface . ,forms)
		   (systemd-codegen-introspect
		    (concat "org.freedesktop." suffix)
		    (concat "/org/freedesktop/" suffix)))
      (insert ";;; " interface "\n\n")
      (dolist (form forms)
	(pp form (current-buffer))
	(insert "\n")))
    (delete-char -1)
    (emacs-lisp-mode)
    (goto-char (point-min))
    (while (re-search-forward "^(\\(defun\\|gv-define-setter\\)" nil t)
      (goto-char (match-beginning 0))
      (down-list 1) (forward-sexp 2) (delete-char 4) (up-list 1))
    (goto-char (point-min))
    (while (re-search-forward "(dbus-\\(get\\|set\\)-property" nil t)
      (goto-char (match-beginning 0))
      (down-list 1) (forward-sexp 4) (insert "\n") (up-list -1) (indent-sexp)
      (up-list 1))
    (goto-char (point-min))
    (while (re-search-forward "(apply #'dbus-call-method" nil t)
      (goto-char (match-beginning 0))
      (down-list 1) (forward-sexp 5) (insert "\n") (up-list -1) (indent-sexp)
      (up-list 1))
    (goto-char (point-min))
    (while (re-search-forward "(dbus-call-method" nil t)
      (goto-char (match-beginning 0))
      (down-list 1) (forward-sexp 4) (insert "\n") (up-list -1) (indent-sexp)
      (up-list 1))
    (buffer-string)))

(provide 'systemd-codegen)
;;; systemd-codegen.el ends here
