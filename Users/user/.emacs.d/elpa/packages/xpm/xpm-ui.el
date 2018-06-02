;;; xpm-ui.el --- xpm-* plus pretty redisplay   -*- lexical-binding: t -*-

;; Copyright (C) 2014-2017 Free Software Foundation, Inc.

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

;; TODO
;;
;; ??? hmm, since this will probably be the future home of xpm-mode,
;;     why not rename the file as xpm-mode.el?

;;; Code:

;; todo: var ‘xpm-current-px’ (or maybe ‘xpm-quill’)

(eval-when-compile (require 'cl-lib))
(require 'xpm)
(require 'xpm-palette)

(defun xpm-set-pen-func (parent normal _none)
  (lambda (color)
    ;; see "hang" below
    (let* ((was (current-buffer))
           (px (get-text-property 0 'px color))
           (again (assoc px normal)))
      (switch-to-buffer parent)
      (message "%S | %S %s | %S" was px color again))))

(defun xpm-list-palette-display ()
  "Display palette in another buffer."
  (interactive)
  (xpm--w/gg (cpp pinfo) (xpm--gate)
    (let ((inhibit-read-only t)
          (name (format "*%s Palette*" (buffer-name)))
          normal none)
      ;; normalize and extract "None" if necessary
      (cl-loop
       for (px . alist) in (xpm--palette-alist cpp pinfo)
       ;; todo: handle case where there is no ‘c’
       do (let ((color (cdr (assq 'c alist))))
            (if (member color '("none" "None"))
                (setq none px)
              (push (cons px color)
                    normal)))
       finally do (setq normal (nreverse normal)))
      (list-colors-display (mapcar 'cdr normal) name
                           (xpm-set-pen-func (current-buffer)
                                             normal
                                             none))
      (switch-to-buffer name)
      (delete-other-windows)
      (goto-char (point-min))
      ;; ugly; better to not ‘insert’ and just add text properties.
      ;; also, focus is on px so we can hang it on ‘color-name’ directly
      (when none
        (insert (propertize (format "%S\tnone" none)
                            'color-name (propertize "none" 'px none))
                "\n"))
      (while normal
        (let* ((px (car (pop normal)))
               (all (text-properties-at (point)))
               (color (plist-get all 'color-name))
               (button (plist-get all 'button))
               (action (plist-get all 'action)))
          (insert (propertize
                   (format "%S\t" px)
                   'color-name (propertize color 'px px)
                   'button button
                   'action action
                   'category 'default-button
                   'follow-link t)))
        (forward-line 1))
      (goto-char (point-min)))))

;;; xpm-ui.el ends here
