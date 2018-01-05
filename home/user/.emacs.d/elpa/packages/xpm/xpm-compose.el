;;; xpm-compose.el --- two or more buffers     -*- lexical-binding: t -*-

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

;;; Code:

(require 'xpm)
(require 'cl-lib)

(defun xpm--lines ()
  ;; (maybe) todo: use rectangle funcs
  (xpm--w/gg (w h origin flags) xpm--gg
    (save-excursion
      (goto-char origin)
      (cl-loop
       with skip = (if (memq 'intangible-sides flags)
                       1
                     4)
       repeat h
       collect (let ((p (point)))
                 (forward-char w)
                 (prog1 (buffer-substring-no-properties p (point))
                   (forward-char skip)))))))

(defun xpm--clone (src)
  (insert-buffer-substring src)
  (setq xpm--gg (xpm--copy-gg (buffer-local-value 'xpm--gg src))))

(defun xpm-buffer-from (image &optional name)
  "Return a new XPM buffer initialized from IMAGE.
IMAGE should have type `xpm'.  NAME is the new buffer name,
which defaults to the name specified in IMAGE."
  (let* ((plist (cdr image))
         source populate)
    (cond ((setq source (plist-get plist :file))
           (setq populate 'insert-file-contents))
          ((setq source (plist-get plist :data))
           (setq populate 'insert))
          (t (error "Invalid image: %S" image)))
    (with-current-buffer (generate-new-buffer
                          (or name "*TMP* for xpm-buffer-from"))
      (funcall populate source)
      (unless name
        (goto-char (point-min))
        (re-search-forward "\\(\\S-+\\)\\[\\]")
        (rename-buffer (match-string 1)))
      (current-buffer))))

(defun xpm-compose (name one two px)
  "Return new buffer NAME, by composing buffers ONE and TWO.
This copies all pixels from TWO that are not PX."
  (when (characterp px)
    (setq px (string px)))
  (with-current-buffer (generate-new-buffer name)
    (xpm--w/gg (w h cpp origin flags) (xpm--clone one)
      (let ((lines (with-current-buffer two
                     (xpm--lines))))
        ;; fluency from congruency...
        (cl-assert (= cpp (length px)))
        (cl-assert (= h (length lines)))
        (cl-assert (or (zerop h)           ; GIGO :-/
                    (= (* cpp w) (length (car lines)))))
        ;; do it
        (goto-char origin)
        (cl-loop
         with skip = (if (memq 'intangible-sides flags)
                         1
                       4)
         for line in lines
         do (cl-loop
             ;; this is slow and stupid
             ;; todo: use ‘compare-strings’
             for x below w
             do (let* ((i (* x cpp))
                       (el (substring line i (+ i cpp))))
                  (if (string= px el)
                      (forward-char cpp)
                    (insert el)
                    (delete-char cpp))))
         do (when (< (point) (point-max))
              (forward-char skip)))
        (current-buffer)))))

(defun xpm-fill (px)
  "Fill with PX."
  (interactive "sPX: ")
  (xpm--w/gg (w h) (xpm--gate)
    (save-excursion
      (cl-loop
       with x = (cons 0 (1- w))
       for y below h
       do (xpm-put-points px x y)))))

(provide 'xpm-compose)


(defun ttn-test-xpm-compose ()
  (interactive)
  (cl-flet ((zonk (name) (let ((buf (get-buffer name)))
                           (when buf (kill-buffer buf)))))
    (mapc #'zonk '("one" "two" "zow"))
    ;; create
    (let* ((palette '((?\s . "black")   ; bg
                      (?#  . "green")   ; fg
                      (?X  . "red")
                      (?-  . "None")))
           (one (xpm-generate-buffer "one" 10 10 1 palette))
           (two (xpm-generate-buffer "two" 10 10 1 palette)))
      (with-current-buffer one (xpm-fill ?#))
      (with-current-buffer two
        (xpm-fill ?-)
        (cl-flet
            ((vec () (let ((v (make-vector 42 nil)))
                       (cl-loop
                        for i below 42
                        do (aset v i (random 10)))
                       v)))
          (xpm-put-points ?\s (vec) (vec))))
      (cl-assert (and (bufferp one)
                   (bufferp two))))
    ;; mogrify
    (let* ((debug-ignored-errors nil)
           (one (get-buffer "one"))
           (two (get-buffer "two"))
           (zow (xpm-compose "zow" one two ?-)))
      (when (bufferp zow)
        (switch-to-buffer zow)))))

;;; xpm-compose.el ends here
