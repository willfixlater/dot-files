;;; xpm-ops.el --- drawing operations        -*- lexical-binding: t -*-

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

;;; Code:

(require 'queue)
(require 'cl-lib)
(require 'xpm)

(defun xpm-flood-fill (px x y)
  (xpm--w/gg (cpp origin y-mult) (xpm--gate)
    (let ((q (queue-create))
          bye)
      (cl-labels
          ((pos (x y) (+ origin (* cpp x) (* y-mult y)))
           (cur () (let ((p (point)))
                     (buffer-substring-no-properties
                      p (+ p cpp))))
           (oldp () (string= bye (cur)))
           (extent (coord)
                   (let* ((x (car coord))
                          (y (cdr coord))
                          (p (goto-char (pos x y)))
                          (beg x)
                          (end x))
                     (when (oldp)
                       (cl-loop
                        while (oldp)
                        do (backward-char cpp)
                        do (cl-decf beg)
                        finally do (cl-incf beg))
                       (goto-char p)
                       (cl-loop
                        while (oldp)
                        do (forward-char cpp)
                        do (cl-incf end)
                        finally do (cl-decf end))
                       (cons beg end)))))
        (setq bye (let ((p (pos x y)))
                    (buffer-substring-no-properties
                     p (+ p cpp))))
        (queue-enqueue q (cons x y))
        (cl-loop
         until (queue-empty q)
         do (let* ((coord (queue-dequeue q))
                   (ext (extent coord)))
              (when ext
                (xpm-put-points px ext y)
                ;; todo: expansion and queuing of y-1 and y+1
                )))))))

;;; xpm-ops.el ends here
