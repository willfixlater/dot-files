;;; xpm-palette.el --- manage PX/COLOR set     -*- lexical-binding: t -*-

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

(require 'cl-lib)
(require 'xpm)

(defun xpm--palette-alist (cpp pinfo)
  (cl-flet ((sub (beg len) (buffer-substring-no-properties
                            beg (+ beg len))))
    (cl-loop
     with bye = (point)
     with (beg . ht) = pinfo
     initially do (goto-char beg)
     with (p px color)
     repeat (hash-table-count ht)
     do (setq p (1+ (point))
              px (sub p cpp))
     collect
     (cons px (if (consp (setq color (gethash px ht)))
                  color
                (goto-char (cl-incf p cpp))
                (puthash                ; optimism
                 px (cl-loop
                     with ls = (split-string
                                (sub p (skip-chars-forward "^\"")))
                     while ls
                     collect (cons (intern (pop ls))
                                   (pop ls)))
                 ht)))
     do (forward-line 1)
     finally do (goto-char bye))))

(defun xpm--validate-px (cpp px)
  (when (/= cpp (length px))
    (error "Invalid px %S (expecting length %d)" px cpp))
  t)

(defun xpm--adjust-npal (n palette)
  ;; Change count of colors by adding N to the current value.
  ;; But first, move point to POS, which should be
  ;; the colors list bol (and leave it there when done).
  ;; See `xpm-drop-px' and `xpm-add-px'.
  (goto-char (car palette))
  (save-excursion
    (search-backward "\n\"")
    (forward-char 2)                    ; LF, double-quote
    (forward-sexp 2)                    ; WIDTH and HEIGHT
    (let* ((p (point))
           (count (string-to-number
                   (delete-and-extract-region
                    p (progn (forward-sexp 1)
                             (point))))))
      (insert (format " %d" (cl-incf count n))))))

(defun xpm-drop-px (px &optional noerror)
  "Drop PX from palette.
Signal error if PX is not found.
Optional arg NOERROR inhibits this.
Return the deleted entry if PX was found."
  (xpm--w/gg (cpp pinfo origin) (xpm--gate)
    (let* ((ht (cdr pinfo))
           (ent (when (xpm--validate-px cpp px)
                  (gethash px ht))))
      (unless (or ent noerror)
        (error "No such px: %S" px))
      (when ent
        (remhash px ht)
        (xpm--adjust-npal -1 pinfo)
        (re-search-forward (concat "^\"" px "\\s-.*$") origin)
        (delete-region (match-beginning 0) (1+ (match-end 0)))
        ent))))

(defun xpm-add-px (px color &optional append)
  "Add an entry associating PX with COLOR to the palette.
If COLOR is a string, it is associated using the ‘c’ type.
Otherwise, it should be an alist with symbolic types and
string values, for instance:

 ((s . \"border\")
  (c . \"blue\"))

Aside from ‘c’olor and ‘s’ymbolic, there is also ‘g’rayscale,
‘m’onochrome and ‘g4’ (four-level gray scale).

The new entry is normally added to the front.
Optional arg APPEND non-nil means add it to the rear."
  (xpm--w/gg (cpp pinfo origin) (xpm--gate)
    (let ((alist (pcase color
                   ((pred stringp) (list (cons 'c color)))
                   ((pred consp) color)
                   (_ (error "Invalid COLOR: %S" color))))
          (ht (cdr pinfo)))
      (xpm--validate-px cpp px)
      (xpm-drop-px px t)
      (xpm--adjust-npal 1 pinfo)
      (unless (or (not append)
                  (zerop (hash-table-count ht)))
        (goto-char (1- origin))
        (skip-chars-backward "^,")
        (forward-line 1))
      (insert "\"" px "  " (mapconcat
                            (lambda (pair)
                              (format "%s %s" (car pair) (cdr pair)))
                            alist
                            " ")
              "\",\n")
      (puthash px alist ht))))

(provide 'xpm-palette)

;;; xpm-palette.el ends here
