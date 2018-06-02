;;; gnome-c-style.el --- minor mode for editing GNOME-style C source code -*- lexical-binding: t; -*-
;; Copyright (C) 2016 Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@gnu.org>
;; Keywords: GNOME, C, coding style
;; Version: 0.1
;; Maintainer: Daiki Ueno <ueno@gnu.org>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a minor mode to help editing C source code
;; in the GNOME C coding style:
;;
;; <https://developer.gnome.org/programming-guidelines/stable/c-coding-style.html.en#header-files>
;; <https://developer.gnome.org/programming-guidelines/stable/c-coding-style.html.en#functions>
;;
;; It basically provides two functions: code alignment and snippet
;; insertion.  To align code, use `gnome-c-style-align-decls-region'
;; to line-up multiple function declarations in region, and
;; `gnome-c-style-align-arglist-at-point' to line-up arguments in the
;; argument list at point.
;;
;; To insert code snippet, use `gnome-c-snippet-insert'.  The command
;; will let you choose a template to be inserted.  This package also
;; provide commands to insert package/class names in upper case,
;; capital case, and lower case.  For complete list of commands, do
;; M-x describe-bindings.

;;; Code:

(require 'gnome-c-align)
(require 'gnome-c-snippet)

(defgroup gnome-c-style nil
  "GNOME-style C source code editing"
  :prefix "gnome-c-"
  :group 'c)

(defvar gnome-c-style-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-c\C-ga" 'gnome-c-align-arglist-at-point)
    (define-key keymap "\C-c\C-gr" 'gnome-c-align-decls-region)
    (define-key keymap "\C-c\C-gf" 'gnome-c-align-set-column)
    (define-key keymap "\C-c\C-gg" 'gnome-c-align-guess-columns)
    (define-key keymap "\C-c\C-g\C-g" 'gnome-c-align-guess-optimal-columns)
    (define-key keymap "\C-c\C-gc" 'gnome-c-snippet-insert-package_class)
    (define-key keymap "\C-c\C-gC" 'gnome-c-snippet-insert-PACKAGE_CLASS)
    (define-key keymap "\C-c\C-g\C-c" 'gnome-c-snippet-insert-PackageClass)
    (define-key keymap "\C-c\C-gs" 'gnome-c-snippet-insert)
    keymap))

;;;###autoload
(define-minor-mode gnome-c-style-mode
  "A minor-mode for editing GNOME-style C source code."
  nil " GNOME" gnome-c-style-mode-map)

(provide 'gnome-c-style)

;;; gnome-c-style.el ends here
