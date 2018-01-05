;;; package-fixes.el --- package.el bug fixes ported to older Emacsen  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Artur Malabarba <emacs@endlessparentheses.com>
;; Keywords: tools
;; Version: 0

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

;; This package fixes some critical bugs in package.el 1.0.1 which
;; cause bad .elc files to be created during package upgrades when a
;; macro changes.  It is designed to be required as a dependency by
;; packages whose installation is affected by these bugs.

;; This package can be safely installed on Emacs >= 25, in which
;; case it does nothing.

;;; Code:


;;; Emacs < 25
(unless (fboundp 'package--list-loaded-files)
  (require 'package)
  (require 'find-func)

  (declare-function package-fixes--autoloads-file-name "package-fixes")
  (declare-function find-library-name "find-func")
  (declare-function package-fixes--list-loaded-files "package-fixes")
  (declare-function package-fixes--activate-autoloads-and-load-path "package-fixes")

  ;; None of these functions are defined in Emacs < 25.1.  Defining
  ;; them here doesn't actually do anything yet, they will be used by
  ;; the advices below.
  (defun package-fixes--autoloads-file-name (pkg-desc)
    "Return the absolute name of the autoloads file, sans extension.
PKG-DESC is a `package-desc' object."
    (expand-file-name
     (format "%s-autoloads" (package-desc-name pkg-desc))
     (package-desc-dir pkg-desc)))

  (defun package-fixes--activate-autoloads-and-load-path (pkg-desc)
    "Load the autoloads file and add package dir to `load-path'.
PKG-DESC is a `package-desc' object."
    (let* ((old-lp load-path)
           (pkg-dir (package-desc-dir pkg-desc))
           (pkg-dir-dir (file-name-as-directory pkg-dir)))
      (with-demoted-errors "Error loading autoloads: %s"
        (load (package-fixes--autoloads-file-name pkg-desc) nil t))
      (when (and (eq old-lp load-path)
                 (not (or (member pkg-dir load-path)
                          (member pkg-dir-dir load-path))))
        ;; Old packages don't add themselves to the `load-path', so we have to
        ;; do it ourselves.
        (push pkg-dir load-path))))

  (defun package-fixes--list-loaded-files (dir)
    "Recursively list all files in DIR which correspond to loaded features.
Returns the `file-name-sans-extension' of each file, relative to
DIR, sorted by most recently loaded last."
    (let* ((history (delq nil
                          (mapcar (lambda (x)
                                    (let ((f (car x)))
                                      (and f (file-name-sans-extension f))))
                                  load-history)))
           (dir (file-truename dir))
           ;; List all files that have already been loaded.
           (list-of-conflicts
            (delq
             nil
             (mapcar
              (lambda (x) (let* ((file (file-relative-name x dir))
                            ;; Previously loaded file, if any.
                            (previous
                             (ignore-errors
                               (file-name-sans-extension
                                (file-truename (find-library-name file)))))
                            (pos (when previous (member previous history))))
                       ;; Return (RELATIVE-FILENAME . HISTORY-POSITION)
                       (when pos
                         (cons (file-name-sans-extension file) (length pos)))))
              (directory-files-recursively dir "\\`[^\\.].*\\.el\\'")))))
      ;; Turn the list of (FILENAME . POS) back into a list of features.  Files in
      ;; subdirectories are returned relative to DIR (so not actually features).
      (let ((default-directory (file-name-as-directory dir)))
        (mapcar (lambda (x) (file-truename (car x)))
                (sort list-of-conflicts
                      ;; Sort the files by ascending HISTORY-POSITION.
                      (lambda (x y) (< (cdr x) (cdr y))))))))

  (defun package-fixes--load-files-for-activation (pkg-desc reload)
    "Load files for activating a package given by PKG-DESC.
Load the autoloads file, and ensure `load-path' is setup.  If
RELOAD is non-nil, also load all files in the package that
correspond to previously loaded files."
    (let* ((loaded-files-list (when reload
                                (package-fixes--list-loaded-files (package-desc-dir pkg-desc)))))
      ;; Add to load path, add autoloads, and activate the package.
      (package-fixes--activate-autoloads-and-load-path pkg-desc)
      ;; Call `load' on all files in `package-desc-dir' already present in
      ;; `load-history'.  This is done so that macros in these files are updated
      ;; to their new definitions.  If another package is being installed which
      ;; depends on this new definition, not doing this update would cause
      ;; compilation errors and break the installation.
      (with-demoted-errors "Error in package--load-files-for-activation: %s"
        (mapc (lambda (feature) (load feature nil t))
              ;; Skip autoloads file since we already evaluated it above.
              (remove (file-truename (package-fixes--autoloads-file-name pkg-desc))
                      loaded-files-list)))))

  
;;; 24.1, 24.2, 24.3
  (defadvice package--make-autoloads-and-compile (around fix-package--make-autoloads-and-compile
                                                         (name pkg-dir) activate)
    "Fixed `package--make-autoloads-and-compile'.
Behave the same as `package--make-autoloads-and-compile', except
it uses `package-fixes--load-files-for-activation' instead of just
loading the autoloads file."
    (package-generate-autoloads name pkg-dir)
    (package-fixes--load-files-for-activation pkg-desc :reload)
    (let ((load-path (cons pkg-dir load-path)))
      ;; We must load the autoloads file before byte compiling, in
      ;; case there are magic cookies to set up non-trivial paths.
      (byte-recompile-directory pkg-dir 0 t)))

;;; 24.4, 24.5
  (defadvice package--compile (after fix-package--compile (pkg-desc) activate)
    "Like `package--compile', but reload package first.
Uses `package-fixes--load-files-for-activation' to reload files."
    (package-activate-1 pkg-desc)
    (package-fixes--load-files-for-activation pkg-desc :reload)
    (byte-recompile-directory (package-desc-dir pkg-desc) 0 t)))

(provide 'package-fixes)
;;; package-fixes.el ends here
