;;; flower.el --- can `xpm-raster' DTRT?     -*- lexical-binding: t -*-

;; Copyright (C) 2014-2017 Free Software Foundation, Inc.

;; Author: Thien-Thi Nguyen <ttn@gnu.org>
;; Maintainer: Thien-Thi Nguyen <ttn@gnu.org>

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

;; This file helps visualize `xpm-raster' failure modes.  Maybe one
;; day it will be rendered useless by improvements to `xpm-raster'.
;;
;; NB: There is no `provide' form.
;; NB: Loading munges the global keymap -- YHBW!

;;; Code:

(require 'xpm)
(require 'xpm-m2z)
(require 'cl-lib)

(defvar flower-size 99
  "Number of pixels in the flower image (a square).
For best results, this should be at least 99 and odd.")

(defun flower (&optional again)
  "Stress `xpm-raster' in various ways."
  (interactive "P")
  (let ((buf (get-buffer "flower")))
    (when buf (kill-buffer buf)))
  (switch-to-buffer
   (xpm-generate-buffer "flower" flower-size flower-size 2
                        '(("  " . "green")
                          (".." . "yellow")
                          ("OO" . "red")
                          ("--" . "black"))))
  (setq truncate-lines t)
  (let* ((τ (* 4 2 (atan 1)))
         (half (/ flower-size 2.0))
         (mag-fns (vector (lambda (θ) (ignore θ) 1)
                          (lambda (θ) (sin θ))
                          (lambda (θ) (cos θ))
                          (lambda (θ) (sin (* 0.5 τ θ)))
                          (lambda (θ) (cos (* 0.5 τ θ)))
                          (lambda (θ) (sin (* 0.25 τ θ)))
                          (lambda (θ) (cos (* 0.25 τ θ)))
                          (lambda (θ) (sin (* τ θ)))
                          (lambda (θ) (cos (* τ θ)))))
         (n-mag-fns (length mag-fns)))
    (cl-flet
        ((random-mag-fn () (aref mag-fns (random n-mag-fns)))
         (form (fn &rest args) (apply fn half half (random 42) args)))
      (let* ((x-mag-fn (random-mag-fn))
             (y-mag-fn (random-mag-fn))
             (form (if again
                       (get 'flower 'form)
                     (delete-dups
                      (if (zerop (random 5))
                          (let ((one (form 'xpm-m2z-circle))
                                (two (form 'xpm-m2z-ellipse (random 42))))
                            (append one two))
                        (cl-loop
                         with bias = (* 0.42 half)
                         with mm = (+ bias (random (truncate bias)))
                         for θ below τ by 0.003
                         collect
                         (cl-flet
                             ((at (f mfn)
                                  (truncate (+ half (* mm (funcall mfn θ)
                                                       (funcall f θ))))))
                           (cons (at 'cos x-mag-fn)
                                 (at 'sin y-mag-fn)))))))))
        (put 'flower 'form form)
        (xpm-raster form "OO" ".."))))
  (image-mode)
  ;; strangely, image-mode screws up the markers, so we need to do
  ;; this again if we want to do subsequent xpm-* access:
  ;;+ (xpm-grok t)
  t)

;;;---------------------------------------------------------------------------
;;; load-time actions

(global-set-key [f9] 'flower)
(global-set-key
 [(meta f9)]
 (lambda () (interactive)
   (message "xpm-raster-inhibit-continuity-optimization now %s"
            (setq xpm-raster-inhibit-continuity-optimization
                  (not xpm-raster-inhibit-continuity-optimization)))))

;;; flower.el ends here
