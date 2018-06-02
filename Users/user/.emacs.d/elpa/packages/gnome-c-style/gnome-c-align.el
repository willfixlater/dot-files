;; gnome-c-align.el --- GNOME-style code alignment -*- lexical-binding: t; -*-
;; Copyright (C) 2016 Free Software Foundation, Inc.

;; Author: Daiki Ueno <ueno@gnu.org>
;; Keywords: GNOME, C, coding style

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

;;; Code:

(require 'cc-mode)
(require 'cl-lib)

(defcustom gnome-c-align-max-column 80
  "Maximum number of columns per line."
  :type '(choice (integer :tag "Columns")
		 (const :tag "No wrap"))
  :group 'gnome-c-style)

(defvar gnome-c-align-identifier-start-column nil)
(make-variable-buffer-local 'gnome-c-align-identifier-start-column)

(defvar gnome-c-align-arglist-start-column nil)
(make-variable-buffer-local 'gnome-c-align-arglist-start-column)

(defvar gnome-c-align-arglist-identifier-start-column nil)
(make-variable-buffer-local 'gnome-c-align-arglist-identifier-start-column)

(cl-defstruct (gnome-c-align--argument
	       (:constructor nil)
	       (:constructor gnome-c-align--make-argument (type-start
							   type-identifier-end
							   type-end
							   identifier-start
							   identifier-end))
	       (:copier nil)
	       (:predicate nil))
  (type-start nil :read-only t)
  (type-identifier-end nil :read-only t)
  (type-end nil :read-only t)
  (identifier-start nil :read-only t)
  (identifier-end nil :read-only t))

(defun gnome-c-align--marker-column (marker)
  (save-excursion
    (goto-char marker)
    (current-column)))

(defun gnome-c-align--indent-to-column (column)
  ;; Prefer 'char **foo' than 'char ** foo'
  (when (looking-back "\\*+" nil t)
    (setq column (- column (- (match-end 0) (match-beginning 0))))
    (goto-char (match-beginning 0)))
  ;; FIXME: should respect indent-tabs-mode?
  (let (indent-tabs-mode)
    (indent-to-column column)))

(defun gnome-c-align--argument-type-width (arg)
  (- (gnome-c-align--marker-column (gnome-c-align--argument-type-end arg))
     (gnome-c-align--marker-column (gnome-c-align--argument-type-start arg))))

(defun gnome-c-align--argument-type-identifier-width (arg)
  (- (gnome-c-align--marker-column
      (gnome-c-align--argument-type-identifier-end arg))
     (gnome-c-align--marker-column
      (gnome-c-align--argument-type-start arg))))

(defun gnome-c-align--arglist-identifier-start-column (arglist start-column)
  (let ((max-type-identifier-width
	 (apply #'max
		0
		(mapcar #'gnome-c-align--argument-type-identifier-width
			arglist)))
	(max-extra-width
	 (apply #'max
		0
		(mapcar
		 (lambda (argument)
		   (- (gnome-c-align--argument-type-end argument)
		      (gnome-c-align--argument-type-identifier-end argument)))
		 arglist))))
    (+ start-column max-type-identifier-width max-extra-width)))

(defun gnome-c-align--argument-identifier-width (argument)
  (if (gnome-c-align--argument-identifier-start argument)
      (- (gnome-c-align--marker-column
	  (gnome-c-align--argument-identifier-end argument))
	 (gnome-c-align--marker-column
	  (gnome-c-align--argument-identifier-start argument)))
    0))

(defun gnome-c-align--arglist-identifier-width (arglist)
  (apply #'max 0 (mapcar #'gnome-c-align--argument-identifier-width arglist)))

(defun gnome-c-align--normalize-arglist-region (arglist beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "\\s-+" nil t)
	(replace-match " "))
      (goto-char (point-min))
      (while (re-search-forward "\\s-*," nil t)
	(replace-match ",\n"))
      (goto-char (point-min))
      (delete-trailing-whitespace)
      ;; Remove whitespace at the beginning of line
      (goto-char (point-min))
      (while (re-search-forward "^\\s-+" nil t)
	(replace-match ""))
      ;; Remove empty lines
      (goto-char (point-min))
      (delete-matching-lines "^$")
      ;; 'int * * * foo' -> 'int ***foo'
      (dolist (argument arglist)
	(goto-char (gnome-c-align--argument-type-end argument))
	(while (re-search-backward
		"\\(\\*+\\)\\s-+"
		(gnome-c-align--argument-type-identifier-end argument)
		t)
	  (replace-match "\\1"))
	(when (gnome-c-align--argument-identifier-start argument)
	  (goto-char (gnome-c-align--argument-identifier-start argument))
	  (if (looking-back "\\* " nil)
	      (delete-char -1)))
	(goto-char (gnome-c-align--argument-type-end argument))))))

(defun gnome-c-align--parse-arglist (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (let (type-start
	    type-identifier-end
	    type-end
	    identifier-start
	    identifier-end
	    arglist
	    last-token-start)
	(goto-char (point-max))
	(while (not (bobp))
	  (c-backward-syntactic-ws)
	  (setq identifier-end (point-marker))
	  ;; Array argument, such as 'int a[]'
	  (if (eq (preceding-char) ?\])
	      (c-backward-sexp))
	  (c-backward-token-2)
	  (setq identifier-start (point-marker))
	  (c-backward-syntactic-ws)
	  (if (or (bobp) (eq (preceding-char) ?,))
	      (progn
		;; Identifier is omitted, or '...'.
		(setq type-start identifier-start
		      type-identifier-end identifier-end
		      type-end identifier-end
		      identifier-start nil
		      identifier-end nil)
		(c-backward-token-2))
	    (setq type-end (point-marker)
		  last-token-start type-end)
	    (while (and (not (bobp))
			(progn
			  (c-backward-token-2)
			  (unless (eq (char-after) ?,)
			    (setq last-token-start (point-marker)))))
	      (c-backward-syntactic-ws))
	    (setq type-start last-token-start)
	    (save-excursion
	      (goto-char type-end)
	      (skip-chars-backward "* " type-start)
	      (c-backward-syntactic-ws)
	      (setq type-identifier-end (point-marker))))
	  (push (gnome-c-align--make-argument type-start
					      type-identifier-end
					      type-end
					      identifier-start
					      identifier-end)
		arglist))
	arglist))))

;;;###autoload
(defun gnome-c-align-arglist-at-point (&optional identifier-start-column)
  "Reformat argument list at point, aligning argument to the right end."
  (interactive)
  (save-excursion
    (let* (start-column arglist)
      (cl-destructuring-bind (beg end)
	  (gnome-c-align--arglist-region-at-point (point))
	(goto-char beg)
	(setq start-column (current-column))
	(save-restriction
	  (narrow-to-region beg end)
	  (setq arglist (gnome-c-align--parse-arglist (point-min) (point-max)))
	  (gnome-c-align--normalize-arglist-region
	   arglist (point-min) (point-max))
	  (unless identifier-start-column
	    (setq identifier-start-column
		  (gnome-c-align--arglist-identifier-start-column arglist 0)))
	  (dolist (argument arglist)
	    (goto-char (gnome-c-align--argument-type-start argument))
	    (let ((column (if (bobp) 0 start-column)))
	      (when (not (bobp))
		(gnome-c-align--indent-to-column start-column))
	      (when (gnome-c-align--argument-identifier-start argument)
		(setq column (+ column identifier-start-column))
		(goto-char (gnome-c-align--argument-identifier-start argument))
		(gnome-c-align--indent-to-column column)))))))))

(cl-defstruct (gnome-c-align--decl
	       (:constructor nil)
	       (:constructor gnome-c-align--make-decl (start
						       end
						       identifier-start
						       identifier-end
						       arglist-start
						       arglist-end
						       arglist))
	       (:copier nil)
	       (:predicate nil))
  (start nil :read-only t)
  (end nil :read-only t)
  (identifier-start nil :read-only t)
  (identifier-end nil :read-only t)
  (arglist-start nil :read-only t)
  (arglist-end nil :read-only t)
  (arglist nil :read-only t))

(defun gnome-c-align--decls-identifier-start-column (decls start-column)
  (apply #'max
	 start-column
	 (delq nil
	       (mapcar
		(lambda (decl)
		  (let ((decl-column
			 (+ start-column
			    (gnome-c-align--marker-column
			     (gnome-c-align--decl-identifier-start decl)))))
		    (if (and gnome-c-align-max-column
			     (> decl-column gnome-c-align-max-column))
			nil
		      decl-column)))
		decls))))

(defun gnome-c-align--decl-identifier-width (decl)
  (- (gnome-c-align--marker-column
      (gnome-c-align--decl-identifier-end decl))
     (gnome-c-align--marker-column
      (gnome-c-align--decl-identifier-start decl))))

(defun gnome-c-align--decls-arglist-start-column (decls start-column)
  (let ((arglist-width
	 (+ (gnome-c-align--decls-arglist-identifier-start-column decls 0)
	    (gnome-c-align--decls-arglist-identifier-width decls)
	    (length ");"))))
    (apply #'max
	   start-column
	   (delq nil
		 (mapcar
		  (lambda (decl)
		    (let ((decl-column
			   (+ start-column
			      (gnome-c-align--decl-identifier-width decl)
			      1)))
		      (if (and gnome-c-align-max-column
			       (> (+ decl-column arglist-width)
				  gnome-c-align-max-column))
			  nil
			decl-column)))
		  decls)))))

(defun gnome-c-align--decls-arglist-identifier-width (decls)
  (apply #'max 0 (mapcar (lambda (decl)
			   (gnome-c-align--arglist-identifier-width
			    (gnome-c-align--decl-arglist decl)))
			 decls)))

(defun gnome-c-align--decls-arglist-identifier-start-column (decls start-column)
  (apply #'max 0 (mapcar (lambda (decl)
			   ;; FIXME: should wrap lines inside argument list?
			   (gnome-c-align--arglist-identifier-start-column
			    (gnome-c-align--decl-arglist decl)
			    start-column))
			 decls)))

(defun gnome-c-align--parse-decl (beg end)
  ;; Parse at most one func declaration found in BEG END.
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (let (arglist-start
	    arglist-end
	    identifier-start
	    identifier-end
	    vfunc-p)
	(goto-char (point-min))
	(c-forward-syntactic-ws)
	(unless (looking-at
		 "typedef\\|#\\|G_\\(?:DECLARE\\|DEFINE\\)")
	  (while (and (not (eobp))
		      (not (eq (char-after) ?\()))
	    (c-forward-token-2)
	    (c-forward-syntactic-ws))
	  ;; Identifier is vfunc.
	  (when (looking-at "(\\s-*\\*")
	    (c-forward-sexp)
	    (c-forward-syntactic-ws)
	    (setq vfunc-p t))
	  (when (eq (char-after) ?\()
	    (setq arglist-start (point-marker))
	    (c-backward-syntactic-ws)
	    (setq identifier-end (point-marker))
	    (if vfunc-p
		(c-backward-sexp)
	      (c-backward-token-2))
	    (setq identifier-start (point-marker))
	    (goto-char arglist-start)
	    (c-forward-sexp)
	    (setq arglist-end (point-marker))
	    (gnome-c-align--make-decl beg end
				      identifier-start identifier-end
				      arglist-start arglist-end
				      (gnome-c-align--parse-arglist
				       (1+ arglist-start)
				       (1- arglist-end)))))))))

(defun gnome-c-align--normalize-decl (decl)
  (save-excursion
    ;; Replace newlines with a space
    (save-restriction
      ;; Ignore lines before identifier-start
      (goto-char (gnome-c-align--decl-identifier-start decl))
      (beginning-of-line)
      (narrow-to-region (point)
			(gnome-c-align--decl-arglist-end decl))
      (goto-char (point-min))
      (while (re-search-forward "\n" nil t)
	(replace-match " ")))
    ;; Replace consequent spaces with a space
    (save-restriction
      ;; Ignore lines before identifier-start
      (goto-char (gnome-c-align--decl-identifier-start decl))
      (beginning-of-line)
      (narrow-to-region (point)
			(gnome-c-align--decl-arglist-end decl))
      (goto-char (point-min))
      (while (re-search-forward "\\s-+" nil t)
	(replace-match " ")))
    (goto-char (gnome-c-align--decl-identifier-start decl))
    (if (looking-back "\\* " nil)
	(delete-char -1))
    ;; Normalize the argument list
    (gnome-c-align--normalize-arglist-region
     (gnome-c-align--decl-arglist decl)
     (gnome-c-align--decl-arglist-start decl)
     (gnome-c-align--decl-arglist-end decl))))

(defun gnome-c-align--arglist-region-at-point (point)
  (save-excursion
    (let (start)
      (goto-char point)
      (c-beginning-of-statement-1)
      (c-backward-syntactic-ws)
      (unless (eq ?\( (preceding-char))
	(error "No containing argument list"))
      (setq start (point))
      (backward-char)
      (condition-case nil
	  (c-forward-sexp)
	(error
	 (error "No closing parenthesis")))
      (backward-char)
      (list start (point)))))

;;;###autoload
(defun gnome-c-align-set-column (symbol)
  "Set alignment column of SYMBOL."
  (interactive
   (let ((symbol-name (completing-read "Symbol to change: "
				       '("identifier-start"
					 "arglist-start"
					 "arglist-identifier-start")
				       nil t)))
     (list (intern (format "gnome-c-align-%s-column" symbol-name)))))
  (set symbol (current-column)))

(defun gnome-c-align--scan-decls (beg end)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let (decls)
	(while (not (eobp))
	  (let (decl-start decl-end decl)
	    (c-forward-syntactic-ws)
	    (setq decl-start (point-marker))
	    (c-end-of-statement)
	    (setq decl-end (point-marker))
	    (setq decl (gnome-c-align--parse-decl decl-start decl-end))
	    (when decl
	      (push decl decls))))
	decls))))

(defun gnome-c-align--guess-optimal-columns (beg end)
  (let ((buffer (current-buffer))
	decls)
    (with-temp-buffer
      (insert-buffer-substring-no-properties buffer beg end)
      (c-mode)
      (setq decls (gnome-c-align--scan-decls (point-min) (point-max)))
      (mapc #'gnome-c-align--normalize-decl decls)
      (let* ((identifier-start-column
	      (gnome-c-align--decls-identifier-start-column
	       decls 0))
	     (arglist-start-column
	      (gnome-c-align--decls-arglist-start-column
	       decls identifier-start-column))
	     (arglist-identifier-start-column
	      (gnome-c-align--decls-arglist-identifier-start-column
	       decls (+ (length "(") arglist-start-column))))
	(list (cons 'identifier-start-column
		    identifier-start-column)
	      (cons 'arglist-start-column
		    arglist-start-column)
	      (cons 'arglist-identifier-start-column
		    arglist-identifier-start-column))))))

;;;###autoload
(defun gnome-c-align-guess-optimal-columns (beg end)
  "Compute the optimal alignment rule from the declarations in BEG and END.

This sets `gnome-c-align-identifier-start-column',
`gnome-c-align-arglist-start-column', and
`gnome-c-align-arglist-identifier-start-column'."
  (interactive "r")
  (let ((columns (gnome-c-align--guess-optimal-columns beg end)))
    (setq gnome-c-align-identifier-start-column
	  (cdr (assq 'identifier-start-column columns))
	  gnome-c-align-arglist-start-column
	  (cdr (assq 'arglist-start-column columns))
	  gnome-c-align-arglist-identifier-start-column
	  (cdr (assq 'arglist-identifier-start-column columns)))
    (message
     "identifier-start: %d, arglist-start: %d, arglist-identifier-start: %d"
     gnome-c-align-identifier-start-column
     gnome-c-align-arglist-start-column
     gnome-c-align-arglist-identifier-start-column)))

;;;###autoload
(defun gnome-c-align-guess-columns (beg end)
  "Guess the existing alignment rule from the declarations in BEG and END.

This sets `gnome-c-align-identifier-start-column',
`gnome-c-align-arglist-start-column', and
`gnome-c-align-arglist-identifier-start-column'."
  (interactive "r")
  (let ((decls (gnome-c-align--scan-decls beg end))
	arglist)
    (unless decls
      (error "No function declaration in the region"))
    (setq arglist (gnome-c-align--parse-arglist
		   (1+ (gnome-c-align--decl-arglist-start (car decls)))
		   (1- (gnome-c-align--decl-arglist-end (car decls)))))
    (unless arglist
      (error "Empty argument list"))
    (unless (gnome-c-align--argument-identifier-start (car arglist))
      (error "No identifier in the argument list"))
    (setq gnome-c-align-identifier-start-column
	  (gnome-c-align--marker-column
	   (gnome-c-align--decl-identifier-start (car decls)))
	  gnome-c-align-arglist-start-column
	  (gnome-c-align--marker-column
	   (gnome-c-align--decl-arglist-start (car decls)))
	  gnome-c-align-arglist-identifier-start-column
	  (gnome-c-align--marker-column
	   (gnome-c-align--argument-identifier-start (car arglist))))
    (message
     "identifier-start: %d, arglist-start: %d, arglist-identifier-start: %d"
     gnome-c-align-identifier-start-column
     gnome-c-align-arglist-start-column
     gnome-c-align-arglist-identifier-start-column)))

;;;###autoload
(defun gnome-c-align-decls-region (beg end)
  "Reformat function declarations in the region between BEG and END.

The `gnome-c-align-identifier-start-column',
`gnome-c-align-arglist-start-column', and
`gnome-c-align-arglist-identifier-start-column' variables
control the widths.

To set those variables, use \\[gnome-c-align-set-column],
\\[gnome-c-align-guess-columns], or
\\[gnome-c-align-guess-optimal-columns].

If they are not set, this function internally calls
\\[gnome-c-align-guess-optimal-columns] before formatting."
  (interactive "r")
  (save-excursion
    (let (decls)
      (save-restriction
	(narrow-to-region beg end)
	(unless (and gnome-c-align-identifier-start-column
		     gnome-c-align-arglist-start-column
		     gnome-c-align-arglist-identifier-start-column)
	  (let ((columns (gnome-c-align--guess-optimal-columns beg end)))
	    (unless gnome-c-align-identifier-start-column
	      (setq gnome-c-align-identifier-start-column
		    (cdr (assq 'identifier-start-column columns))))
	    (unless gnome-c-align-arglist-start-column
	      (setq gnome-c-align-arglist-start-column
		    (cdr (assq 'arglist-start-column columns))))
	    (unless gnome-c-align-arglist-identifier-start-column
	      (setq gnome-c-align-arglist-identifier-start-column
		    (cdr (assq 'arglist-identifier-start-column columns))))))
	(setq decls (gnome-c-align--scan-decls beg end))
	(dolist (decl decls)
	  (gnome-c-align--normalize-decl decl)
	  (goto-char (gnome-c-align--decl-identifier-start decl))
	  (gnome-c-align--indent-to-column
	   gnome-c-align-identifier-start-column)
	  (goto-char (gnome-c-align--decl-identifier-end decl))
	  (when (>= (current-column) gnome-c-align-arglist-start-column)
	    (insert "\n"))
	  (goto-char (gnome-c-align--decl-arglist-start decl))
	  (gnome-c-align--indent-to-column
	   gnome-c-align-arglist-start-column)
	  (forward-char)
	  (gnome-c-align-arglist-at-point
	   (- (- gnome-c-align-arglist-identifier-start-column
		 (length "("))
	      gnome-c-align-arglist-start-column)))))))

(provide 'gnome-c-align)

;;; gnome-c-align.el ends here
