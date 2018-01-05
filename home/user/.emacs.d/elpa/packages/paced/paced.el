;;; paced.el --- Predictive Abbreviation Completion and Expansion using Dictionaries -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Free Software Foundation, Inc.

;; Author: Ian Dunn <dunni@gnu.org>
;; Maintainer: Ian Dunn <dunni@gnu.org>
;; Keywords: convenience, completion
;; Package-Requires: ((emacs "25.1") (async "1.9.1"))
;; URL: https://savannah.nongnu.org/projects/paced-el/
;; Version: 1.0
;; Created: 22 Jan 2017
;; Modified: 08 Dec 2017

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Paced (Predictive Abbreviation Completion and Expansion using Dictionaries)
;; scans a group of files (determined by "population commands") to construct a
;; usage table (dictionary).  Words (or symbols) are sorted by their usage, and
;; may be later presented to the user for completion.  A dictionary can then be
;; saved to a file, to be loaded later.

;; Population commands determine how a dictionary should be filled with words or
;; symbols.  A dictionary may have multiple population commands, and population
;; may be performed asynchronously.  Once population is finished, the contents
;; are sorted, with more commonly used words at the front.  Dictionaries may be
;; edited through EIEIO's customize-object interface.

;; Completion is done through `completion-at-point'.  The dictionary to use for
;; completion can be customized.

;;; Code:

(eval-when-compile (require 'subr-x))

(require 'thingatpt)
(require 'map)
(require 'eieio-base)
(require 'rx)

;; Compatibility for Emacs < 26.1
(unless (fboundp 'if-let*)
  (defalias 'if-let* 'if-let))
(unless (fboundp 'when-let*)
  (defalias 'when-let* 'when-let))

(defgroup paced nil
  "Predictive Abbreviation Completion and Expansion using Dictionaries"
  :group 'convenience)

(defcustom paced-thing-at-point-constituent 'symbol
  "Symbol defining THING which function `paced-mode' works on.
This symbol should be understandable by
`bounds-of-thing-at-point'.  This symbol defines what function `paced-mode'
considers to be the basic unit of expansion.  If if it set to `symbol',
for example, \"paced-mode\" would be offered as an expansion, while
if it is set to `word' \"paced\" and \"mode\" would be offered."
  :group 'paced
  :type 'symbol
  :options '(symbol word))

(defcustom paced-completion-ignore-case t
  "Non-nil to ignore case when completing.

Note that this does not affect dictionary population."
  :group 'paced
  :type 'boolean)

(defcustom paced-dictionary-directory (locate-user-emacs-file "paced-dictionaries/")
  "Directory in which the dictionaries are saved.

This is only used in `paced-load-all-dictionaries', so it's up to
the user whether to save dictionaries here."
  :group 'paced
  :type 'directory)

(defcustom paced-dictionary-directory-whitelist-regexp ".*"
  "Regexp to match when reading from the dictionary directory.

Any files that match this regexp will be loaded by
`paced-load-all-dictionaries'."
  :group 'paced
  :type 'regexp)

(defcustom paced-dictionary-directory-blacklist-regexp "$^"
  "Regexp to match for files NOT to load with `paced-load-all-dictionaries'.

This is the string \"$^\" by default, which matches nothing, thus
allowing all files."
  :group 'paced
  :type 'regexp)

(defcustom paced-load-all-dictionaries-recursively nil
  "Whether to recursively load all files with `paced-load-all-dictionaries'."
  :group 'paced
  :type 'boolean)

(defcustom paced-repopulate-saves-dictionary t
  "Whether to save a dictionary after repopulation."
  :group 'paced
  :type 'boolean)

(defcustom paced-populate-warn-about-reset t
  "Whether to warn the user about resetting a dictionary when repopulating."
  :group 'paced
  :type 'boolean)



(defun paced--default-dictionary-sort-func (usage-hash)
  "Default dictionary sort function.

Sort hash-table USAGE-HASH by the weights (values) in the table."
  ;; Unfortunately, there's no way to sort a hash-table, so we first convert it
  ;; into an alist, and sort that.
  (let ((seq (map-into usage-hash 'list)))
    (setq seq
          (seq-sort
           (pcase-lambda (`(_ . ,usage-lhs)
                          `(_ . ,usage-rhs))
             (> usage-lhs usage-rhs))
           seq))
    (map-into seq 'hash-table)))

(defclass paced-dictionary (eieio-named eieio-persistent)
  ((object-name :initarg :object-name
                :documentation "Symbol to use to refer to this dictionary.")
   (usage-hash :initarg :usage-hash
               :initform (make-hash-table :test #'equal)
               :type hash-table
               :documentation "Stores the usage data for this dictionary.

This is a hash table, mapping a word to the times it has been used.")
   (population-commands
    :initarg :population-commands
    :initform nil
    :type (list-of paced-population-command)
    :custom (repeat (object :objectcreatefcn paced-new-population-command-custom))
    :label "Population Commands"
    :documentation "Commands to use when populating this dictionary.

Each entry is a `paced-population-command'.")
   (file-header-line :type string
		     :allocation :class
		     :initform ";; Paced Dictionary"
		     :documentation
		     "Header line for the save file.
This is used with the `object-write' method.")
   (case-handling :initarg :case-handling
                  :initform downcase
                  :type (member downcase upcase preserve downcase-first upcase-first mixed-case)
                  :custom (choice (const :tag "Downcase All Words" downcase)
                                  (const :tag "Upcase All Words" upcase)
                                  (const :tag "Preserve Case" preserve)
                                  (const :tag "Downcase Just the First Letter" downcase-first)
                                  (const :tag "Upcase Just the First Letter" upcase-first)
                                  (const :tag "Preserve Case on Mixed-Case Words" mixed-case))
                  :label "Case Handling"
                  :documentation "How case should be handled during population.

It can be one of the following:

* downcase        Downcase every word
* upcase          Upcase every word
* preserve        Preserve case
* downcase-first  Downcase the first letter of each word, leave the rest the same
* upcase-first    Upcase the first letter of each word, leave the rest the same
* mixed-case      Preserve case on mixed-case words; single-case words
                  are downcased.  See `paced-mixed-case-word-p' for an
                  explanation of how \"mixed-case\" is defined.")
   (updated :initarg :updated
            :initform nil
            :type boolean
            :documentation "Non-nil if this dictionary has been updated since it was last saved.")
   (default-population-properties
     :initarg :default-population-properties
     :initform nil
     :type list
     :label "Default Properties"
     :custom (alist :tag "Properties" :key-type variable :value-type sexp)
     :documentation "Default properties for population commands.

Each element is of the form (VAR VALUE).  Each VAR will be set to
VALUE during population for this dictionary.

Properties set in the individual population commands will
override settings here.

Some suggested variables for this are `paced-exclude-function'
and `paced-thing-at-point-constituent'.")
   (sort-method :initarg :sort-method
                :initform 'paced--default-dictionary-sort-func
                :type function
                :label "Sort Method"
                :custom function
                :documentation "Method by which this dictionary should sort its usage table.

This should be a function of one argument, the usage-hash slot,
and return a sorted hash-table.

This defaults to `paced--default-dictionary-sort-func'."))
  "Paced dictionary.")

(defvar paced--registered-dictionaries (make-hash-table :test 'equal)
  "Internal list of registered dictionaries.

Do not edit this list manually.  Use `paced-make-dictionary'
instead.")

(defun paced-reset-registered-dictionaries ()
  "Reset the registered dictionary list.

WARNING: This will result in the loss of all dictionaries.  Only
do this if you know what you're doing, or are under the
supervision of someone who does."
  (interactive)
  (when (yes-or-no-p
         "Warning: This will result in loss of all dictionaries.  Continue?")
    (setq paced--registered-dictionaries (make-hash-table :test 'equal))))

(defsubst paced-named-dictionary (key)
  "Return a registered dictionary with name KEY.

If none exists, return nil."
  (map-elt paced--registered-dictionaries key nil))

(defsubst paced-dictionary-names ()
  "Get the names of the registered dictionaries."
  (map-keys paced--registered-dictionaries))

(defsubst paced-read-dictionary ()
  "Read the name of a registered dictionary."
  (completing-read "Dictionary: " (map-keys paced--registered-dictionaries)))

(defsubst paced-dictionary-key-registered-p (key)
  "Return non-nil if a dictionary with name KEY has been registered."
  (map-contains-key paced--registered-dictionaries key))

(defsubst paced-ensure-registered (key)
  "Throw an error if a dictionary with name KEY has not been registered."
  (unless (paced-dictionary-key-registered-p key)
    (error "No paced dictionary called '%s' has been registered" key)))

(cl-defmethod paced-dictionary-register ((dict paced-dictionary))
  "Registered dictionary DICT."
  (map-put paced--registered-dictionaries (oref dict object-name) dict))

(defsubst paced--ensure-dictionary-directory ()
  "Ensure that `paced-dictionary-directory' exists."
  (make-directory paced-dictionary-directory t))

(defun paced-make-dictionary (name filename case-handling)
  "Make a paced dictionary called NAME.

NAME is a string used to identify the new dictionary.

If a paced dictionary is already registered with name NAME, then
it is replaced with a new, empty one.

FILENAME is a file in which to store the new dictionary.

CASE-HANDLING is a symbol that denotes how to handle case during
population.  See the case-handling slot of class
`paced-dictionary' for details.

Return value is the new dictionary."
  (let ((new-dict (paced-dictionary
                   :object-name name
                   :file filename
                   :case-handling case-handling)))
    (paced-dictionary-register new-dict)
    new-dict))

(defun paced-create-new-dictionary (name file)
  "Create a new dictionary called NAME.

FILE is the file in which to store the new dictionary.

Once named, the dictionary can be edited through the EIEIO
customization interface."
  (declare (interactive-only paced-make-dictionary))
  (interactive (list (read-string "Name: ")
                     (read-file-name "Storage File: " paced-dictionary-directory)))
  (let ((new-dict (paced-dictionary :object-name name
                                    :file file)))
    (customize-object new-dict)))

(cl-defmethod paced-dictionary-name ((obj paced-dictionary))
  "Return the name of dictionary OBJ."
  (oref obj object-name))

(defcustom paced-global-dict-enable-alist nil
  "List that determines which dictionaries should be active.

Each entry has the form (CONDITION . DICT-KEY), where CONDITION
is one of the following forms:

- A mode name, such as `org-mode' or `text-mode', indicating that
  the named dictionary should be active in any mode derived from
  that mode.

- A symbol, in which case the named dictionary is active whenever
  the value of that symbol is non-nil.

- A function symbol, in which case the function is called with no
  arguments to determine if the given dictionary should be
  enabled.  If the function returns non-nil the dictionary is enabled.

- A lambda function, in which case it is called with no
  arguments, and if it returns non-nil, the dictionary is
  enabled.

- The form (or CONDITION1 CONDITION2 ...), which enables the
  given dictionary if any of the conditions are met.

- The form (and CONDITION1 CONDITION2 ...), which enables the
  given dictionary if all of the conditions are met.

No matter what this list indicates, dictionaries will not be
enabled unless `paced-mode' is active."
  :group 'paced
  :type '(alist :key-type sexp :value-type string))

(defvar-local paced-local-dict-enable-alist nil
  "Local enable list.

Has the same form as and takes priority over
`paced-global-dict-enable-alist'.")

(defun paced-dict-enable-list ()
  "Return the combination of the local and global enable-alists.

See `paced-local-dict-enable-alist' and
`paced-global-dict-enable-alist' for more information."
  (append paced-local-dict-enable-alist
          paced-global-dict-enable-alist))

(defun paced-mode-symbol-p (sym)
  "Return non-nil if SYM is a mode symbol."
  (string-match-p (rx "-mode" string-end) (symbol-name sym)))

(defun paced-test-dict-enable-condition (condition)
  "Determines if CONDITION passes in the current buffer.

See `paced-global-dict-enable-alist' for an explanation."
  (pcase condition
    ((and (pred symbolp)
          (pred paced-mode-symbol-p))
     (derived-mode-p condition))
    ((and (pred symbolp)
          (pred boundp))
     (symbol-value condition))
    ((and (pred symbolp)
          (pred fboundp))
     (funcall condition))
    ((pred functionp)
     (funcall condition))
    (`(or . ,rest)
     (seq-some 'paced-test-dict-enable-condition rest))
    (`(and . ,rest)
     (seq-every-p 'paced-test-dict-enable-condition rest))))

(defun paced-current-dictionary ()
  "Determine the current dictionary.

Returns nil if no dictionary should be enabled.

If a dictionary is found in the list that doesn't exist, it will
be skipped."
  (let ((conditions (paced-dict-enable-list))
        (dictionary))
    (while (and conditions
                (not dictionary))
      (pcase-let* ((`(,condition . ,dict) (pop conditions)))
        (when (and (paced-dictionary-key-registered-p dict)
                   (paced-test-dict-enable-condition condition))
          (setq dictionary dict))))
    (when dictionary
      (paced-named-dictionary dictionary))))

(cl-defmethod paced-dictionary-save ((dict paced-dictionary))
  "Save dictionary DICT according to its filename."
  (when (oref dict updated)
    (eieio-persistent-save dict))
  (oset dict updated nil))

(defun paced-save-named-dictionary (key)
  "Save dictionary named KEY."
  (declare (interactive-only paced-dictionary-save))
  (interactive (list (paced-read-dictionary)))
  (paced-ensure-registered key)
  (let ((dict (paced-named-dictionary key)))
    (paced-dictionary-save dict)))

(defun paced-load-dictionary-from-file (file)
  "Load dictionary from FILE."
  (interactive
   (list (read-file-name "Dictionary File: " paced-dictionary-directory)))
  (when-let* ((new-dict (eieio-persistent-read file 'paced-dictionary)))
    (paced-dictionary-register new-dict)))

(defun paced-save-all-dictionaries ()
  "Save all registered dictionaries."
  (interactive)
  (map-apply
   (lambda (_ dict)
     (paced-dictionary-save dict))
   paced--registered-dictionaries))

;;;###autoload
(defun paced-load-all-dictionaries ()
  "Load all dictionaries in `paced-dictionary-directory'."
  (interactive)
  (message "Loading all dictionaries from %s" paced-dictionary-directory)
  (paced--ensure-dictionary-directory)
  (let ((files-to-load
         (if paced-load-all-dictionaries-recursively
             (directory-files-recursively paced-dictionary-directory
                                          paced-dictionary-directory-whitelist-regexp)
           (directory-files paced-dictionary-directory t
                            paced-dictionary-directory-whitelist-regexp))))
    (dolist (dict-file files-to-load)
      (when (and (file-regular-p dict-file)
                 (not (string-match-p paced-dictionary-directory-blacklist-regexp dict-file)))
        (paced-load-dictionary-from-file dict-file)))))

(cl-defmethod eieio-done-customizing ((dict paced-dictionary))
  (paced-dictionary-register dict)
  (paced--ensure-dictionary-directory)
  (paced-dictionary-save dict))



(defvar-local paced--current-source nil
  "The source from which a dictionary is being populated.

This is used internally to inform the user of the current source,
since population mostly uses temporary buffers.")

(defvar-local paced-exclude-function (lambda () nil)
  "Local predicate to determine if thing at point should be excluded.

This should be a function of no arguments that returns non-nil if
the current thing-at-point should be excluded from paced dictionaries.

By default, this allows everything.

A useful function for this is `paced-in-comment-p'.")

(defun paced-in-comment-p (&optional pos)
  "Return non-nil if POS is in a comment.

If POS is not specified, defaults to `point'."
  (nth 8 (syntax-ppss (or pos (point)))))

(defun paced-excluded-p ()
  "Return non-nil to exclude current thing at point.

See `paced-exclude-function' for more."
  (funcall paced-exclude-function))

(defun paced-bounds-of-thing-at-point ()
  "Get the bounds of the thing at point."
  (bounds-of-thing-at-point paced-thing-at-point-constituent))

(defun paced-thing-at-point ()
  "Return the current thing at point.

The thing is determined by `paced-thing-at-point-constituent'.

Text properties are excluded."
  (when-let* ((bounds (paced-bounds-of-thing-at-point)))
    (buffer-substring-no-properties
     (car bounds) (cdr bounds))))

(defun paced-forward-thing (&optional number)
  "Move forward NUMBER things.

Things is based on `paced-thing-at-point-constituent'."
  (interactive "p")
  (forward-thing paced-thing-at-point-constituent number))

(defun paced-mixed-case-word-p (word)
  "Return non-nil if WORD is mixed-case.

A mixed-case word is one with both uppercase and lowercase
letters, but ignoring the first letter if it's uppercase.  This
is due to assuming the first letter is unimportant, as per
sentence starting."
  ;; Mixed case would typically be an uppercase letter followed by a lowercase
  ;; letter, or a lowercase letter followed by an uppercase letter.  Since we're
  ;; ignoring the first letter of a word if it's uppercase, we need to check for
  ;; two distinct uppercase letters, followed by a lowercase letter.
  (let ((case-fold-search nil)) ;; Case is important
    (string-match-p (rx (or (and lower upper) ;; lower followed by upper
                            ;; Two distinct uppercase letters, as in HAs
                            (and upper upper lower)))
                    word)))

(defun paced--handle-word-case (case-handling word)
  "Process WORD based on CASE-HANDLING.

This is a separate function only for testing; use
`paced-dictionary-process-word' instead."
  (pcase case-handling
    (`preserve word)
    (`downcase (downcase word))
    (`upcase (upcase word))
    (`downcase-first
     ;; Downcase the first letter
     (concat (downcase (substring word 0 1))
             (substring word 1)))
    ;; Upcase the first letter
    (`upcase-first
     (concat (upcase (substring word 0 1))
             (substring word 1)))
    (`mixed-case
     (if (paced-mixed-case-word-p word) word (downcase word)))))

(cl-defmethod paced-dictionary-process-word ((dict paced-dictionary) word)
  "Return WORD, modified based on DICT's case handling."
  (paced--handle-word-case (oref dict case-handling) word))

(cl-defmethod paced-dictionary-add-word ((dict paced-dictionary) word)
  "Add WORD to paced dictionary DICT."
  (let ((new-word (paced-dictionary-process-word dict word)))
    ;; Use the full name here to silence the byte-compiler
    (cl-incf (map-elt (oref dict usage-hash) new-word 0))
    (oset dict updated t)))

(defsubst paced-add-word-to-current-dict (word)
  "Add WORD to the current paced dictionary."
  (if-let* ((dict (paced-current-dictionary)))
      (paced-dictionary-add-word dict word)
    (error "No current dictionary found")))

(cl-defmethod paced-dictionary-populate-from-buffer ((dict paced-dictionary) &optional buffer)
  "Repopulate DICT from BUFFER.

If BUFFER is nil, use the current one."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (let* ((reporter-string
              (concat (format "Populating dictionary %s" (paced-dictionary-name dict))
                      (when paced--current-source (format " from %s"
                                                          paced--current-source))
                      "..."))
             (reporter (make-progress-reporter reporter-string 0 100)))
        (while (paced-forward-thing)
          (progress-reporter-do-update
           reporter
           (floor (* 100.0 (/ (float (point)) (point-max)))))
          (unless (paced-excluded-p)
            (paced-dictionary-add-word dict (paced-thing-at-point))))
        (progress-reporter-done reporter)))))

(defun paced-populate-dictionary-from-region (dict start end)
  "Populate DICT from the region in the current buffer between START and END.

Note that this doesn't add the current buffer to DICT's
population commands, so if DICT is later repopulated using
`paced-dictionary-repopulate' or
`paced-repopulate-named-dictionary', anything added with this
command will be lost."
  (save-restriction
    (narrow-to-region start end)
    (paced-dictionary-populate-from-buffer dict)))

(defun paced-populate-buffer-dictionary (&optional buffer)
  "Populate BUFFER's current dictionary with BUFFER.

This means add a usage of each included thing in buffer.

If called interactively, the current buffer is used.  In order to
only populate the dictionary from a region,
`paced-populate-from-region'.

Note that this doesn't add BUFFER to the dictionary's population
commands, so if it is later repopulated using
`paced-dictionary-repopulate' or
`paced-repopulate-named-dictionary', anything added with this
command will be lost.

In order to make changes permanent, use
`paced-add-buffer-file-to-dictionary'."
  (interactive)
  (if-let* ((dict (paced-current-dictionary)))
      (paced-dictionary-populate-from-buffer dict buffer)
    (user-error "No dictionary found")))

(defun paced-populate-from-region (start end)
  "Populate the current dictionary from the region START to END.

Note that this doesn't add the current buffer to the dictionary's
population commands, so if it is later repopulated using
`paced-dictionary-repopulate' or
`paced-repopulate-named-dictionary', anything added with this
command will be lost."
  (interactive "r")
  (if-let* ((dict (paced-current-dictionary)))
      (paced-populate-dictionary-from-region dict start end)
    (user-error "No dictionary found")))

(defun paced-add-current-thing-to-dict ()
  "Add the current thing at point to the current dictionary.

No check is done to determine if the current thing should be
excluded.

Note that this doesn't add anything to the dictionary's
population commands, so if it is later repopulated using
`paced-dictionary-repopulate' or
`paced-repopulate-named-dictionary', anything added with this
command will be lost."
  (interactive)
  (paced-add-word-to-current-dict (paced-thing-at-point)))

(cl-defmethod paced-dictionary-reset ((dict paced-dictionary))
  "Reset the usage-hash of paced-dictionary DICT."
  (oset dict usage-hash (oref-default dict usage-hash)))

(defun paced-reset-named-dictionary (key)
  "Reset the paced dictionary with key KEY."
  (interactive
   (list (paced-read-dictionary)))
  (paced-ensure-registered key)
  (let ((dict (paced-named-dictionary key)))
    (paced-dictionary-reset dict)))

(cl-defmethod paced-dictionary-sort ((dict paced-dictionary))
  "Sort the words in dictionary DICT by usage."
  (oset dict usage-hash
        (funcall (oref dict sort-method)
                 (oref dict usage-hash))))

(defun paced-sort-named-dictionary (key)
  "Sort the paced dictionary with key KEY."
  (interactive (list (paced-read-dictionary)))
  (paced-ensure-registered key)
  (let ((dict (paced-named-dictionary key)))
    (paced-dictionary-sort dict)))



(define-minor-mode paced-mode
  "Toggle paced mode.

This adds `paced-completion-at-point' to
`completion-at-point-functions'."
  :init-value nil
  :lighter " paced"
  :group 'paced
  (if paced-mode
      (add-hook 'completion-at-point-functions 'paced-completion-at-point 'append 'local)
    (remove-hook 'completion-at-point-functions 'paced-completion-at-point 'local)))

(define-globalized-minor-mode global-paced-mode paced-mode paced-mode
  :group 'paced)


                                        ; ;;;;;;;;;;;;;;;; ;
                                        ; ;; Completion ;; ;
                                        ; ;;;;;;;;;;;;;;;; ;

(cl-defmethod paced-dictionary-fix-completion-case ((dict paced-dictionary) prefix completions)
  "Account for case differences in the prefix by prepending PREFIX to COMPLETIONS.

The specific case differences should mirror those handled by
case-handling in `paced-dictionary-process-word'."
  ;; Anything we changed during population, we want to maintain that part of the
  ;; prefix during completion.
  (if (not (listp completions))
      ;; If completions is not a list, it's likely 't', in which
      ;; case just return the original prefix.
      (list prefix)
    (pcase (oref dict case-handling)
      (`preserve completions)
      ((or `downcase `upcase)
       ;; Changed entire word, so maintain entire prefix
       (let ((prefix-length (length prefix)))
         (mapcar
          (lambda (completion)
            (when (stringp completion)
              (concat prefix (substring-no-properties completion prefix-length))))
          completions)))
      ((or `downcase-first `upcase-first)
       ;; Only changed the first letter, so maintain just one letter of the
       ;; original prefix
       (let ((prefix-length 1))
         (mapcar
          (lambda (completion)
            (when (stringp completion)
              (concat (substring prefix 0 prefix-length)
                      (substring-no-properties completion prefix-length))))
          completions)))
      (`mixed-case
       ;; Only change prefix on single-case completion options
       (let ((prefix-length (length prefix)))
         (mapcar
          (lambda (completion)
            (when (stringp completion)
              (if (paced-mixed-case-word-p completion)
                  completion
                (concat (substring prefix 0 prefix-length)
                        (substring-no-properties completion prefix-length)))))
          completions))))))

(cl-defmethod paced-dictionary-completions ((dict paced-dictionary) prefix action &optional pred)
  "Get the completions for PREFIX in DICT.

ACTION is a completion action, one of 'nil, 't, or 'lambda.  See
Info node `(elisp)Programmed Completion' for an explanation of
each of them.

PRED is a predicate to supply to completion, and will return
non-nil if the completion option should be allowed.

Case handling is handled here; any part of a word that was
modified by `paced-dictionary-process-word' will be replaced with
the prefix before completions are returned."
  (let* ((completion-ignore-case paced-completion-ignore-case)
         (usage-hash (oref dict usage-hash))
         completions)
    (pcase action
      (`nil
       (setq completions (try-completion prefix usage-hash pred)))
      (`t
       (setq completions (all-completions prefix usage-hash pred)))
      (`lambda
        (setq completions (test-completion prefix usage-hash pred))))
    (paced-dictionary-fix-completion-case dict prefix completions)))

(defun paced-completion-table-function (string pred action)
  "Completion table function for paced dictionaries."
  (let* ((completion-ignore-case paced-completion-ignore-case))
    (pcase action
      ((or `nil `t `lambda)
       (when-let* ((dict (paced-current-dictionary)))
         (paced-dictionary-completions dict string action pred)))
      (`(boundaries . _) nil)
      (`metadata
       `(metadata . ((category . paced)
                     (annotation-function . nil)
                     (display-sort-function . identity)
                     (cycle-sort-function . identity)))))))

(defcustom paced-auto-update-p nil
  "Whether to update from completions.

This only works for an existing entry."
  :group 'paced
  :type 'boolean)

(defun paced-completion-auto-update (word status)
  "Automatically update the current dictionary with WORD depending on STATUS.

This should only be called from `paced-completion-at-point'."
  (cl-case status
    (sole
     ;; We're done with completion, but the user may still be typing.
     ;; Therefore, don't add it.
     )
    (exact
     ;; Might not be the entire completion, so don't add it.
     )
    (finished
     (when paced-auto-update-p
       (paced-add-word-to-current-dict word)))))

(defun paced-completion-at-point ()
  "Function for `completion-at-point-functions' to get the paced completions."
  ;; Don't expand unless we're in a buffer with paced-mode enabled.
  (when (and paced-mode)
    (when-let* ((bounds (paced-bounds-of-thing-at-point)))
      (list (car bounds) (cdr bounds) 'paced-completion-table-function
            :exit-function 'paced-completion-auto-update
            :exclusive 'no))))


                                        ; ;;;;;;;;;;;;;;;;;; ;
                                        ; ;; Repopulation ;; ;
                                        ; ;;;;;;;;;;;;;;;;;; ;

(defun paced--insert-file-contents (file)
  "Insert the contents of FILE into the current buffer.

Unlike `insert-file-contents', this handles mode hooks, which
paced requires for repopulation (syntax tables, exclude functions, etc.).

Returns nil if FILE doesn't exist."
  (if (not (file-exists-p file))
      (progn (message "Paced couldn't find file %s" file) nil)
    (insert-file-contents file)
    (let ((buffer-file-name file))
      (after-find-file))
    t))

(defclass paced-population-command ()
  ((props :initarg :props
          :initform nil
          :type list
          :label "Properties"
          :custom (alist :tag "Options" :key-type variable :value-type sexp)
          :documentation "A list of variables to set during population.

Each element is of the form (VAR VALUE).

Some suggested variables for this are `paced-exclude-function'
and `paced-thing-at-point-constituent'.")))

(defun paced-merge-properties (&rest props)
  "Merge the properties in PROPS to a single form understood by `let'.

PROPS is a list of alists, each mapping a variable to a value for
that variable

The maps in the end of PROPS take precedence over the beginning."
  (let ((merged-map (apply 'map-merge 'list props)))
    (map-apply (lambda (var val) (list var val)) merged-map)))

(cl-defmethod paced-dictionary-prepare-properties ((dict paced-dictionary)
                                                   (cmd  paced-population-command))
  "Merge the properties of DICT and CMD into a single form understood by `let'.

Properties in CMD take precedence over those in DICT."
  (paced-merge-properties (oref dict default-population-properties)
                          (oref cmd props)))

(cl-defgeneric paced-population-command-source-list ((_cmd paced-population-command)))

(cl-defgeneric paced-population-command-setup-buffer ((_cmd paced-population-command) _source)
  "Prepare a temporary buffer with SOURCE.

Return non-nil if setup was successful and population can continue.")

(cl-defmethod paced-population-command-populate-dictionary ((dict paced-dictionary) (cmd paced-population-command))
  "Populate DICT from CMD."
  (let ((sources (paced-population-command-source-list cmd))
        ;; Turn props into a form understood by `let'.
        (props (paced-dictionary-prepare-properties dict cmd)))
    (dolist (source sources)
      (with-temp-buffer
        ;; If pre is nil, continue.
        ;; Otherwise, continue if pre returns non-nil
        ;; This allows users to specify conditions under which repopulation
        ;; should be disabled.
        (let ((paced--current-source source))
          (when (paced-population-command-setup-buffer cmd source)
            (eval (macroexp-let* props `(paced-dictionary-populate-from-buffer ,dict)))))))))

(defclass paced-file-population-command (paced-population-command)
  ((file :initarg :file
         :initform ""
         :type string
         :label "File"
         :custom (file :tag "File")
         :documentation "File from which to populate."))
  "Populates a dictionary from all words in a single file.")

(cl-defmethod paced-population-command-source-list ((cmd paced-file-population-command))
  (list (oref cmd file)))

(cl-defmethod paced-population-command-setup-buffer ((_cmd paced-file-population-command) source)
  (paced--insert-file-contents source))

(defclass paced-buffer-population-command (paced-population-command)
  ((buffer :initarg :buffer
           :initform ""
           :type string
           :label "Buffer"
           :custom (string :tag "Buffer")
           :documentation "Name of the buffer from which to populate."))
  "Populates a dictionary from all words in a given buffer.

That buffer must be a string, and must exist during population.")

(cl-defmethod paced-population-command-source-list ((cmd paced-buffer-population-command))
  (list (oref cmd buffer)))

(cl-defmethod paced-population-command-setup-buffer ((_cmd paced-buffer-population-command) source)
  (cond
   ((not (stringp source))
    (message "Paced buffer populator got an invalid argument: %s" source)
    nil)
   ((not (get-buffer source))
    (message "Paced buffer populator got a buffer that doesn't exist: %s" source)
    nil)
   (t
    (set-buffer source))))

(defclass paced-file-function-population-command (paced-population-command)
  ((file :initarg :file
         :initform ""
         :type string
         :label "File"
         :custom (file :tag "File")
         :documentation "File from which to populate.")
   (setup-func :initarg :setup-func
               :initform (lambda () t)
               :type function
               :label "Setup Function"
               :custom (function :tag "Setup Function")
               :documentation "Additional setup function."))
  "Populate from a given file, using a setup function.

That function is called with no arguments, with a temporary
buffer containing the file's contents, and must return non-nil if
population may continue.")

(cl-defmethod paced-population-command-source-list ((cmd paced-file-function-population-command))
  (list (oref cmd file)))

(cl-defmethod paced-population-command-setup-buffer ((cmd paced-file-function-population-command) source)
  (and (paced--insert-file-contents source)
       (funcall (oref cmd setup-func))))

(defclass paced-directory-regexp-population-command (paced-population-command)
  ((directory :initarg :directory
              :initform ""
              :type string
              :label "Directory"
              :custom (directory :tag "Directory")
              :documentation "Directory to search for files from which to populate.")
   (regexp :initarg :regexp
           :initform ".*"
           :type string
           :label "File Regexp"
           :custom (string :tag "File Regexp")
           :documentation "Regular expression to match files.")
   (recursive :initarg :recursive
              :initform t
              :type boolean
              :label "Recursive"
              :custom boolean
              :documentation "Whether to search through the directory recursively."))
  "Population command to populate from files in a directory that
match a regular expression.")

(cl-defmethod paced-population-command-source-list ((cmd paced-directory-regexp-population-command))
  (with-slots (directory regexp recursive) cmd
    (if recursive
        (directory-files-recursively directory regexp)
      (directory-files directory t regexp))))

(cl-defmethod paced-population-command-setup-buffer ((_cmd paced-directory-regexp-population-command) source)
  (paced--insert-file-contents source))

(defclass paced-file-list-population-command (paced-population-command)
  ((generator :initarg :generator
              :initform (lambda () nil)
              :type function
              :label "Generator"
              :custom (function :tag "Generator")
              :documentation "Function of no arguments that returns a list of files."))
  "Populate a dictionary from a list of files.")

(cl-defmethod paced-population-command-source-list ((cmd paced-file-list-population-command))
  (funcall (oref cmd generator)))

(cl-defmethod paced-population-command-setup-buffer ((_cmd paced-file-list-population-command) source)
  (paced--insert-file-contents source))

(defun paced-new-population-command-custom ()
  "Prompt for a population command type and creates a new command of that type."
  (let* ((type (completing-read "Command Type: "
                                (eieio-class-children 'paced-population-command))))
    (funcall (intern type))))

(cl-defmethod paced-dictionary-repopulate ((dict paced-dictionary))
  "Repopulate dictionary DICT from its population commands.

Population commands are stored in the field of the same name.

Note that this will empty the dictionary's contents before
repopulating it."
  ;; Empty the dictionary
  (paced-dictionary-reset dict)
  (dolist (cmd (oref dict population-commands))
    (paced-population-command-populate-dictionary dict cmd))
  (paced-dictionary-sort dict)
  (when paced-repopulate-saves-dictionary
    (paced-dictionary-save dict)))

(defun paced-repopulate-named-dictionary (key)
  "Repopulate dictionary named KEY from its population commands.

Population commands are stored in the field of the same name.

Note that this will empty the dictionary's contents before
repopulating it.  If `paced-populate-warn-about-reset' is
non-nil, confirmation will be requested before continuing."
  (interactive
   (list (paced-read-dictionary)))
  (paced-ensure-registered key)
  (let ((dict (paced-named-dictionary key)))
    (when (or (not paced-populate-warn-about-reset)
              (y-or-n-p "Warning: Repopulating dictionary will reset it.  Continue?"))
      (paced-dictionary-repopulate dict))))

(cl-defmethod paced-dictionary-add-population-command ((dict paced-dictionary)
                                                       (cmd paced-population-command))
  "Add population command CMD to dictionary DICT."
  (cl-pushnew cmd (oref dict population-commands) :test 'equal))

(defun paced-add-buffer-file-to-dictionary (&optional buffer)
  "Populate the dictionary of BUFFER with BUFFER.

The file corresponding to BUFFER is then added to the current
dictionary's population commands.

Custom settings for the populator, such as the exclude function,
must be set with `paced-edit-named-dictionary' or
`paced-edit-current-dictionary'."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (unless (buffer-file-name)
      (user-error "paced-add-buffer-file-to-dictionary called inside a non-file buffer."))
    (if-let* ((dict      (paced-current-dictionary))
              (file-name (buffer-file-name))
              (cmd (paced-file-population-command :file file-name)))
        (progn
          (paced-dictionary-populate-from-buffer dict buffer)
          (paced-dictionary-add-population-command dict cmd))
      (user-error "No dictionary found for current buffer"))))



(cl-defmethod paced-dictionary-edit ((dict paced-dictionary))
  "Edit paced-dictionary DICT."
  (customize-object dict))

(defun paced-edit-named-dictionary (name)
  "Edit the paced-dictionary named NAME."
  (interactive (list (paced-read-dictionary)))
  (if-let* ((dict (paced-named-dictionary name)))
      (paced-dictionary-edit dict)
    (error "No paced dictionary called '%s' has been registered" name)))

(defun paced-edit-current-dictionary ()
  "Edit the current paced dictionary."
  (interactive)
  (if-let* ((dict (paced-current-dictionary)))
      (paced-dictionary-edit dict)
    (user-error "No dictionary found for current buffer")))



(declare-function lm-report-bug "lisp-mnt" (topic))

(defun paced-submit-bug-report (topic)
  "Report a bug with topic TOPIC."
  (interactive "sTopic: ")
  (require 'lisp-mnt)
  (let* ((src-file (locate-library "paced.el" t))
         (src-buf-live (find-buffer-visiting src-file))
         (src-buf (find-file-noselect src-file)))
    (with-current-buffer src-buf
      (lm-report-bug topic))
    ;; Kill the buffer if it wasn't live
    (unless src-buf-live
      (kill-buffer src-buf))))

(provide 'paced)

;;; paced.el ends here
