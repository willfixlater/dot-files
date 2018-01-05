;;; paced-async.el --- Support for asynchronous population -*- lexical-binding: t; -*-

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

;; Population can be a slow task.  The more files from which you want to
;; populate a dictionary, the longer repopulation is going to take.

;; This extension to paced takes advantage of John Wiegley's async package to
;; repopulate a dictionary asynchronously.

;;; Code:

(require 'paced)
(require 'async)

(defcustom paced-async-load-file (locate-user-emacs-file "paced-async.el")
  "File to load with user-specific population settings.

This may contain commands required for running each dictionary's
population commmands, or load additional files."
  :group 'paced
  :type 'file)

(cl-defmethod paced-repopulate-dictionary-async ((dictionary paced-dictionary))
  "Repopulate DICTIONARY asynchronously.

Note that DICTIONARY isn't modified directly by this process, but
the updated dictionary is loaded when repopulation completes.
Therefore, you can continue using it without issue during
repopulation.

Side note: All dictionaries are reloaded when this function
finishes, so temporary changes to any dictionaries will be lost
as a result."
  (let* ((variables '("load-path"        ;; Find libraries, including paced
                      ;; Settings
                      "paced-thing-at-point-constituent"
                      "paced-dictionary-directory"
                      ;; The load file itself
                      "paced-async-load-file"))
         (inject-string (concat "^\\("
                                (mapconcat 'identity variables "\\|")
                                "\\)$")))
    (message "Repopulating dictionary %s" (paced-dictionary-name dictionary))
    (async-start
     `(lambda ()
        ;; Inject the environment to get `load-path' and user settings
        ,(async-inject-variables inject-string)
        ;; Require paced and load the async settings file
        (require 'paced)
        (load-file paced-async-load-file)
        ;; Repopulate and save the dictionary
        (paced-dictionary-repopulate ,dictionary)
        (paced-dictionary-save ,dictionary))
     (lambda (_result)
       (message "Finished repopulating dictionary")
       (paced-load-all-dictionaries)))))

;;;###autoload
(defun paced-repopulate-named-dictionary-async (key)
  "Repopulate dictionary named KEY from its population commands, asynchronously.

Population commands are stored in the field of the same name.

Note that this will empty the dictionary's contents."
  (interactive
   (list (paced-read-dictionary)))
  (paced-ensure-registered key)
  (let ((dict (paced-named-dictionary key)))
    (paced-repopulate-dictionary-async dict)))

(provide 'paced-async)

;;; paced-async.el ends here
