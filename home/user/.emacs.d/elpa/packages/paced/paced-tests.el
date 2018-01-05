;;; paced-tests.el --- Tests for paced -*- lexical-binding: t; -*-

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

;;; Code:

(require 'paced)
(require 'ert)

(defconst paced-test-dir
  (expand-file-name "test-files" (file-name-directory (or load-file-name buffer-file-name))))

(defsubst paced-test-file (base-name)
  (expand-file-name base-name paced-test-dir))

(defconst paced-first-test-file  (paced-test-file "first.txt"))
(defconst paced-second-test-file (paced-test-file "second.cpp"))
(defconst paced-third-test-file  (paced-test-file "third.org"))

(defconst paced-test-dict-save-file (paced-test-file "paced-dictionary-case-sensitive"))
(defconst paced-test-default-registered-map (make-hash-table :test 'equal))

(ert-deftest paced-handle-word-case ()
  (let* ((word "EiEiO"))
    (should (string-equal (paced--handle-word-case 'preserve word) "EiEiO"))
    (should (string-equal (paced--handle-word-case 'downcase word) "eieio"))
    (should (string-equal (paced--handle-word-case 'upcase   word) "EIEIO"))
    (should (string-equal (paced--handle-word-case 'downcase-first word) "eiEiO"))
    (should (string-equal (paced--handle-word-case 'upcase-first word) "EiEiO"))
    (should (string-equal (paced--handle-word-case 'mixed-case word) "EiEiO"))))

(ert-deftest paced-mixed-case-word ()
  (should-not (paced-mixed-case-word-p "HAS"))
  (should     (paced-mixed-case-word-p "HAs"))
  (should     (paced-mixed-case-word-p "HaS"))
  (should-not (paced-mixed-case-word-p "Has"))
  (should     (paced-mixed-case-word-p "hAS"))
  (should     (paced-mixed-case-word-p "hAs"))
  (should     (paced-mixed-case-word-p "haS"))
  (should-not (paced-mixed-case-word-p "has")))

(ert-deftest paced-create-dictionary ()
  ;; Delete the old save file
  (when (file-exists-p paced-test-dict-save-file)
    (delete-file paced-test-dict-save-file))
  (let* ((paced--registered-dictionaries paced-test-default-registered-map)
         (new-dict (paced-make-dictionary "test-dict-case"
                                          paced-test-dict-save-file
                                          'downcase)))
    (should (= (map-length paced--registered-dictionaries) 1))
    (should (paced-dictionary-p new-dict))
    (oset new-dict updated t) ;; Mark it as updated so it saves
    (paced-dictionary-save new-dict)
    (should (file-exists-p paced-test-dict-save-file)))
  (let* ((paced--registered-dictionaries paced-test-default-registered-map))
    ;; Now verify that we can load it again
    (paced-load-dictionary-from-file paced-test-dict-save-file)
    (should (= (map-length paced--registered-dictionaries) 1))
    (should (map-contains-key paced--registered-dictionaries "test-dict-case"))
    (should (paced-dictionary-p (paced-named-dictionary "test-dict-case")))))

(defvar paced-test-enable-symbol nil)

(ert-deftest paced-enable-list-symbol ()
  "Test case for `paced-dictionary-enable-alist' being an arbitrary symbol."
  (let* ((paced--registered-dictionaries paced-test-default-registered-map)
         (paced-global-dict-enable-alist '((paced-test-enable-symbol . "test-dict-case")))
         (new-buffer (find-file-noselect paced-first-test-file))
         (new-dict (paced-make-dictionary "test-dict-case"
                                          paced-test-dict-save-file
                                          'downcase)))
    (with-current-buffer new-buffer
      (setq paced-test-enable-symbol nil)
      (should-not (paced-current-dictionary))
      (setq-local paced-test-enable-symbol t)
      (should (paced-dictionary-p (paced-current-dictionary)))
      (should (string-equal (paced-dictionary-name (paced-current-dictionary)) "test-dict-case")))
    (kill-buffer new-buffer)))

(ert-deftest paced-enable-list-mode ()
  "Test case for `paced-dictionary-enable-alist' being a mode symbol."
  (let* ((paced--registered-dictionaries paced-test-default-registered-map)
         (paced-global-dict-enable-alist '((text-mode . "test-dict-case")))
         (buffer-one (find-file-noselect paced-first-test-file))
         (buffer-two (find-file-noselect paced-second-test-file))
         (new-dict (paced-make-dictionary "test-dict-case"
                                          paced-test-dict-save-file
                                          'downcase)))
    (with-current-buffer buffer-two
      (should-not (paced-current-dictionary)))
    (kill-buffer buffer-two)
    (with-current-buffer buffer-one
      (should (paced-dictionary-p (paced-current-dictionary)))
      (should (string-equal (paced-dictionary-name (paced-current-dictionary)) "test-dict-case")))
    (kill-buffer buffer-one)))

(defun paced-test-function-symbol ()
  paced-test-enable-symbol)

(ert-deftest paced-enable-list-function-symbol ()
  "Test case for `paced-dictionary-enable-alist' being a function symbol."
  (let* ((paced--registered-dictionaries paced-test-default-registered-map)
         (paced-global-dict-enable-alist '((paced-test-function-symbol . "test-dict-case")))
         (buffer-one (find-file-noselect paced-first-test-file))
         (new-dict (paced-make-dictionary "test-dict-case"
                                          paced-test-dict-save-file
                                          'downcase)))
    (with-current-buffer buffer-one
      (setq-local paced-test-enable-symbol nil)
      (should-not (paced-current-dictionary))
      (setq-local paced-test-enable-symbol t)
      (should (paced-dictionary-p (paced-current-dictionary)))
      (should (string-equal (paced-dictionary-name (paced-current-dictionary)) "test-dict-case")))
    (kill-buffer buffer-one)))

(ert-deftest paced-enable-list-lambda-function ()
  "Test case for `paced-dictionary-enable-alist' being a lambda form."
  (let* ((paced--registered-dictionaries paced-test-default-registered-map)
         (paced-global-dict-enable-alist '(((lambda nil paced-test-enable-symbol) . "test-dict-case")))
         (buffer-one (find-file-noselect paced-first-test-file))
         (new-dict (paced-make-dictionary "test-dict-case"
                                          paced-test-dict-save-file
                                          'downcase)))
    (with-current-buffer buffer-one
      (setq-local paced-test-enable-symbol nil)
      (should-not (paced-current-dictionary))
      (setq-local paced-test-enable-symbol t)
      (should (paced-dictionary-p (paced-current-dictionary)))
      (should (string-equal (paced-dictionary-name (paced-current-dictionary)) "test-dict-case")))
    (kill-buffer buffer-one)))

(ert-deftest paced-enable-list-and-form ()
  "Test case for `paced-dictionary-enable-alist' being an 'and' form."
  (let* ((paced--registered-dictionaries paced-test-default-registered-map)
         (paced-global-dict-enable-alist '(((and text-mode paced-test-enable-symbol) . "test-dict-case")))
         (buffer-one (find-file-noselect paced-first-test-file))
         (buffer-two (find-file-noselect paced-second-test-file))
         (new-dict (paced-make-dictionary "test-dict-case"
                                          paced-test-dict-save-file
                                          'downcase)))
    (with-current-buffer buffer-two
      (setq-local paced-test-enable-symbol nil)
      (should-not (paced-current-dictionary))
      (setq-local paced-test-enable-symbol t)
      (should-not (paced-current-dictionary)))
    (kill-buffer buffer-two)
    (with-current-buffer buffer-one
      (setq-local paced-test-enable-symbol nil)
      (should-not (paced-current-dictionary))
      (setq-local paced-test-enable-symbol t)
      (should (paced-dictionary-p (paced-current-dictionary)))
      (should (string-equal (paced-dictionary-name (paced-current-dictionary)) "test-dict-case")))
    (kill-buffer buffer-one)))

(ert-deftest paced-enable-list-or-form ()
  "Test case for `paced-dictionary-enable-alist' being an 'or' form."
  (let* ((paced--registered-dictionaries paced-test-default-registered-map)
         (paced-global-dict-enable-alist '(((or text-mode paced-test-enable-symbol) . "test-dict-case")))
         (buffer-one (find-file-noselect paced-first-test-file))
         (buffer-two (find-file-noselect paced-second-test-file))
         (new-dict (paced-make-dictionary "test-dict-case"
                                          paced-test-dict-save-file
                                          'downcase)))
    (with-current-buffer buffer-two
      (setq-local paced-test-enable-symbol nil)
      (should-not (paced-current-dictionary))
      (setq-local paced-test-enable-symbol t)
      (should (paced-dictionary-p (paced-current-dictionary)))
      (should (string-equal (paced-dictionary-name (paced-current-dictionary)) "test-dict-case")))
    (kill-buffer buffer-two)
    (with-current-buffer buffer-one
      (setq-local paced-test-enable-symbol nil)
      (should (paced-dictionary-p (paced-current-dictionary)))
      (should (string-equal (paced-dictionary-name (paced-current-dictionary)) "test-dict-case"))
      (setq-local paced-test-enable-symbol t)
      (should (paced-dictionary-p (paced-current-dictionary)))
      (should (string-equal (paced-dictionary-name (paced-current-dictionary)) "test-dict-case")))
    (kill-buffer buffer-one)))

(ert-deftest paced-populate-file ()
  "Test case for single file populator."
  (let* ((paced--registered-dictionaries paced-test-default-registered-map)
         (paced-global-dict-enable-alist '((text-mode . "test-dict-case")))
         (cmd (paced-file-population-command :file paced-first-test-file))
         (test-dict (paced-make-dictionary "test-dict-case"
                                           paced-test-dict-save-file
                                           'downcase)))
    (should (paced-dictionary-p test-dict))
    (oset test-dict population-commands (list cmd))
    (paced-dictionary-repopulate test-dict)
    (let ((usage-hash (oref test-dict usage-hash)))
      (should (eq (map-length usage-hash) 4))
      (should (seq-set-equal-p (map-keys usage-hash) '("one" "two" "three" "four")))
      (should (eq (map-elt usage-hash "one") 1))
      (should (eq (map-elt usage-hash "two") 2))
      (should (eq (map-elt usage-hash "three") 3))
      (should (eq (map-elt usage-hash "four") 4)))))

(ert-deftest paced-populate-buffer ()
  "Test case for single buffer populator."
  (let* ((paced--registered-dictionaries paced-test-default-registered-map)
         (paced-global-dict-enable-alist '((text-mode . "test-dict-case")))
         (buffer "first.txt")
         (buffer-one (find-file-noselect paced-first-test-file))
         (cmd (paced-buffer-population-command :buffer buffer))
         (test-dict (paced-make-dictionary "test-dict-case"
                                           paced-test-dict-save-file
                                           'downcase)))
    (should (paced-dictionary-p test-dict))
    (oset test-dict population-commands (list cmd))
    (paced-dictionary-repopulate test-dict)
    (let ((usage-hash (oref test-dict usage-hash)))
      (should (eq (map-length usage-hash) 4))
      (should (seq-set-equal-p (map-keys usage-hash) '("one" "two" "three" "four")))
      (should (eq (map-elt usage-hash "one") 1))
      (should (eq (map-elt usage-hash "two") 2))
      (should (eq (map-elt usage-hash "three") 3))
      (should (eq (map-elt usage-hash "four") 4)))
    (kill-buffer buffer-one)))

(ert-deftest paced-populate-file-function ()
  "Test case for file-function populator."
  (let* ((paced--registered-dictionaries paced-test-default-registered-map)
         (paced-global-dict-enable-alist '((text-mode . "test-dict-case")))
         (pre-func (lambda () (insert (buffer-string)) t))
         (cmd (paced-file-function-population-command :file paced-first-test-file
                                                      :setup-func pre-func))
         (test-dict (paced-make-dictionary "test-dict-case"
                                           paced-test-dict-save-file
                                           'downcase)))
    (should (paced-dictionary-p test-dict))
    (oset test-dict population-commands (list cmd))
    (paced-dictionary-repopulate test-dict)
    (let ((usage-hash (oref test-dict usage-hash)))
      (should (eq (map-length usage-hash) 4))
      (should (seq-set-equal-p (map-keys usage-hash) '("one" "two" "three" "four")))
      (should (eq (map-elt usage-hash "one") 2))
      (should (eq (map-elt usage-hash "two") 4))
      (should (eq (map-elt usage-hash "three") 6))
      (should (eq (map-elt usage-hash "four") 8)))))

(ert-deftest paced-populate-directory-regexp ()
  "Test case for directory-regexp populator."
  (let* ((paced--registered-dictionaries paced-test-default-registered-map)
         (paced-global-dict-enable-alist '((text-mode . "test-dict-case")))
         (cmd (paced-directory-regexp-population-command :directory paced-test-dir
                                                         :regexp ".*\\.txt"
                                                         :recursive t))
         (test-dict (paced-make-dictionary "test-dict-case"
                                           paced-test-dict-save-file
                                           'downcase)))
    (should (paced-dictionary-p test-dict))
    (oset test-dict population-commands (list cmd))
    (paced-dictionary-repopulate test-dict)
    (let ((usage-hash (oref test-dict usage-hash)))
      (should (eq (map-length usage-hash) 4))
      (should (seq-set-equal-p (map-keys usage-hash) '("one" "two" "three" "four")))
      (should (eq (map-elt usage-hash "one") 1))
      (should (eq (map-elt usage-hash "two") 2))
      (should (eq (map-elt usage-hash "three") 3))
      (should (eq (map-elt usage-hash "four") 4)))))

(ert-deftest paced-populate-file-list ()
  "Test case for file-list populator."
  (let* ((paced--registered-dictionaries paced-test-default-registered-map)
         (paced-global-dict-enable-alist '((text-mode . "test-dict-case")))
         (file-list (lambda () `(,paced-first-test-file)))
         (cmd (paced-file-list-population-command :generator file-list))
         (test-dict (paced-make-dictionary "test-dict-case"
                                           paced-test-dict-save-file
                                           'downcase)))
    (should (paced-dictionary-p test-dict))
    (oset test-dict population-commands (list cmd))
    (paced-dictionary-repopulate test-dict)
    (let ((usage-hash (oref test-dict usage-hash)))
      (should (eq (map-length usage-hash) 4))
      (should (seq-set-equal-p (map-keys usage-hash) '("one" "two" "three" "four")))
      (should (eq (map-elt usage-hash "one") 1))
      (should (eq (map-elt usage-hash "two") 2))
      (should (eq (map-elt usage-hash "three") 3))
      (should (eq (map-elt usage-hash "four") 4)))))

(ert-deftest paced-multiple-population-commands ()
  (let* ((paced--registered-dictionaries paced-test-default-registered-map)
         (paced-global-dict-enable-alist '((text-mode . "test-dict-case")))
         (cmd1 (paced-file-population-command :file paced-first-test-file))
         (cmd2 (paced-file-population-command :file paced-third-test-file))
         (test-dict (paced-make-dictionary "test-dict-case"
                                           paced-test-dict-save-file
                                           'downcase)))
    (should (paced-dictionary-p test-dict))
    (oset test-dict population-commands (list cmd1 cmd2))
    (paced-dictionary-repopulate test-dict)
    (let ((usage-hash (oref test-dict usage-hash)))
      (should (eq (map-length usage-hash) 7))
      (should (seq-set-equal-p (map-keys usage-hash) '("one" "two" "three" "four" "five" "six" "seven")))
      (should (eq (map-elt usage-hash "one") 1))
      (should (eq (map-elt usage-hash "two") 2))
      (should (eq (map-elt usage-hash "three") 3))
      (should (eq (map-elt usage-hash "four") 4))
      (should (eq (map-elt usage-hash "five") 5))
      (should (eq (map-elt usage-hash "six") 3))
      (should (eq (map-elt usage-hash "seven") 1)))))

(ert-deftest paced-populator-settings ()
  (let* ((paced--registered-dictionaries paced-test-default-registered-map)
         (paced-global-dict-enable-alist '((text-mode . "test-dict-case")))
         (exclude-command (lambda nil (nth 8 (syntax-ppss)))) ;; exclude comments
         (cmd1 (paced-file-population-command :file paced-first-test-file))
         (cmd2 (paced-file-population-command :file paced-second-test-file
                                              :props `((paced-exclude-function quote ,exclude-command))))
         (test-dict (paced-make-dictionary "test-dict-case"
                                           paced-test-dict-save-file
                                           'downcase)))
    (should (paced-dictionary-p test-dict))
    (oset test-dict population-commands (list cmd1 cmd2))
    (paced-dictionary-repopulate test-dict)
    (let ((usage-hash (oref test-dict usage-hash)))
      (should (eq (map-length usage-hash) 4))
      (should (seq-set-equal-p (map-keys usage-hash) '("one" "two" "three" "four")))
      (should (eq (map-elt usage-hash "one") 1))
      (should (eq (map-elt usage-hash "two") 2))
      (should (eq (map-elt usage-hash "three") 3))
      (should (eq (map-elt usage-hash "four") 4)))))

(ert-deftest paced-populate-sort-order ()
  "Test case for sorting after population."
  (let* ((paced--registered-dictionaries paced-test-default-registered-map)
         (paced-global-dict-enable-alist '((text-mode . "test-dict-case")))
         (cmd (paced-file-population-command :file paced-first-test-file))
         (test-dict (paced-make-dictionary "test-dict-case"
                                           paced-test-dict-save-file
                                           'downcase)))
    (should (paced-dictionary-p test-dict))
    (oset test-dict population-commands (list cmd))
    (paced-dictionary-repopulate test-dict)
    (let ((usage-hash (oref test-dict usage-hash)))
      (should (eq (map-length usage-hash) 4))
      (should (equal (map-keys usage-hash) '("four" "three" "two" "one")))
      (should (eq (map-elt usage-hash "one") 1))
      (should (eq (map-elt usage-hash "two") 2))
      (should (eq (map-elt usage-hash "three") 3))
      (should (eq (map-elt usage-hash "four") 4)))))

(ert-deftest paced-populate-non-existent-file ()
  (let* ((paced--registered-dictionaries paced-test-default-registered-map)
         (file "first.txt")
         (cmd (paced-file-population-command :file file))
         (test-dict (paced-make-dictionary "test-dict-case"
                                           paced-test-dict-save-file
                                           'downcase)))
    (should (paced-dictionary-p test-dict))
    (oset test-dict population-commands (list cmd))
    (paced-dictionary-repopulate test-dict)
    (let ((usage-hash (oref test-dict usage-hash)))
      (should (map-empty-p usage-hash)))))

(ert-deftest paced-populate-non-existent-buffer ()
  (let* ((paced--registered-dictionaries paced-test-default-registered-map)
         (buffer "first.txt")
         (cmd (paced-buffer-population-command :buffer buffer))
         (test-dict (paced-make-dictionary "test-dict-case"
                                           paced-test-dict-save-file
                                           'downcase)))
    (should (paced-dictionary-p test-dict))
    (oset test-dict population-commands (list cmd))
    (paced-dictionary-repopulate test-dict)
    (let ((usage-hash (oref test-dict usage-hash)))
      (should (map-empty-p usage-hash)))))

(ert-deftest paced-completions-try-completion ()
  (let* ((paced--registered-dictionaries paced-test-default-registered-map)
         (paced-global-dict-enable-alist '((text-mode . "test-dict-case")))
         (cmd (paced-file-population-command :file paced-first-test-file))
         (test-dict (paced-make-dictionary "test-dict-case"
                                           paced-test-dict-save-file
                                           'downcase)))
    (should (paced-dictionary-p test-dict))
    (oset test-dict population-commands (list cmd))
    (paced-dictionary-repopulate test-dict)
    (let ((completions (paced-dictionary-completions test-dict "o" nil)))
      (should (equal completions '("o"))))))

(ert-deftest paced-completions-try-completion-mixed-case ()
  (let* ((paced--registered-dictionaries paced-test-default-registered-map)
         (paced-global-dict-enable-alist '((text-mode . "test-dict-case")))
         (cmd (paced-file-population-command :file paced-first-test-file))
         (test-dict (paced-make-dictionary "test-dict-case"
                                           paced-test-dict-save-file
                                           'mixed-case)))
    (should (paced-dictionary-p test-dict))
    (oset test-dict population-commands (list cmd))
    (paced-dictionary-repopulate test-dict)
    (let ((completions (paced-dictionary-completions test-dict "o" nil)))
      (should (equal completions '("o"))))))

(ert-deftest paced-completions-all-completions ()
  (let* ((paced--registered-dictionaries paced-test-default-registered-map)
         (paced-global-dict-enable-alist '((text-mode . "test-dict-case")))
         (cmd (paced-file-population-command :file paced-first-test-file))
         (test-dict (paced-make-dictionary "test-dict-case"
                                           paced-test-dict-save-file
                                           'downcase)))
    (should (paced-dictionary-p test-dict))
    (oset test-dict population-commands (list cmd))
    (paced-dictionary-repopulate test-dict)
    (should (equal (paced-dictionary-completions test-dict "o" t)
                   '("one")))
    (should (equal (paced-dictionary-completions test-dict "on" t)
                   '("one")))
    (should (equal (paced-dictionary-completions test-dict "t" t)
                   '("three" "two")))
    (should (equal (paced-dictionary-completions test-dict "v" t)
                   nil))))

(ert-deftest paced-completions-test-completion ()
  (let* ((paced--registered-dictionaries paced-test-default-registered-map)
         (paced-global-dict-enable-alist '((text-mode . "test-dict-case")))
         (cmd (paced-file-population-command :file paced-first-test-file))
         (test-dict (paced-make-dictionary "test-dict-case"
                                           paced-test-dict-save-file
                                           'downcase)))
    (should (paced-dictionary-p test-dict))
    (oset test-dict population-commands (list cmd))
    (paced-dictionary-repopulate test-dict)
    (should (paced-dictionary-completions test-dict "one" 'lambda))
    (should-not (paced-dictionary-completions test-dict "o" 'lambda))))

(ert-deftest paced-merge-properties ()
  (let* ((prop-list-1 '((a . "abc") (b . "xyz")))
         (prop-list-2 '((a . "def") (c . "ghi")))
         (prop-list-3 nil)
         (merged-1   (paced-merge-properties prop-list-1 prop-list-2))
         (merged-2   (paced-merge-properties prop-list-3 prop-list-2)))
    (should (equal (map-elt merged-1 'a) '("def")))
    (should (equal (map-elt merged-1 'b) '("xyz")))
    (should (equal (map-elt merged-1 'c) '("ghi")))
    (should (equal (map-elt merged-2 'a) '("def")))
    (should (equal (map-elt merged-2 'c) '("ghi")))))

(provide 'paced-tests)

;;; paced-tests.el ends here
