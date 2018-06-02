;;; shell-quasiquote.el --- Turn s-expressions into shell command strings.

;; Copyright (C) 2015  Free Software Foundation, Inc.

;; Author: Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;; Keywords: extensions, unix
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

;; "Shell quasiquote" -- turn s-expressions into POSIX shell command strings.
;;
;; Shells other than POSIX sh are not supported.
;;
;; Quoting is automatic and safe against injection.
;;
;;   (let ((file1 "file one")
;;         (file2 "file two"))
;;     (shqq (cp -r ,file1 ,file2 "My Files")))
;;       => "cp -r 'file one' 'file two' 'My Files'"
;;
;; You can splice many arguments into place with ,@foo.
;;
;;   (let ((files (list "file one" "file two")))
;;     (shqq (cp -r ,@files "My Files")))
;;       => "cp -r 'file one' 'file two' 'My Files'"
;;
;; Note that the quoting disables a variety of shell expansions like ~/foo,
;; $ENV_VAR, and e.g. {x..y} in GNU Bash.
;;
;; You can use ,,foo to escape the quoting.
;;
;;   (let ((files "file1 file2"))
;;     (shqq (cp -r ,,files "My Files")))
;;       => "cp -r file1 file2 'My Files'"
;;
;; And ,,@foo to splice and escape quoting.
;;
;;   (let* ((arglist '("-x 'foo bar' -y baz"))
;;          (arglist (append arglist '("-z 'qux fux'"))))
;;     (shqq (command ,,@arglist)))
;;       => "command -x 'foo bar' -y baz -z 'qux fux'"
;;
;; Neat, eh?


;;; Code:

(defun shqq--atom-to-string (atom)
  (cond
   ((symbolp atom) (symbol-name atom))
   ((stringp atom) atom)
   ((numberp atom) (number-to-string atom))
   (t (error "Bad shqq atom: %S" atom))))

(defun shqq--quote-atom (atom)
  (shell-quote-argument (shqq--atom-to-string atom)))

(defmacro shqq (parts)
  "First, PARTS is turned into a list of strings.  For this,
every element of PARTS must be one of:

- a symbol, evaluating to its name,

- a string, evaluating to itself,

- a number, evaluating to its decimal representation,

- \",expr\", where EXPR must evaluate to an atom that will be
  interpreted according to the previous rules,

- \",@list-expr\", where LIST-EXPR must evaluate to a list whose
  elements will each be interpreted like the EXPR in an \",EXPR\"
  form, and spliced into the list of strings,

- \",,expr\", where EXPR is interpreted like in \",expr\",

- or \",,@expr\", where EXPR is interpreted like in \",@expr\".

In the resulting list of strings, all elements except the ones
resulting from \",,expr\" and \",,@expr\" forms are quoted for
shell grammar.

Finally, the resulting list of strings is concatenated with
separating spaces."
  (let ((parts
         (mapcar
          (lambda (part)
            (cond
             ((atom part) (shqq--quote-atom part))
             ;; We use the match-comma helpers because pcase can't match ,foo.
             (t (pcase part
                  ;; ,,foo i.e. (, (, foo))
                  (`(,`\, (,`\, ,form))  form)
                  ;; ,,@foo i.e. (, (,@ foo))
                  (`(,`\, (,`\,@ ,form)) `(mapconcat #'identity ,form " "))
                  ;; ,foo
                  (`(,`\, ,form) `(shqq--quote-atom ,form))
                  ;; ,@foo
                  (`,@,form `(mapconcat #'shqq--quote-atom ,form " "))
                  (_
                   (error "Bad shqq part: %S" part))))))
          parts)))
    `(mapconcat #'identity (list ,@parts) " ")))

(provide 'shell-quasiquote)
;; Local Variables:
;; coding: utf-8
;; End:
;;; shell-quasiquote.el ends here
