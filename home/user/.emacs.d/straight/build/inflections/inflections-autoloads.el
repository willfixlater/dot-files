;;; inflections-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "inflections" "inflections.el" (0 0 0 0))
;;; Generated autoloads from inflections.el

(autoload 'inflection-singularize-string "inflections" "\
Return the singularized version of STR.

\(fn STR)" nil nil)

(define-obsolete-function-alias 'singularize-string 'inflection-singularize-string)

(autoload 'inflection-pluralize-string "inflections" "\
Return the pluralized version of STR.

\(fn STR)" nil nil)

(define-obsolete-function-alias 'pluralize-string 'inflection-pluralize-string)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "inflections" '("inflection-" "define-inflectors")))

;;;***

(provide 'inflections-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; inflections-autoloads.el ends here
