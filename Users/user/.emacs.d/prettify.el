(global-prettify-symbols-mode 1)

(defvar my-lisp-prettify-alist
  '(("->" . (?- (Br . Bc) ?- (Br . Bc) ?>))
    ("->>" .  (?\s (Br . Bl) ?\s (Br . Bl) ?\s
                             (Bl . Bl) ?- (Bc . Br) ?- (Bc . Bc) ?>
                             (Bc . Bl) ?- (Br . Br) ?>))))

(eval-after-load 'clojure-mode
  '(setq clojure--prettify-symbols-alist
         (append my-lisp-prettify-alist
                 clojure--prettify-symbols-alist)))

(eval-after-load 'lisp-mode
  '(setq lisp--prettify-symbols-alist
         (append my-lisp-prettify-alist
                 lisp--prettify-symbols-alist)))
