(global-prettify-symbols-mode 1)

(defconst my-lisp-prettify-alist
  '(("fn" . (?λ (Br . Bl) ?.))
    ("*" . "×")
    ("/" . "÷")
    ("->" . (?- (Br . Bc) ?- (Br . Bc) ?>))
    ("->>" .  (?\s (Br . Bl) ?\s (Br . Bl) ?\s
                   (Bl . Bl) ?- (Bc . Br) ?- (Bc . Bc) ?>
                   (Bc . Bl) ?- (Br . Br) ?>))))

(eval-after-load 'clojure-mode
  '(setq clojure--prettify-symbols-alist
         my-lisp-prettify-alist))

(eval-after-load 'lisp-mode
  '(setq lisp--prettify-symbols-alist
         my-lisp-prettify-alist))
