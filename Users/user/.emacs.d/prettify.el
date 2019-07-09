(defconst clojure--prettify-symbols-alist
  '(("fn" . (?λ (Br . Bl) ?.))
    ("*" . "×")
    ("/" . "÷")
    ("->" . (?- (Br . Bc) ?- (Br . Bc) ?>))
    ("->>" .  (?\s (Br . Bl) ?\s (Br . Bl) ?\s
                   (Bl . Bl) ?- (Bc . Br) ?- (Bc . Bc) ?>
                   (Bc . Bl) ?- (Br . Br) ?>))))

(defconst lisp--prettify-symbols-alist
  '(("*" . "×")
    ("/" . "÷")))

(add-hook 'clojure-mode-hook 'prettify-symbols-mode)
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
