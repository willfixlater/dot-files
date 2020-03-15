(require 'aligned-mode-line)

;; Set the mode line
(let* ((left '(list "%b   " mode-name))
       (center '(list (buffer-state-*)
                      (if window-system "λ" "|")
                      (buffer-state-+)))
       (right "[%l,%c] %p%%"))
  (aligned-mode-line left center right))

;; Set face attributes
(defun set-my-face-attributes (_)
  "Set my custom face attributes."
  (set-face-attribute 'mode-line nil :font "Fira Code-11:bold:antialias=true")
  (set-face-attribute 'mode-line-inactive nil :font "Fira Code-11:bold:antialias=true")
  (set-face-attribute 'header-line nil :background "#1d1f21" :foreground "#373b41" :box nil)
  (set-face-attribute 'fringe nil :background "#1d1f21"))

(set-my-face-attributes nil)

;; Set default frame values
(setq default-frame-alist
      '((font . "Fira Code-11:bold:antialias=true")
        (cursor-type . hbar)
        (cursor-color . "#ffffff")
        (vertical-scroll-bars . nil)
        (left-fringe . 20)
        (right-fringe . 20)))

(add-hook 'after-make-frame-functions #'set-my-face-attributes)

(fringe-mode '(20 . 20))

;; Header line gives vertical space between buffer and title bar, here
;; the vertical bar prevents a gap in window borders.
(when window-system
  (setq header-line-format "▏"))
