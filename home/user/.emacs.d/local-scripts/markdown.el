(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(autoload 'gfm-mode "markdown-mode" "Major mode for editing Github Flavored Markdown files" t)

(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(setq markdown-command "pandoc -c file:///C:/Users/shayd/.emacs.d/styles/github-markdown.css -f gfm -t html5 --metadata=title:\"Markdown Preview\" --highlight-style pygments --standalone")