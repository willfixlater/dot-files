(require 'eglot)

(setq dart-format-on-save t)

(defun project-try-dart (dir)
  (let ((project (or (locate-dominating-file dir "pubspec.yaml")
                     (locate-dominating-file dir "BUILD"))))
    (if project
        (cons 'dart project)
      (cons 'transient dir))))
(add-hook 'project-find-functions #'project-try-dart)
(cl-defmethod project-roots ((project (head dart)))
  (list (cdr project)))

(add-to-list 'eglot-server-programs '(dart-mode "dart" "/opt/flutter/bin/cache/dart-sdk/bin/snapshots/analysis_server.dart.snapshot" "--lsp")) 
(add-hook 'dart-mode-hook 'eglot-ensure)
