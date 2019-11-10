(require 'eglot)

(setq eglot-server-programs '((dart-mode "dart" "/opt/flutter/bin/cache/dart-sdk/bin/snapshots/analysis_server.dart.snapshot" "--lsp")))
(add-hook 'dart-mode-hook 'eglot-ensure)
