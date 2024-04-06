;; +lang/typescript/init.el -*- lexical-binding: t; -*-

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook
  ((typescript-mode typescript-ts-mode) . lsp-deferred))

(provide '+lang/typescript/init)
