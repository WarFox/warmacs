;; +lang/javascript/init.el -*- lexical-binding: t; -*-

(use-package javascript-mode
  :ensure nil
  :hook
  ((javascript-mode javascript-ts-mode) . lsp-deferred))

(use-package add-node-modules-path)

(use-package npm-mode)

(use-package import-js)

(use-package js-doc)

(use-package js2-mode)

(use-package js2-refactor)

(use-package nodejs-repl)

(use-package prettier-js)

(provide '+lang/javascript/init)
