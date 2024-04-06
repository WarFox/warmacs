;; +lang/javascript/init.el -*- lexical-binding: t; -*-

(use-package javascript-mode
  :ensure nil
  :hook
  ((javascript-mode javascript-ts-mode) . lsp-deferred))

(use-package add-node-modules-path)

(use-package npm-mode)

(use-package import-js)

(use-package js-doc
  :after js2-mode)

(use-package js2-mode
  :after javascript-mode)

(use-package js2-refactor
  :after js2-mode)

(use-package nodejs-repl
  :after js2-mode)

(use-package prettier-js
  :after js2-mode)

(provide '+lang/javascript/init)
