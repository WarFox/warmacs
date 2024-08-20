;; +lang/kotlin/init.el --- Java Layer -*- lexical-binding: t; -*-

(use-package kotlin-ts-mode
  :mode "\\.kt\\'")

(use-package flycheck-kotlin
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-kotlin-setup))

(provide '+lang/kotlin/init)
