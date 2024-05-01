;; +lang/sh/init.el --- Shell Layer -*- lexical-binding: t; -*-

(use-package fish-mode
  :if (executable-find "fish")
  :mode "\\.fish\\'")

(provide '+lang/sh/init)

;;; +lang/sh/init.el ends here
