;; +lang/markdown/init.el -*- lexical-binding: t; -*-

(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :custom
  (markdown-command "multimarkdown")
  (markdown-enable-math t)
  (markdown-enable-wiki-links t)
  (markdown-asymmetric-header t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-gfm-additional-languages '("sh"))
  (markdown-gfm-uppercase-checkbox t))

(provide '+lang/markdown/init)
