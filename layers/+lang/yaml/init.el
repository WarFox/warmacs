;; yaml  -*- lexical-binding: t; -*-

(use-package yaml-mode
  :general
  (warmacs/local-leader-menu yaml
      :keymaps '(yaml-mode-map yaml-ts-mode-map)
      "=" '(:ignore t :which-key "format")
      "= =" '(yaml-mode-format-buffer :which-key "yaml-prettify-buffer")
      "= r" '(yaml-mode-format-region :which-key "yaml-prettify-region")))

(provide '+lang/yaml/init)
