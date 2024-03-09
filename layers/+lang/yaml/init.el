;; yaml  -*- lexical-binding: t; -*-

(use-package yaml-mode
  :general
  (warmacs/local-leader-menu yaml
      :keymaps '(yaml-mode-map yaml-ts-mode-map)
      "=" '(:ignore t :wk "format")
      "= =" '(yaml-mode-format-buffer :wk "format-buffer")
      "= r" '(yaml-mode-format-region :wk "format-region")))

(use-package yaml-tomato
  :general
  (warmacs/local-leader-menu yaml
      :keymaps '(yaml-mode-map yaml-ts-mode-map)
      "y" '(:ignore t :wk "yaml-tomato")
      "y y" '(yaml-tomato :wk "yaml-tomato")))

(provide '+lang/yaml/init)
