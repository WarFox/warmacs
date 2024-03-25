;; yaml  -*- lexical-binding: t; -*-

(use-package yaml-mode
  :general-config
  (warmacs/local-leader-menu yaml
      :keymaps '(yaml-mode-map yaml-ts-mode-map)
      "=" '(:ignore t :wk "format")
      "==" '("format-buffer" . yaml-mode-format-buffer)
      "=r" '("format-region" . yaml-mode-format-region)))

(use-package yaml-tomato
  :general-config
  (warmacs/local-leader-menu yaml
      :keymaps '(yaml-mode-map yaml-ts-mode-map)
      "y" '(:ignore t :wk "yaml-tomato")
      "yy" '("yaml-tomato" . yaml-tomato)))

(provide '+lang/yaml/init)
