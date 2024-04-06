;; yaml  -*- lexical-binding: t; -*-

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :general-config
  (warmacs/set-local-leader-keys
    :keymaps '(yaml-mode-map yaml-ts-mode-map)
    "=" '(:ignore t :wk "format")
    "==" '("format-buffer" . yaml-mode-format-buffer)
    "=r" '("format-region" . yaml-mode-format-region)))

(use-package yaml-tomato
  :after yaml-mode
  :general-config
  (warmacs/set-local-leader-keys
    :keymaps '(yaml-mode-map yaml-ts-mode-map)
    "y" '(:ignore t :wk "yaml-tomato")
    "yy" '("yaml-tomato" . yaml-tomato)))

(provide '+lang/yaml/init)
