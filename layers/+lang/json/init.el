;; +lang/json/init.el -*- lexical-binding: t; -*-

(use-package json-ts-mode
  :ensure nil
  :general-config
  (warmacs/set-local-leader-keys
    :keymaps '(json-mode-map json-ts-mode-map)
    "=" '(:ignore t :wk "format")
    "h" '(:ignore t :wk "help")))

(use-package jsonnet-mode
  :general-config
  (warmacs/set-local-leader-keys
    :keymaps 'jsonnet-mode-map
    "=" '(:ignore t :wk "format")
    "==" '("format-buffer" . jsonnet-format-buffer)
    "=r" '("format-region" . jsonnet-format-region)))

(use-package json-snatcher
  :general-config
  (warmacs/set-local-leader-keys json-snatcher-mode-map
      :keymaps '(json-mode-map json-ts-mode-map)
      "hp" '("print-path" . jsons-print-path)))

(provide '+lang/json/init)
