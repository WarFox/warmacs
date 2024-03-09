;; +lang/json/init.el -*- lexical-binding: t; -*-

(use-package json-ts-mode
  :ensure nil
  :general
  (warmacs/local-leader-menu json
      :keymaps '(json-mode-map json-ts-mode-map)
      "=" '(:ignore t :wk "format")
      "h" '(:ignore t :wk "help")))

(use-package jsonnet-mode
  :general
  (warmacs/local-leader-menu jsonnet
      :keymaps 'jsonnet-mode-map
      "=" '(:ignore t :wk "format")
      "= =" '(jsonnet-format-buffer :wk "format-buffer")
      "= r" '(jsonnet-format-region :wk "format-region")))

(use-package json-snatcher
  :general
  (warmacs/local-leader-menu json-snatcher
      :keymaps '(json-mode-map json-ts-mode-map)
      "hp" '(jsons-print-path :wk "print-path")))

(provide '+lang/json/init)
