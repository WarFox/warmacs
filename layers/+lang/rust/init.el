;; rust.el --* lexical-binding: t; -*-

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-ts-mode)
  :custom
  (rust-format-on-save t)
  :general-config
  (warmacs/set-local-leader-keys
    :keymaps '(rust-mode-map rust-ts-mode-map)
    "=" #'rust-format-buffer))

(provide '+lang/rust/init)
