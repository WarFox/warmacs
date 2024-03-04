;; rust.el --* lexical-binding: t; -*-

(use-package rust-mode
  :custom
  (rust-format-on-save t)
  :general
  (warmacs/local-leader-menu rust
      :keymaps '(rust-mode-map rust-ts-mode-map)
      "=" #'rust-format-buffer))

(provide '+lang/rust/init)
