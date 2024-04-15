;; +tools/terraform/init.el -*- lexical-binding: t -*-

(use-package terraform-mode
  :hook
  (terraform-mode . terraform-format-on-save-mode)
  :general-config
  (warmacs/set-local-leader-keys
    :keymaps 'terraform-mode-map
    "h" '(:ignore t :wk "Help")
    "hh" #'terraform-doc-at-point))

(use-package terraform-doc
  :after terraform-mode)

(provide '+tools/terraform/init)
