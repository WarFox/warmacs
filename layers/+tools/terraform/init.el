;; +tools/terraform/init.el -*- lexical-binding: t -*-

(use-package terraform-mode
  :hook (terraform-mode . terraform-format-on-save-mode)
  :general
  (warmacs/local-leader-menu terraform
      "h" '(:ignore t :wk "Help")
      "hh" #'terraform-doc-at-point))

(use-package terraform-doc)

(provide '+tools/terraform/init)
