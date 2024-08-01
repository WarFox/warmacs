;; +lang/typescript/init.el -*- lexical-binding: t; -*-

(use-package typescript-mode
  :mode ("\\.ts\\'" . typescript-ts-mode)
  :hook
  ((typescript-ts-mode typescript-tsx-mode) . lsp-deferred)
  ((typescript-ts-mode typescript-tsx-mode) . npm-mode)
  :general-config
  (warmacs/set-local-leader-keys
    :keymaps '(typescript-ts-mode-map typescript-tsx-mode-map)
    "n" '(:ignore t :wk "npm")
    "ni" 'npm-mode-npm-install
    "nr" 'npm-mode-npm-run
    "ns" 'npm-mode-npm-install-save
    "nd" 'npm-mode-npm-install-save-dev
    "nn" 'npm-mode-npm-init
    "nu" 'npm-mode-npm-uninstall
    "nl" 'npm-mode-npm-list
    "np" 'npm-mode-visit-project-file))

(provide '+lang/typescript/init)
