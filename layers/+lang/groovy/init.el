;; +tools/groovy/init.el --- Groovy Layer -*- lexical-binding: t; -*-

(use-package groovy-mode
  :mode "\\.gr\\(oovy\\|adle\\)$"
  :general-config
  (warmacs/set-local-leader-keys
    :keymaps 'groovy-mode-map
    "'"  #'run-groovy
    "s" '(:ignore t :wk "repl")
    "sf" #'groovy-send-definition
    "sr" #'groovy-send-region))

(use-package groovy-imports
  :hook
  (groovy-mode . groovy-imports-scan-file)
  ;; :init
  ;; (add-hook 'groovy-mode-hook 'groovy-imports-scan-file)
  :general-config
  (warmacs/set-local-leader-keys
    :keymaps 'groovy-mode-map
    "ri" #'groovy-imports-add-import-dwim))

(provide '+lang/groovy/init)
