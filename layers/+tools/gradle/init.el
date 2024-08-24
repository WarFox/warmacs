;; +tools/gradle/init.el --- Gradle Layer -*- lexical-binding: t; -*-

(use-package gradle-mode
  :general-config
  (warmacs/set-local-leader-keys
    :keymaps 'gradle-mode-map
    "w"  '(:ignore t :wk "workspace")
    "wu"  #'lsp-java-update-project-configuration))

(provide '+tools/gradle/init)
