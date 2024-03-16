;; +lang/java/init.el --- Java Layer -*- lexical-binding: t; -*-

(use-package lsp-java
  :hook
  ((java-mode java-ts-mode) . lsp-deferred)
  :general

  ;; key bindings
  (warmacs/local-leader-menu java
      :keymaps '(java-mode-map java-ts-mode-map)
      "wu"  'lsp-java-update-project-configuration

      ;; refactoring
      "ro" 'lsp-java-organize-imports
      "rcp" 'lsp-java-create-parameter
      "rcf" 'lsp-java-create-field
      "rci" 'lsp-java-convert-to-static-import
      "rec" 'lsp-java-extract-to-constant
      "rel" 'lsp-java-extract-to-local-variable
      "rem" 'lsp-java-extract-method

      ;; assign/add
      "rai" 'lsp-java-add-import
      "ram" 'lsp-java-add-unimplemented-methods
      "rat" 'lsp-java-add-throws
      "raa" 'lsp-java-assign-all
      "raf" 'lsp-java-assign-to-field
      "raF" 'lsp-java-assign-statement-to-field
      "ral" 'lsp-java-assign-statement-to-local

      ;; generate
      "rgt" 'lsp-java-generate-to-string
      "rge" 'lsp-java-generate-equals-and-hash-code
      "rgo" 'lsp-java-generate-overrides
      "rgg" 'lsp-java-generate-getters-and-setters

      ;; create/compile
      "cc"  'lsp-java-build-project
      "cp"  'lsp-java-spring-initializr

      "gkk" 'lsp-java-type-hierarchy
      "gku" 'spacemacs/lsp-java-super-type
      "gks" 'spacemacs/lsp-java-sub-type

      ;; test
      "tb" 'lsp-jt-browser))

(use-package dap-java
  :after dap-mode
  :ensure nil)

(provide '+lang/java/init)
