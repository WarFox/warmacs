;; +lang/java/init.el --- Java Layer -*- lexical-binding: t; -*-

(use-package lsp-java
  :hook
  (java-ts-mode . lsp-deferred)
  :general-config
  (warmacs/set-local-leader-keys
    :keymaps 'java-ts-mode-map
    :prefix-map 'lsp-command-map ;; extend lsp command map
    "=" '(:ignore t :wk "format")
    "a" '(:ignore t :wk "actions")
    "F" '(:ignore t :wk "folders")
    "G" '(:ignore t :wk "peeks")
    "h" '(:ignore t :wk "help")
    "T" '(:ignore t :wk "toggles")
    "w"  '(:ignore t :wk "workspace")

    ;; create/compile
    "c"  '(:ignore t :wk "create/compile")
    "cc"  #'lsp-java-build-project
    "cp"  #'lsp-java-spring-initializr

    ;; goto
    "g" '(:ignore t :wk "goto")
    "gT" #'lsp-java-type-hierarchy

    ;; refactoring
    "r" '(:ignore t :wk "refactoring")
    "ro" '("organize-import" . lsp-java-organize-imports)

    ;; add/assign
    "ra"  '(:ignore t :wk "add/assign")
    "rai" '("add-import"                . lsp-java-add-import)
    "ram" '("add-unimplemented-methods" . lsp-java-add-unimplemented-methods)
    "rat" '("add-throws"                . lsp-java-add-throws)
    "raa" '("assign-all"                . lsp-java-assign-all)
    "raf" '("assign-to-field"           . lsp-java-assign-to-field)
    "raF" '("statement-to-field"        . lsp-java-assign-statement-to-field)
    "ral" '("assign-statement-to-local" . lsp-java-assign-statement-to-local)

    ;; create/convert
    "rc"  '(:ignore t :wk "create/convert")
    "rcp" #'lsp-java-create-parameter
    "rcf" #'lsp-java-create-field
    "rcl" #'lsp-java-create-local
    "rci" #'lsp-java-convert-to-static-import

    ;; extract
    "re"  '(:ignore t :wk "extract")
    "rec" #'lsp-java-extract-to-constant
    "rel" #'lsp-java-extract-to-local-variable
    "rem" #'lsp-java-extract-method

    ;; generate
    "rg"  '(:ignore t :wk "generate")
    "rgt" '("to-string"            . lsp-java-generate-to-string)
    "rge" '("equals-and-hash-code" . lsp-java-generate-equals-and-hash-code)
    "rgo" '("overrides"            . lsp-java-generate-overrides)
    "rgg" '("getters-and-setters"  . lsp-java-generate-getters-and-setters)

    ;; test
    "t"  '(:ignore t :wk "tests")
    "tt" #'dap-java-run-test-method
    "tT" #'dap-java-run-test-class
    "tb" #'lsp-jt-browser))

(use-package dap-java
  :after dap-mode
  :ensure nil
  :general-config
  (warmacs/set-local-leader-keys
    :keymaps 'java-ts-mode-map
    ;; debug
    "d" '(:ignore t :wk "debug")
    "dd" #'dap-java-debug
    "dt" '(:ignore t :wk "test")
    "dtc" #'dap-java-debug-test-class
    "dtm" #'dap-java-debug-test-method))

(provide '+lang/java/init)
