;; +lang/clojure/init.el -*- lexical-binding: t; -*-

(use-package clojure-mode)

(use-package clojure-ts-mode)

(use-package sesman)

(use-package cider
  :hook ((clojure-mode clojure-ts-mode) . cider-mode)
  :general-config
  (warmacs/set-local-leader-keys
    :keymaps '(clojure-mode-map clojure-ts-mode-map)
    "=" '(:ignore t :wk "format")
    "==" #'cider-format-buffer
    "=f" #'cider-format-defun
    "=l" #'clojure-align
    "=r" #'cider-format-region

    "d" '(:ignore t :wk "debug")
    "db" #'cider-debug-defun-at-point
    "de" '("cider-error-buffer" . (lambda () (interactive) (warmacs/switch-to-buffer cider-error-buffer)))
    "di" '(:ignore t :wk "inspect")
    "die" #'cider-inspect-last-sexp
    "dif" #'cider-inspect-defun-at-point
    "dii" #'cider-inspect
    "dil" #'cider-inspect-last-result
    "div" #'cider-inspect-expr

    "h" '(:ignore t :wk "help")
    "ha" #'cider-apropos
    "hc" #'cider-cheatsheet
    "hd" #'cider-clojuredocs
    "hj" #'cider-javadoc
    "hn" #'cider-browse-ns
    "hN" #'cider-browse-ns-all
    "hs" #'cider-browse-spec
    "hS" #'cider-browse-spec-all

    "e" '(:ignore t :wk "eval")
    "e;" #'cider-eval-defun-to-comment
    "e(" #'cider-eval-list-at-point
    "eB" #'cider-eval-buffer-and-replace
    "eD" #'cider-eval-last-sexp-to-repl
    "eE" #'cider-eval-last-sexp-and-replace
    "eN" #'cider-eval-ns-form-and-replace
    "eR" #'cider-eval-region-and-replace
    "eb" #'cider-eval-buffer
    "ed" #'cider-eval-last-sexp-to-repl
    "ee" #'cider-eval-last-sexp
    "ef" #'cider-eval-defun-at-point
    "ei" #'cider-interrupt
    "el" #'cider-load-buffer
    "en" #'cider-eval-ns-form
    "er" #'cider-eval-region

    "n" '(:ignore t :wk "namespace")
    "nn" #'cider-ns-reload
    "nN" #'cider-ns-reload-all

    "'"  '("sesman-start" . sesman-start)
    "s"  '(:ignore t :wk "session")
    "sq" #'cider-quit
    "sr" #'cider-restart
    "sn" #'cider-repl-set-ns
    "sb" #'cider-switch-to-repl-buffer
    "sc" #'cider-connect
    "ss" #'cider-switch-to-last-clojure-buffer))

(use-package clj-refactor
  :hook ((clojure-mode clojure-ts-mode) . clj-refactor-mode)
  :preface
  (add-to-list 'elpaca-ignored-dependencies 'inflections)
  :init
  (add-hook 'clojure-mode-hook 'clj-refactor-mode)
  :config
  (cljr-add-keybindings-with-prefix "C-c C-m")
  ;; Usually we do not set keybindings in :config, however this must be done
  ;; here because it reads the variable `cljr--all-helpers'. Since
  ;; `clj-refactor-mode' is added to the hook, this should trigger when a
  ;; clojure buffer is opened anyway, so there's no "keybinding delay".
  :general-config
  ;; keybindings for refactoring
  (warmacs/set-local-leader-keys
    :keymaps '(clojure-mode-map clojure-ts-mode-map)
    "r" '(:ignore t :wk "refactor")
    "rr" #'cljr-rename-file
    "rR" #'cljr-rename-symbol
    "ra" #'cljr-add-missing-libspec
    "rc" #'cljr-clean-ns
    "rd" #'cljr-debug
    "re" #'cljr-expand-let
    "rf" #'cljr-find-usages
    "ri" #'cljr-introduce-let
    "rj" #'cljr-join-thread
    "rl" #'cljr-move-to-let
    "rm" #'cljr-move-to-ns
    "rn" #'cljr-negate-expr
    "ro" #'cljr-sort-ns
    "rp" #'cljr-project-clean
    "rq" #'cljr-add-ns-to-ns
    "rr" #'cljr-remove-unused-requires))

(provide '+lang/clojure/init)
