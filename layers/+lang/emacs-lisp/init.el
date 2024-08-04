;; +lang/emacs-lisp/init.el -*- lexical-binding: t; -*-

(use-package emacs-lisp
  :ensure nil
  :general
  (warmacs/set-local-leader-keys
    :keymaps 'emacs-lisp-mode-map
    "c" '(:ignore t :wk "compile")
    "cc" #'emacs-lisp-byte-compile
    "e" '(:ignore t :wk "eval")
    "eb" #'eval-buffer
    "ee" #'eval-print-last-sexp
    "ef" #'eval-defun
    "er" #'eval-region
    "h" '(:ignore t :wk "help")
    "hh" #'helpful-at-point))

(use-package elisp-def)

(use-package overseer
  :general
  (warmacs/set-local-leader-keys
    :keymaps 'emacs-lisp-mode-map
    "t" '(:ignore t :wk "test")
    "ta" #'overseer-test
    "tt" #'overseer-test-run-test
    "tb" #'overseer-test-this-buffer
    "tf" #'overseer-test-file
    "tg" #'overseer-test-tags
    "tp" #'overseer-test-prompt
    "tA" #'overseer-test-debug
    "tq" #'overseer-test-quiet
    "tv" #'overseer-test-verbose
    "th" #'overseer-help))

(provide '+lang/emacs-lisp/init)
