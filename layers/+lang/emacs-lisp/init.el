;; +lang/emacs-lisp/init.el -*- lexical-binding: t; -*-

(use-package emacs-lisp
  :ensure nil
  :general
  (warmacs/local-leader-menu emacs-lisp
      ","  #'lisp-state-toggle-lisp-state
      "tq" #'ert

      "c" '(:ignore t :which-key "compile")
      "cc" #'emacs-lisp-byte-compile
      "e" '(:ignore t :which-key "eval")
      "eb" #'eval-buffer
      "ee" #'eval-print-last-sexp
      "ef" #'eval-defun
      "er" #'eval-region
      "e$" #'lisp-state-eval-sexp-end-of-line
      "el" #'lisp-state-eval-sexp-end-of-line

      "h" '(:ignore t :which-key "help")
      "hh" #'helpful-at-point))

(use-package elisp-def)

(use-package overseer
  :general
  (warmacs/local-leader-menu emacs-lisp
      "t" '(:ignore t :which-key "test")
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
