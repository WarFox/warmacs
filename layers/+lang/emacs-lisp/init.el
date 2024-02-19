;; +lang/emacs-lisp/init.el -*- lexical-binding: t; -*-

(use-package emacs-lisp
  :ensure nil
  :general
  (warmacs/local-leader-menu emacs-lisp
      "e" '(:ignore t :which-key "eval")
      "eb" 'eval-buffer
      "er" 'eval-region
      "ee" 'eval-print-last-sexp
      "ef" 'eval-defun))

(provide '+lang/emacs-lisp/init)
