;;; warmacs-help.el -*- lexical-binding: t; -*-

;; Help everywhere for emacs-lisp
(use-package helpful
  :general
  (warmacs/leader-menu "help" "h"
    "h" 'helpful-at-point)
  (general-def
    [remap describe-function] #'helpful-function
    [remap describe-symbol] #'helpful-symbol
    [remap describe-variable] #'helpful-variable
    [remap describe-command] #'helpful-command
    [remap describe-key] #'helpful-key)
  :general-config
  (general-def
    :keymaps 'helpful-mode-map :states 'normal "q" 'quit-window))

(provide 'warmacs-help)
