;; +webservices/leetcode/init.el -*- lexical-binding: t; -*-

(use-package leetcode
  :commands leetcode
  :general
  (warmacs/leader-menu "Webservices" "W"
    "L" '(:ignore t :wk "LeetCode")
    "Ll" #'leetcode)
  :general-config
  (general-def
    :keymaps 'leetcode--problems-mode-map
    "TAB" #'leetcode-show-current-problem
    "<return>" #'leetcode-show-current-problem)
  (warmacs/leader-menu "Webservices" "W"
    "Ld" #'leetcode-show-current-problem
    "Lr" #'leetcode-refresh
    "Lt" #'leetcode-try
    "Lu" #'leetcode-submit
    "Lq" #'leetcode-quit))

(provide '+webservices/leetcode/init)
