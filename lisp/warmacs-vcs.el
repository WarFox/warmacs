;; warmacs-vcs.el -*- lexical-binding: t; -*-

;; Configure version control systems for warmacs

;; Git

(use-package magit
  :general
  (warmacs/leader-menu "git" "g"
    "s" #'magit-status))

(provide 'warmacs-vcs)
