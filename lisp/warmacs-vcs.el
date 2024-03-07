;; warmacs-vcs.el -*- lexical-binding: t; -*-

;; Configure version control systems for warmacs

;; Git

(use-package magit
  :general
  (warmacs/leader-menu "git" "g"
    "s" #'magit-status)

  (warmacs/local-leader-keys
    :keymaps 'git-commit-mode-map
    "a" #'git-commit-ack
    "b" #'git-commit-search-message-backward
    "d" #'magit-diff-while-committing
    "f" #'git-commit-search-message-forward
    "g" #'git-commit-suggested
    "i" #'git-commit-insert-trailer
    "m" #'git-commit-modified
    "n" #'git-commit-next-message
    "p" #'git-commit-prev-message
    "o" #'git-commit-cc
    "r" #'git-commit-review
    "R" #'git-commit-reported
    "s" #'git-commit-signoff
    "S" #'git-commit-save-message
    "t" #'git-commit-test
    ;; with-editor-mode
    "c" #'with-editor-finish
    "k" #'with-editor-cancel))

(provide 'warmacs-vcs)
