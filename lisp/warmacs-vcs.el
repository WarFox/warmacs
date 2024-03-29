;; warmacs-vcs.el -*- lexical-binding: t; -*-

;; Configure version control systems for warmacs

;; Git

(use-package magit
  :general
  (warmacs/leader-menu "git" "g"
    "c"  #'magit-clone
    "f"  '(:ignore t :wk "file")
    "ff" #'magit-find-file
    "fl" #'magit-log-buffer-file
    "fd" #'magit-diff
    "fD" #'magit-file-delete
    "fm" #'magit-file-dispatch
    "fr" #'magit-file-rename
    "i"  #'magit-init
    "L"  #'magit-list-repositories
    "m"  #'magit-dispatch
    "s"  #'magit-status)

  ;; Git Commit Mode
  :general-config
  (warmacs/set-local-leader-keys
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

(use-package git-timemachine
  :general
  (warmacs/leader-menu "git" "g"
    "t" #'git-timemachine-help))

(use-package git-modes)

(use-package gitignore-templates
  :general
  (warmacs/leader-menu "git" "g"
    "fi" 'gitignore-templates-new-file)
  :general-config
  (warmacs/set-local-leader-keys
    :keymaps 'gitignore-mode-map
    "i" 'gitignore-templates-insert))

(use-package git-link
  :general
  (warmacs/leader-menu "git" "g"
    "l" '(:ignore t :wk "links")
    "lc" #'git-link-commit
    "ll" #'git-link))

(provide 'warmacs-vcs)
