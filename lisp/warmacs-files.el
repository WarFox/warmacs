;;; warmacs-files.el -*- lexical-binding: t; -*-

(elpaca nil ;; defer
  (warmacs/leader-menu "Files" "f"
    "f" #'find-file
    "r" #'consult-recent-file))

(provide 'warmacs-files)
