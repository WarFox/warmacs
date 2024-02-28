;; warmacs-projects.el -*- lexical-binding: t; -*-

;;
;; Projects Support

(use-package projectile
  :custom
  (projectile-switch-project-action #'consult-projectile-find-file)
  :general
  (warmacs/leader-menu "Projects" "p"
    "!" #'projectile-run-shell-command-in-root
    "&" #'projectile-run-async-shell-command-in-root
    "%" #'projectile-replace-regexp
    "a" #'projectile-toggle-between-implementation-and-test
    "b" #'projectile-switch-to-buffer
    "c" #'projectile-compile-project
    "d" #'projectile-find-dir
    "D" #'projectile-dired
    "e" #'projectile-edit-dir-locals
    "f" #'projectile-find-file
    "F" #'projectile-find-file-dwim
    "g" #'projectile-find-tag
    "G" #'projectile-regenerate-tags
    "I" #'projectile-invalidate-cache
    "k" #'projectile-kill-buffers
    "l" #'projectile-switch-open-project
    "p" #'projectile-switch-project
    "r" #'projectile-recentf
    "R" #'projectile-replace
    "S" #'projectile-save-project-buffers
    "t" #'treemacs
    "T" #'projectile-test-project
    "v" #'projectile-vc))


(provide 'warmacs-projects)
