;; warmacs-projects.el -*- lexical-binding: t; -*-

;;
;; Projects Support

(use-package projectile
  :custom
  (projectile-switch-project-action #'consult-projectile-find-file)
  (projectile-sort-order #'recently-active)
  (projectile-cache-file (expand-file-name
                          "projectile.cache"
                          warmacs-cache-dir))
  (projectile-known-projects-file (expand-file-name
                                   "projectile-bookmarks.eld"
                                   warmacs-cache-dir))
  :config
  (projectile-mode 1)
  :general
  (warmacs/leader-menu "Projects" "p"
    "!" #'projectile-run-shell-command-in-root
    "&" #'projectile-run-async-shell-command-in-root
    "/" #'projectile-ripgrep
    "%" #'projectile-replace-regexp
    "a" #'projectile-toggle-between-implementation-and-test
    "b" #'projectile-switch-to-buffer
    "B" #'projectile-switch-to-buffer-other-window
    "c" #'projectile-compile-project
    "d" #'projectile-find-dir
    "D" #'projectile-dired
    "e" #'projectile-edit-dir-locals
    "f" #'projectile-find-file
    "F" #'projectile-find-file-other-window
    "g" #'projectile-find-tag
    "G" #'projectile-regenerate-tags
    "I" #'projectile-invalidate-cache
    "k" #'projectile-kill-buffers
    "K" #'projectile-reset-known-projects
    "l" #'projectile-switch-open-project
    "p" #'projectile-switch-project
    "r" #'projectile-recentf
    "R" #'projectile-replace
    "S" #'projectile-save-project-buffers
    "t" #'treemacs
    "T" #'projectile-test-project
    "v" #'projectile-vc))

(provide 'warmacs-projects)
