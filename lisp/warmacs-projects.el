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
    "p" #'projectile-switch-project
    "l" #'projectile-switch-open-project)
  :general-config
  (warmacs/leader-menu "Projects" "p"
    :prefix-map 'projectile-command-map ;; extend projectile command map
    "/" #'projectile-ripgrep
    "%" #'projectile-replace-regexp
    "4" '(:ignore t :wk "other-window")
    "5" '(:ignore t :wk "other-frame")
    "a" #'projectile-toggle-between-implementation-and-test
    "B" #'projectile-switch-to-buffer-other-window
    "ESC" #'keyboard-escape-quit
    "F" #'projectile-find-file-other-window
    "G" #'projectile-regenerate-tags
    "K" #'projectile-reset-known-projects
    "l" #'projectile-switch-open-project
    "r" #'projectile-recentf
    "R" #'projectile-replace
    "s" '(:ignore t :wk "search")
    "t" #'treemacs
    "T" #'projectile-test-project
    "x" '(:ignore t :wk "x!")
    "x4" '(:ignore t :wk "other-window")))

(provide 'warmacs-projects)
