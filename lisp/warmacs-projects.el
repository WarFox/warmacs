;; warmacs-projects.el -*- lexical-binding: t; -*-

;;
;; Projects Support

(use-package projectile)

(elpaca nil ;; defer
  (warmacs/leader-menu "Projects" "p"
    "f" #'projectile-find-file
    "t" #'treemacs))

(provide 'warmacs-projects)
