;; warmacs-treemacs.el -*- lexical-binding: t; -*-

(use-package treemacs
  :commands
  (treemacs treemacs-find-file)
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :hook
  (treemacs-mode . (lambda ()
                     (treemacs-follow-mode 1) ;; current file
                     (treemacs-project-follow-mode 1))) ;; current project
  :custom
  (treemacs-collapse-dirs                   3)
  (treemacs-deferred-git-apply-delay        0.5)
  (treemacs-directory-name-transformer      #'identity)
  (treemacs-display-in-side-window          t)
  (treemacs-eldoc-display                   'simple)
  (treemacs-expand-after-init               t)
  (treemacs-file-event-delay                2000)
  (treemacs-file-extension-regex            treemacs-last-period-regex-value)
  (treemacs-file-follow-delay               0.2)
  (treemacs-file-name-transformer           #'identity)
  (treemacs-find-workspace-method           'find-for-file-or-pick-first)
  (treemacs-follow-after-init               t)
  (treemacs-git-command-pipe                "")
  (treemacs-goto-tag-strategy               'refetch-index)
  (treemacs-header-scroll-indicators        '(nil . "^^^^^^"))
  (treemacs-hide-dot-git-directory          t)
  (treemacs-indentation                     2)
  (treemacs-indentation-string              " ")
  (treemacs-is-never-other-window           nil)
  (treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask"))
  (treemacs-max-git-entries                 5000)
  (treemacs-missing-project-action          'ask)
  (treemacs-move-forward-on-expand          nil)
  (treemacs-no-delete-other-windows         t)
  (treemacs-no-png-images                   nil)
  (treemacs-persist-file                    (expand-file-name "treemacs-persist" warmacs-cache-dir))
  (treemacs-position                        'left)
  (treemacs-project-follow-cleanup          t)
  (treemacs-project-follow-into-home        nil)
  (treemacs-read-string-input               'from-child-frame)
  (treemacs-recenter-after-file-follow      nil)
  (treemacs-recenter-after-project-expand   'on-distance)
  (treemacs-recenter-after-project-jump     'always)
  (treemacs-recenter-after-tag-follow       nil)
  (treemacs-recenter-distance               0.1)
  (treemacs-select-when-already-in-treemacs 'move-back)
  (treemacs-show-cursor                     nil)
  (treemacs-show-hidden-files               t)
  (treemacs-silent-filewatch                nil)
  (treemacs-silent-refresh                  nil)
  (treemacs-sorting                         'alphabetic-asc)
  (treemacs-space-between-root-nodes        t)
  (treemacs-tag-follow-cleanup              t)
  (treemacs-tag-follow-delay                1.5)
  (treemacs-text-scale                      nil)
  (treemacs-user-header-line-format         nil)
  (treemacs-user-mode-line-format           nil)
  (treemacs-wide-toggle-width               70)
  (treemacs-width                           35)
  (treemacs-width-increment                 1)
  (treemacs-width-is-initially-locked       t)
  (treemacs-workspace-switch-cleanup        nil)
  :config
  ;; The default width and height of the icons is 22 pixels. If you are
  ;; using a Hi-DPI display, uncomment this to double the icon size.
  ;;(treemacs-resize-icons 44)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-git-commit-diff-mode 1)

  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))

  (treemacs-hide-gitignored-files-mode nil)
  :general
  (warmacs/leader-menu-Files
    "t" #'treemacs-find-file)
  (general-def
    :keymaps 'global-map
    "M-0"        #'treemacs-select-window
    "C-x t 1"    #'treemacs-delete-other-windows
    "C-x t t"    #'treemacs
    "C-x t d"    #'treemacs-select-directory
    "C-x t B"    #'treemacs-bookmark
    "C-x t C-t"  #'treemacs-find-file
    "C-x t M-t"  #'treemacs-find-tag))

(use-package treemacs-evil
  :demand t
  :after
  (treemacs evil))

;; Treemacs respects perspectives
(use-package treemacs-perspective
  :demand t
  :after
  (treemacs perspective)
  :config
  (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-magit
  :demand t
  :after
  (treemacs magit))

(use-package treemacs-icons-dired
  :hook
  (dired-mode . treemacs-icons-dired-enable-once)
  :after
  (treemacs dired))

(use-package treemacs-nerd-icons
  :demand t
  :after
  (treemacs nerd-icons)
  :config
  (treemacs-load-theme "nerd-icons"))

(provide 'warmacs-treemacs)
