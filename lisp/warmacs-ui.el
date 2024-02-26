;; warmacs-ui.el -*- lexical-binding: t; -*-

;; UI and Theme configurations

(defcustom warmacs-theme 'doom-one
  "The default theme to use.")

;; Replaced all-the-icons with nerd-icons
(use-package nerd-icons)

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :config
  (nerd-icons-completion-mode)
  :hook
  ('marginalia-mode . #'nerd-icons-completion-marginalia-setup))

(use-package doom-themes
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  (load-theme warmacs-theme t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :custom
  (doom-modeline-height 15)
  :hook
  (elpaca-after-init . doom-modeline-mode)
  :init
  (column-number-mode 1)
  (size-indication-mode 1))

(use-package centaur-tabs
  :custom
  (centaur-tabs-set-icons t)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "⚠")
  (centaur-tabs-cycle-scope 'tabs)
  :hook
  ((prog-mode text-mode) . centaur-tabs-mode)
  ((dired-mode
    dashboard-mode
    term-mode
    calendar-mode
    org-agenda-mode
    helpful-mode) . centaur-tabs-local-mode)
  :config
  (unless (daemonp)
    (setq centaur-tabs-set-bar 'left))

  (centaur-tabs-headline-match)
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-mode t)
  :general
  ("C-c t" '(:ignore t :which-key "tabs"))
  ("C-c t s"  'centaur-tabs-switch-group)
  ("C-c t p"  'centaur-tabs-group-by-projectile-project)
  ("C-c t g"  'centaur-tabs-group-buffer-groups)
  (:keymaps 'evil-normal-state-map
            "g t"      'centaur-tabs-forward
            "g T"      'centaur-tabs-backward
            "g C-t"    'centaur-tabs-move-current-tab-to-right
            "g C-S-t"  'centaur-tabs-move-current-tab-to-left))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

(provide 'warmacs-ui)
