;; warmacs-ui.el -*- lexical-binding: t; -*-

(use-package all-the-icons)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :custom
  (doom-modeline-height 15)
  :hook (elpaca-after-init . doom-modeline-mode)
  :init
  (column-number-mode 1)
  (size-indication-mode 1))

(elpaca nil
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
            "g C-S-t"  'centaur-tabs-move-current-tab-to-left)))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

(provide 'warmacs-ui)
