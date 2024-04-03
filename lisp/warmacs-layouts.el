;; warmacs-layouts.el --- Layouts for warmacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Manage layouts with perspectives
;; When a project is opened, a corresponding layout is created.
;; Layout groups projected related buffers togethe, allowing to switch between projets easily.
;; Implemented using eyebrowse + perspective

;;; Code:

(use-package persp-projectile
  :after (perspective projectile)) ;; persp-mode extension for projectile

;; original persp-mode
(use-package perspective
  :bind
  ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  ;; Setp persp-conf directory inside warmacs-cache
  (persp-save-dir (concat warmacs-cache-dir "persp-confs/"))
  (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
  (persp-add-on-switch-or-display t)
  (persp-add-on-buffer-management t)
  (persp-auto-resume-time -1)
  (persp-nil-name "default")
  (persp-nil-hidden t)
  (persp-autokill-buffer-on-remove 'kill-weak)
  :init
  (persp-mode 1)
  :general
  (warmacs/leader-menu "Layouts" "l"
    :prefix-map 'perspective-map
    "l" #'persp-switch))

(provide 'warmacs-layouts)
