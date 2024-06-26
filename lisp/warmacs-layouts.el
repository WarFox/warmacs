;; warmacs-layouts.el --- Layouts for warmacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Manage layouts with perspectives
;; When a project is opened, a corresponding layout is created.
;; Layout groups projected related buffers togethe, allowing to switch between projets easily.
;; Implemented using eyebrowse + perspective

;;; Code:

(use-package persp-projectile
  :demand t
  :after
  (perspective projectile)) ;; persp-mode extension for projectile

;; original persp-mode
(use-package perspective
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
  (persp-avoid-killing-last-buffer-in-perspective t)
  :init
  (setq persp-state-default-file (expand-file-name "persp-state" warmacs-cache-dir))
  ;; initiates persp-mode
  (persp-state-load persp-state-default-file)
  :general-config
  (general-def
    "C-x C-b" #'persp-list-buffers) ; or use a nicer switcher, see below
  (warmacs/leader-menu "Layouts" "l"
    :prefix-map 'perspective-map
    "d" #'persp-kill
    "l" #'persp-switch))

(provide 'warmacs-layouts)
