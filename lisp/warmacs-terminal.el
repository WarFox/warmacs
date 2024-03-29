;; warmacs-terminal.el -*- lexical-binding: t; -*-;

;; Terminal Setup with Eshell and vterm

;; Eshell

(use-package eshell
  :ensure nil
  :config
  (setq
   eshell-scroll-to-bottom-on-input 'all
   eshell-prefer-lisp-functions t
   eshell-prompt-function (lambda ()
                            (concat
                             (propertize (eshell/pwd) 'face '(:foreground "green"))
                             (propertize " λ " 'face '(:foreground "green"))))
   eshell-prompt-regexp " λ "
   eshell-highlight-prompt nil)
  :general
  (warmacs/set-local-leader-keys
    :keymaps 'emacs-lisp-mode-map
    "'" '("eshell" . eshell)))

;; vterm
(use-package vterm
  :commands vterm
  :general
  (warmacs/set-leader-keys
    "'" '("terminal" . vterm))
  ;; open vterm at project root using "p '"
  (warmacs/leader-menu "Projects" "p"
    "'" #'projectile-run-vterm))

(provide 'warmacs-terminal)
