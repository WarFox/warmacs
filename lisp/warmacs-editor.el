;;; warmacs-editor.el -*- lexical-binding: t; -*-

(use-package editorconfig)

(use-package aggressive-indent
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))

(use-package emacs
  :ensure nil
  :custom
  ;; Use space instead of tabs in all buffers
  (indent-tabs-mode nil)
  ;; Disable creating backup files
  (make-backup-files nil)
  ;; Enable auto-saving
  (auto-save-default t)
  ;; Line numbers
  ;; Explicitly define a width to reduce the cost of on-the-fly computation
  (display-line-numbers-width 3)
  ;; Show absolute line numbers for narrowed regions to make it easier to tell the
  ;; buffer is narrowed, and where you are, exactly.
  (display-line-numbers-widen t)
  ;; Relative line numbers
  (display-line-numbers-type 'relative)
  ;; Avoid shifting the buffer contents when line numbers are toggled.
  ;; Do not shrink line number width
  (display-line-numbers-grow-only t)
  :hook
  ;; Enable line numbers in most text-editing modes. We avoid
  ;; `global-display-line-numbers-mode' because there are many special and
  ;; temporary modes where we don't need/want them.
  ((prog-mode text-mode conf-mode) . #'display-line-numbers-mode)
  :config
  ;; Enable auto-saving
  (auto-save-visited-mode 1))

(use-package evil-mc
  :after evil
  :hook
  ((prog-mode org-mode markdown-mode) . evil-mc-mode)
  :general
  (general-def
    :keymaps 'evil-normal-state-map
    "C-n" #'evil-mc-make-cursor-move-next-line))

(use-package evil-surround
  :after evil
  :hook
  ((prog-mode org-mode markdown-mode) . evil-surround-mode))

(use-package evil-nerd-commenter
  :after evil
  :commands
  (evilnc-comment-operator evilnc-copy-and-comment-operator)
  :general
  (general-def
    :keymaps 'evil-normal-state-map
    "gc" #'evilnc-comment-operator
    "gC" #'evilnc-copy-and-comment-operator))

;; Jump to things in Emacs tree-style
(use-package avy
  :general
  (warmacs/leader-menu "Jump" "j"
    "c" #'avy-goto-char-timer
    "j" #'avy-goto-char
    "l" #'avy-goto-line
    "w" #'avy-goto-word-1
    "W" #'avy-goto-word-0
    "s" #'avy-goto-subword-1
    "S" #'avy-goto-subword-0
    "p" #'avy-pop-mark
    "o" #'avy-org-goto-heading-timer
    "O" #'avy-org-goto-heading
    "b" #'avy-pop-mark
    "B" #'avy-pop-mark
    "r" #'avy-resume))

(provide 'warmacs-editor)
