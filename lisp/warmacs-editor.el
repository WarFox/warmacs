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

;;
;; Evil mode
(use-package evil
  :demand t
  :custom
  (evil-want-integration t) ;; This is optional since it's already set to t by default.
  (evil-want-keybinding nil) ;; set to nil to use evil-collection
  :config
  (evil-mode 1))

(use-package evil-collection
  :demand t
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package evil-mc
  :after evil
  :hook
  ((prog-mode org-mode markdown-mode occur-mode grep-mode) . evil-mc-mode)
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

(use-package smartparens
  :hook (prog-mode text-mode markdown-mode)
  :config
  (require 'smartparens-config)
  :general-config
  (warmacs/leader-menu "lisp" "k"
    :keymaps 'smartparens-mode-map
    "$"   #'sp-end-of-sexp
    "`"   '(:ignore t :wk "hybrid")
    "`k"  #'sp-kill-hybrid-sexp
    "`p"  #'sp-push-hybrid-sexp
    "`s"  #'sp-slurp-hybrid-sexp
    "`t"  #'sp-transpose-hybrid-sexp
    "1"   #'digit-argument
    "2"   #'digit-argument
    "3"   #'digit-argument
    "4"   #'digit-argument
    "5"   #'digit-argument
    "6"   #'digit-argument
    "7"   #'digit-argument
    "8"   #'digit-argument
    "9"   #'digit-argument
    "a"   #'sp-absorb-sexp
    "b"   #'sp-forward-barf-sexp
    "B"   #'sp-backward-barf-sexp
    "c"   #'sp-convolute-sexp
    "D"   '(:ignore t :wk "delete-backward")
    "Ds"  #'sp-backward-kill-symbol
    "Dw"  #'sp-backward-kill-word
    "Dx"  #'sp-backward-kill-sexp
    "d"   '(:ignore t :wk "delete")
    "ds"  #'sp-kill-symbol
    "dw"  #'sp-kill-word
    "dx"  #'sp-kill-sexp
    "e"   #'sp-splice-sexp-killing-forward
    "E"   #'sp-splice-sexp-killing-backward
    "h"   #'sp-backward-symbol
    "H"   #'sp-backward-sexp
    "i"   #'evil-insert-state
    "I"   #'evil-insert-line
    "J"   #'sp-join-sexp
    "L"   #'sp-forward-sexp
    "p"   #'evil-paste-after
    "P"   #'evil-paste-before
    "r"   #'sp-raise-sexp
    "s"   #'sp-forward-slurp-sexp
    "S"   #'sp-backward-slurp-sexp
    "t"   #'sp-transpose-sexp
    "U"   #'sp-backward-up-sexp
    "C-r" #'undo-tree-redo
    "v"   #'evil-visual-char
    "V"   #'evil-visual-line
    "C-v" #'evil-visual-block
    "W"   #'sp-unwrap-sexp
    "y"   #'sp-copy-sexp))

(use-package apheleia
  :config
  (apheleia-global-mode +1))

(provide 'warmacs-editor)
