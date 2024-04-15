;;; warmacs-flycheck.el --- Warmacs Flycheck -*- lexical-binding: t; -*-

;;; Commentary:

;; Flycheck configuration

;;; Code:
(use-package flycheck
  :preface
  ;; toggle flycheck window
  (defun warmacs/toggle-flycheck-error-list ()
    "Toggle flycheck's error list window.
If the error list is visible, hide it.  Otherwise, show it."
    (interactive)
    (if-let ((window (flycheck-get-error-list-window)))
        (save-selected-window (quit-window nil window))
      (flycheck-list-errors)))

  (defun warmacs/goto-flycheck-error-list ()
    "Open and go to the error list buffer."
    (interactive)
    (if (flycheck-get-error-list-window)
        (switch-to-buffer flycheck-error-list-buffer)
      (flycheck-list-errors)
      (switch-to-buffer-other-window flycheck-error-list-buffer)))

  :hook
  (prog-mode . flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  :general-config
  (general-def
    "C-c !" '(:ignore t :wk "flycheck"))
  (warmacs/leader-menu "Errors" "e"
    "b" #'flycheck-buffer
    "c" #'flycheck-clear
    "d" #'flycheck-disable-checker
    "e" #'consult-flycheck
    "h" #'flycheck-describe-checker
    "l" #'warmacs/toggle-flycheck-error-list
    "L" #'warmacs/goto-flycheck-error-list
    "s" #'flycheck-select-checker
    "S" #'flycheck-set-checker-executable
    "v" #'flycheck-verify-setup
    "y" #'flycheck-copy-errors-as-kill
    "x" #'flycheck-explain-error-at-point))

(provide 'warmacs-flycheck)
;;; warmacs-flycheck.el ends here
