;; warmacs-restart.el -*- lexical-binding: t; -*-

;; Quit/Restart menu

(use-package restart-emacs
  ;; we are using lambdas as general keybindings
  ;; so we need to autoload restart-emacs using commands
  :commands (restart-emacs restart-emacs-start-new-emacs)
  :preface
  (defun warmacs/restart-emacs (&optional args)
    (interactive)
    (restart-emacs args))

  (defun warmacs/kill-emacs (prompt &optional args)
    (interactive)
    (if (not prompt)
        (save-some-buffers nil t))
    (kill-emacs args))
  :after
  (files)
  :hook
  (kill-emacs . persp-state-save)
  :init
  ;; unbind 'restart-emacs to avoid conflict with built-in function in files.el
  ;; https://github.com/syl20bnr/spacemacs/pull/16186/files
  (fmakunbound 'restart-emacs)
  (autoload 'restart-emacs "restart-emacs" nil t)
  :general
  (warmacs/leader-menu "quit" "q"
    "d" '("restart-emacs-debug-init" . (lambda (&optional args)
                                         (interactive)
                                         (warmacs/restart-emacs (cons "--debug-init" args))))
    "R" #'warmacs/restart-emacs
    "q" '("prompt-kill-emacs" . (lambda (&optional args)
	                          (interactive)
                                  (warmacs/kill-emacs t args)))))

(provide 'warmacs-restart)
