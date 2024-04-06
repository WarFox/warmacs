;; warmacs-restart.el -*- lexical-binding: t; -*-

;; Quit/Restart menu

(use-package restart-emacs
  :commands (restart-emacs kill-emacs)
  :preface
  (defun warmacs/restart-emacs (&optional args)
    (interactive)
    (restart-emacs))

  (defun warmacs/kill-emacs (prompt &optional args)
    (interactive)
    (if (not prompt)
        (save-some-buffers nil t))
    (kill-emacs args))
  :general
  (warmacs/leader-menu "quit" "q"
    "d" '("restart-emacs-debug-init" . (lambda (&optional args)
                                         (interactive)
                                         (warmacs/restart-emacs (cons "--debug-init" args))))
    "R" #'warmacs/restart-emacs
    "t" '("restart-emacs-timed-requires" . (lambda (&optional args)
                                             (interactive)
                                             (warmacs/restart-emacs (cons "--timed-requires" args))))
    "T" '("restart-emacs-adv-timers" . (lambda (&optional args)
	                                 (interactive)
                                         (warmacs/restart-emacs (cons "--adv-timers" args))))
    "q" '("prompt-kill-emacs" . (lambda (&optional args)
	                          (interactive)
                                  (warmacs/kill-emacs t args)))))

(provide 'warmacs-restart)
