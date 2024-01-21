;; warmacs-restart.el -*- lexical-binding: t; -*-

;; Quit/Restart menu

(use-package restart-emacs
  :init
  (progn
    (defun warmacs/restart-emacs (&optional args)
    (interactive)
    (restart-emacs))

    (defun warmacs/kill-emacs (prompt &optional args)
    (interactive)
    (if (not prompt)
        (save-some-buffers nil t))
    (kill-emacs args)))
  :config
  (warmacs/leader-menu "quit" "q"
    "d" #'((lambda (&optional args)
            (interactive)
            (warmacs/restart-emacs (cons "--debug-init" args)))
           :which-key "restart-emacs-debug-init")
    "R" #'warmacs/restart-emacs
    "t" #'((lambda (&optional args)
             (interactive)
             (warmacs/restart-emacs (cons "--timed-requires" args)))
            :which-key "restart-emacs-timed-requires")
    "T" #'((lambda (&optional args)
	     (interactive)
            (warmacs/restart-emacs (cons "--adv-timers" args)))
           :which-key "restart-emacs-adv-timers")
    "q" #'((lambda (&optional args)
	     (interactive)
            (warmacs/kill-emacs t args))
           :which-key "prompt-kill-emacs")))

(provide 'warmacs-restart)
