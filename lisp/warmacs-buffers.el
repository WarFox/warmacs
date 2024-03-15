;; warmacs-buffers.el -*- lexical-binding: t; -*-

;; Buffer management

(use-package emacs
  :ensure nil
  :config
  (defun warmacs/switch-to-buffer (buffer)
    "Switch to BUFFER."
    (interactive
     (consult-buffer))
    (switch-to-buffer buffer))

  (defun warmacs/scratch-buffer-other-window ()
    "Open scratch buffer in a new window."
    (interactive)
    (switch-to-buffer-other-window (get-buffer-create "*scratch*")))
  :general
  (warmacs/leader-menu "Buffers" "b"
    "b" #'consult-buffer
    "d" #'kill-current-buffer
    "e" #'erase-buffer
    "h" #'dashboard-open
    "m" #'((lambda () (interactive) (warmacs/switch-to-buffer "*Messages*")) :wk "*Messages*")
    "n" #'next-buffer
    "o" #'((lambda () (interactive) (warmacs/switch-to-buffer nil)) :wk "other-buffer")
    "p" #'previous-buffer
    "r" #'rename-buffer
    "s" #'scratch-buffer
    "S" #'(warmacs/scratch-buffer-other-window :wk "scratch-other-window")
    "w" #'((lambda () (interactive) (warmacs/switch-to-buffer "*Warnings*")) :wk "*Warnings*")
    "y" #'((lambda () (interactive) (clipboard-kill-ring-save (point-min) (point-max))) :wk "copy-buffer")))

(provide 'warmacs-buffers)
