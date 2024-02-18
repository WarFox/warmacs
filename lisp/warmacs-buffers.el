;; warmacs-buffers.el -*- lexical-binding: t; -*-

;; Buffer management

(defun warmacs/switch-to-buffer (buffer)
  "Switch to BUFFER."
  (interactive
     (consult-buffer))
  (switch-to-buffer buffer))

(defun warmacs/scratch-buffer-other-window ()
  "Open scratch buffer in a new window."
  (interactive)
  (switch-to-buffer-other-window (get-buffer-create "*scratch*")))

(warmacs/leader-menu "Buffers" "b"
  "b" #'consult-buffer
  "d" #'kill-current-buffer
  "e" #'erase-buffer
  "h" #'dashboard-open
  "m" #'((lambda () (interactive) (warmacs/switch-to-buffer "*Messages*")) :which-key "*Messages*")
  "n" #'next-buffer
  "o" #'((lambda () (interactive) (warmacs/switch-to-buffer nil)) :which-key "other-buffer")
  "p" #'previous-buffer
  "r" #'rename-buffer
  "s" #'scratch-buffer
  "S" #'(warmacs/scratch-buffer-other-window :which-key "scratch-other-window")
  "y" #'((lambda () (interactive) (clipboard-kill-ring-save (point-min) (point-max))) :which-key "copy-buffer"))

(provide 'warmacs-buffers)
