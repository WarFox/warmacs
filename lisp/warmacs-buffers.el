;; warmacs-buffers.el -*- lexical-binding: t; -*-

;; Buffer management

(use-package emacs
  :ensure nil
  :preface
  (defun warmacs/switch-to-buffer (buffer)
    "Switch to BUFFER."
    (interactive
     (consult-buffer))
    (switch-to-buffer buffer))

  (defun warmacs/scratch-buffer-other-window ()
    "Open scratch buffer in a new window."
    (interactive)
    (switch-to-buffer-other-window (get-scratch-buffer-create)))

  (defun warmacs/new-buffer ()
    "Create a new buffer."
    (interactive)
    (let ((buffer (generate-new-buffer "*untitled*")))
      (set-buffer-major-mode buffer)
      (set-window-buffer nil buffer)))
  :general
  (warmacs/leader-menu "Buffers" "b"
    "b" #'consult-buffer
    "d" #'kill-current-buffer
    "e" #'erase-buffer
    "h" #'dashboard-open
    "m" #'((lambda () (interactive) (warmacs/switch-to-buffer "*Messages*")) :wk "*Messages*")
    "n" #'next-buffer
    "N" #'(warmacs/new-buffer :wk "new-buffer")
    "o" #'((lambda () (interactive) (warmacs/switch-to-buffer nil)) :wk "other-buffer")
    "p" #'previous-buffer
    "r" #'rename-buffer
    "s" #'(scratch-buffer :wk "*scratch*")
    "S" #'(warmacs/scratch-buffer-other-window :wk "*scratch*-other-window")
    "w" #'((lambda () (interactive) (warmacs/switch-to-buffer "*Warnings*")) :wk "*Warnings*")
    "y" #'((lambda () (interactive) (clipboard-kill-ring-save (point-min) (point-max))) :wk "copy-buffer")))

;; Scratch buffer

(use-package persistent-scratch
  :custom
  (persistent-scratch-save-file (expand-file-name ".persistent-scratch" warmacs-cache-dir))
  (persistent-scratch-autosave-interval 60)
  (persistent-scratch-what-to-save '(point narrowing))
  :config
  (persistent-scratch-autosave-mode t)
  (persistent-scratch-setup-default))

(provide 'warmacs-buffers)
