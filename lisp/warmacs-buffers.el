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
    "m" '("*Messages*" . (lambda () (interactive) (warmacs/switch-to-buffer "*Messages*")))
    "n" #'next-buffer
    "N" '("new-buffer" . warmacs/new-buffer)
    "o" '("other-buffer" . (lambda () (interactive) (warmacs/switch-to-buffer nil)))
    "p" #'previous-buffer
    "r" #'rename-buffer
    "s" '("*scratch*" . scratch-buffer)
    "S" '("*scratch*-other-window" . warmacs/scratch-buffer-other-window)
    "w" '("*Warnings*" . (lambda () (interactive) (warmacs/switch-to-buffer "*Warnings*")))
    "y" '("copy-buffer" . (lambda () (interactive) (clipboard-kill-ring-save (point-min) (point-max))))))

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
