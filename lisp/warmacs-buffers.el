;; warmacs-buffers.el -*- lexical-binding: t; -*-

(elpaca nil ;; defer
  (warmacs/leader-menu "Buffers" "b"
    "b" #'consult-buffers
    "p" #'previous-buffer
    "n" #'next-buffer
    "o" #'((lambda ()
	     (interactive)
	     (switch-to-buffer nil))
	   :which-key "other-buffer")
    "y" #'((lambda ()
	     (interactive)
	     (clipboard-kill-ring-save (point-min) (point-max)))
	   :which-key "copy-buffer")
    "s" #'scratch-buffer
    "m" #'((lambda ()
	     (interactive)
	     (switch-to-buffer "*Messages*"))
	   :which-key "*Messages*")))

(provide 'warmacs-buffers)
