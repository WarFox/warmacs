;;; warmacs-windows.el -- Warmacs Windows -*- lexical-binding: t; -*-

;;; Commentary:
;; Window management

;;; Code:
(use-package ace-window
  :general
  (warmacs/leader-menu "Windows" "w"
    "W"  #'ace-window))

;; Prefer popup windows for buffers that cause mild annoyance
(use-package popwin
  :config
  (popwin-mode 1))

(use-package emacs
  :ensure nil
  :config
  (defun warmacs/delete-window (&optional arg)
    "Delete the current window.
  If the universal prefix argument is used then kill the buffer too."
    (interactive "P")
    (if (equal '(4) arg)
	(kill-buffer-and-window)
      (delete-window)))

  ;; from https://gist.github.com/3402786
  (defun warmacs/toggle-maximize-buffer ()
    "Maximize buffer."
    (interactive)
    (save-excursion
      (if (and (= 1 (length (cl-remove-if
			     (lambda (w)
			       (or (and (fboundp 'treemacs-is-treemacs-window?)
					(treemacs-is-treemacs-window? w))
				   (and (bound-and-true-p neo-global--window)
					(eq neo-global--window w))))
			     (window-list))))
	       (assoc ?_ register-alist))
	  (jump-to-register ?_)
	(window-configuration-to-register ?_)
	(delete-other-windows))))
  :general
  (warmacs/leader-menu "Windows" "w"
    "d"  #'warmacs/delete-window
    "f"  #'follow-mode
    "F"  #'make-frame
    "H"  #'evil-window-move-far-left
    "<S-left>"  #'evil-window-move-far-left
    "h"  #'evil-window-left
    "<left>"  #'evil-window-left
    "J"  #'evil-window-move-very-bottom
    "<S-down>"  #'evil-window-move-very-bottom
    "j"  #'evil-window-down
    "<down>"  #'evil-window-down
    "K"  #'evil-window-move-very-top
    "<S-up>"  #'evil-window-move-very-top
    "k"  #'evil-window-up
    "<up>"  #'evil-window-up
    "L"  #'evil-window-move-far-right
    "<S-right>"  #'evil-window-move-far-right
    "l"  #'evil-window-right
    "<right>"  #'evil-window-right
    "m"  #'warmacs/toggle-maximize-buffer
    "o"  #'other-frame
    "s"  #'split-window-below
    "S"  #'split-window-below-and-focus
    "-"  #'split-window-below
    "U"  #'winner-redo
    "u"  #'winner-undo
    "v"  #'split-window-right
    "V"  #'split-window-right-and-focus
    "w"  #'other-window
    "x"  #'kill-buffer-and-window
    "/"  #'split-window-right
    "="  #'balance-windows-area))

(provide 'warmacs-windows)
