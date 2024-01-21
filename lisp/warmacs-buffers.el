;; warmacs-buffers.el -*- lexical-binding: t; -*-

(elpaca nil ;; defer
  (warmacs/leader-menu "Buffers" "b"
    "b" #'switch-buffers
    "p" #'previous-buffer
    "n" #'next-buffer))

(provide 'warmacs-buffers)
