;;; warmacs-files.el -*- lexical-binding: t; -*-

;; File Management

;;;###autoload
(defun warmacs/delete-file (file)
  "Delete a file with y-or-n-p prompt"
  (interactive "fDelete file: ")
  (if (y-or-n-p (format "Are you sure you want to delete %s?" file))
      (progn
	(f-delete file)
	(message "Deleted %s" file))
    (message "Canceled")))

;;;###autoload
(defun warmacs/delete-current-buffer-file ()
    "Delete the file associated with the current buffer and kill the buffer, with y-or-n-p prompts"
    (interactive)
    (let ((filename (f-this-file)))
      (when filename
	(if (y-or-n-p (format "Are you sure you want to delete %s?" filename))
	    (progn
	      (f-delete filename)
	      (message "Deleted %s" filename)
	      (when (y-or-n-p (format "Kill buffer %s?" (buffer-name)))
		(kill-buffer)))
	  (message "Canceled")))))

(elpaca nil ;; defer
  (warmacs/leader-menu "Files" "f"
    "f" #'find-file
    "d" #'(warmacs/delete-file :wk "Delete file")
    "D" #'(warmacs/delete-current-buffer-file :wk "Delete current file")
    "r" #'consult-recent-file))

(provide 'warmacs-files)
