;;; warmacs-files.el -*- lexical-binding: t; -*-

;; File Management

(use-package emacs
  :ensure nil
  :config
  (defun warmacs/delete-file (file)
    "Delete a file with y-or-n-p prompt"
    (interactive "fDelete file: ")
    (if (y-or-n-p (format "Are you sure you want to delete %s?" file))
        (progn
          (f-delete file)
          (message "Deleted %s" file))
      (message "Canceled")))

  (defun warmacs/delete-current-buffer-file ()
    "Delete the file associated with the current buffer and kill the
buffer, with y-or-n-p prompts"
    (interactive)
    (when-let ((filename (f-this-file)))
      (if (y-or-n-p (format "Are you sure you want to delete %s?" filename))
          (progn
            (f-delete filename)
            (message "Deleted %s" filename)
            (when (y-or-n-p (format "Kill buffer %s?" (buffer-name)))
              (kill-buffer)))
        (message "Canceled"))))

  (defun warmacs/rename-current-buffer-file ()
    "Rename the current and it's visiting file. If the buffer isn't
visiting a file, ask if it should be saved to a file, or just
renamed."
    (interactive)
    (if-let ((filename (buffer-file-name)))
        (let* ((new-name (read-file-name "New name: " (file-name-directory filename))))
          (if (y-or-n-p (format "Rename %s to %s?" filename new-name))
              (progn
                (rename-file filename new-name 1)
                (set-visited-file-name new-name)
                (set-buffer-modified-p nil)
                (message "Renamed %s to %s" filename new-name))
            (message "Canceled")))
      (if (y-or-n-p "Buffer is not visiting a file. Save it to a file?")
          (save-buffer)
        (message "Canceled"))))

  :general
  (warmacs/leader-menu "Files" "f"
    "f" #'find-file
    "d" #'(warmacs/delete-file :wk "Delete file")
    "D" #'(warmacs/delete-current-buffer-file :wk "Delete current file")
    "r" #'(recentf :wk "Recent files")
    "R" #'(warmacs/rename-current-buffer-file :wk "Rename file")))

(use-package recentf
  :ensure nil
  :custom
  (recentf-auto-cleanup 'never)
  (recentf-max-menu-items 15)
  (recentf-max-saved-items 100)
  (recentf-save-file (expand-file-name "recentf" warmacs-cache-dir))
  :config
  (add-to-list 'recentf-exclude (expand-file-name "*" warmacs-cache-dir))
  (recentf-mode 1))

(provide 'warmacs-files)
