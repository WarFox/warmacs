;;; warmacs-files.el -*- lexical-binding: t; -*-

;; File Management

(use-package emacs
  :ensure nil
  :preface
  (defun warmacs/delete-file (file)
    "Delete a file with y-or-n-p prompt"
    (interactive "fDelete file: ")
    (if (y-or-n-p (format "Are you sure you want to delete %s?" file))
        (progn
          (f-delete file)
          (message "Deleted %s" file))
      (message "Cancelled")))

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
        (message "Cancelled"))))

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
            (message "Cancelled")))
      (if (y-or-n-p "Buffer is not visiting a file. Save it to a file?")
          (save-buffer)
        (message "Cancelled"))))

  (defun warmacs/copy-current-buffer-file ()
    "Copy the current buffer's file to a new location."
    (interactive)
    (if-let ((filename (buffer-file-name)))
        (let* ((new-name (read-file-name "Copy to: " (file-name-directory filename))))
          (if (y-or-n-p (format "Copy %s to %s?" filename new-name))
              (progn
                (copy-file filename new-name 1)
                (message "Copied %s to %s" filename new-name))
            (message "Cancelled")))
      (message "Buffer is not visiting a file")))

  (defun warmacs/copy-file-path ()
    "Copy the full path to the current file to kill-ring."
    (interactive)
    (let ((file-name (buffer-file-name)))
      (if file-name
          (let ((file-path (format "%s:%d" file-name (line-number-at-pos))))
            (message file-path)
            (kill-new file-path))
        (error "Buffer not visiting a file"))))

  (defun warmacs--projectile-file-path-with-line-column ()
    (when (projectile-project-p)
      (let* ((root (projectile-project-root))
             (file-path (file-relative-name buffer-file-name root)))
        (concat file-path
                ":"
                (number-to-string (line-number-at-pos))
                ":"
                (number-to-string (current-column))))))

  (defun warmacs/projectile-copy-file-path ()
    "Copy the file path with line and column number relative to project."
    (interactive)
    (let ((file-path (warmacs--projectile-file-path-with-line-column)))
      (if file-path
          (progn
            (message file-path)
            (kill-new file-path))
        (error "Buffer not visiting a file"))))

  :general
  (warmacs/leader-menu "Files" "f"
    "f" #'find-file
    "c" '("Copy File" . warmacs/copy-current-buffer-file)
    "d" '("Delete a file" . warmacs/delete-file)
    "D" '("Delete current file" . warmacs/delete-current-buffer-file)
    "r" '("Recent files" . recentf)
    "R" #'("Rename file" . warmacs/rename-current-buffer-file)
    "y" '(:ignore t :wk "Copy")
    "yy" #'warmacs/copy-file-path
    "yY" #'warmacs/projectile-copy-file-path))

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
