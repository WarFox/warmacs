;; warmacs.el -*- lexical-binding: t; -*-

;; Warmacs is a modular, portable, and modern Emacs configuration
;; This file is not part of GNU Emacs.

;; Author: Deepu Mohan Puthrote <git@deepumohan.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; Core of Warmacs

;;; Load Warmacs's stdlib
(add-to-list 'load-path (file-name-directory load-file-name))
(require 'warmacs-lib)

;; Start up Optimisations

(unless (daemonp)

  (let ((old-value (default-toplevel-value 'file-name-handler-alist)))
    (setq file-name-handler-alist
      (if (eval-when-compile
            (locate-file-internal "calc-loaddefs.el" load-path))
        nil
        (list (rassq 'jka-compr-handler old-value))))
    (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
    (put 'file-name-handler-alist 'initial-value old-value)
    (add-hook 'emacs-startup-hook :depth 101
      (defun warmacs--reset-file-handler-alist-h ()
        (setq file-name-handler-alist
          (delete-dups (append file-name-handler-alist old-value))))))

  (unless noninteractive
    (setq frame-inhibit-implied-resize t)

    (setq inhibit-startup-screen t
          inhibit-startup-echo-area-message user-login-name)

    (advice-add #'display-startup-echo-area-message :override #'ignore)
    (advice-add #'display-startup-screen :override #'ignore)

    (setq initial-major-mode 'fundamental-mode
          initial-scratch-message nil)

    (unless initial-window-system
      (define-advice tty-run-terminal-initialization (:override (&rest _) defer)
        (advice-remove #'tty-run-terminal-initialization #'tty-run-terminal-initialization@defer)
        (add-hook 'window-setup-hook
                  (warmacs-partial #'tty-run-terminal-initialization
                                (selected-frame) nil t))))

    (unless init-file-debug
      (define-advice load-file (:override (file) silence)
        (load file nil 'nomessage))
      (define-advice startup--load-user-init-file (:before (&rest _) undo-silence)
        (advice-remove #'load-file #'load-file@silence))

      (put 'load-suffixes 'initial-value (default-toplevel-value 'load-suffixes))
      (put 'load-file-rep-suffixes 'initial-value (default-toplevel-value 'load-file-rep-suffixes))
      (set-default-toplevel-value 'load-suffixes '(".elc" ".el"))
      (set-default-toplevel-value 'load-file-rep-suffixes '(""))

      (add-hook 'warmacs-before-init-hook
        (defun warmacs--reset-load-suffixes-h ()
          (setq load-suffixes (get 'load-suffixes 'initial-value)
                load-file-rep-suffixes (get 'load-file-rep-suffixes 'initial-value))))

      (setq custom-dont-initialize t)
      (add-hook 'warmacs-before-init-hook
        (defun warmacs--reset-custom-dont-initialize-h ()
          (setq custom-dont-initialize nil)))

      (put 'mode-line-format 'initial-value (default-toplevel-value 'mode-line-format))
      (setq-default mode-line-format nil)
      (dolist (buf (buffer-list))
        (with-current-buffer buf (setq mode-line-format nil)))

      (setq-default inhibit-redisplay t
                    inhibit-message t)

      (defun warmacs--reset-inhibited-vars-h ()
        (setq-default inhibit-redisplay nil
                      ;; Inhibiting `message' only prevents redraws and
                      inhibit-message nil)
        (redraw-frame))
      (add-hook 'after-init-hook #'warmacs--reset-inhibited-vars-h)
      (define-advice startup--load-user-init-file (:after (&rest _) undo-inhibit-vars)
        (when init-file-had-error
          (warmacs--reset-inhibited-vars-h))
        (unless (default-toplevel-value 'mode-line-format)
          (setq-default mode-line-format (get 'mode-line-format 'initial-value))))

      (advice-add #'tool-bar-setup :override #'ignore)
      (define-advice startup--load-user-init-file (:before (&rest _) defer-tool-bar-setup)
        (advice-remove #'tool-bar-setup #'ignore)
        (add-transient-hook! 'tool-bar-mode (tool-bar-setup))))))

;;
;;; Custom hooks

(defcustom warmacs-before-init-hook ()
  "before init"
  :group 'warmacs
  :type 'hook)

(defcustom warmacs-after-init-hook ()
  "after init"
  :group 'warmacs
  :type 'hook)

;;
;;; Directory variables

(defconst warmacs-emacs-dir user-emacs-directory
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")

(defconst warmacs-core-dir (file-name-directory load-file-name)
  "The root directory of Warmacs's core files. Must end with a slash.")

(defconst warmacs-layers-dir (expand-file-name "layers/" warmacs-emacs-dir)
  "The root directory for Warmacs's layers. Must end with a slash.")

(add-to-list 'load-path warmacs-layers-dir)
(add-to-list 'load-path warmacs-core-dir)

;;
;;;; Global Vars

(defvar warmacs-active-layers nil
  "A list of enabled layers.")

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

(provide 'warmacs)
