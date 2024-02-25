;; warmacs.el -*- lexical-binding: t; -*-

;; Warmacs is a modular, portable, and modern Emacs configuration
;; This file is not part of GNU Emacs.

;; Author: Deepu Mohan Puthrote <git@deepumohan.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; Core of Warmacs

;;
;;; Directory variables

(defconst warmacs-emacs-dir user-emacs-directory
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")

(defconst warmacs-core-dir  (expand-file-name "lisp/" warmacs-emacs-dir)
  "The root directory of Warmacs's core files. Must end with a slash.")

(defconst warmacs-layers-dir (expand-file-name "layers/" warmacs-emacs-dir)
  "The root directory for Warmacs's layers. Must end with a slash.")

(defconst warmacs-user-dir
  (expand-file-name
   (if-let (warmacsdir (getenv "WARMACSDIR"))
       (file-name-as-directory warmacsdir)
     (or (let ((xdgdir
                (file-name-concat
                 (or (getenv-internal "XDG_CONFIG_HOME")
                     "~/.config")
                 "warmacs/")))
           (if (file-directory-p xdgdir) xdgdir))
         "~/.warmacs.d/")))

  "Where your private configuration is placed.

Defaults to ~/.config/warmacs, ~/.warmacs.d or the value of the WARMACSDIR envvar;
whichever is found first. Must end in a slash.")

;;
;;;; Global Vars

(defvar warmacs-active-layers nil
  "A list of layers that are active.")

(defvar warmacs-init-time nil
  "The time it took, in seconds, for Warmacs to initialize.")

;; Make sure warmacs directories are in `load-path'
(add-to-list 'load-path warmacs-layers-dir)
(add-to-list 'load-path warmacs-core-dir)

;;; Load Warmacs's stdlib
(require 'warmacs-lib)

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


;; This section is copied from doom-emacs
(unless (daemonp)
  ;; PERF: `file-name-handler-alist' is consulted on each call to `require',
  ;;   `load', or various file/io functions (like `expand-file-name' or
  ;;   `file-remote-p'). You get a noteable boost to startup time by unsetting
  ;;   or simplifying its value.
  (let ((old-value (default-toplevel-value 'file-name-handler-alist)))
    (set-default-toplevel-value
     'file-name-handler-alist
     ;; HACK: If the bundled elisp for this Emacs install isn't byte-compiled
     ;;   (but is compressed), then leave the gzip file handler there so Emacs
     ;;   won't forget how to read read them.
     ;;
     ;;   calc-loaddefs.el is our heuristic for this because it is built-in to
     ;;   all supported versions of Emacs, and calc.el explicitly loads it
     ;;   uncompiled. This ensures that the only other, possible fallback would
     ;;   be calc-loaddefs.el.gz.
     (if (eval-when-compile
           (locate-file-internal "calc-loaddefs.el" load-path))
         nil
       (list (rassq 'jka-compr-handler old-value))))
    ;; Make sure the new value survives any current let-binding.
    (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
    ;; Remember it so it can be reset where needed.
    (put 'file-name-handler-alist 'initial-value old-value)
    ;; COMPAT: ...but restore `file-name-handler-alist' later, because it is
    ;;   needed for handling encrypted or compressed files, among other things.
    (add-hook 'emacs-startup-hook
              (defun warmacs--reset-file-handler-alist-h ()
                (set-default-toplevel-value
                 'file-name-handler-alist
                 ;; Merge instead of overwrite because there may have been changes to
                 ;; `file-name-handler-alist' since startup we want to preserve.
                 (delete-dups (append file-name-handler-alist old-value))))))

  (unless noninteractive
    ;; PERF: Resizing the Emacs frame (to accommodate fonts that are smaller or
    ;;   larger than the system font) appears to impact startup time
    ;;   dramatically. The larger the delta in font size, the greater the delay.
    ;;   Even trivial deltas can yield a ~1000ms loss, though it varies wildly
    ;;   depending on font size.
    (setq frame-inhibit-implied-resize t)

    ;; PERF,UX: Reduce *Message* noise at startup. An empty scratch buffer (or
    ;;   the dashboard) is more than enough, and faster to display.
    (setq inhibit-startup-screen t
          inhibit-startup-echo-area-message user-login-name)

    ;; PERF,UX: Remove "For information about GNU Emacs..." message at startup.
    ;;   It's redundant with our dashboard and incurs a premature redraw.
    (advice-add #'display-startup-echo-area-message :override #'ignore)

    ;; PERF: Suppress the vanilla startup screen completely. We've disabled it
    ;;   with `inhibit-startup-screen', but it would still initialize anyway.
    ;;   This involves some file IO and/or bitmap work (depending on the frame
    ;;   type) that we can no-op for a free 50-100ms boost in startup time.
    (advice-add #'display-startup-screen :override #'ignore)

    ;; PERF: Shave seconds off startup time by starting the scratch buffer in
    ;;   `fundamental-mode', rather than, say, `org-mode' or `text-mode', which
    ;;   pull in a ton of packages.
    (setq initial-major-mode 'fundamental-mode
          initial-scratch-message nil)

    (unless init-file-debug
      ;; PERF,UX: Site files tend to use `load-file', which emits "Loading X..."
      ;;   messages in the echo area. Writing to the echo-area triggers a
      ;;   redisplay, which can be expensive during startup. This may also cause
      ;;   an flash of white when creating the first frame.
      (define-advice load-file (:override (file) silence)
        (load file nil 'nomessage))
      ;; COMPAT: But undo our `load-file' advice later, as to limit the scope of
      ;;   any edge cases it could induce.
      (define-advice startup--load-user-init-file (:before (&rest _) undo-silence)
        (advice-remove #'load-file #'load-file@silence))

      ;; PERF: `load-suffixes' and `load-file-rep-suffixes' are consulted on
      ;;   each `require' and `load'. Warmacs won't load any modules this early, so
      ;;   omit .so for a tiny startup boost. Is later restored in warmacs-start.
      (put 'load-suffixes 'initial-value (default-toplevel-value 'load-suffixes))
      (put 'load-file-rep-suffixes 'initial-value (default-toplevel-value 'load-file-rep-suffixes))
      (set-default-toplevel-value 'load-suffixes '(".elc" ".el"))
      (set-default-toplevel-value 'load-file-rep-suffixes '(""))
      ;; COMPAT: Undo any problematic startup optimizations; from this point, I
      ;;   make no assumptions about what might be loaded in userland.
      (add-hook 'warmacs-before-init-hook
                (defun warmacs--reset-load-suffixes-h ()
                  (setq load-suffixes (get 'load-suffixes 'initial-value)
        		load-file-rep-suffixes (get 'load-file-rep-suffixes 'initial-value))))

      ;; PERF: warmacs uses `defcustom' to indicate variables that users are
      ;;   expected to reconfigure. Trouble is it fires off initializers meant
      ;;   to accommodate any user attempts to configure them before they were
      ;;   defined. This is unnecessary before $WARMACSDIR/init.el is loaded, so I
      ;;   disable them until it is.
      (setq custom-dont-initialize t)
      (add-hook 'warmacs-before-init-hook
                (defun warmacs--reset-custom-dont-initialize-h ()
        	  (setq custom-dont-initialize nil)))

      ;; PERF: The mode-line procs a couple dozen times during startup. This is
      ;;   normally quite fast, but disabling the default mode-line and reducing
      ;;   the update delay timer seems to shave off ~30-50ms.
      (put 'mode-line-format 'initial-value (default-toplevel-value 'mode-line-format))
      (setq-default mode-line-format nil)
      (dolist (buf (buffer-list))
        (with-current-buffer buf (setq mode-line-format nil)))

      ;; PERF,UX: Premature redisplays can substantially affect startup times and
      ;;   produce ugly flashes of unstyled Emacs.
      (setq-default inhibit-redisplay t
                    inhibit-message t)
      ;; COMPAT: Then reset with after-init-hook and advice, because
      ;;   `startup--load-user-init-file' will never be interrupted by
      ;;   errors.  And if these settings are left set, Emacs could
      ;;   appear frozen or garbled.
      (defun warmacs--reset-inhibited-vars-h ()
        (setq-default inhibit-redisplay nil
                      ;; Inhibiting `message' only prevents redraws and
                      inhibit-message nil)
        (redraw-frame))
      (add-hook 'after-init-hook #'warmacs--reset-inhibited-vars-h)
      (define-advice startup--load-user-init-file (:after (&rest _) undo-inhibit-vars)
        (when init-file-had-error
          (message "Error during startup; resetting inhibited vars")
          (warmacs--reset-inhibited-vars-h))
        (unless (default-toplevel-value 'mode-line-format)
          (setq-default mode-line-format (get 'mode-line-format 'initial-value))))

      ;; PERF: Disable the UI elements by default, so that there's less
      ;;   for the frame to initialize. However, the toolbar is still populated
      ;;   regardless, so I lazy load it until tool-bar-mode is actually used.
      (advice-add #'tool-bar-setup :override #'ignore)
      (define-advice startup--load-user-init-file (:before (&rest _) defer-tool-bar-setup)
        (advice-remove #'tool-bar-setup #'ignore)
        (add-hook 'tool-bar-mode (tool-bar-setup))))))

(add-hook 'warmacs-before-init-hook
          (defun warmacs--begin-init-h ()
            "Begin the startup process."
            ;; HACK: Ensure OS checks are as fast as possible (given their ubiquity).
            (setq features (cons :system (delq :system features)))
            ;; Remember these variables' initial values, so we can safely reset them at
            ;; a later time, or consult them without fear of contamination.
            (dolist (var '(exec-path load-path process-environment))
              (put var 'initial-value (default-toplevel-value var)))))

(add-hook 'warmacs-after-init-hook
          (defun warmacs--end-init-h ()
            "Set `warmacs-init-time'."
            (setq warmacs-init-time (float-time (time-subtract (current-time) before-init-time)))))

(unless noninteractive
  ;; This is the absolute latest a hook can run in Emacs' startup process.
  (define-advice command-line-1 (:after (&rest _) run-after-init-hook)
    (run-hooks 'warmacs-after-init-hook)))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

(provide 'warmacs)
