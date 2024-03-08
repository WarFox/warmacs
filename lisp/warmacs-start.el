;; warmacs-start.el -*- lexical-binding: t; -*-

;; Start warmacs

;;
;;; Custom hooks

(defcustom warmacs-first-input-hook ()
  "Transient hooks run before the first user input."
  :type 'hook
  :local 'permanent-local
  :group 'warmacs)

(defcustom warmacs-first-file-hook ()
  "Transient hooks run before the first interactively opened file."
  :type 'hook
  :local 'permanent-local
  :group 'warmacs)

(defcustom warmacs-first-buffer-hook ()
  "Transient hooks run before the first interactively opened buffer."
  :type 'hook
  :local 'permanent-local
  :group 'warmacs)


;;; Reasonable defaults for interactive sessions

;;; Runtime optimizations
(setq-default
 ;; don't do case-insensitive search in auto-mode-alist
 auto-mode-case-fold nil

 bidi-display-reordering 'left-to-right
 bidi-paragraph-direction 'left-to-right

 bidi-inhibit-bpa t  ; Emacs 27+ only

 cursor-in-non-selected-windows nil

 highlight-nonselected-windows nil)

(setq-default
 fast-but-imprecise-scrolling t
 pixel-scroll-precision-mode t)

(setq ffap-machine-p-known 'reject)

(setq idle-update-delay 1.0)  ; default is 0.5

(setq inhibit-compacting-font-caches t)

(if (boundp 'pgtk-wait-for-event-timeout)
  (setq pgtk-wait-for-event-timeout 0.001))

;; (setq read-process-output-max (* 64 1024))  ; 64kb
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(setq redisplay-skip-fontification-on-input t)

;; The GC introduces annoying pauses and stuttering into our Emacs experience,
;; so we use `gcmh' to stave off the GC while we're using Emacs, and provoke it
;; when it's idle. However, if the idle delay is too long, we run the risk of
;; runaway memory usage in busy sessions. If it's too low, then we may as well
;; not be using gcmh at all.
(setq gcmh-idle-delay 'auto  ; default is 15s
      gcmh-auto-idle-delay-factor 10
      gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb
(add-hook 'warmacs-first-buffer-hook #'gcmh-mode)

;;; Disable UI elements early
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

(with-system darwin

  (mapc
    (lambda (hook)
      (add-hook hook
        (defun warmacs-restore-menu-bar-in-gui-frames-h (&optional frame)
          (when-let (frame (or frame (selected-frame)))
            (when (display-graphic-p frame)
              (set-frame-parameter frame 'menu-bar-lines 1))))))
    '(window-setup-hook after-make-frame-functions))

  ;; Enable built-in trash support via finder API if available (only on Emacs
  ;; macOS Port)
  (when (boundp 'mac-system-move-file-to-trash-use-finder)
    (setq mac-system-move-file-to-trash-use-finder t))

  ;; Use GNU ls as `gls' from `coreutils' if available.  Add `(setq
  ;; dired-use-ls-dired nil)' to your config to suppress the Dired warning when
  ;; not using GNU ls.
  (let ((gls (executable-find "gls")))
    (when gls
      (setq insert-directory-program gls))))


;;; Encodings
;; Contrary to what many Emacs users have in their configs, you don't need more
;; than this to make UTF-8 the default coding system:
(set-language-environment "UTF-8")
;; ...but `set-language-environment' also sets `default-input-method', which is
;; a step too opinionated.
(setq default-input-method nil)

;; Before init hooks
(run-hooks 'warmacs-before-init-hook)

;;
;; Start Warmacs
(require 'warmacs-packages)
;; keybindings, search and completions must be loaded before everything eles
(require 'warmacs-keybindings)
(require 'warmacs-ui)
(require 'warmacs-completions)
;; load after the keybindings, search and completions
(require 'warmacs-vcs)
(require 'warmacs-treemacs)
(require 'warmacs-editor)
(require 'warmacs-buffers)
(require 'warmacs-files)
(require 'warmacs-projects)
(require 'warmacs-dashboard)
(require 'warmacs-windows)
(require 'warmacs-restart)
(require 'warmacs-ai)
(require 'warmacs-help)
(require 'warmacs-tree-sitter)

;; Run Hooks

;; run warmacs-first-input-hook upon first input
(add-hook 'pre-command-hook
	  (defun warmacs-run-first-input-hook-h ()
	    (remove-hook 'pre-command-hook #'warmacs-run-first-input-hook-h)
	    (run-hooks 'warmacs-first-input-hook)))
;;
;; ;; run warmacs-first-file-hook upon first file
(add-hook 'find-file-hook
	  (defun warmacs-run-first-file-hook-h ()
	    (remove-hook 'find-file-hook #'warmacs-run-first-file-hook-h)
	    (run-hooks 'warmacs-first-file-hook)))
;;
;; ;; run warmacs-first-buffer-hook upon first buffer
(add-hook 'buffer-list-update-hook
	  (defun warmacs-run-first-buffer-hook-h ()
	    (remove-hook 'buffer-list-update-hook #'warmacs-run-first-buffer-hook-h)
	    (run-hooks 'warmacs-first-buffer-hook)))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

(provide 'warmacs-start)
