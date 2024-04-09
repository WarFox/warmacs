;; early-init.el -*- lexical-binding: t; -*-

;; Emacs loads this file before init.el to initialize some settings that cannot

;; Fast startup tricks from Doom emacs
(setq
 ;; A big contributor to startup times is garbage collection. We up the gc
 ;; threshold to temporarily prevent it from running, then reset it later by
 ;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
 gc-cons-threshold most-positive-fixnum

 ;; Load byte-compiled source files. Saves us a little IO time to skip
 ;; all the mtime checks on each lookup.
 load-prefer-newer t

 ;; If non-nil, compile loaded .elc files asynchronously.
 ;; After compilation, each function definition is updated to use the
 ;; natively-compiled one.
 native-comp-jit-compilation t

 ;; In Emacs 27+, package initialization occurs before `user-init-file' is
 ;; loaded, but after `early-init-file'. Package initialization is handled by
 ;; elpaca, so we must prevent Emacs from doing it early!
 package-enable-at-startup nil

 ;; We want to start with the simplest shell available for speed
 shell-file-name "/bin/sh"

 ;;do not look for start-site.el
 site-run-file nil

 ;;do not look for default.el
 inhibit-default-init t)

;; UX: Respect DEBUG envvar as an alternative to --debug-init, and to make
;;   startup sufficiently verbose from this point on.
(when (getenv-internal "DEBUG")
  (setq init-file-debug t))

;; Reduce debug output unless we've asked for it.
(setq debug-on-error init-file-debug
      jka-compr-verbose init-file-debug)

(when init-file-debug
  (message "Emacs is starting in debug mode...")
  (setq-default use-package-verbose t
                use-package-expand-minimally nil
                use-package-compute-statistics t
                debug-on-error t)

  ;; Start the built-in profiler during debug mode
  (when (fboundp 'profiler-start)
    (profiler-start 'cpu)
    (add-hook 'after-init-hook #'profiler-report)))

;;
;;; Bootstrap

;; Ensure warmacs is running out of this file's directory
(setq user-emacs-directory
      (file-name-directory (file-truename load-file-name)))

(let ((load-suffixes '(".elc" ".el")))
  ;; ;; Load the core of Warmacs
  (load (expand-file-name "lisp/warmacs" user-emacs-directory)
	nil (not init-file-debug) nil 'must-suffix)
  ;; Undo settings
  (setq load-prefer-newer t)
  ;; 16MB
  (setq gc-cons-threshold (* 16 1024 1024)))

(require 'warmacs-start)


;; Continue to load the user's init file
;; Set user-init-file based on warmcas-user-dir
;; ~/warmacs.d/init.el
(setq user-init-file (expand-file-name "init.el" warmacs-user-dir))
(load user-init-file 'noerror (not init-file-debug) nil 'must-suffix)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
