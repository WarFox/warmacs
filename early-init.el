;; early-init.el -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

;; Fast startup tricks from Doom emacs
;; A big contributor to startup times is garbage collection. We up the gc
;; threshold to temporarily prevent it from running, then reset it later by
;; enabling `gcmh-mode'. Not resetting it will cause stuttering/freezes.
(setq
 gc-cons-threshold most-positive-fixnum

;; Load byte-compiled source files. Saves us a little IO time to skip
;; all the mtime checks on each lookup.
 load-prefer-newer nil

;; Prevent unwanted runtime compilation for gccemacs (native-comp)
;; users; packages are compiled ahead-of-time when they are installed
;; and site files are compiled when gccemacs is installed.
native-comp-deferred-compilation nil

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Package initialization is handled by
;; straight.el, so we must prevent Emacs from doing it early!
package-enable-at-startup nil

;; We want to start with the simplest shell available for speed
shell-file-name "/bin/sh")

;;
;;; Bootstrap

;; Ensure warmacs is running out of this file's directory
(setq user-emacs-directory (file-name-directory load-file-name))

(let ((load-suffixes '(".elc" ".el")))
  ;; ;; Load the core of Warmacs
  (load (expand-file-name "lisp/warmacs" user-emacs-directory)
    nil (not init-file-debug) nil 'must-suffix)
  ;; Undo settings
  (setq load-prefer-newer t)
  ;; 16MB
  (setq gc-cons-threshold (* 16 1024 1024)))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
