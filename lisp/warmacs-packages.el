;; warmacs-packages.el -*- lexical-binding: t; -*-

;;
;; elpaca
;; We use elpaca to manage our packgaes

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Uncomment for systems which cannot create symlinks:
;; (elpaca-no-symlink-mode)

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable Elpaca support for use-package's :ensure keyword.
  (elpaca-use-package-mode)
  ;; Assume :ensure t unless specified otherwise
  (setq use-package-always-ensure t
        use-package-always-defer t))

;; Block until current queue processed.
;; Necessary to use the Elpaca's `:ensure` support after this point
(elpaca-wait)

;; setup emacs-async and async-byte-compilation
(use-package async
  :config
  (require 'async-bytecomp)
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1))

;; Ensure gcmh-mode. This is activated by warmacs-first-buffer-hook
(use-package gcmh)

;;
;; Evil mode
(use-package evil
  :demand t
  :custom
  (evil-want-integration t) ;; This is optional since it's already set to t by default.
  (evil-want-keybinding nil) ;; set to nil to use evil-collection
  :config
  (evil-mode 1))

(use-package evil-collection
  :demand t
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package evil-lisp-state
  :after evil)

;; Unload and install seq, we need seq version higher than the one bundled with emacs

;; Elpaca checks for version requirement
;; Magit needs seq version higher than the one bundled with emacs 30.1
(defun +elpaca-unload-seq (e)
  (and (featurep 'seq) (unload-feature 'seq t))
  (elpaca--continue-build e))

(defun +elpaca-seq-build-steps ()
  (append (butlast (if (file-exists-p (expand-file-name "seq" elpaca-builds-directory))
                       elpaca--pre-built-steps elpaca-build-steps))
          (list '+elpaca-unload-seq 'elpaca--activate-package)))

(use-package seq
  :ensure `(seq :build ,(+elpaca-seq-build-steps)))


;; Setup basic packages
(use-package s)
(use-package dash)
(use-package f)
(use-package jsonrpc)

;; In order to turn off elpaca-use-package-mode for a given declaration, specify :ensure nil:
;; Note this will cause the declaration to be interpreted immediately (not deferred).
;; `emacs' is a pseudo-feature which can be used to configure built-in functionality.
(use-package emacs
  :ensure nil
  :config (setq ring-bell-function #'ignore))

(provide 'warmacs-packages)
