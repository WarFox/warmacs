;; warmacs.el -*- lexical-binding: t; -*-

;; Start up Optimisations

(unless (or (daemonp)
          noninteractive
          init-file-debug)

  ;; Premature redisplays can substantially affect startup times and produce
  ;; ugly flashes of unstyled Emacs.
  (setq-default
    inhibit-redisplay t
    inhibit-message t)

  (add-hook 'window-setup-hook
    (lambda ()
      (setq-default inhibit-redisplay nil
        inhibit-message nil)
      (redisplay)))

  ;; Site files tend to use `load-file', which emits "Loading X..." messages in
  ;; the echo area, which in turn triggers a redisplay. Redisplays can have a
  ;; substantial effect on startup times and in this case happens so early that
  ;; Emacs may flash white while starting up.
  (define-advice load-file (:override (file) silence)
    (load file nil :nomessage))

  ;; Undo our `load-file' advice above, to limit the scope of any edge cases it
  ;; may introduce down the road.
  (define-advice startup--load-user-init-file (:before (&rest _) init-warmacs)
    (advice-remove #'load-file #'load-file@silence))

  (setq

    inhibit-startup-message t ; No startup message

    highlight-nonselected-windows nil

    max-lisp-eval-depth 10000 ; default 1600 is too low

    max-specpdl-size 10000 ; default 2500 is too low

    ;; More performant rapid scrolling over unfontified regions. May cause brief
    ;; spells of inaccurate syntax highlighting right after scrolling, which should
    ;; quickly self-correct.
    fast-but-imprecise-scrolling t

    ;; Don't ping things that look like domain names.
    ffap-machine-p-known 'reject

    ;; Resizing the Emacs frame can be a terribly expensive part of changing the
    ;; font. By inhibiting this, we halve startup times, particularly when we use
    ;; fonts that are larger than the system default (which would resize the frame).
    frame-inhibit-implied-resize t

    ;; The GC introduces annoying pauses and stuttering into our Emacs experience,
    ;; so we use `gcmh' to stave off the GC while we're using Emacs, and provoke it
    ;; when it's idle. However, if the idle delay is too long, we run the risk of
    ;; runaway memory usage in busy sessions. If it's too low, then we may as well
    ;; not be using gcmh at all.
    gcmh-idle-delay 'auto  ; default is 15s
    gcmh-auto-idle-delay-factor 10
    gcmh-high-cons-threshold (* 32 1024 1024)  ; 32mb

    ;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
    idle-update-delay 1.0  ; default is 0.5

    ;; Font compacting can be terribly expensive, especially for rendering icon
    ;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
    ;; hasn't been determined, but do it there anyway, just in case. This increases
    ;; memory usage, however!
    inhibit-compacting-font-caches t

    ;; PGTK builds only: this timeout adds latency to frame operations, like
    ;; `make-frame-invisible', which are frequently called without a guard because
    ;; it's inexpensive in non-PGTK builds. Lowering the timeout from the default
    ;; 0.1 should make childframes and packages that manipulate them (like `lsp-ui',
    ;; `company-box', and `posframe') feel much snappier. See emacs-lsp/lsp-ui#613.
    pgtk-wait-for-event-timeout 0.001

    ;; Increase how much is read from processes in a single chunk (default is 4kb).
    ;; This is further increased elsewhere, where needed (like our LSP layer).
    read-process-output-max (* 64 1024)  ; 64kb

    ;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
    ;; receiving input, which should help a little with scrolling performance.
    redisplay-skip-fontification-on-input t))

;;
;;; Directory variables

(defvar warmacs-emacs-dir user-emacs-directory
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")

(defconst warmacs-core-dir (file-name-directory load-file-name)
  "The root directory of Warmacs's core files. Must end with a slash.")

(defvar warmacs-layers-dir (expand-file-name "layers/" warmacs-emacs-dir)
  "The root directory for Warmacs's modules. Must end with a slash.")

;; elpaca

(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
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
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
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
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

;;When installing a package which modifies a form used at the top-level
;;(e.g. a package which adds a use-package key word),
;;use `elpaca-wait' to block until that package has been installed/configured.
;;For example:
;;(use-package general :demand t)
;;(elpaca-wait)

;; Expands to: (elpaca evil (use-package evil :demand t))
(use-package evil
  :demand t
  :config
  (evil-mode))

;; setup keybindings
(use-package which-key
  :config
  (which-key-mode 1)
  :custom
  (which-key-idle-delay 0.1)
  :diminish
  which-key-mode)

;; general
(use-package general
  :custom
  (general-use-package-emit-autoloads t)
  :init
  (setq
    warmacs-leader-key "SPC"
    warmacs-local-leader-key ",")
  (general-evil-setup)
  :config
  ;; Spacemacs-like menu
  ;; https://gist.github.com/progfolio/1c96a67fcec7584b31507ef664de36cc
  ;; https://www.reddit.com/r/emacs/comments/des3cl/comment/f2yw45k/?utm_source=share&utm_medium=web2x&context=3

  (general-create-definer warmacs/leader-keys
    :keymaps 'override
    :states  '(insert emacs normal hybrid motion visual operator)
    :prefix  warmacs-leader-key
    :non-normal-prefix (concat "C-" warmacs-leader-key)
    "" '(:ignore t :whick-key "leader key"))

  (general-create-definer warmacs/local-leader-keys
    :major-modes t
    :keymaps 'override
    :states '(emacs normal hybrid motion visual operator)
    :prefix warmacs-local-leader-key
    :non-normal-prefix (concat "C-SPC " warmacs-local-leader-key)
    "" '(:ignore t :which-key (lambda (arg) `(,(cadr (split-string (car arg) " ")) . ,(replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))))

  (warmacs/leader-keys
    "SPC" '(execute-extended-command :which-key "M-x")))

;;Turns off elpaca-use-package-mode current declaration
;;Note this will cause the declaration to be interpreted immediately (not deferred).
;;Useful for configuring built-in emacs features.
(use-package emacs
  :elpaca nil
  :config (setq ring-bell-function #'ignore))

;; Don't install anything. Defer execution of BODY
(elpaca nil
  (message "deferred"))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:

(provide 'warmacs)
