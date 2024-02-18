;; warmacs-keybindings.el -*- lexical-binding: t; -*-

;; Set leader keys as constant
(defconst warmacs-leader-key "SPC")
(defconst warmacs-local-leader-key ",")

; Use the left alt/option key as meta
; Use the right alt/option key for stock Apple stuff
; e.g Use the right alt/option-option key on Mac for inputing special characters like #
(with-system darwin
  (setq ns-alternate-modifier 'meta)
  (setq ns-right-alternate-modifier 'none))

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
  :demand t
  :ensure t
  :custom
  (general-use-package-emit-autoloads t)
  :init
  (general-evil-setup)
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

  (defmacro warmacs/leader-menu (name infix-key &rest body)
    "Create a definer named warmacs/leader-NAME-menu wrapping warmacs/leader-keys.
     Create prefix map: warmacs-leader-NAME-menu-map. Prefix bindings in BODY with INFIX-KEY."
    (declare (indent 2))
    `(progn
       (general-create-definer ,(intern (format "warmacs/leader-menu-%s" name))
	 :wrapping warmacs/leader-keys
	 :prefix-command (quote ,(intern (format "leader-menu-%s-command" name)))
	 :infix ,infix-key
	 :wk-full-keys nil
	 "" '(:ignore t :which-key ,name))
       (,(intern (concat "warmacs/leader-menu-" name))
	,@body)))

  :config
  ;; basic menu setup
  (warmacs/leader-keys
    "!" #'shell-command
    ":" #'eval-expression
    "/" #'projectile-ripgrep
    "SPC" '(execute-extended-command :which-key "M-x"))

  (warmacs/leader-menu "applications" "a"
    "p" #'list-processes
    "P" #'proced)

  (warmacs/leader-menu "git" "g"
    "s" #'magit-status)

  (warmacs/leader-menu "toggle" "T"
    "f" #'toggle-frame-maximized
    "F" #'toggle-frame-fullscreen)

  (warmacs/leader-menu "Kill Ring" "r"
    "y" #'yank-pop)


  (warmacs/leader-menu "Search" "s"))

;; Ensure general.el is configured
(elpaca-wait)

(provide 'warmacs-keybindings)
