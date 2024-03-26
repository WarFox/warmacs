;; warmacs-keybindings.el -*- lexical-binding: t; -*-

;; Set leader keys as constant
(defconst warmacs-leader-key "SPC")
(defconst warmacs-local-leader-key ",")

; Use the left alt/option key as meta
; Use the right alt/option key for stock Apple stuff
; e.g Use the right alt/option-option key on Mac for inputing special characters like #
(with-system darwin
  (setq-default ns-alternate-modifier 'meta
                ns-right-alternate-modifier 'none))

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

  (general-create-definer warmacs/set-leader-keys
    :keymaps 'override
    :states '(normal motion visual operator)
    :prefix  warmacs-leader-key
    :non-normal-prefix (concat "C-" warmacs-leader-key)
    "" '(:ignore t :wk "leader key"))

  (general-create-definer warmacs/local-leader-keys
    :major-modes t
    :keymaps 'override
    :states '(normal motion visual operator)
    :prefix warmacs-local-leader-key
    :non-normal-prefix (concat "C-" warmacs-local-leader-key)
    "" '(:ignore t :wk (lambda (arg) `(,(cadr (split-string (car arg) " ")) . ,(replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))))

  ;; Macro for creating a leader menu
  (defmacro warmacs/leader-menu (name infix-key &rest body)
    "Create a definer named warmacs/leader-menu-NAME wrapping warmacs/set-leader-keys.
     Create prefix command: warmacs-leader-menu-NAME-command. Prefix bindings in BODY with INFIX-KEY."
    (declare (indent 2))
    `(progn
       ;; Create new definer for new leader menu
       (general-create-definer ,(intern (format "warmacs/leader-menu-%s" name))
         :wrapping warmacs/set-leader-keys
         :infix ,infix-key
         :wk-full-keys nil
         "" '(:ignore t :wk ,name))
       ;; Use new definer to create bindings
       (,(intern (format "warmacs/leader-menu-%s" name))
        ,@body)))

  ;; Macro for creating a local leader menu
  (defmacro warmacs/local-leader-menu (mode &rest body)
    "Create a definer named warmacs/local-leader-menu-MODE wrapping warmacs/local-leader-keys
     Create prefix map: MODE-mode-map
     Parameter mode must be a symbol not end with -mode"
    (declare (indent 2))
    (let ((local-leader-menu-name (concat "warmacs/local-leader-menu-" (symbol-name mode)))
          (local-keymap (concat (symbol-name mode) "-mode-map")))
      `(progn
         ;; Create new definer for local leader
         (general-create-definer ,(intern local-leader-menu-name)
           :wrapping warmacs/local-leader-keys
           :keymaps (quote ,(intern local-keymap))
           :wk-full-keys nil
           "" '(:ignore t :wk ,mode))
         ;; Use new definer to create bindings
         (,(intern local-leader-menu-name)
          ,@body))))

  :config
  ;; basic menu setup
  (warmacs/set-leader-keys
    "!" #'shell-command
    ":" #'eval-expression
    "SPC" '("M-x" . execute-extended-command))

  (warmacs/leader-menu "Applications" "a"
    "l" #'elpaca-manager
    "p" #'list-processes
    "P" #'proced)

  (warmacs/leader-menu "Toggle" "T"
    "f" #'toggle-frame-maximized
    "F" #'toggle-frame-fullscreen)

  (warmacs/leader-menu "Kill Ring" "r"
    "y" #'yank-pop)

  (warmacs/leader-menu "Search" "s"))

;; Ensure general.el is configured
(elpaca-wait)

(use-package emacs
  :ensure nil
  :general
  ;; Make <escape> quit as much as possible
  (general-def
    :keymaps '(minibuffer-local-map
               minibuffer-local-ns-map
               minibuffer-local-completion-map
               minibuffer-local-must-match-map
               minibuffer-local-isearch-map)
    "<escape>" #'keyboard-escape-quit))

(provide 'warmacs-keybindings)
