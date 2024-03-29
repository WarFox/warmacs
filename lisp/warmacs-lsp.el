;; warmacs-lsp -- LSP mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Configure LSP mode, dap mode, and other LSP related packages.

;;; Code:

;; LSP
(use-package lsp-ui
  :general
  (general-def
    :keymaps 'lsp-ui-mode-map
    :states 'normal
    "gd" #'lsp-ui-peek-find-definitions
    "gr" #'lsp-ui-peek-find-references
    "gD" #'lsp-ui-peek-jump-backward
    "gR" #'lsp-ui-peek-jump-forward)
  ([remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  ([remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package consult-lsp
  :after (lsp-mode))

(use-package lsp-treemacs
  :after (lsp-mode treemacs))

(use-package lsp-mode
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-completion-mode . warmacs/lsp-mode-setup-completion)
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  (setq
   lsp-server-install-dir (expand-file-name "lsp/" warmacs-cache-dir)
   lsp-session-file (expand-file-name (file-name-nondirectory ".lsp-session-v1") lsp-server-install-dir)
   lsp-eslint-library-choices-file (expand-file-name ".lsp-eslint-choices" lsp-server-install-dir)
   lsp-yaml-schema-store-local-db (expand-file-name "lsp-yaml-schemas.json" lsp-server-install-dir))

  ;; https://github.com/minad/corfu/wiki#configuring-corfu-for-lsp-mode
  ;; Configure completions with orderless, cape and corfu.
  (defun warmacs/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

  (defun warmacs/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))
    ;; Optionally configure the first word as flex filtered.
    (add-hook 'orderless-style-dispatchers #'warmacs/orderless-dispatch-flex-first nil 'local)
    ;; Optionally configure the cape-capf-buster.
    (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point))))

  :config
  ;; This sets the lsp indentation for all modes derived from web-mode.
  (add-to-list 'lsp--formatting-indent-alist '(web-mode . web-mode-markup-indent-offset))


  (push '("*lsp-help*" :dedicated t :position bottom :stick t :noselect t :height 0.4)
        popwin:special-display-config)

  :general-config
  (warmacs/set-local-leader-keys
   :keymaps 'lsp-mode-map
   ;; format
   "=" '(:ignore t :wk "format")
   "==" #'lsp-format-buffer
   "=r" #'lsp-format-region
   "=o" #'lsp-organize-imports
   ;; code actions
   "a" '(:ignore t :wk "code actions")
   "aa" #'lsp-execute-code-action
   ;; goto
   ;; N.B. implementation and references covered by xref bindings / lsp provider...
   "g" '(:ignore t :wk "goto")
   "gt" #'lsp-find-type-definition
   "gM" #'lsp-ui-imenu
   ;; help
   "h" '(:ignore t :wk "help")
   "hh" #'lsp-describe-thing-at-point
   ;; backend
   "b" '(:ignore t :wk "backend")
   "bd" #'lsp-describe-session
   "br" #'lsp-workspace-restart
   "bs" #'lsp-workspace-shutdown
   "bv" #'lsp-version
   ;; refactor
   "r" '(:ignore t :wk "refactor")
   "rr" #'lsp-rename
   ;; toggles
   "T" '(:ignore t :wk "toggle")
   "Tl" '(:ignore t :wk "lsp")
   "Tld" #'lsp-ui-doc-mode
   "Tls" #'lsp-ui-sideline-mode
   "Tll" #'lsp-lens-mode
   ;; folders
   "F" '(:ignore t :wk "folder")
   "Fs" #'lsp-workspace-folders-switch
   "Fr" #'lsp-workspace-folders-remove
   "Fa" #'lsp-workspace-folders-add
   ;; text/code
   "x" '(:ignore t :wk "text/code")
   "xh" #'lsp-document-highlight
   "xl" #'lsp-lens-show
   "xL" #'lsp-lens-hide))

;; DAP

(use-package dap-mode
  :after lsp-mode
  :config (dap-auto-configure-mode))

(provide 'warmacs-lsp)
