;; warmacs-completions.el -*- lexical-binding: t; -*-

;;
;; Completions, Search, and Embark
;; consult, embark, marginalia, vertico, orderless, corfu

(use-package consult
  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :custom
  ;; Use Consult to select xref locations with preview
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  (advice-add #'multi-occur :override #'consult-line-multi)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :preface

  (defun warmacs/--active-region-or-thing-at-point ()
    "Get the active region or thing at point."
    (interactive)
    (if (region-active-p)
        (buffer-substring-no-properties
         (region-beginning) (region-end))
      (or (thing-at-point 'symbol t) "")))

  (defun warmacs/consult-line ()
    "Search for matching line in current buffer with initial input based on selected region or symbol at point."
    (interactive)
    (consult-line
     (warmacs/--active-region-or-thing-at-point)))

  (defun warmacs/consult-line-multi-project ()
    "Search for matching line in project buffers with initial input based on selected region or symbol at point."
    (interactive)
    (consult-line-multi nil
                        (warmacs/--active-region-or-thing-at-point)))

  (defun warmacs/consult-line-multi ()
    "Search for matching line in all buffers with initial input based on selected region or symbol at point."
    (interactive)
    (consult-line-multi t
                        (warmacs/--active-region-or-thing-at-point)))

  (defun warmacs/search (use-initial-input initial-directory)
    "Search with initial input based on selected region or symbol at point."
    (let* ((initial-input (if use-initial-input
                              (warmacs/--active-region-or-thing-at-point)
                            ""))
           (default-directory (or initial-directory
                                  (read-directory-name "Start from directory: "))))
      (consult-ripgrep default-directory initial-input)))

  (defun warmacs/search-auto ()
    "Search with initial input based on selected region or symbol at point."
    (interactive)
    (warmacs/search t nil))

  (defun warmacs/search-project-root ()
    "Search in current projects root directory."
    (interactive)
    (warmacs/search t (projectile-project-root)))

  (defun warmacs/search-current-directory ()
    "Search in the default directiory of current buffer. "
    (interactive)
    (warmacs/search t default-directory))

  (defun warmacs/search-default ()
    "Search with initial input based on selected region or symbol at point."
    (interactive)
    (warmacs/search-project-root))

  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)


  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)

  :general
  (general-def
    [remap apropos]                       #'consult-apropos
    [remap bookmark-jump]                 #'consult-bookmark
    [remap evil-show-marks]               #'consult-mark
    [remap evil-show-registers]           #'consult-register
    [remap goto-line]                     #'consult-goto-line
    [remap imenu]                         #'consult-imenu
    [remap locate]                        #'consult-locate
    [remap load-theme]                    #'consult-theme
    [remap man]                           #'consult-man
    [remap recentf-open-files]            #'consult-recent-file
    [remap switch-to-buffer]              #'consult-buffer
    [remap switch-to-buffer-other-window] #'consult-buffer-other-window
    [remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame
    [remap projectile-switch-to-buffer]   #'consult-project-buffer
    [remap projectile-switch-project]     #'consult-projectile-switch-project
    [remap projectile-find-file]          #'consult-projectile-find-file
    [remap yank-pop]                      #'consult-yank-pop
    [remap vc-git-grep]                   #'consult-git-grep
    [remap Info-search]                   #'consult-info

    "C-s" #'consult-line
    ;; C-c bindings in `mode-specific-map'
    "C-c M-x" #'consult-mode-command
    "C-c h" #'consult-history
    "C-c k" #'consult-kmacro
    "C-c m" #'consult-man
    "C-c i" #'consult-info
    ;; C-x bindings in `ctl-x-map'
    "C-x M-:" #'consult-complex-command     ;; orig. repeat-complex-command
    "C-x b" #'consult-buffer                ;; orig. switch-to-buffer
    "C-x 4b" #'consult-buffer-other-window ;; orig. switch-to-buffer-other-window
    "C-x 5b" #'consult-buffer-other-frame  ;; orig. switch-to-buffer-other-frame
    "C-x tb" #'consult-buffer-other-tab    ;; orig. switch-to-buffer-other-tab
    "C-x rb" #'consult-bookmark            ;; orig. bookmark-jump
    "C-x pb" #'consult-project-buffer      ;; orig. project-switch-to-buffer
    ;; Custom M-# bindings for fast register access
    "M-#" #'consult-register-load
    "M-'" #'consult-register-store          ;; orig. abbrev-prefix-mark (unrelated)
    ;; Other custom bindings
    ;; M-g bindings in `goto-map'
    "M-g e" #'consult-compile-error
    "M-g f" #'consult-flycheck              ;; Alternative: consult-flymake
    "M-g g" #'consult-goto-line             ;; orig. goto-line
    "M-g M-g" #'consult-goto-line           ;; orig. goto-line
    "M-g o" #'consult-outline               ;; Alternative: consult-org-heading
    "M-g m" #'consult-mark
    "M-g k" #'consult-global-mark
    "M-g i" #'consult-imenu
    "M-g I" #'consult-imenu-multi
    ;; M-s bindings in `search-map'
    "M-s d" #'consult-find                  ;; Alternative: consult-fd
    "M-s c" #'consult-locate
    "M-s g" #'consult-grep
    "M-s G" #'consult-git-grep
    "M-s r" #'consult-ripgrep
    "M-s l" #'consult-line
    "M-s L" #'consult-line-multi
    "M-s k" #'consult-keep-lines
    "M-s u" #'consult-focus-lines
    ;; Isearch integration
    "M-s e" #'consult-isearch-history)

  (general-def
    :keymaps 'isearch-mode-map
    "M-e"   #'consult-isearch-history       ;; orig. isearch-edit-string
    "M-s e" #'consult-isearch-history       ;; orig. isearch-edit-string
    "M-s l" #'consult-line                  ;; needed by consult-line to detect isearch
    "M-s L" #'consult-line-multi)            ;; needed by consult-line to detect isearch
  ;; Minibuffer history
  (general-def
    :keymaps 'minibuffer-local-map
    "M-s" #'consult-history                 ;; orig. next-matching-history-element
    "M-r" #'consult-history)                ;; orig. previous-matching-history-element

  (warmacs/set-leader-keys
    "#" #'consult-register
    "*" #'warmacs/search-default
    "/" #'consult-ripgrep
    "bB" #'consult-buffer
    "fb" #'consult-bookmark
    "fL" #'consult-locate
    "fr" #'consult-recent-file
    "hda" #'apropos-command
    "hdm" #'describe-mode
    "jm" #'consult-mark
    "jM" #'consult-global-mark
    "sb" '("Search project buffers" . warmacs/consult-line-multi-project)
    "sB" '("Search all buffers" . warmacs/consult-line-multi)
    "ss" '("Search in current buffer" . warmacs/consult-line)
    "sl" #'consult-line
    "sL" #'consult-line-multi
    "sk" #'consult-keep-lines
    "rc" #'consult-complex-command
    "su" #'consult-focus-lines
    "sf" #'warmacs/search-auto
    "sd" #'warmacs/search-current-directory
    "sp" #'warmacs/search-project-root
    "ry" #'consult-yank-pop
    "Ts" #'consult-theme))

(use-package consult-projectile
  :after (consult projectile))

(use-package consult-flycheck
  :after (consult flycheck))

(use-package embark
  :general
  (general-def
    "C-." #'embark-act         ;; pick some comfortable binding
    "C-;" #'embark-dwim        ;; good alternative: M-.
    "C-h B" #'embark-bindings) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode 1)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Show completions in a child frame overlay
(use-package vertico-posframe
  :after vertico
  :config
  (vertico-posframe-mode 1))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :general-config
  (general-def
    :keymaps
    'minibuffer-local-map
    "M-A" #'marginalia-cycle)

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode 1))

;; Corfu - completion ui
(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  :hook
  ((prog-mode org-mode shell-mode eshell-mode) . corfu-mode)

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :config
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)
  :init
  (global-corfu-mode 1))

;; Use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package hippie
  :ensure nil
  :init
  (add-to-list 'hippie-expand-try-functions-list #'yas-hippie-try-expand)
  :general
  (general-def
    "M-/" #'hippie-expand))

;; Add extensions
(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :general
  (general-def
    "C-c dp" #'completion-at-point ;; capf
    "C-c p" '(:ignore t :wk "cape")
    "C-c pt" #'complete-tag        ;; etags
    "C-c pd" #'cape-dabbrev        ;; or dabbrev-completion
    "C-c ph" #'cape-history
    "C-c pf" #'cape-file
    "C-c pk" #'cape-keyword
    "C-c ps" #'cape-elisp-symbol
    "C-c pe" #'cape-elisp-block
    "C-c pa" #'cape-abbrev
    "C-c pl" #'cape-line
    "C-c pw" #'cape-dict
    "C-c pr" #'cape-rfc1345
    "C-c p:" #'cape-emoji
    "C-c p_" #'cape-tex
    "C-c p&" #'cape-sgml)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-sgml)
  (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  (add-to-list 'completion-at-point-functions #'cape-line))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package emacs
  :ensure nil
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; Use wgrep to edit grep results
;; Useful with embark-export
(use-package wgrep
  :after '(evil-collection)
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t)
  :config
  (evil-collection-wgrep-setup))

;; Snippets
(use-package yasnippet
  :commands (yas-global-mode yas-minor-mode yas-activate-extra-mode)
  :preface
  ;; We don't want undefined variable errors
  (defvar yas-global-mode nil)
  (defvar yas-snippet-dirs nil)
  :hook
  ((prog-mode org-mode) . yas-minor-mode)
  :init
  (setq yas-triggers-in-field t
        yas-wrap-around-region t)
  :general-config
  (general-def
    :keymaps 'yas-minor-mode-map
    "M-s-/" #'yas-expand)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package consult-yasnippet
  :after (consult yasnippet)
  :general-config
  (warmacs/leader-menu "Insert" "i"
    "s" 'consult-yasnippet)
  (general-def
    :keymaps 'yas-minor-mode-map
    "C-c y" 'consult-yasnippet))

(use-package yasnippet-capf
  :after yasnippet
  :config
  (add-to-list 'completion-at-point-functions 'yasnippet-capf))

(provide 'warmacs-completions)

;;; warmacs-completions.el ends here
