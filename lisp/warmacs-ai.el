;; warmacs-ai.el -*- lexical-binding: t; -*-

;; Suppor AI driven editing

;; GitHub Copilot
(use-package copilot
  :ensure
  (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :hook
  (prog-mode . copilot-mode))

(elpaca nil ;; defer

  ;; GitHub Copilot
  (with-eval-after-load 'company
    ;; disable inline previews
    (delq 'company-preview-if-just-one-frontend company-frontends))

  (with-eval-after-load 'copilot
    (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
    (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
    (define-key copilot-completion-map (kbd "C-TAB") 'copilot-accept-completion-by-word)
    (define-key copilot-completion-map (kbd "C-<tab>") 'copilot-accept-completion-by-word)))

(provide 'warmacs-ai)
