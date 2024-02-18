;;; warmacs-editor.el -*- lexical-binding: t; -*-

(use-package editorconfig)

(use-package aggressive-indent
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))

;; Enable line numbers in most text-editing modes. We avoid
;; `global-display-line-numbers-mode' because there are many special and
;; temporary modes where we don't need/want them.
(use-package emacs
  :ensure nil
  :custom
  ;;
  ;; Line numbers
  ;; Explicitly define a width to reduce the cost of on-the-fly computation
  (display-line-numbers-width 3)
  ;; Show absolute line numbers for narrowed regions to make it easier to tell the
  ;; buffer is narrowed, and where you are, exactly.
  (display-line-numbers-widen t)
  ;; Relative line numbers
  (display-line-numbers-type 'relative)
  :hook
  ((prog-mode text-mode conf-mode) . #'display-line-numbers-mode))

(provide 'warmacs-editor)
