;; -*- lexical-binding: t; -*-

;;
;; Scala Layer

(use-package scala-mode
  :custom
  ;; Compatibility with `aggressive-indent'
  (scala-indent:align-forms t)
  (scala-indent:align-parameters t)
  (scala-indent:default-run-on-strategy)
  (scala-indent:operator-strategy)
  :config
  ;; Automatically insert asterisk in a comment when enabled
  (defun scala/newline-and-indent-with-asterisk ()
    (interactive)
    (newline-and-indent)
    (scala-indent:insert-asterisk-on-multiline-comment))

  (defun scala/join-line ()
    "Adapt `scala-indent:join-line' to behave more like evil's line join.

`scala-indent:join-line' acts like the vanilla `join-line',
joining the current line with the previous one. The vimmy way is
to join the current line with the next.

Try to move to the subsequent line and then join. Then manually move
point to the position of the join."
    (interactive)
    (let (join-pos)
      (save-excursion
        (goto-char (line-end-position))
        (unless (eobp)
          (forward-line)
          (call-interactively 'scala-indent:join-line)
          (setq join-pos (point))))

      (when join-pos
        (goto-char join-pos))))

  ;; display compilation buffer at the bottom
  (defun scala/display-sbt-at-bottom (buffer args)
    "Display a short buffer in a dedicated window at frame bottom.
For use with `sbt:display-buffer-action'."
    (set-window-dedicated-p
     (display-buffer-at-bottom buffer (cons '(window-height . 12) args))
     t))
  (setq sbt:display-buffer-action
        (list #'scala/display-sbt-at-bottom))

  :general
  (:states 'insert :keymaps 'scala-mode-map
           "RET" #'scala/newline-and-indent-with-asterisk)
  (:states 'normal :keymaps 'scala-mode-map
           "J" #'scala/join-line)

  ;; (evil-define-key 'insert scala-mode-map
  ;;   (kbd "RET") 'scala/newline-and-indent-with-asterisk)

  ;; (evil-define-key 'normal scala-mode-map "J" 'scala/join-line)
  )

(use-package sbt-mode
  :after scala-mode
  :commands (sbt-start sbt-command)
  :config
  ;; WORKAROUND: https://github.com/hvesalai/emacs-sbt-mode/issues/31
  ;; allows for using SPACE in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false"))
  :general
  (warmacs/set-local-leader-keys
    :keymaps 'scala-mode-map
    "=" '(:ignore t :wk "format")
    "==" '("format-all" . (lambda () (interactive) (sbt-command "scalafmtAll")))
    "b" '(:ignore t :wk "build")
    "b." #'sbt-hydra
    "bb" #'sbt-command
    "bc" '("compile" . (lambda () (interactive) (sbt-command "compile")))
    "bt" '("test" . (lambda () (interactive) (sbt-command "test")))
    "bT" '("compile-test" . (lambda () (interactive) (sbt-command "Test / compile")))))

(provide '+lang/scala/init)
