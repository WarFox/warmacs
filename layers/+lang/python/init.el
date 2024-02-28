;; -*- lexical-binding: t; -*-

(use-package python-mode
  :hook
  (python-mode . flycheck-mode)
  :custom
  (python-indent-offset 4)
  (python-shell-interpreter "python3")
  (python-shell-interpreter-args "-i")
  (python-shell-prompt-detect-function nil)
  (python-shell-completion-native-enable nil)
  (python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: ")
  (python-shell-prompt-input-regexp "In \\[[0-9]+\\]: ")
  :general
  (warmacs/local-leader-menu python
      "'" #'run-python
      "b" '(:ignore t :wk "build")
      "bb"  #'poetry-build
      "vod" 'poetry-venv-deactivate
      "vow" 'poetry-venv-workon
      "vot" 'poetry-venv-toggle))

(use-package poetry
  :after python-mode)

(use-package pyvenv
  :after python-mode
  :config
  (pyvenv-mode 1))

(provide '+lang/python/init)