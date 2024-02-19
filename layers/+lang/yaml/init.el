;; yaml  -*- lexical-binding: t; -*-

(use-package yaml-mode
  :hook
  (yaml-mode . (lambda ()
		 (display-line-numbers-mode 1)))
  :general
  (:keymaps 'yaml-mode-map
	    :states '(normal visual insert emacs)
	    :prefix ","
	    "=" '(:ignore t :which-key "format")
	    "= =" '(yaml-mode-format-buffer :which-key "yaml-prettify-buffer")
	    "= r" '(yaml-mode-format-region :which-key "yaml-prettify-region")))

(provide '+lang/yaml/init)
