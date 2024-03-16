;; warmacs-treesitter.el -*- lexical-binding: t; -*-

;; Configure Tree-sitter when it is available.

(when (treesit-available-p)

  (use-package treesit
    :ensure nil
    :preface
    (defun warmacs/treesit-install-grammars ()
      "Install tree-sitter grammars if they are absent."
      (interactive)
      (dolist (grammar
               '((bash . ("https://github.com/tree-sitter/tree-sitter-bash" "v0.20.5"))
                 (css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
                 (clojure . ("https://github.com/sogaiu/tree-sitter-clojure" "v0.0.12"))
                 (elisp "https://github.com/Wilfred/tree-sitter-elisp")
                 (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
                 (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
                 (java . ("https://github.com/tree-sitter/tree-sitter-java" "v0.20.2"))
                 (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
                 (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
                 (make "https://github.com/alemuller/tree-sitter-make")
                 (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
                 (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
                 (toml "https://github.com/tree-sitter/tree-sitter-toml")
                 (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
                 (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
                 (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
        (add-to-list 'treesit-language-source-alist grammar)
        ;; Only install `grammar' if we don't already have it
        ;; installed. However, if you want to *update* a grammar then
        ;; this obviously prevents that from happening.
        (unless (treesit-language-available-p (car grammar))
          (treesit-install-language-grammar (car grammar)))))

    ;; Note that this does *not* extend to hooks! Make sure you
    ;; migrate them also
    (dolist (mapping
             '((bash-mode . bash-ts-mode)
               (css-mode . css-ts-mode)
               (clojure-mode . clojure-ts-mode)
               (go-mode . go-ts-mode)
               (go-mod-mode . go-mod-ts-mode)
               (html-mode . html-ts-mode)
               (java-mode . java-ts-mode)
               (js2-mode . js-ts-mode)
               (json-mode . json-ts-mode)
               (js-json-mode . json-ts-mode)
               (python-mode . python-ts-mode)
               (rust-mode . rust-ts-mode)
               (typescript-mode . typescript-ts-mode)
               (yaml-mode . yaml-ts-mode)))
      (add-to-list 'major-mode-remap-alist mapping))
    :config
    (warmacs/treesit-install-grammars)))

(provide 'warmacs-tree-sitter)
