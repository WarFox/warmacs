;;; init.el --- org layer init file for spacemacs -*- lexical-binding: t -*-

;;
;;; Configuration variables

(defvar org-enable-roam-protocol t
  "Enable org-roam protocol.")

(defvar org-projectile-file "~/org/projects.org"
  "The file path to the org-projectile file.")

(defvar org-enable-roam-ui t
  "Enable org-roam-ui if non-nil.")

(defvar org-enable-roam t
  "Enable org-roam if non-nil.")

;;
;;; Packages

(use-package evil-org
  :after org
  :config
  (setq evil-org-use-additional-insert t))

(use-package htmlize)

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :ensure nil ;; org-mode is installed in warmacs-packages.el early in the startup process
  :commands (orgtbl-mode)
  :init
  (setq org-clock-persist-file (concat warmacs-cache-dir
				       "org-clock-save.el")
        org-id-locations-file (concat warmacs-cache-dir
                                      ".org-id-locations")
        org-publish-timestamp-directory (concat warmacs-cache-dir
                                                ".org-timestamps/")
        org-directory "~/org" ;; needs to be defined for `org-default-notes-file'
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-log-done 'time
        org-startup-with-inline-images t
        org-latex-prefer-user-labels t
        org-image-actual-width nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        ;; this is consistent with the value of
        ;; `helm-org-headings-max-depth'.
        org-imenu-depth 8)

  (autoload #'org-clock-jump-to-current-clock "org-clock")

  :general-config
  (warmacs/set-local-leader-keys
    :keymaps 'org-mode-map
    "'" #'org-edit-special
    "c" #'org-capture

    ;; Clock
    ;; These keybindings should match those under the "aoC" prefix (below)
    "C" '(:ignore t :wk "clock")
    "Cc" #'org-clock-cancel
    "Cd" #'org-clock-display
    "Ce" #'org-evaluate-time-range
    "Cg" #'org-clock-goto
    "Ci" #'org-clock-in
    "CI" #'org-clock-in-last
    "Co" #'org-clock-out
    "CR" #'org-clock-report
    "Cr" #'org-resolve-clocks

    "d" '(:ignore t :wk "date")
    "dd" #'org-deadline
    "ds" #'org-schedule
    "dt" #'org-time-stamp
    "dT" #'org-time-stamp-inactive

    "e" '(:ignore t :wk "export")
    "ee" #'org-export-dispatch

    "f" '(:ignore t :wk "feed")
    "fi" #'org-feed-goto-inbox
    "fu" #'org-feed-update-all

    "a" #'org-agenda
    "[" #'org-agenda-file-to-front
    "]" #'org-remove-file

    "p" #'org-priority

    "T" '(:ignore t :wk "toggle")
    "Tc" #'org-toggle-checkbox
    "Te" #'org-toggle-pretty-entities
    "Ti" #'org-toggle-inline-images
    "Tn" #'org-num-mode
    "Tl" #'org-toggle-link-display
    "Tt" #'org-show-todo-tree
    "TT" #'org-todo
    "TV" #'space-doc-mode
    "Tx" #'org-latex-preview

    ;; More cycling options (timestamps, headlines, items, properties)
    "L" #'org-shiftright
    "H" #'org-shiftleft
    "J" #'org-shiftdown
    "K" #'org-shiftup

    ;; Change between TODO sets
    "C-S-l" #'org-shiftcontrolright
    "C-S-h" #'org-shiftcontrolleft
    "C-S-j" #'org-shiftcontroldown
    "C-S-k" #'org-shiftcontrolup

    ;; Subtree editing
    "s" '(:ignore t :wk "subtree")
    "sa" #'org-toggle-archive-tag
    "sA" #'org-archive-subtree-default
    "sb" #'org-tree-to-indirect-buffer
    "sd" #'org-cut-subtree
    "sy" #'org-copy-subtree
    "sp" #'org-paste-subtree
    "sh" #'org-promote-subtree
    "sj" #'org-move-subtree-down
    "sk" #'org-move-subtree-up
    "sl" #'org-demote-subtree
    "sn" #'org-narrow-to-subtree
    "sw" 'widen
    "sr" #'org-refile
    "ss" #'org-sparse-tree
    "sS" #'org-sort

    ;; tables
    "t" '(:ignore t :wk "table")
    "ta" #'org-table-align
    "tb" #'org-table-blank-field
    "tc" #'org-table-convert
    "td" '(:ignore t :wk "delete")
    "tdc" #'org-table-delete-column
    "tdr" #'org-table-kill-row
    "te" #'org-table-eval-formula
    "tE" #'org-table-export
    "tf" #'org-table-field-info
    "th" #'org-table-previous-field
    "tH" #'org-table-move-column-left
    "ti" '(:ignore t :wk "insert")
    "tic" #'org-table-insert-column
    "tih" #'org-table-insert-hline
    "tiH" #'org-table-hline-and-move
    "tir" #'org-table-insert-row
    "tI" #'org-table-import
    "tj" #'org-table-next-row
    "tJ" #'org-table-move-row-down
    "tK" #'org-table-move-row-up
    "tl" #'org-table-next-field
    "tL" #'org-table-move-column-right
    "tn" #'org-table-create
    "tN" #'org-table-create-with-table.el
    "tr" #'org-table-recalculate
    "tR" #'org-table-recalculate-buffer-tables
    "ts" #'org-table-sort-lines
    "tt" '(:ignore t :wk "toggle")
    "ttf" #'org-table-toggle-formula-debugger
    "tto" #'org-table-toggle-coordinate-overlays
    "tw" #'org-table-wrap-region

    ;; Source blocks / org-babel
    "b" '(:ignore t :wk "babel")
    "bp" #'org-babel-previous-src-block
    "bn"     #'org-babel-next-src-block
    "be"     #'org-babel-execute-maybe
    "bo"     #'org-babel-open-src-block-result
    "bv"     #'org-babel-expand-src-block
    "bu"     #'org-babel-goto-src-block-head
    "bg"     #'org-babel-goto-named-src-block
    "br"     #'org-babel-goto-named-result
    "bb"     #'org-babel-execute-buffer
    "bs"     #'org-babel-execute-subtree
    "bd"     #'org-babel-demarcate-block
    "bt"     #'org-babel-tangle
    "bf"     #'org-babel-tangle-file
    "bc"     #'org-babel-check-src-block
    "bj"     #'org-babel-insert-header-arg
    "bl"     #'org-babel-load-in-session
    "bi"     #'org-babel-lob-ingest
    "bI"     #'org-babel-view-src-block-info
    "bz"     #'org-babel-switch-to-session
    "bZ"     #'org-babel-switch-to-session-with-code
    "ba"     #'org-babel-sha1-hash
    "bx"     #'org-babel-do-key-sequence-in-edit-buffer
    ;; Multi-purpose keys
    "," #'org-ctrl-c-ctrl-c
    "*" #'org-ctrl-c-star
    "-" #'org-ctrl-c-minus
    "#" #'org-update-statistics-cookies
    "RET"   #'org-ctrl-c-ret
    "M-RET" #'org-meta-return
    ;; attachments
    "A" #'org-attach
    ;; insertion
    "i" '(:ignore t :wk "insert")
    "ib" #'org-insert-structure-template
    "id" #'org-insert-drawer
    "ie" #'org-set-effort
    "if" #'org-footnote-new
    "ih" #'org-insert-heading
    "iH" #'org-insert-heading-after-current
    "ii" #'org-insert-item
    "il" #'org-insert-link
    "in" #'org-add-note
    "ip" #'org-set-property
    "is" #'org-insert-subheading
    "it" #'org-set-tags-command
    ;; region manipulation
    "x" '(:ignore t :wk "text")
    "xo" #'org-open-at-point)

  (general-def
    :keymaps 'global-map
    "\C-cl" #'org-store-link
    "\C-ca" #'org-agenda
    "\C-cc" #'org-capture))

(use-package org-agenda
  :ensure nil
  :init
  (setq org-agenda-restore-windows-after-quit t)

  :general
  (warmacs/leader-menu "Applications" "a"
    "o" '(:ignore t :wk "org")
    "o#" #'org-agenda-list-stuck-projects
    "oa" #'org-agenda-list
    "oo" #'org-agenda
    "oc" #'org-capture
    "oe" #'org-store-agenda-views

    "of" '(:ignore t :wk "feed")
    "ofi" #'org-feed-goto-inbox
    "ofu" #'org-feed-update-all

    ;; Clock
    ;; These keybindings should match those under the "mC" prefix (above)
    "oC" '(:ignore t :wk "clock")
    "oCc" #'org-clock-cancel
    "oCg" #'org-clock-goto
    "oCi" #'org-clock-in
    "oCI" #'org-clock-in-last
    "oCo" #'org-clock-out
    "oCr" #'org-resolve-clocks

    "ol" #'org-store-link
    "om" #'org-tags-view
    "os" #'org-search-view
    "ot" #'org-todo-list)

  :general-config
  (warmacs/set-local-leader-keys
   :keymaps 'org-agenda-mode-map
   warmacs-local-leader-key #'org-agenda-ctrl-c-ctrl-c
   "a" #'org-agenda
   "c" #'org-agenda-capture
   "Cc" #'org-agenda-clock-cancel
   "Ci" #'org-agenda-clock-in
   "Co" #'org-agenda-clock-out
   "Cj" #'org-agenda-clock-goto
   "dd" #'org-agenda-deadline
   "ds" #'org-agenda-schedule
   "ie" #'org-agenda-set-effort
   "ip" #'org-agenda-set-property
   "iP" #'org-agenda-priority
   "it" #'org-agenda-set-tags
   "sr" #'org-agenda-refile))

(use-package org-projectile
  :commands (org-projectile-location-for-project)
  :general
  (warmacs/leader-menu-Applications
    "op" '(:ignore t :wk "project")
    "opc" #'org-projectile/capture
    "opo" #'org-projectile/goto-todos)
  ;; load org-projectile if org-capture is loaded
  (with-eval-after-load #'org-capture
    (require #'org-projectile))
  :config
  (if (file-name-absolute-p org-projectile-file)
      (progn
        (setq org-projectile-projects-file org-projectile-file)
        (push (org-projectile-project-todo-entry :empty-lines 1)
              org-capture-templates))
    (org-projectile-per-project)
    (setq org-projectile-per-project-filepath org-projectile-file)))


(use-package org-roam
  :if org-enable-roam
  ;; Do not enable automatic db update until after user had a chance to setup
  ;; org-roam. See https://github.com/syl20bnr/spacemacs/issues/15724
  ;; :hook (after-init . org-roam-setup)
  :general
  (warmacs/leader-menu "Applications" "a"
    "or" '(:ignore t :wk "org-roam")
    "ora" #'org-roam-alias-add
    "orc" #'org-roam-capture
    "ord" '(:ignore t :wk "dailies")
    "ordy" #'org-roam-dailies-goto-yesterday
    "ordt" #'org-roam-dailies-goto-today
    "ordT" #'org-roam-dailies-goto-tomorrow
    "ordd" #'org-roam-dailies-goto-date
    "orf" #'org-roam-node-find
    "org" #'org-roam-graph
    "ori" #'org-roam-node-insert
    "orj" #'org-roam-dailies-capture-today
    "orl" #'org-roam-buffer-toggle
    "ort" '(:ignore t :wk "tags")
    "orta" #'org-roam-tag-add
    "ortr" #'org-roam-tag-remove)
  (warmacs/set-local-leader-keys
    :keymaps 'org-mode-map
    "r" '(:ignore t :wk "org-roam")
    "ra" #'org-roam-alias-add
    "rc" #'org-roam-capture
    "rd" '(:ignore t :wk "dailies")
    "rdy" #'org-roam-dailies-goto-yesterday
    "rdt" #'org-roam-dailies-goto-today
    "rdT" #'org-roam-dailies-goto-tomorrow
    "rdd" #'org-roam-dailies-goto-date
    "rf" #'org-roam-node-find
    "rg" #'org-roam-graph
    "ri" #'org-roam-node-insert
    "rj" #'org-roam-dailies-capture-today
    "rl" #'org-roam-buffer-toggle
    "rt" '(:ignore t :wk "tags")
    "rta" #'org-roam-tag-add
    "rtr" #'org-roam-tag-remove)
  :custom
  (org-roam-db-location "~/.org-roam.db")
  (org-roam-directory (file-truename "~/Dropbox/org-mode/"))
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "${slug}.org"
			 "#+title: ${title}")
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
   '(("d" "daily" plain (function org-roam-capture--get-point) ""
      :immediate-finish t
      :file-name "journals/%<%Y-%m-%d>"
      :head "#+title: %<<%Y-%m-%d>>"))))

(use-package org-table
  :ensure nil
  :config
  (defun markdown-org-table-align-advice ()
    "Replace \"+\" sign with \"|\" in tables."
    (when (member major-mode '(markdown-mode gfm-mode))
      (save-excursion
        (save-restriction
          (narrow-to-region (org-table-begin) (org-table-end))
          (goto-char (point-min))
          (while (search-forward "-+-" nil t)
            (replace-match "-|-"))))))

  ;; use github markdown syntax for tables in markdown files
  (advice-add #'org-table-align :after 'markdown-org-table-align-advice))

;; load org-tempo
(use-package org-tempo
  :ensure nil
  :after org)

(use-package org-roam-protocol
             :ensure nil
  :if org-enable-roam-protocol
  :after org-protocol)

(use-package org-roam-ui
  :after org-roam
  :general
  (warmacs/leader-menu "Applications" "a"
    "oru" #'org-roam-ui-mode)
  (warmacs/set-local-leader-keys
   :keymaps 'org-mode-map
   "ru" #'org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(provide '+emacs/org/init)
