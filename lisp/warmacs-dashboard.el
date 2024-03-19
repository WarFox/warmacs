;; warmacs-dashboard.el -*- lexical-binding: t; -*-

(use-package dashboard
  :custom
  (dashboard-banner-logo-title "Welcome to Warmacs!")
  (dashboard-center-content t) ;; center the content
  ;; customise icons
  (dashboard-icon-type 'nerd-icons)    ;; use `nerd-icons' package
  (dashboard-display-icons-p t) ;; display icons on both GUI and terminal
  (dashboard-set-heading-icons nil) ;; display icons for heading sections
  (dashboard-set-file-icons t) ;; display icons for file lists
  (dashboard-items '((recents  . 5)
		     (bookmarks . 5)
		     (projects . 5)
		     (agenda . 5)
		     (registers . 5)))
  ;; display a navigator under the banner
  (dashboard-set-navigator t)
  (dashboard-navigator-buttons
   `(;; Format: "(icon title help action face prefix suffix)"
     ((,(nerd-icons-octicon "nf-oct-mark_github" :height 1.1 :v-adjust 0.0)
       "Homepage"
       "Browse homepage"
       (lambda (&rest _) (browse-url "https://github.com/WarFox/warmacs")))
      ("?" "" "?/h" #'show-help nil "<" ">"))))
  :hook
  (elpaca-after-init .  #'dashboard-insert-startupify-lists)
  (elpaca-after-init .  #'dashboard-initialize)
  :config
  (dashboard-setup-startup-hook))

(provide 'warmacs-dashboard)
