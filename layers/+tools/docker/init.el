;; +tools/docker/init.el -*- lexical-binding: t; -*-

(use-package docker
  :general
  (warmacs/leader-menu-Applications
    "d" '(:ignore t :wk "Docker")
    "dd" #'docker)
  :general-config
  (general-nmap :keymaps 'docker-image-mode-map "q" 'quit-window)
  (general-nmap :keymaps 'docker-container-mode-map "q" 'quit-window)
  (general-nmap :keymaps 'docker-volume-mode-map "q" 'quit-window)
  (general-nmap :keymaps 'docker-network-mode-map "q" 'quit-window)
  (general-nmap :keymaps 'docker-machine-mode-map "q" 'quit-window))

(use-package dockerfile-mode
  :general-config
  (warmacs/set-local-leader-keys
    :keymaps 'dockerfile-mode-map
    "b" '(:ignore t :wk "Build")
    "bb" #'dockerfile-build-buffer
    "bB" #'dockerfile-build-no-cache-buffer
    "d" '(:ignore t :wk "Docker")
    "dd" #'docker
    "di" #'docker-images
    "dp" #'docker-containers))

(use-package tramp-container
  :after (docker dockerfile-mode)
  :general
  (warmacs/leader-menu-Applications
    "dt" #'tramp-containers)
  (warmacs/set-local-leader-keys
    :keymaps dockerfile-mode-map
    "dt" #'tramp-containers))

(provide '+tools/docker/init)
