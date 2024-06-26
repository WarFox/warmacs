;; +tools/docker/init.el -*- lexical-binding: t; -*-

(use-package docker
  :commands
  (docker docker-images docker-containers)
  :general
  (warmacs/leader-menu-Applications
    "d" '(:ignore t :wk "Docker")
    "dd" #'docker)
  :general-config
  (general-nmap
    :keymaps '(docker-image-mode-map
               docker-container-mode-map
               docker-volume-mode-map
               docker-network-mode-map
               docker-machine-mode-map)
    "q" 'quit-window))

(use-package dockerfile-mode
  :mode "Dockerfile\\'"
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

(use-package docker-compose-mode
  :mode "docker-compose.yml\\'")

;; tram container is built in
(use-package tramp-container
  :ensure nil
  :after
  (docker dockerfile-mode)
  :general
  (warmacs/leader-menu-Applications
    "dt" #'tramp-containers)
  (warmacs/set-local-leader-keys
    :keymaps dockerfile-mode-map
    "dt" #'tramp-containers))

(provide '+tools/docker/init)
