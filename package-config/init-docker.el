;; -*- lexical-binding: t; -*-

(use-package docker
  :straight t
  :defer t

  :custom
  ;; this makes docker.el play nice with popper
  (docker-pop-to-buffer-action '(display-buffer-reuse-window display-buffer-same-window))
  (docker-show-messages nil))

(provide 'init-docker)
