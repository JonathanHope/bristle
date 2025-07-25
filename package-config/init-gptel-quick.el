;; -*- lexical-binding: t; -*-

(use-package gptel-quick
  :straight (gptel-quick :type git :host github :repo "karthink/gptel-quick")
  :defer t

  :custom
  (gptel-quick-timeout 20))

(provide 'init-gptel-quick)
