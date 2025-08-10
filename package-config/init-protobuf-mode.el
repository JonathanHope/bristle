;; -*- lexical-binding: t; -*-

(use-package protobuf-mode
  :straight t
  :defer t

  :mode (("\\.proto$" . protobuf-mode)))

(provide 'init-protobuf-mode)
