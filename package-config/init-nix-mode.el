;; -*- lexical-binding: t; -*-

(use-package nix-ts-mode
  :straight t
  :defer t

  :mode (("\\.nix$" . nix-ts-mode)))

(provide 'init-nix-mode)
