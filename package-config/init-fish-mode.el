;; -*- lexical-binding: t; -*-

(use-package fish-mode
  :straight t
  :defer t

  :custom
  (fish-indent-offset 2)

  :mode (("\\.fish$" . fish-mode)))

(provide 'init-fish-mode)
