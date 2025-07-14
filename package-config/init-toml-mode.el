;; -*- lexical-binding: t; -*-

(straight-use-package '(toml-ts-mode :type built-in))

(use-package toml-ts-mode
  :defer t

  :mode (("\\.toml$" . toml-ts-mode)))

(provide 'init-toml-mode)
