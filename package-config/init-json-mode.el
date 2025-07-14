;; -*- lexical-binding: t; -*-

(straight-use-package '(json-ts-mode :type built-in))

(use-package json-ts-mode
  :defer t

  :mode
  (("\\.json$" . json-ts-mode)))

(provide 'init-json-mode)
