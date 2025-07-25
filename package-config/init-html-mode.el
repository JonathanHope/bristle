;; -*- lexical-binding: t; -*-

(straight-use-package '(html-ts-mode :type built-in))

(use-package html-ts-mode
  :defer t

  :mode (("\\.html$" . html-ts-mode)))

(provide 'init-html-mode)
