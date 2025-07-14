;; -*- lexical-binding: t; -*-

(straight-use-package '(yaml-ts-mode :type built-in))

(use-package yaml-ts-mode
  :defer t

  :mode
  (("\\.yaml$" . yaml-ts-mode)
   ("\\.yml$" . yaml-ts-mode)))

(provide 'init-yaml-mode)
