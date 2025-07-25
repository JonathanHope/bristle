;; -*- lexical-binding: t; -*-

(straight-use-package '(dockerfile-ts-mode :type built-in))

(use-package dockerfile-ts-mode
  :defer t

  :mode (("Dockerfile\\(?:\\..*\\)?\\'" . dockerfile-ts-mode)))

(provide 'init-dockerfile-mode)
