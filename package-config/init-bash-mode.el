;; -*- lexical-binding: t; -*-

(straight-use-package '(bash-ts-mode :type built-in))

(use-package bash-ts-mode
  :defer t

  :custom
  (sh-indentation 2))

(provide 'init-bash-mode)
