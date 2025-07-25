;; -*- lexical-binding: t; -*-

(use-package paredit
  :defer t
  :straight t
  :commands (paredit-forward-slurp-sexp paredit-forward-barf-sexp))

(provide 'init-paredit)
