;; -*- lexical-binding: t; -*-

(use-package terraform-mode
  :straight t
  :defer t

  :mode (("\\.tf\\'" . terraform-mode)
         ("\\.tfvars\\'" . terraform-mode)))

(provide 'init-terraform-mode)
