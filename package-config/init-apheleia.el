;; -*- lexical-binding: t; -*-

(use-package apheleia
  :straight t
  :defer t

  :hook ((typescript-ts-mode . apheleia-mode)
         (tsx-ts-mode . apheleia-mode)
         (js-ts-mode . apheleia-mode)
         (go-ts-mode . apheleia-mode)
         (terraform-mode . apheleia-mode))

  :custom
  ;; without this files in dev containers will not be formatted
  (apheleia-remote-algorithm 'remote))

(provide 'init-apheleia)
