;; -*- lexical-binding: t; -*-

(straight-use-package '(go-ts-mode :type built-in))

(use-package go-ts-mode
  :defer t

  :mode (("\\.go$" . go-ts-mode))

  :init
  ;; just treat go.mod/go.sum as text files
  (add-to-list 'auto-mode-alist '("go\\.mod\\'" . text-mode))
  (add-to-list 'auto-mode-alist '("go\\.sum\\'" . text-mode))
  
  :custom
  (go-ts-mode-indent-offset 2))

(provide 'init-go-mode)
