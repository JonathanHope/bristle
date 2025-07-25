;; -*- lexical-binding: t; -*-

(use-package jinx
  :straight t
  :defer t
  
  :hook
  (text-mode-hook . jinx-mode)
  (prog-mode-hook . jinx-mode)
  (conf-mode-hook . jinx-mode))

(provide 'init-jinx)
