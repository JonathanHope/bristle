;; -*- lexical-binding: t; -*-

(use-package markdown-mode
  :defer t
  :straight t
  
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))

  :config
  (setq markdown-command "multimarkdown")
  (setq markdown-split-window-direction 'right))

(provide 'init-markdown-mode)
