;; -*- lexical-binding: t; -*-

(use-package engine-mode
  :straight t
  :defer t

  :config
  (defengine kagi
             "https://kagi.com/search?q=%s")

  (defengine github
             "https://github.com/search?q=%s"))

(provide 'init-engine-mode)
