;; -*- lexical-binding: t; -*-

(use-package ox-pandoc
  :straight t
  :defer t

  :custom
  (org-pandoc-options '((wrap . "none"))))

(provide 'init-ox-pandoc)
