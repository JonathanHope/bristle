;; -*- lexical-binding: t; -*-

(use-package tempel
  :straight t

  :init
  (add-to-list 'auto-mode-alist '("templates\\'" . emacs-lisp-mode))
  
  :bind
  (:map tempel-map
        ("<tab>" . tempel-next)
        ("<backtab>" . tempel-previous)
        ("M-RET" . tempel-done)))

(provide 'init-tempel)
