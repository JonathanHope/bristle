;; -*- lexical-binding: t; -*-

(use-package hl-todo
  :straight t

  :init
  (global-hl-todo-mode)

  :custom
  hl-todo-keyword-faces
        '(("TODO" . "#bf616a")))

(provide 'init-hl-todo)
