;; -*- lexical-binding: t; -*-

(straight-use-package 
 `(consult-hl-todo
   :type nil
   :local-repo ,(concat user-emacs-directory "consult-hl-todo")
   :files ("consult-hl-todo.el")))

(use-package consult-hl-todo
  :after (consult hl-todo)
  :defer t)

(provide 'init-consult-hl-todo)
