;; -*- lexical-binding: t; -*-

(straight-use-package 
 `(ob-go
   :type nil
   :local-repo ,(concat user-emacs-directory "ob-go")
   :files ("ob-go.el")))

(use-package ob-go)

(provide 'init-ob-go)
