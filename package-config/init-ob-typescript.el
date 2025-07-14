;; -*- lexical-binding: t; -*-

(straight-use-package 
 `(ob-typescript
   :type nil
   :local-repo ,(concat user-emacs-directory "ob-typescript")
   :files ("ob-typescript.el")))

(use-package ob-typescript)

(provide 'init-ob-typescript)
