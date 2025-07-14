;; -*- lexical-binding: t; -*-

(straight-use-package 
 `(pine
   :type nil
   :local-repo ,(concat user-emacs-directory "pine")
   :files ("pine.el")))

(use-package pine
  :after (dired project))

(provide 'init-pine)
