;; -*- lexical-binding: t; -*-

(straight-use-package 
 `(stadion
   :type nil
   :local-repo ,(concat user-emacs-directory "stadion")
   :files ("stadion.el")))

(use-package stadion
  :defer t)

(provide 'init-stadion)
