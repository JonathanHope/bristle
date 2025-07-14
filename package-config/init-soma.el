;; -*- lexical-binding: t; -*-

(straight-use-package 
 `(soma
   :type nil
   :local-repo ,(concat user-emacs-directory "soma")
   :files ("soma.el")))

(use-package soma
  :after (consult denote)
  :defer t)

(provide 'init-soma)
