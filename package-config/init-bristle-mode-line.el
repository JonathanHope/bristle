;; -*- lexical-binding: t; -*-

(straight-use-package 
 `(bristle-mode-line
   :type nil
   :local-repo ,(concat user-emacs-directory "bristle-mode-line")
   :files ("bristle-mode-line.el")
   :after (gestalt popper)))

(use-package bristle-mode-line
  :after (gestalt popper)
  :config
  (bristle-mode-line-init)

  :custom
  (bristle-mode-line-simplified-buffers '("*Legend*" "*Pine*")))

(provide 'init-bristle-mode-line)
