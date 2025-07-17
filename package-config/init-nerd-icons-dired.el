;; -*- lexical-binding: t; -*-

(use-package nerd-icons-dired
  :straight t
  :defer t
  :after dired
  
  :hook
  (dired-mode . nerd-icons-dired-mode)

  :init
  (advice-add 'dired-create-empty-file :after
              (lambda (&rest _args)
                (revert-buffer)))
  
  (advice-add 'dired-create-directory :after
              (lambda (&rest _args)
                (revert-buffer)))
  
  (advice-add 'dired-do-copy :after
              (lambda (&rest _args)
                (revert-buffer)))

  (advice-add 'dired-do-rename :after
              (lambda (&rest _args)
                (revert-buffer))))

(provide 'init-nerd-icons-dired)
