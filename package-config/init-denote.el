;; -*- lexical-binding: t; -*-

(use-package denote
  :straight t
  :defer t

  :hook (dired-mode . denote-dired-mode)
  
  :config
  (setq denote-directory (expand-file-name "~/Notes"))
  (setq denote-save-buffers t)
  (denote-rename-buffer-mode 1))

(provide 'init-denote)
