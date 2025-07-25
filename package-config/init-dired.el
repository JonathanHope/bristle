;; -*- lexical-binding: t; -*-

(straight-use-package '(dired :type built-in))

(use-package dired
  :defer t

  :init
  (add-hook 'dired-mode-hook 'dired-mode-setup)
  
  :custom
  (dired-use-ls-dired t)
  (dired-listing-switches "-alh --group-directories-first")
  (dired-free-space nil)
  (dired-keep-marker-copy nil)

  :config
  (defun dired-mode-setup ()
    (dired-hide-details-mode 1)))

(provide 'init-dired)
