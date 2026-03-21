;; -*- lexical-binding: t; -*-

(use-package emacs-everywhere
  :straight t
  :defer t
  :custom
  (emacs-everywhere-frame-parameters
   '((name . "Emacs Everywhere")
     (width . 120)
     (height . 48)
     (top . 0.5)
     (left . 0.5)
     (fullscreen . nil)))
  :config
  (remove-hook 'emacs-everywhere-init-hooks #'emacs-everywhere-set-frame-position)
  (add-hook 'emacs-everywhere-init-hooks
            (lambda ()
              (remove-hook 'post-command-hook #'verb--var-preview t)
              (erase-buffer)
              (set-buffer-modified-p nil))
            90)
  (add-hook 'emacs-everywhere-final-hooks
            (lambda ()
              (set-buffer-modified-p nil))
            90))

(provide 'init-emacs-everywhere)
