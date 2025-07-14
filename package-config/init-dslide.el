;; -*- lexical-binding: t; -*-

(use-package dslide
  :straight t
  :defer t

  :hook ((dslide-start . bristle--start-presenting)
         (dslide-stop . bristle--stop-presenting))

  :custom
  (dslide-start-from 'first)
  (dslide-header-date nil)
  (dslide-header-author nil)
  (dslide-header-email nil)

  :init
  (defvar bristle--jinx-was-enabled nil
    "Track whether jinx was enabled before presentation.")
  
  (advice-add 'dslide-deck-start :before #'bristle--before-dslide)
  
  :config
  (defun bristle--start-presenting ()
    "Setup to start presenting an org file."
    (hide-mode-line-mode +1)
    (olivetti-mode +1)
    (setq cursor-type nil)
    (fontaine-set-preset 'present))

  (defun bristle--stop-presenting ()
    "Setup to stop presenting an org file."
    (fontaine-set-preset 'default)
    (when (bound-and-true-p bristle--jinx-was-enabled)
      (jinx-mode +1)
      (kill-local-variable 'bristle--jinx-was-enabled)
      (setq org-hide-emphasis-markers nil)))

  (defun bristle--before-dslide (&rest _args)
    "Disable jinx-mode before starting dslide presentation."
    (when (bound-and-true-p jinx-mode)
      (setq-local bristle--jinx-was-enabled t)
      (jinx-mode -1)
      (setq org-hide-emphasis-markers t))))

(provide 'init-dslide)
