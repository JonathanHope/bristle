;; -*- lexical-binding: t; -*-

(use-package transient
  :defer t
  :straight t

  :bind
  (:map transient-base-map ("q" . transient-quit-one))
  (:map transient-base-map ("<escape>" . transient-quit-one))
  (:map transient-edit-map ("q" . transient-quit-one))
  (:map transient-edit-map ("<escape>" . transient-quit-one))
  (:map transient-sticky-map ("q" . transient-quit-one))
  (:map transient-sticky-map ("<escape>" . transient-quit-one)))

(use-package magit
  :defer t
  :straight t
  :after transient

  :bind (:map magit-status-mode-map
         ("SPC" . gestalt-leader-transient-facet))

  :init
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (setq transient-save-history nil))

(use-package git-modes
  :defer t
  :straight t)

(provide 'init-magit)
