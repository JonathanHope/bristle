;; -*- lexical-binding: t; -*-

(straight-use-package '(windmove :type built-in))

(use-package windmove
  :config
  (when (fboundp 'windmove-default-keybindings)
    (windmove-default-keybindings)))

(provide 'init-windmove)
