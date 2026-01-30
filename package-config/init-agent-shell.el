;; -*- lexical-binding: t; -*-

(use-package agent-shell
  :straight t
  :defer t

  :custom
  (agent-shell-highlight-blocks t)
  (agent-shell-show-welcome-message nil))

(provide 'init-agent-shell)
