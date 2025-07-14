;; -*- lexical-binding: t; -*-

(use-package verb
  :straight t
  :defer t

  :custom
  (verb-auto-kill-response-buffers t)
  (verb-json-use-mode 'json-ts-mode))

(provide 'init-verb)
