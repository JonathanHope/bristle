;; -*- lexical-binding: t; -*-

(use-package inheritenv
  :straight t

  :config
  (inheritenv-add-advice 'call-process)
  (inheritenv-add-advice 'shell-command-to-string)
  (inheritenv-add-advice 'compile))

(provide 'init-inheritenv)


