;; -*- lexical-binding: t; -*-

(use-package embark
  :straight t
  :defer t

  :custom
  (embark-prompter #'embark-completing-read-prompter)
  (embark-indicators '(embark-minimal-indicator))

  :bind
  (:map minibuffer-local-map
        ("C-<tab>" . embark-act)))

(use-package embark-consult
  :straight t
  :after (embark consult))

(provide 'init-embark)
