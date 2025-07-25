;; -*- lexical-binding: t; -*-

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t

  :bind
  (:map copilot-mode-map
        ("<backtab>" . copilot-accept-completion))
  
  :hook ((typescript-ts-mode . copilot-mode)
         (tsx-ts-mode . copilot-mode)
         (js-ts-mode . copilot-mode)
         (go-ts-mode . copilot-mode)))

(provide 'init-copilot)
