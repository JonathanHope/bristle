;; -*- lexical-binding: t; -*-

(use-package consult
  :straight t

:bind (("C-c M-x" . consult-mode-command)
       ("C-c h" . consult-history)
       ("C-c k" . consult-kmacro)
       ("C-c m" . consult-man)
       ("C-c i" . consult-info)
       ([remap Info-search] . consult-info)
       ("C-x M-:" . consult-complex-command)
       ("C-x b" . consult-buffer)
       ("C-x 4 b" . consult-buffer-other-window)
       ("C-x 5 b" . consult-buffer-other-frame)
       ("C-x t b" . consult-buffer-other-tab)
       ("C-x r b" . consult-bookmark)
       ("C-x p b" . consult-project-buffer)
       ("M-#" . consult-register-load)
       ("M-'" . consult-register-store)
       ("C-M-#" . consult-register)
       ("M-y" . consult-yank-pop)
       ("M-g e" . consult-compile-error)
       ("M-g f" . consult-flymake)
       ("M-g g" . consult-goto-line)
       ("M-g M-g" . consult-goto-line)
       ("M-g o" . consult-outline)
       ("M-g m" . consult-mark)
       ("M-g k" . consult-global-mark)
       ("M-g i" . consult-imenu)
       ("M-g I" . consult-imenu-multi)
       ("M-s d" . consult-find)
       ("M-s c" . consult-locate)
       ("M-s g" . consult-grep)
       ("M-s G" . consult-git-grep)
       ("M-s r" . consult-ripgrep)
       ("M-s l" . consult-line)
       ("M-s L" . consult-line-multi)
       ("M-s k" . consult-keep-lines)
       ("M-s u" . consult-focus-lines)
       ("M-s e" . consult-isearch-history)
       :map isearch-mode-map
       ("M-e" . consult-isearch-history)
       ("M-s e" . consult-isearch-history)
       ("M-s l" . consult-line)
       ("M-s L" . consult-line-multi)
       :map minibuffer-local-map
       ("M-s" . consult-history)
       ("M-r" . consult-history))
  
  :custom
  (register-preview-delay 0.5)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize consult-ripgrep :preview-key nil))

(provide 'init-consult)
