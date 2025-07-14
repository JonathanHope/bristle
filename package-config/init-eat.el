;; -*- lexical-binding: t; -*-

(use-package eat
  :straight (eat
             :type git
             :host codeberg
             :repo "akib/emacs-eat"
             :files ("*.el" ("term" "term/*.el") "*.texi"
                     "*.ti" ("terminfo/e" "terminfo/e/*")
                     ("terminfo/65" "terminfo/65/*")
                     ("integration" "integration/*")
                     (:exclude ".dir-locals.el" "*-tests.el")))
  
  :init
  (add-hook 'eshell-load-hook #'eat-eshell-mode)

  :custom
  (completion-in-region-function #'consult-completion-in-region))

(provide 'init-eat)
