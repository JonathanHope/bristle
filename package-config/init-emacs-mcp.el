;; -*- lexical-binding: t; -*-

(straight-use-package 
 `(emacs-mcp
   :type nil
   :local-repo ,(concat user-emacs-directory "emacs-mcp")
   :files ("emacs-mcp.el" "emacs-mcp-tools.el")))

(use-package emacs-mcp
  :defer t)

(provide 'init-emacs-mcp)
