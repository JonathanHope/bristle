;; -*- lexical-binding: t; -*-

(straight-use-package 
 `(devcontainer
   :type nil
   :local-repo ,(concat user-emacs-directory "devcontainer")
   :files ("devcontainer.el")))

(use-package devcontainer
  :defer t
  :after eat
  
  :init
  (add-hook 'devcontainer-start-hook
            (lambda ()
              (setq pine-root-directory-function #'devcontainer-workspace-tramp-path)
              (pine-change-directory (devcontainer-workspace-tramp-path))
              (pine-show)
              (when-let* ((buf (get-buffer "*eshell*")))
                (kill-buffer buf))))
  
  (add-hook 'devcontainer-stop-hook
            (lambda ()
              (setq pine-root-directory-function nil)
              (pine-hide)
              (kill-buffer "*Pine*"))))

(provide 'init-devcontainer)
