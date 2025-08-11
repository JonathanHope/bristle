;; -*- lexical-binding: t; -*-

(use-package nerd-icons
  :straight t
  :defer t)

(use-package dashboard
  :straight t

  :custom
  ;; center the content
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)

  ;; display icons and use nerd icons for them
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-file-icons t) 
  (dashboard-set-heading-icons t)

  ;; configure the sections
  (dashboard-startupify-list '(dashboard-insert-banner
                               dashboard-insert-newline
                               dashboard-insert-navigator
                               dashboard-insert-newline
                               dashboard-insert-items))
  
  ;; configure the banner
  (dashboard-startup-banner (expand-file-name "banner.txt" user-emacs-directory))

  ;; configure the nav
  (dashboard-navigator-buttons
   `(((,(nerd-icons-octicon "nf-oct-file_directory" :height 1 :v-adjust 0.0)
       "dired"
       "Open file manager"
       (lambda (&rest _) (call-interactively #'dired)))
      (,(nerd-icons-mdicon "nf-md-calculator" :height 1 :v-adjust 0.0)
       "calc"
       "Open calculator"
       (lambda (&rest _) (full-calc)))
      (,(nerd-icons-octicon "nf-oct-git_branch" :height 1 :v-adjust 0.0)
       "magit"
       "Git status"
       (lambda (&rest _) (call-interactively #'magit)))
      (,(nerd-icons-mdicon "nf-md-file" :height 1 :v-adjust 0.0)
       "ibuffer"
       "List buffers"
       (lambda (&rest _) (ibuffer)))
      (,(nerd-icons-mdicon "nf-md-robot" :height 1 :v-adjust 0.0)
       "gptel"
       "AI chat"
       (lambda (&rest _) (call-interactively #'gptel)))
      (,(nerd-icons-mdicon "nf-md-email" :height 1 :v-adjust 0.0)
       "notmuch"
       "Email client"
       (lambda (&rest _) (notmuch)))
      (,(nerd-icons-mdicon "nf-md-notebook" :height 1 :v-adjust 0.0)
       "notes"
       "Consult notes"
       (lambda (&rest _) (call-interactively #'soma-consult-notes))))))

  ;; configure dashboard items
  (dashboard-items '((projects  . 10)
                          (bookmarks . 10)
                          (recents   . 5)))
  
  :config
  ;; start with the dashboard
  (dashboard-setup-startup-hook)

  ;; set the icons on the item headers
  (dashboard-modify-heading-icons '((projects  . "nf-oct-project")
                                    (bookmarks . "nf-oct-book")
                                    (recents   . "nf-oct-clock")))

  ;; there is a bug where the vertical centering doesn't work without a refresh
  (add-hook 'dashboard-after-initialize-hook 
            (lambda () (revert-buffer nil t)))

  ;; setup the leader key
  (define-key dashboard-mode-map (kbd "SPC") #'gestalt-leader-transient-facet))

(provide 'init-dashboard)
