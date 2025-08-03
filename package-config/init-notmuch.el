;; -*- lexical-binding: t; -*-

(use-package notmuch
  :straight t
  :defer t

  :init
  (add-hook 'notmuch-tree-mode-hook 'notmuch-tree-outline-mode)
  
  :custom
  ;; TODO: think this through more
  (notmuch-fcc-dirs (format-time-string "/home/jhope/Maildir/jhope/Sent/cur/%Y-%m-%d_%T"))
  (notmuch-maildir-use-notmuch-insert nil)
  (send-mail-function 'sendmail-send-it)
  (sendmail-program "msmtp")
  (mail-specificy-envelope-from t)
  (message-sendmail-envelope-from 'header)
  (mail-envelope-from 'header)
  
  ;; hide the notmuch logo
  (notmuch-show-logo nil)

  ;; hide most widgets on the hello page; just show the count
  (notmuch-hello-sections '(notmuch-hello-insert-header))

  ;; configure the columns shown on the search page
  (notmuch-search-result-format
   `(("date" . "%10s ")
     ("authors" . "%-20s ")
     ("subject" . "%-85.85s ")
     ("tags" . " (%s)")))

  ;; denote tags that will be added/removed
  (notmuch-tag-deleted-formats
   '(("unread" (notmuch-apply-face bare-tag '(:underline "#bf616a")))
     (".*" (notmuch-apply-face tag '(:underline "#bf616a")))))
  (notmuch-tag-added-formats
   '((".*" (notmuch-apply-face tag '(:underline "#a3be8c")))))

  ;; open messages full screen in tree mode
  (notmuch-tree-show-out t)

  ;; configure the columns in tree mode
  (notmuch-tree-result-format
   '(("date" . "%12s  ")
     ("authors" . "%-20.20s ")
     ((("tree" . "%s ")
       ("subject" . "%s "))
      . "%-85.85s")
     ("tags" . " (%s)"))))

(provide 'init-notmuch)
