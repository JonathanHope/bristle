;; -*- lexical-binding: t; -*-

(use-package popper
  :straight t
  :bind (("C-`" . popper-toggle)
         ("C-~" . bristle--show-terminal)
         ("M-`" . popper-cycle)
         ("C-M-`" . popper-toggle-type))

  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Warnings\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*Legend\\*"
          "\\*eshell\\*"
          "\\*eldoc\\*"
          "\\*HTTP Response.*\\*"
          "\\*HTTP Headers.*\\*"
          "\\*Verb Variables\\*"
          "\\*pandoc\\*"
          "\\*docker-.*\\*"
          "\\*llm-scratch\\*"
          "\\*json\\*"
          "\\*jwt\\*"
          "\\*dape-repl\\*"
          "\\*Mcp-Hub\\*"
          "\\*eat\\*"
          "\\*WoMan.*\\*"
          "\\*devdocs\\*"
          help-mode
          compilation-mode
          elisp-compile-mode))
  (popper-mode +1)

  :custom
  (popper-display-function #'bristle--popper-display-function)
  (popper-mode-line "")

  :config
  (defun bristle--popper-display-function (buffer &optional alist)
    "Display popup BUFFER at bottom, with conditional selection."
    (let ((window (display-buffer-in-side-window
                 buffer
                 (append alist
                         `((side . bottom)
                           (slot . 0)
                           (window-height . ,(floor (* (frame-height) 0.4))))))))
      (when (and window (bristle--should-select-popup-p buffer))
        (select-window window))
      window))

  (defun bristle--should-select-popup-p (buffer)
    "Return t if BUFFER should be selected when displayed as popup."
    (with-current-buffer buffer
      (cond
       ((eq major-mode 'eshell-mode) t)
       ((string= (buffer-name) "*Legend*") nil)
       ((string= (buffer-name) "*Messages*") nil)
       (t t))))

  (defun bristle--show-terminal ()
    "If in a devcontainer use that for a terminal, otherwise use eshell."
    (interactive)
    (if (devcontainer-running-p)
        (devcontainer-terminal)
      (eshell)))
  
  ;; popper and transient both try to use a dedicated bottom window
  ;; if popper is active we switch it to a dedicated top window

  (defun bristle--popper-aware-transient-display-action (orig-fun &rest args)
    "Use dedicated top slot for transient when invoked from a popper buffer."
    (if (and (bound-and-true-p popper-popup-status)
             (memq popper-popup-status '(popup user-popup)))
      '(display-buffer-in-side-window
        (side . top)
        (dedicated t)
        (inhibit-same-window . t))
      (apply orig-fun args)))
  
  (advice-add 'transient--display-action :around #'bristle--popper-aware-transient-display-action))

(provide 'init-popper)
