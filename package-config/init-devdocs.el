;; -*- lexical-binding: t; -*-

(use-package devdocs
  :straight t
  :defer t

  :init
  (add-hook 'go-ts-mode-hook
            (lambda () (setq-local devdocs-current-docs '("go"))))
  (add-hook 'typescript-ts-mode-hook
            (lambda () (setq-local devdocs-current-docs '("javascript"))))
  (add-hook 'js-ts-mode-hook
          (lambda () (setq-local devdocs-current-docs '("javascript"))))
  
  :config
  (defun bristle--devdocs-tag-pre (dom)
    "Custom mappings for language -> mode."
    (let ((start (point)))
      (if-let* ((lang (and devdocs-fontify-code-blocks
                           (dom-attr dom 'data-language)))
                (mode (or (cdr (assoc lang '(("go" . go-ts-mode)
                                             ("js" . js-ts-mode))))
                          (intern (concat lang "-mode"))))
                (buffer (and (fboundp mode) (current-buffer))))
          (insert
           (with-temp-buffer
             (shr-tag-pre dom)
             (let ((inhibit-message t)
                   (message-log-max nil))
               (ignore-errors (delay-mode-hooks (funcall mode)))
               (font-lock-ensure))
             (buffer-string)))
        (shr-tag-pre dom))
      (add-face-text-property start (point) 'devdocs-code-block t)))

  (setf (alist-get 'pre (alist-get t devdocs--rendering-functions))
        'bristle--devdocs-tag-pre))

(provide 'init-devdocs)

