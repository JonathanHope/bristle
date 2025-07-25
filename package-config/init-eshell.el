;; -*- lexical-binding: t; -*-

(straight-use-package '(eshell :type built-in))

(use-package em-hist)

(use-package eshell
  :defer t

  :init
  (defface eshell-prompt-bullet
    '((t :inherit font-lock-constant-face))
    "Face for bullet part of prompt."
    :group 'eshell)
  
  (defface eshell-prompt-lambda
    '((t :inherit font-lock-constant-face))
    "Face for lambda part of prompt."
    :group 'eshell)
  
  :custom
  (eshell-banner-message "")
  (eshell-prompt-function
        (lambda nil
          (concat
           (propertize "━⬢ " 'face 'eshell-prompt-bullet)
           (propertize "λ" 'face 'eshell-prompt-lambda)
           (propertize " " 'face `()))))

  :bind (:map eshell-hist-mode-map
              ("M-r" . bristle--eshell-history))
  
  :config
  (defun eshell/clear ()
    "Clear terminal"
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)))

  (defun bristle--eshell-history ()
    "Select from eshell history using completing-read."
    (interactive)
    (when (derived-mode-p 'eshell-mode)
      (let* ((history-list (ring-elements eshell-history-ring))
             (selected (completing-read "History: " history-list nil t)))
        (when selected
          (eshell-kill-input)
          (insert selected))))))

(provide 'init-eshell)
