;; -*- lexical-binding: t; -*-

(straight-use-package '(sgml-mode :type built-in))

(use-package sgml-mode
  :defer t

  :mode (("\\.xml$" . sgml-mode))

  :bind
  (:map sgml-mode-map
        ;; ("<tab>" . bristle--sgml-xml-finish-element-new-line)
        ("<backtab>" . bristle--sgml-xml-finish-element-same-line))

  :init
  (add-hook 'sgml-mode-hook (lambda () (setq mode-name "XML")))

  :config
  (defun bristle--sgml-backward-symbol (&optional arg)
    "Move backward until encountering the beginning of a symbol."
    (interactive "p")
    (forward-symbol (- (or arg 1))))

  (defun bristle--sgml-forward-start-end-tag (&optional arg)
    "Move forward over a tag start or end."
    (interactive "p")
    (forward-symbol arg)
    (forward-char))

  (defun bristle--sgml-backward-start-end-tag (&optional arg)
    "Move backward over a tag start or end."
    (interactive "p")
    (bristle--sgml-backward-symbol arg)
    (backward-char))

  (defun bristle--sgml-xml-finish-element-new-line (&optional arg)
  "Wrap an arbitrary identifier in brackets, complete it, create a new line, and apply indentation."
  (interactive "p")
  (setq arg (or arg 1))
  (if (string-match "[A-Za-z0-9\_\.\-]" (char-to-string (char-before)))
      (progn
        (bristle--sgml-backward-symbol arg)
        (insert "<")
        (forward-symbol arg)
        (insert ">")
        (sgml-close-tag)
        (bristle--sgml-backward-start-end-tag arg)
        (newline-and-indent)
        ;; Now fix the indentation of the closing tag
        (save-excursion
          (forward-line 1)  ; Move to the closing tag line
          (indent-according-to-mode)))
    (sgml-indent-line)))

  (defun bristle--sgml-xml-finish-element-same-line (&optional arg)
    "Wrap an arbitrary identifier in brackets and complete the tag."
    (interactive "p")
    (when (string-match "[A-Za-z0-9\_\.\-]" (char-to-string (char-before)))
      (bristle--sgml-backward-symbol arg)
      (insert "<")
      (forward-symbol arg)
      (insert ">")
      (sgml-close-tag)
      (bristle--sgml-backward-start-end-tag arg))))

(provide 'init-xml-mode)
