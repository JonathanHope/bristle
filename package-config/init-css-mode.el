;; -*- lexical-binding: t; -*-

(straight-use-package '(css-ts-mode :type built-in))

(use-package css-ts-mode
  :defer t

  :custom
  (css-indent-offset 2)
  
  :mode
  (("\\.css$" . css-ts-mode)))

(provide 'init-css-mode)
