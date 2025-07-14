;; -*- lexical-binding: t; -*-

(straight-use-package '(typescript-ts-mode :type built-in))

(use-package typescript-ts-mode
  :defer t  

  :mode (("\\.ts$" . typescript-ts-mode)))

(straight-use-package '(tsx-ts-mode :type built-in))

(use-package tsx-ts-mode
  :defer t  

  :mode (("\\.tsx$" . tsx-ts-mode)))

(straight-use-package '(js-ts-mode :type built-in))

(use-package js-ts-mode
  :defer t  

  :mode (("\\.js$" . js-ts-mode)
         ("\\.cjs$" . js-ts-mode)
         ("\\.mjs$" . js-ts-mode)))

(provide 'init-typescript-mode)
