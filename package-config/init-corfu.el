;; -*- lexical-binding: t; -*-

(use-package corfu
  :straight (:files (:defaults "extensions/*.el"))
  :defer t

  :init
  (global-corfu-mode)

  :custom
  (corfu-cycle t)
  (corfu-preview-current nil))

(use-package corfu-popupinfo
  :straight nil
  :after corfu
  :defer t

  :custom
  (corfu-popupinfo-max-height 20))

(provide 'init-corfu)
