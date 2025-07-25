;; -*- lexical-binding: t; -*-

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-flat
                                vertico-grid
                                vertico-buffer
                                vertico-multiform
                                vertico-unobtrusive))

  :custom
  (vertico-count-format nil)

  :config
  (vertico-mode)
  (vertico-multiform-mode)

  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid)))

(provide 'init-vertico)
