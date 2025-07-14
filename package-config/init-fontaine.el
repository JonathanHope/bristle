;; -*- lexical-binding: t; -*-

(use-package fontaine
  :straight t

  :custom
  (fontaine-presets
   '((default
     :default-family "PragmataPro Mono"
     :default-height 105
     :line-spacing 1

     :bristle-mode-line-family "PragmataPro"
     :bristle-mode-line-inactive-family "PragmataPro"
     :bristle-mode-line-file-status-family "PragmataPro"
     :bristle-mode-line-buffer-name-family "PragmataPro"
     :bristle-mode-line-project-family "PragmataPro"
     :bristle-mode-line-mode-family "PragmataPro"
     :bristle-mode-line-lighter-family "PragmataPro"
     :bristle-mode-line-row-column-family "PragmataPro"
     :bristle-mode-line-scroll-bar-family "PragmataPro"
     :bristle-mode-line-popper-family "PragmataPro")

     (present
      :inherit default
      :default-height 120)))

  :config
  (fontaine-set-preset 'default)
  (fontaine-mode 1))

(provide 'init-fontaine)
