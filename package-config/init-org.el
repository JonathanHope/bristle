;; -*- lexical-binding: t; -*-

(straight-use-package '(org :type built-in))

(use-package org
  :defer t

  :hook
  (org-mode . visual-line-mode)
  (org-mode . melitt-mode)
  (org-mode . bristle--set-org-mode-font)
  (org-mode . verb-mode)

  :bind (:map org-src-mode-map
         ("C-c C-c" . org-edit-src-exit)
         ("C-c C-k" . org-edit-src-abort))
  
  :custom
  ;; syntax highlighting in source blocks
  (org-src-fontify-natively t)

  ;; don't prompt when evaluating source blocks
  (org-confirm-babel-evaluate nil)

  ;; preserve source whitespace on export
  (org-src-preserve-indentation t)

  ;; launch the source editor in the same window
  (org-src-window-setup 'current-window)

  ;; don't indent everything by headline level
  (org-adapt-indentation  nil)

  ;; don't startup folder
  (org-startup-truncated nil)

  ;; don't persist the org cache
  (org-element-cache-persistent nil)

  ;; use SVG for latex previews
  (org-preview-latex-default-process 'dvisvgm)

  ;; start org documents with latex previews
  (org-startup-with-latex-preview t)

  ;; start with inline image previews
  (org-startup-with-inline-images t)

  ;; set up which major modes to use for source blocks
  (org-src-lang-modes '(("mermaid" . mermaid-ts)
                        ("octave" . octave)
                        ("go" . go-ts)
                        ("json" . json-ts)
                        ("typescript" . typescript-ts)
                        ("tsx" . tsx-ts)
                        ("yaml" . yaml-ts)
                        ("dockerfile" . dockerfile-ts)
                        ("shell" . bash-ts)
                        ("graphql" . graphql)
                        ("terraform" . terraform)
                        ("verb" . verb)
                        ("xml" . sgml)
                        ("duckdb" . sql)))

  :init
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
  
  :config
  (defun bristle--set-org-mode-font ()
    "Sets the default font be be variable pitch, just for org."
    (interactive)
    (setq buffer-face-mode-face '(:family "PragmataPro"))
    (buffer-face-mode))

  ;; disable styling of generated latex previews
  (plist-put org-format-latex-options :foreground nil)
  (plist-put org-format-latex-options :background nil)

  ;; scale down the generated SVGs a bit
  (plist-put org-format-latex-options :scale 0.6)

  ;; configure babel language support
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((mermaid . t)
     (octave . t)
     (shell . t)
     (verb . t)
     (go . t)
     (duckdb . t))))

(provide 'init-org)
