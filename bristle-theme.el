;; bristle-theme.el --- Custom color theme based on base16 ocean.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Jonathan Hope

;; Author: Jonathan Hope <jonathan.douglas.hope@gmail.com>
;; Version: 1.0
;; Keywords: theme

;;; Commentary:

;; bristle-theme is a theme based on base16 ocean.

;;; Code:

(deftheme bristle
  "")

(custom-theme-set-faces
 'bristle

 ;; basics
 '(default     ((t (:foreground "#eff1f5" :background "#2b303b"))))
 '(bold        ((t (:weight bold))))
 '(bold-italic ((t (:slant italic :weight bold))))
 '(underline   ((t (:underline t))))
 '(italic      ((t (:slant italic))))
 '(shadow      ((t (:foreground "#65737e"))))
 '(success     ((t (:foreground "#a3be8c"))))
 '(error       ((t (:foreground "#bf616a"))))
 '(warning     ((t (:foreground "#d08770"))))

 ;; emacs interface
 '(cursor              ((t (:foreground "#eff1f5" :background "#eff1f5"))))
 '(fringe              ((t (:background "#343d46"))))
 '(linum               ((t (:foreground "#65737e" :background "#2b303b" :weight normal :slant normal))))
 '(hl-line             ((t (:background "#343a47")))) ;; bg hsl lightened 5%
 '(border              ((t (:background "#343d46"))))
 '(border-glyph        ((t (:background "#343d46"))))
 '(highlight           ((t (:foreground "#2b303b" :background "#8fa1b3"))))
 '(link                ((t (:foreground "#8fa1b3" :underline t))))
 '(link-visited        ((t (:foreground "#b48ead" :underline t))))
 '(minibuffer-prompt   ((t (:foreground "#a3be8c" :weight bold))))
 '(region              ((t (:background "#4d566a" :slant italic))))
 '(secondary-selection ((t (:foreground "#2b303b" :background "#a3c6d0"))))
 '(header-line         ((t (:inherit mode-line))))
 '(whitespace-space ((t (:foreground "#65737e" :slant italic))))
 '(whitespace-tab ((t (:foreground "#65737e" :slant italic))))
 '(whitespace-newline ((t (:foreground "#65737e" :slant italic))))
 '(whitespace-trailing ((t (:foreground "#65737e" :slant italic))))
 '(whitespace-space-before-tab ((t (:foreground "#65737e" :slant italic))))
 '(whitespace-space-after-tab ((t (:foreground "#65737e" :slant italic))))
 '(whitespace-line ((t (:foreground "#65737e" :slant italic))))
 '(whitespace-indentation ((t (:foreground "#65737e" :slant italic))))
 '(trailing-whitespace ((t (:foreground "#65737e" :slant italic))))
 '(whitespace-big-indent ((t (:foreground "#65737e" :slant italic))))
 '(whitespace-empty ((t (:foreground "#65737e" :slant italic))))
 '(whitespace-hspace ((t (:foreground "#65737e" :slant italic))))

 ;; modeline
 '(mode-line           ((t (:foreground "#b48ead" :background "#343D46" :box nil))))
 '(mode-line-inactive  ((t (:foreground "#4f5b66" :background "#343D46" :box nil))))

 ;; parentheses
 '(show-paren-match    ((t (:foreground "#eff1f5" :weight bold :underline t))))
 '(show-paren-mismatch ((t (:foreground "#eff1f5" :background "#bf616a" :weight bold))))

 ;; diff-hl
 '(diff-hl-change  ((t (:background "#8fa1b3"))))
 '(diff-hl-insert  ((t (:background "#a3be8c"))))
 '(diff-hl-delete  ((t (:background "#bf616a"))))
 '(diff-hl-unknwon ((t (:background "#ebcb8b"))))

 ;; magit
 '(magit-branch                       ((t (:foreground "#a3be8c" :weight bold))))
 '(magit-cherry-equivalent            ((t (:foreground "#96b5b4"))))
 '(magit-cherry-unmatched             ((t (:foreground "#bf616a"))))
 '(magit-diff-add                     ((t (:foreground "#2b303b" :background "#a3be8c"))))
 '(magit-diff-del                     ((t (:foreground "#2b303b" :background "#bf616a"))))
 '(magit-diff-file-header             ((t (:foreground "#2b303b" :background "#a3c6d0"))))
 '(magit-diff-hunk-header             ((t (:foreground "#2b303b" :background "#a3c6d0"))))
 '(magit-diff-merge-current           ((t (:foreground "#b48ead"))))
 '(magit-diff-merge-diff3-separator   ((t (:foreground "#b48ead"))))
 '(magit-diff-merge-proposed          ((t (:foreground "#ab7967"))))
 '(magit-diff-merge-separator         ((t (:foreground "#ab7967"))))
 '(magit-diff-added-highlight         ((t (:foreground "#a3be8c"))))
 '(magit-diff-removed-highlight       ((t (:foreground "#bf616a"))))
 '(magit-diff-added                   ((t (:foreground "#a3be8c"))))
 '(magit-diff-removed                 ((t (:foreground "#bf616a"))))
 '(magit-diffstat-removed             ((t (:foreground "#bf616a"))))
 '(magit-diffstat-added               ((t (:foreground "#a3be8c"))))
 '(magit-diff-context                 ((t (:foreground "#65737e"))))
 '(magit-diff-context-highlight       ((t (:foreground "#65737e"))))
 '(magit-diff-hunk-heading            ((t (:background "#4f5b66"))))
 '(magit-diff-hunk-heading-highlight  ((t (:background "#4f5b66"))))
 '(magit-diff-whitespace-warning      ((t (:background "#bf616a"))))
 '(magit-diff-none                    ((t (:slant italic))))
 '(magit-header                       ((t (:background "#343d46" :weight bold))))
 '(magit-item-highlight               ((t (:foreground "#2b303b" :background "#8fa1b3"))))
 '(magit-item-mark                    ((t (:foreground "#2b303b" :background "#8fa1b3"))))
 '(magit-key-mode-args-face           ((t (:foreground "#2b303b" :background "#dfe1e8"))))
 '(magit-key-mode-button-face         ((t (:foreground "#96b5b4"))))
 '(magit-key-mode-header-face         ((t (:foreground "#b48ead"))))
 '(magit-key-mode-switch-face         ((t (:foreground "#bf616a" :weight bold))))
 '(magit-log-author                   ((t (:foreground "#8fa1b3"))))
 '(magit-log-date                     ((t (:foreground "#b48ead"))))
 '(magit-log-graph                    ((t (:foreground "#eff1f5"))))
 '(magit-log-head-label-bisect-bad    ((t (:foreground "#2b303b" :background "#bf616a"))))
 '(magit-log-head-label-bisect-good   ((t (:foreground "#2b303b" :background "#a3be8c"))))
 '(magit-log-head-label-bisect-skip   ((t (:foreground "#2b303b" :background "#ebcb8b"))))
 '(magit-log-head-label-default       ((t (:foreground "#8fa1b3" :background "#2b303b" :box 1))))
 '(magit-log-head-label-head          ((t (:foreground "#d08770" :background "#2b303b" :box 1))))
 '(magit-log-head-label-local         ((t (:foreground "#b48ead" :background "#2b303b" :box 1))))
 '(magit-log-head-label-patches       ((t (:foreground "#bf616a" :background "#2b303b" :box 1))))
 '(magit-log-head-label-remote        ((t (:foreground "#a3be8c" :background "#2b303b" :box 1))))
 '(magit-log-head-label-tags          ((t (:foreground "#ebcb8b" :background "#2b303b" :box 1))))
 '(magit-log-head-wip                 ((t (:foreground "#96b5b4" :background "#2b303b" :box 1))))
 '(magit-log-reflog-label-amend       ((t (:foreground "#ebcb8b" :background "#2b303b" :box 1))))
 '(magit-log-reflog-label-checkout    ((t (:foreground "#96b5b4" :background "#2b303b" :box 1))))
 '(magit-log-reflog-label-cherry-pick ((t (:foreground "#a3be8c" :background "#2b303b" :box 1))))
 '(magit-log-reflog-label-commit      ((t (:foreground "#8fa1b3" :background "#2b303b" :box 1))))
 '(magit-log-reflog-label-merge       ((t (:foreground "#ab7967" :background "#2b303b" :box 1))))
 '(magit-log-reflog-label-other       ((t (:foreground "#eff1f5" :background "#2b303b" :box 1))))
 '(magit-log-reflog-label-rebase      ((t (:foreground "#b48ead" :background "#2b303b" :box 1))))
 '(magit-log-reflog-label-remote      ((t (:foreground "#d08770" :background "#2b303b" :box 1))))
 '(magit-log-reflog-label-reset       ((t (:foreground "#bf616a" :background "#2b303b" :box 1))))
 '(magit-log-sha1                     ((t (:foreground "#bf616a" :background "#2b303b" :box 1))))
 '(magit-process-ng                   ((t (:foreground "#2b303b" :background "#bf616a" :weight bold))))
 '(magit-process-ok                   ((t (:foreground "#2b303b" :background "#a3be8c" :weight bold))))
 '(magit-section-title                ((t (:background "#343d46"))))
 '(magit-section-highlight            ((t (:background "#343a47"))))
 '(magit-tag                          ((t (:foreground "#b48ead" :background "#2b303b" :box 1))))
 '(magit-valid-signature              ((t (:foreground "#a3be8c" :weight bold))))
 '(magit-whitespace-warning-face      ((t (:foreground "#a7adba"))))
 '(magit-branch-local                 ((t (:foreground "#96b5b4"))))
 '(magit-branch-remote                ((t (:foreground "#a3be8c"))))
 '(magit-branch-current               ((t (:foreground "#96b5b4"))))
 '(magit-section-heading              ((t (:foreground "#ebcb8b" :weight bold))))
 '(magit-hash                         ((t (:foreground "#4f5b66"))))

 ;; org
 '(org-agenda-date           ((t (:foreground "#8fa1b3"))))
 '(org-agenda-date-today     ((t (:foreground "#96b5b4" :weight bold))))
 '(org-agenda-date-weekend   ((t (:foreground "#8fa1b3" :background "#343d46"))))
 '(org-agenda-structure      ((t (:foreground "#8fa1b3"))))
 '(org-agenda-done           ((t (:foreground "#a3be8c"))))
 '(org-date                  ((t (:foreground "#8fa1b3" :underline t))))
 '(org-done                  ((t (:foreground "#96b5b4" :weight bold))))
 '(org-todo                  ((t (:foreground "#d08770" :weight bold))))
 '(org-checkbox              ((t (:foreground "#c0c5ce" :weight bold))))
 '(org-drawer                ((t (:foreground "#8fa1b3"))))
 '(org-ellipsis              ((t (:foreground "#eff1f5" :underline t))))
 '(org-footnote              ((t (:foreground "#96b5b4" :underline t))))
 '(org-hide                  ((t (:foreground "#343d46"))))
 '(org-latex-and-related     ((t (:foreground "#d08770"))))
 '(org-list-dt               ((t (:foreground "#dfe1e8" :weight bold))))
 '(org-scheduled             ((t (:foreground "#a3be8c"))))
 '(org-scheduled-today       ((t (:foreground "#a3be8c" :weight bold))))
 '(org-table                 ((t (:foreground "#d08770"))))
 '(org-todo                  ((t (:foreground "#eff1f5"))))
 '(org-level-1               ((t (:foreground "#a3be8c" :weight bold))))
 '(org-level-2               ((t (:foreground "#a3be8c" :weight bold))))
 '(org-level-3               ((t (:foreground "#a3be8c" :weight bold))))
 '(org-level-4               ((t (:foreground "#a3be8c" :weight bold))))
 '(org-level-5               ((t (:foreground "#a3be8c" :weight bold))))
 '(org-level-6               ((t (:foreground "#a3be8c" :weight bold))))
 '(org-tag                   ((t (:foreground "#ebcb8b" :weight bold))))
 '(org-block                 ((t (:foreground "#eff1f5"))))
 '(org-block-begin-line      ((t (:background "#343d46" :foreground "#c0c5ce" :weight bold))))
 '(org-block-end-line        ((t (:background "#343d46" :foreground "#c0c5ce" :weight bold))))
 '(org-priority              ((t (:foreground "#ebcb8b" :weight bold))))
 '(org-special-keyword       ((t (:foreground "#65737e" :slant italic))))
 '(org-document-info-keyword ((t (:foreground "#65737e" :slant italic))))
 '(org-document-title        ((t (:foreground "#65737e" :slant italic))) )
 '(org-headline-done         ((t (:foreground "#a3be8c" :weight bold))) )
 '(org-code                  ((t (:box t))) )

 ;; flyspell
 '(flyspell-duplicate ((t (:foreground "#ebcb8b" :weight bold))))
 '(flyspell-incorrect ((t (:foreground "#bf616a" :weight bold))))

 ;; flycheck
 '(flycheck-info ((t (:weight bold :underline t))))
 '(flycheck-warning ((t (:foreground "#ebcb8b" :weight bold :underline t))))
 '(flycheck-error ((t (:foreground "#bf616a" :weight bold :underline t))))

 ;; flymake
 '(flymake-warning ((t (:foreground "#ebcb8b" :weight bold))))
 '(flymake-error ((t (:foreground "#bf616a" :weight bold))))

 ;; jinx
 '(jinx-misspelled ((t (:foreground "#ebcb8b" :weight bold))))

 ;; isearch
 '(isearch ((t (:underline t))))

 ;; verb
 '(verb-http-keyword ((t (:foreground "#b48ead" :weight bold))))
 
 ;; corfu
 '(corfu-default ((t (:background "#343D46" :foreground: "#eff1f5"))))
 '(corfu-current ((t (:background "#4f5b66" :foreground: "#eff1f5"))))
 '(corfu-bar ((t (:background "#eff1f5"))))
 '(corfu-annotations ((t (:foreground "#8fa1b3"))))

 ;; mode line
 '(bristle-mode-line               ((t (:foreground "#b48ead" :background "#343D46" :box nil))))
 '(bristle-mode-line-inactive      ((t (:foreground "#4f5b66" :weight bold))))
 '(bristle-mode-line-file-status   ((t (:foreground "#d08770" :weight bold))))
 '(bristle-mode-line-buffer-name   ((t (:foreground "#a3be8c" :weight bold))))
 '(bristle-mode-line-project       ((t (:foreground "#8fa1b3" :weight bold))))
 '(bristle-mode-line-mode          ((t (:foreground "#AA7961" :weight bold))))
 '(bristle-mode-line-lighter       ((t (:foreground "#ebcb8b" :weight bold))))
 '(bristle-mode-line-row-column    ((t (:foreground "#96b5b4" :weight bold))))
 '(bristle-mode-line-scroll-bar    ((t (:foreground "#dfe1e8"))))
 '(bristle-mode-line-popper        ((t (:foreground "#ebcb8b"))))
 '(bristle-mode-transient-override ((t (:foreground "#a7adba" :weight bold))))

 ;; Selectrum/Vertico/Orderless/Consult
 '(selectrum-current-candidate ((t (:background "#343d46" :weight bold))))
 '(vertico-current ((t (:background "#343d46" :weight bold))))
 '(consult-preview-line ((t (:background "#343d46" :weight bold))))
 '(orderless-match-face-0      ((t (:underline t :weight bold))))
 '(orderless-match-face-1      ((t (:underline t :weight bold))))
 '(orderless-match-face-2      ((t (:underline t :weight bold))))
 '(orderless-match-face-3      ((t (:underline t :weight bold))))
 '(consult-preview-match       ((t (:underline t :weight bold))))
 '(completions-common-part       ((t (:underline t :weight bold))))

 ;; highlight numbers
 '(highlight-numbers-number ((t (:foreground "#96b5b4" :slant italic))))

 ;; slate
 '(slate-header-face ((t (:foreground "#b48ead" :weight bold))))
 '(slate-priority-a-face ((t (:foreground "#bf616a" :weight bold))))
 '(slate-priority-b-face ((t (:foreground "#d08770" :weight bold))))
 '(slate-priority-c-face ((t (:foreground "#ebcb8b" :weight bold))))
 '(slate-file-name-face ((t (:foreground "#96b5b4" :weight bold))))
 '(slate-divider-face ((t (:foreground "#4f5b66" :weight bold))))
 '(slate-line-number-face ((t (:foreground "#c0c5ce" :weight bold))))
 '(slate-todo-face ((t (:foreground "#a3be8c"))))
 '(slate-filter-text-face ((t (:foreground "#eff1f5"))))
 '(slate-tags-face ((t (:foreground "#dfe1e8" :weight bold))))

 ;; dired
 '(dired-header ((t (:foreground "#a3be8c" :weight bold))))
 '(dired-marked ((t (:foreground "#bf616a" :weight bold))))

 ;; avy
 '(avy-lead-face ((t (:foreground "#bf616a" :weight bold :underline t))))
 '(avy-lead-face-0 ((t (:foreground "#bf616a" :weight bold :underline t))))
 '(avy-lead-face-1 ((t (:foreground "#bf616a" :weight bold :underline t))))
 '(avy-lead-face-2 ((t (:foreground "#bf616a" :weight bold :underline t))))

 ;; hl-todo
 '(hl-todo ((t (:foreground "#bf616a" :weight bold :underline t))))

 ;; smerge
 '(smerge-upper ((t ())))
 '(smerge-lower ((t ())))
 '(smerge-markers ((t (:background "#343d46" :foreground "#c0c5ce" :weight bold))))
 '(smerge-refined-added ((t (:weight bold))))
 '(smerge-refined-removed ((t (:weight bold))))

 ;; markdown
 '(markdown-markup-face                ((t (:foreground "#65737e"))))
 '(markdown-header-face-1              ((t (:foreground "#a3be8c" :weight bold))))
 '(markdown-header-face-2              ((t (:foreground "#a3be8c" :weight bold))))
 '(markdown-header-face-3              ((t (:foreground "#a3be8c" :weight bold))))
 '(markdown-header-face-4              ((t (:foreground "#a3be8c" :weight bold))))
 '(markdown-header-face-5              ((t (:foreground "#a3be8c" :weight bold))))
 '(markdown-header-face-6              ((t (:foreground "#a3be8c" :weight bold))))
 '(markdown-code-face                  ((t (:background "#343a47"))))
 '(markdown-pre-face                   ((t (:background "#343a47"))))

 
 ;; notmuch
 '(notmuch-tree-match-tag-face ((t (:foreground "#ebcb8b"))))
 '(notmuch-tag-face ((t (:foreground "#ebcb8b"))))
 '(notmuch-tag-unread ((t (:foreground "#bf616a"))))
 '(notmuch-search-date ((t (:foreground "#a3be8c"))))
 '(notmuch-tree-match-date-face ((t (:foreground "#a3be8c"))))
 '(notmuch-search-matching-authors ((t (:foreground "#b48ead"))))
 '(notmuch-tree-match-author-face ((t (:foreground "#b48ead"))))
 '(notmuch-tree-match-tree-face ((t (:foreground "#d08770"))))
 '(notmuch-message-summary-face ((t (:weight bold))))
 '(message-header-name ((t (:foreground "#a3be8c"))))
 '(message-header-subject ((t (:foreground "#eff1f5"))))
 '(message-header-to ((t (:foreground "#eff1f5"))))
 '(message-header-other ((t (:foreground "#eff1f5"))))
 '(message-mml ((t (:foreground "#8fa1b3"))))
 '(message-header-cc ((t (:foreground "#eff1f5"))))
 '(message-cited-text-1 ((t (:foreground "#65737e" :slant italic))))
 '(message-cited-text-2 ((t (:foreground "#65737e" :slant italic))))
 '(message-cited-text-3 ((t (:foreground "#65737e" :slant italic))))
 '(message-cited-text-4 ((t (:foreground "#65737e" :slant italic))))

 ;; terraform
 '(terraform--resource-type-face ((t (:foreground "#c0c5ce"))))

 ;; eshell
 '(eshell-prompt-bullet ((t (:foreground "#b48ead" :weight bold))))
 '(eshell-prompt-lambda ((t (:foreground "#a3be8c" :weight bold))))
 '(eshell-ls-backup ((t (:foreground "#d08770"))))
 '(eshell-ls-archive ((t (:foreground "#b48ead"))))
 '(eshell-ls-clutter ((t (:foreground "#d08770"))))
 '(eshell-ls-missing ((t (:foreground "#bf616a"))))
 '(eshell-ls-product ((t (:foreground "#d08770"))))
 '(eshell-ls-special ((t (:foreground "#b48ead"))))
 '(eshell-ls-symlink ((t (:foreground "#ebcb8b"))))
 '(eshell-ls-readonly ((t (:foreground "#96b5b4"))))
 '(eshell-ls-directory ((t (:foreground "#8fa1b3"))))
 '(eshell-ls-executable ((t (:foreground "#a3be8c"))))
 '(eshell-ls-executable ((t (:foreground "#343D46"))))
 
 ;; eat
 '(eat-term-color-0 ((t (:foreground "#2b303b"))))
 '(eat-term-color-1 ((t (:foreground "#bf616a"))))
 '(eat-term-color-2 ((t (:foreground "#a3be8c"))))
 '(eat-term-color-3 ((t (:foreground "#ebcb8b"))))
 '(eat-term-color-4 ((t (:foreground "#a7adba"))))
 '(eat-term-color-5 ((t (:foreground "#b48ead"))))
 '(eat-term-color-6 ((t (:foreground "#96b5b4"))))
 '(eat-term-color-7 ((t (:foreground "#96b5b4"))))
 '(eat-term-color-8 ((t (:foreground "#343D46"))))
 '(eat-term-color-9 ((t (:foreground "#bf616a"))))

 ;; dired-subtree
 '(dired-subtree-depth-1-face ((t ())))
 '(dired-subtree-depth-2-face ((t ())))
 '(dired-subtree-depth-3-face ((t ())))
 '(dired-subtree-depth-4-face ((t ())))
 '(dired-subtree-depth-5-face ((t ())))
 '(dired-subtree-depth-6-face ((t ())))

 ;; soma
 '(soma-id-face               ((t (:foreground "#65737e" :weight bold))))
 '(soma-filetags-face         ((t (:foreground "#ebcb8b" :slant italic))))

 ;; tempel
 '(tempel-field ((t ())))
 '(tempel-form ((t ())))
 '(tempel-default ((t ())))

 ;; transient
 '(transient-heading              ((t (:foreground "#b48ead" :weight bold))))
 '(transient-key-exit             ((t (:foreground "#96b5b4" :weight bold))))
 '(transient-key-stack            ((t (:foreground "#bf616a" :weight bold))))
 '(transient-key-recurse          ((t (:foreground "#bf616a" :weight bold))))

 ;; legend
 '(legend-key                    ((t (:foreground "#a3be8c" :weight bold)))) 
 '(legend-separator              ((t (:foreground "#65737e")))) 
 '(legend-column-separator       ((t (:foreground "#65737e")))) 
 '(legend-description            ((t (:foreground "#c0c5ce"))))

 ;; re-builder
 '(reb-match-0 ((t (:weight bold :underline t))))
 '(reb-match-1 ((t (:foreground "#a3be8c" :weight bold :underline t))))
 '(reb-match-2 ((t (:foreground "#b48ead" :weight bold :underline t))))

 ;; dape
 '(dape-breakpoint-face              ((t (:foreground "#bf616a"))))
 '(dape-log-face                     ((t :inherit dape-breakpoint-face :height 0.85 :box (:line-width -1))))
 '(dape-expression-face              ((t :inherit dape-breakpoint-face :height 0.85 :box (:line-width -1))))
 '(dape-hits-face                    ((t :inherit dape-breakpoint-face :height 0.85 :box (:line-width -1))))

 ;; ibuffer
 '(ibuffer-title-face              ((t (:foreground "#ebcb8b" :weight bold))))
 '(ibuffer-marked-face             ((t (:foreground "#bf616a" :weight bold))))

 ;; ediff
 '(ediff-current-diff-A    ((t (:background "#4d566a"))))
 '(ediff-current-diff-B    ((t (:background "#4d566a"))))
 '(ediff-odd-diff-A        ((t (:background "#343a47"))))
 '(ediff-odd-diff-B        ((t (:background "#343a47"))))
 '(ediff-even-diff-A       ((t (:background "#343a47"))))
 '(ediff-even-diff-B       ((t (:background "#343a47"))))
 '(ediff-fine-diff-A       ((t (:foreground "#bf616a" :weight bold :underline t))))
 '(ediff-fine-diff-B       ((t (:foreground "#a3be8c" :weight bold :underline t))))

 ;; dashboard
 '(dashboard-items-face ((t (:foreground "#eff1f5"))))
 '(dashboard-heading ((t (:foreground "#b48ead" :weight bold))))
 '(dashboard-text-banner ((t (:foreground "#a3be8c" :weight bold))))
 '(dashboard-banner-logo-title ((t (:foreground "#a3be8c" :weight bold))))
 
 ;; shr
 '(shr-text    ((t)))
 
 ;; default font locking
 '(font-lock-builtin-face              ((t (:foreground "#96b5b4"))))
 '(font-lock-comment-delimiter-face    ((t (:foreground "#65737e" :slant italic))))
 '(font-lock-comment-face              ((t (:foreground "#65737e" :slant italic))))
 '(font-lock-constant-face             ((t (:foreground "#96b5b4"))))
 '(font-lock-doc-face                  ((t (:foreground "#a7adba" :slant italic))))
 '(font-lock-doc-string-face           ((t (:foreground "#ebcb8b" :slant italic))))
 '(font-lock-function-name-face        ((t (:foreground "#8fa1b3"))))
 '(font-lock-keyword-face              ((t (:foreground "#b48ead"))))
 '(font-lock-negation-char-face        ((t (:foreground "#a3be8c"))))
 '(font-lock-preprocessor-face         ((t (:foreground "#b48ead"))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "#ebcb8b"))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "#b48ead"))))
 '(font-lock-string-face               ((t (:foreground "#a3be8c" :slant italic))))
 '(font-lock-type-face                 ((t (:foreground "#ebcb8b"))))
 '(font-lock-variable-name-face        ((t (:foreground "#d08770"))))
 '(font-lock-operator-face             ((t (:foreground "#a3be8c"))))
 '(font-lock-bracket-face              ((t (:foreground "#a7adba"))))
 '(font-lock-warning-face              ((t (:foreground "#bf616a" :weight bold)))))

(provide-theme 'bristle)

;; bristle-theme.el ends here
