;; -*- lexical-binding: t; -*-

;; set emacs to use UTF-8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; enable straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; integrate straight with use-package
(straight-use-package '(use-package :type built-in))

;; Configure packages using use-package.
(add-to-list 'load-path (concat user-emacs-directory "package-config"))

;; prevent custom from polluting my init file
(setq custom-file "~/.emacs.d/custom.el")

;; configure packages

;; core

(require 'init-compat)
(require 'init-envrc)
(require 'init-inheritenv)
(require 'init-fontaine)
(require 'init-move-text)
(require 'init-avy)
(require 'init-vertico)
(require 'init-orderless)
(require 'init-marginalia)
(require 'init-consult)
(require 'init-corfu)
(require 'init-embark)
(require 'init-windmove)
(require 'init-hl-todo)
(require 'init-expreg)
(require 'init-legend)
(require 'init-gestalt)
(require 'init-bristle-mode-line)
(require 'init-pcre2el)
(require 'init-stadion)
(require 'init-apheleia)
(require 'init-eglot)
(require 'init-tempel)
(require 'init-dape)
(require 'init-paredit)
(require 'init-consult-hl-todo)
(require 'init-devcontainer)
(require 'init-engine-mode)

;; minor modes

(require 'init-jinx)
(require 'init-rainbow-mode)

;; shell

(require 'init-popper)
(require 'init-eshell)
(require 'init-eat)

;; file manager

(require 'init-dired)
(require 'init-dired-subtree)
(require 'init-nerd-icons-dired)
(require 'init-pine)

;; calculator

(require 'init-calc)

;; notebook

(require 'init-melitt)
(require 'init-org)
(require 'init-denote)
(require 'init-soma)
(require 'init-olivetti)
(require 'init-ob-mermaid)
(require 'init-verb)
(require 'init-hide-mode-line)
(require 'init-dslide)
(require 'init-ox-pandoc)
(require 'init-ob-go)
(require 'init-ob-typescript)
(require 'init-ob-duckdb)

;; source control

(require 'init-magit)

;; docker

(require 'init-docker)

;; side by side diff

(require 'init-ediff)

;; docs

(require 'init-devdocs)

;; buffer management

(require 'init-ibuffer)

;; email

(require 'init-notmuch)

;; language/config support

(require 'init-mermaid-ts-mode)
(require 'init-go-mode)
(require 'init-json-mode)
(require 'init-typescript-mode)
(require 'init-jsdoc)
(require 'init-yaml-mode)
(require 'init-toml-mode)
(require 'init-nix-mode)
(require 'init-html-mode)
(require 'init-css-mode)
(require 'init-fish-mode)
(require 'init-dockerfile-mode)
(require 'init-bash-mode)
(require 'init-graphql-mode)
(require 'init-terraform-mode)
(require 'init-xml-mode)
(require 'init-markdown-mode)

;; llm

(require 'init-gptel) ;; chat
(require 'init-gptel-quick) ;; summary
(require 'init-copilot) ;; completion (with FIM)
(require 'init-mcp) ;; mcp server management
(require 'init-emacs-mcp) ;; MCP server for Emacs itself

;; configure Emacs

(use-package emacs
  :custom
  ;; disable the ~ files when editing
  (create-lockfiles nil)

  ;; no auto save
  (auto-save-default nil)
  (auto-save-list-file-prefix nil)

  ;; no backup files
  (make-backup-files nil)

  ;; hide the initial scratch message
  (initial-scratch-message "")

  ;; disable bell
  (ring-bell-function 'ignore)

  ;; prevent splash screen
  (inhibit-splash-screen t)

  ;; don't delete prompts
  (comint-prompt-read-only t)

  ;; no newlines at end of file
  (require-final-newline nil)

  ;; disable shift to select
  (shift-select-mode nil)
  
  ;; no popup modals
  (use-dialog-box nil)

  ;; most tree-sitter highlighting
  (treesit-font-lock-level 4)

  ;; support opening new minibuffers from inside existing minibuffers
  (enable-recursive-minibuffers t)

  ;; scroll one line at a time
  (hscroll-margin 1)
  (hscroll-step 1)
  (scroll-conservatively 101)
  (scroll-margin 0)
  (scroll-preserve-screen-position t)
  (auto-window-vscroll nil)
  (fast-but-imprecise-scrolling t)

  ;; do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; hide commands in M-x which do not work in the current mode
  (read-extended-command-predicate #'command-completion-default-include-p)

  ;; if the car of one of these is inserted; the cdr will be as well
  (electric-pair-pairs
      '(
        (?\" . ?\")
        (?\{ . ?\})
        (?\[ . ?\])
        (?\` . ?\`)
        (?\( . ?\))))

  ;; pinentry setup
  (epg-pinentry-mode 'loopback)

  ;; hide the tab bar
  (tab-bar-show nil)

  ;; set the default tab buffer
  (tab-bar-new-tab-choice "*scratch*")

  ;; treesitter grammar config
  (treesit-language-source-alist
   '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.2" "typescript/src")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.2" "tsx/src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
     (mermaid "https://github.com/monaqa/tree-sitter-mermaid")
     (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
     (cmake "https://github.com/uyha/tree-sitter-cmake" "v0.2.0")
     (bash "https://github.com/tree-sitter/tree-sitter-bash" "v0.23.3")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (nix "https://github.com/nix-community/tree-sitter-nix")
     (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
     (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc")))

  ;; it was tricky to get scripts to use the ts mode for bash
  ;; I had to remap it
  (major-mode-remap-alist
   '((bash-mode . bash-ts-mode)
     (sh-mode . bash-ts-mode)))

  ;; start the scratch file in text mode
  (initial-major-mode 'text-mode)

  ;; don't yank text properties
  (yank-excluded-properties t)

  ;; scroll the compilation output automatically
  (compilation-scroll-output t)
  
  :config
  
  ;; no tabs, use spaces instead
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
 
  ;; allow up and down casing regions
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)

  ;; allow narrow to region
  (put 'narrow-to-region 'disabled nil)
  
  ;; hide menu, toolbar, and fringe
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (fringe-mode '(0 . 0))

  ;; disable the scrollbar
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

  ;; start in full screen
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; y and n instead of yes and no
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; hide the slash that appears at the end of lines that are going to wrap
  (set-display-table-slot standard-display-table 'wrap ?\ )
  
  ;; don't show the cursor in windows that are not selected
  (setq-default cursor-in-non-selected-windows nil)

  ;; hide initial minibuffer text
  (defun display-startup-echo-area-message ()
    (message ""))

  ;; hide ^M characters
  (add-hook 'after-change-major-mode-hook 'bristle--hide-dos-eol)
  (defun bristle--hide-dos-eol ()
    "Hide ^M in files containing mixed UNIX and DOS line endings."
    (interactive)
    (setq buffer-display-table (make-display-table))
    (aset buffer-display-table ?\^M []))

  ;; insert the other half of common pairs automatically
  (electric-pair-mode 1)

  ;; load the theme
  (load-theme 'bristle t)
  
  ;; leave no way to enable overwrite mode
  (unbind-key "<insert>" global-map)

  ;; disable suspend frame
  (unbind-key "C-z" global-map)
  (unbind-key "C-x C-z" global-map)
  
  ;; turn off the blinking cursor
  (blink-cursor-mode -1)

  ;; clean up the vertical border between windows
  (defface hidden-vertical-border
    '((t (:foreground "#343D46" :background "#343D46")))
    "Face for hiding vertical window borders.")
  (set-display-table-slot standard-display-table
                          'vertical-border (make-glyph-code ?\u200B))
  (set-face-attribute 'vertical-border nil
                      :foreground (face-foreground 'hidden-vertical-border)
                      :background (face-background 'hidden-vertical-border))

  ;; highlight the current line
  (global-hl-line-mode 1)
  
  ;; disable the keyboard quit message
  (put 'quit 'error-message "")

  ;; enable tab-bar-mode (basically workspaces)
  (tab-bar-mode)
  (define-key tab-bar-mode-map (kbd "C-<tab>") nil)
  (tab-bar-rename-tab "default")

  ;; don't use ispell for completion at point
  (with-eval-after-load 'ispell
    (setq completion-at-point-functions
          (delq 'ispell-completion-at-point completion-at-point-functions)))
  (advice-add 'ispell-completion-at-point :around #'ignore)

  ;; start a buffer to pipe llm responses to
  (add-hook 'after-init-hook
            (lambda ()
              (with-current-buffer (get-buffer-create "*llm-scratch*")
                (markdown-mode))))

  ;; random things that should just be treated as text
  (add-to-list 'auto-mode-alist '("\\.env\\(?:\\..*\\)?\\'" . text-mode))
  (add-to-list 'auto-mode-alist '("LICENSE\\'" . text-mode))

  ;; safe dir local vars
  (put 'dape-configs 'safe-local-variable #'listp)

  ;; osx specific handling
  (when (eq system-type 'darwin)
    (setq insert-directory-program "gls"))

  ;; have tramp use remote paths
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; useful functions

(defun what-face (pos)
  "Get the face in use at the current position."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun treesit-install-all ()
  "Install all of the needed treesitter grammars."
    (interactive)
    (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))

(defun lookup-password (service)
  "Look up password for SERVICE using secret-tool.
The password can be set with `secret-tool store --label='SERVICE' SERVICE password'"
  (string-trim
         (shell-command-to-string 
          (format "secret-tool lookup %s password" 
                  (shell-quote-argument service)))))

(defun devdocs-install-all ()
  "Install all of the needed devdocs."
  (interactive)
  (devdocs-install "go")
  (devdocs-install "javascript"))
