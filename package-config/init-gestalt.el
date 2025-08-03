;; -*- lexical-binding: t; -*-

(straight-use-package 
 `(gestalt
   :type nil
   :local-repo ,(concat user-emacs-directory "gestalt")
   :files ("gestalt.el")))

(use-package gestalt
  :init
  (add-hook 'activate-mark-hook #'bristle--enter-region)
  (add-hook 'deactivate-mark-hook #'bristle--leave-region)
  (advice-add 'wdired-finish-edit :after #'gestalt--major-mode-changed)
  (advice-add 'wdired-abort-changes :after #'gestalt--major-mode-changed)
  (add-hook 'after-change-major-mode-hook #'bristle--set-mode-facet)
  (add-hook 'smerge-mode-hook #'bristle--set-mode-facet)
  (add-hook 'after-change-major-mode-hook #'bristle--set-mode-region-facet)
  
  :custom
  (gestalt-facet-modes
   '((prog-mode . move)
     (text-mode . move)
     (conf-mode . move)
     (dired-mode . dired)
     (pine-mode . dirtree)
     (wdired-mode . wdired)
     (calc-mode . calc)
     (mcp-hub-mode . mcp)
     (smerge-mode . move)
     (compilation-mode . move)
     (ibuffer-mode . ibuffer)
     (woman-mode . move)
     (devdocs-mode . move)
     (notmuch-hello-mode . notmuch-hello)
     (notmuch-search-mode . notmuch-search)
     (notmuch-tree-mode . notmuch-tree)
     (notmuch-show-mode . notmuch-show)))
  
  :config
  (gestalt-init)

  ;; variables
  
  (defvar-local bristle--mode-facet nil
    "Mode specific facet to run (if any).")

  (defvar-local bristle--mode-region-facet nil
  "Mode specific region facet to run (if any).")

  ;; hook helpers

  (defun bristle--enter-region ()
    "Switch from the move facet to the region/rectangle facet."
    (when (gestalt-move-facet-p)
      (if (bound-and-true-p rectangle-mark-mode)
          (gestalt-rectangle-facet-activate)
        (gestalt-region-facet-activate))))
  
  (defun bristle--leave-region ()
    "Switch from the region/rectangle facet to the move facet."
    (when (or (gestalt-region-facet-p)
              (gestalt-rectangle-facet-p))
      (gestalt-move-facet-activate)))

  (defun bristle--set-mode-facet ()
  "Some major modes have mode specific facets; this defines those."
  (setq bristle--mode-facet
        (cond
         ((eq major-mode 'org-mode) 'gestalt-org-transient-facet)
         ((eq major-mode 'typescript-ts-mode) 'gestalt-js-transient-facet)
         ((eq major-mode 'tsx-ts-mode) 'gestalt-js-transient-facet)
         ((eq major-mode 'js-ts-mode) 'gestalt-js-transient-facet)
         ((eq major-mode 'notmuch-message-mode) 'gestalt-notmuch-message-transient-facet)
         ((bound-and-true-p smerge-mode) 'gestalt-smerge-facet-activate)
         (t nil))))

  (defun bristle--set-mode-region-facet ()
  "Some major modes have mode specific region facets; this defines those."
  (setq bristle--mode-region-facet
        (cond
         ((eq major-mode 'org-mode) 'gestalt-orgf-transient-facet)
         (t nil))))
  
  ;; global keybinds

  (global-set-key (kbd "C-S-<tab>") #'gestalt-system-override-facet)
  (global-set-key (kbd "C-S-<iso-lefttab>") #'gestalt-system-override-facet)
  (global-set-key (kbd "<escape>") #'bristle--keyboard-quit)

  (defun bristle--keyboard-quit ()
    "Have escape quit many things:

 - iedit
 - wdired
 - eshell command
 - minibuffer
 - template expansion
 - mark
 - facet"
    (interactive)
    (cond ((bound-and-true-p iedit-mode) (iedit-mode))
          ((bound-and-true-p iedit-rectangle-mode) (iedit-rectangle-mode))
          ((derived-mode-p 'wdired-mode) (wdired-abort-changes))
          ((derived-mode-p 'eshell-mode) (eshell-interrupt-process))
          ((active-minibuffer-window) (if (minibufferp)
                                          (abort-minibuffers)
                                        (abort-recursive-edit)))
          ((and (boundp 'tempel--active) tempel--active) (tempel-abort))
          (mark-active (deactivate-mark t))
          (t (progn (gestalt-to-parent-facet) (keyboard-quit)))))

  ;; system level facet
  ;; this is primarily used to manage windows/frames
  ;; as an override facet it can be accessed _anywhere_
  
  (gestalt-define-override-facet
   system "<escape>"
   '("<right>" . ("move right" . windmove-right))
   '("<left>" . ("move left" . windmove-left))
   '("<up>" . ("move up" . windmove-up))
   '("<down>" . ("move down" . windmove-down))
   '("|" . ("split hor" . split-window-horizontally))
   '("_" . ("split ver" . split-window-vertically))
   '("S-<left>" . ("grow left" . bristle--move-splitter-left))
   '("S-<right>" . ("grow right" . bristle--move-splitter-right))
   '("S-<up>" . ("grow up" . bristle--move-splitter-up))
   '("S-<down>" . ("grow down" . bristle--move-splitter-down))
   '("=" . ("win balance" . balance-windows))
   '("d" . ("win delete" . delete-window))
   '("f" . ("find file" . find-file))
   '("b" . ("switch buffer" . consult-buffer))
   '("a" . ("app" . bristle--select-app))
   '("m" . ("minor mode" . bristle--select-minor-mode))
   '("p" . ("font preset" . fontaine-set-preset))
   '("t" . ("dir tree" . pine-toggle))
   '("n" . ("notes" . soma-consult-notes))
   '("N" . ("note create" . denote))
   '("D" . ("frame delete" . delete-frame))
   '("o" . ("frame other" . other-frame))
   '("c" . ("frame create" . make-frame))
   '("+" . ("tab new" . bristle--new-tab))
   '("x" . ("tab close" . tab-bar-close-tab-by-name))
   '("s" . ("tab select" . tab-bar-select-tab-by-name))
   '("?" . ("legend" . bristle--legend-toggle)))

  (defun bristle--move-splitter-left (arg)
    "Move window splitter left."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (shrink-window-horizontally arg)
      (enlarge-window-horizontally arg)))

  (defun bristle--move-splitter-right (arg)
    "Move window splitter right."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (enlarge-window-horizontally arg)
      (shrink-window-horizontally arg)))

  (defun bristle--move-splitter-up (arg)
    "Move window splitter up."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (enlarge-window arg)
      (shrink-window arg)))

  (defun bristle--move-splitter-down (arg)
    "Move window splitter down."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (shrink-window arg)
      (enlarge-window arg)))

  (defun bristle--select-app ()
    "Choose an app to launch."
    (interactive)
    (let* ((commands '(("eshell" . eshell)
                       ("dired" . (lambda () (dired default-directory)))
                       ("calc" . full-calc)
                       ("magit" . magit)
                       ("regex builder" . re-builder)
                       ("ibuffer" . ibuffer)
                       ("ediff" . (lambda () (call-interactively 'ediff-buffers)))
                       ("llm chat" . (lambda () (call-interactively 'gptel)))
                       ("notmuch" . notmuch)))
           (choice (completing-read "App:" (mapcar #'car commands)))
           (command (cdr (assoc choice commands))))
      (when command
        (funcall command))))

  (defun bristle--select-minor-mode ()
  "Select a minor mode to toggle."
  (interactive)
  (let* ((modes '(("whitespace" . whitespace-mode)
                  ("jinx" . jinx-mode)
                  ("rainbow" . rainbow-mode)))
         (choice (completing-read "Toggle mode: " (mapcar #'car modes)))
         (mode-function (cdr (assoc choice modes))))
    (when mode-function
      (call-interactively mode-function))))

  (defun bristle--new-tab (name)
    "Create a new tab with a specific NAME."
    (interactive "sName: ")
    (tab-bar-new-tab)
    (tab-bar-rename-tab name))
  
  ;; move facet
  ;; this is primarily about getting around
  ;; also used to start marking things

  (gestalt-define-facet
   move
   "Facet to do things other than insert text."
   :lighter " [MVE]"
   :keymap (gestalt-make-command-map
            '("i" . ("insert" . gestalt-insert-facet-activate))
            '("l" . ("line" . gestalt-line-facet-activate))
            '("t" . ("todo" . gestalt-todo-facet-activate))
            '("$" . ("spelling" . gestalt-spelling-facet-activate))
            '("#" . ("debug" . gestalt-debug-transient-facet))
            '("m" . ("mark point" . bristle--mark-point))
            '("r" . ("mark rectangle" . rectangle-mark-mode))
            '("p" . ("point" . gestalt-point-transient-facet))
            '("/" . ("lsp" . gestalt-lsp-transient-facet))
            '("." . ("mode specific" . bristle--invoke-mode-facet))
            '("SPC" . ("leader" . gestalt-leader-transient-facet))
            '("-" . ("undo" . undo-only))
            '("=" . ("redo" . undo-redo))
            '("y" . ("yank" . yank))
            '("Y" . ("yank ring" . consult-yank-from-kill-ring))
            '("w" . ("word next" . forward-word))
            '("W" . ("word prev" . backward-word))
            '("s" . ("sexp next" . forward-sexp))
            '("S" . ("sexp prev" . backward-sexp))
            '("f" . ("find in line" . consult-line))
            '("g" . ("goto line #" . consult-goto-line))
            '("d" . ("goto def" . bristke--consult-definition))
            '("j" . ("goto x" . avy-goto-char-timer))
            '("[" . ("goto line beg" . beginning-of-line))
            '("]" . ("goto line end" . end-of-line))
            '("{" . ("goto buffer beg" . beginning-of-buffer))
            '("}" . ("goto buffer end" . end-of-buffer))
            '("n" . ("narrow remove" . widen))
            '("B" . ("pop buffer" . bristle--pop-buffer))
            '("o" . ("open char" . bristle--open-char))
            '("!" . ("save" . save-buffer))
            '(")" . ("slurp" . paredit-forward-slurp-sexp))
            '("(" . ("barf" . paredit-forward-barf-sexp))
            '("?" . ("legend" . bristle--legend-toggle))))

  (defun bristle--mark-point ()
    "Set the region to be the character under the cursor."
    (interactive)
    (set-mark (point)))

  (defun bristle--pop-buffer ()
    "Switch to the most recently visited buffer."
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) 1)))

  (defun bristle--open-char ()
    "Insert a space"
    (interactive)
    (insert " "))

(defun bristke--consult-definition ()
  "Navigate to a definition."
  (interactive)
  (if (eq major-mode 'org-mode)
      (consult-org-heading)
    (consult-imenu)))

(defun bristle--invoke-mode-facet ()
  "Invoke the current mode specific facet (if any)."
  (interactive)
  (when bristle--mode-facet
    (funcall bristle--mode-facet)))

  ;; insert facet
  ;; this facet is effectively the default Emacs behavior

  (gestalt-define-facet
   insert
   "Facet to insert text."
   :lighter " [IST]"
   :parent 'move
   :keymap (gestalt-make-insert-map
            '("C-SPC" . ignore)
            '("<tab>" . bristle--smart-tab))
   (if (gestalt-insert-facet-p) (setq cursor-type 'box) (setq cursor-type 'bar)))
  
  (defun bristle--smart-tab ()
    "Tab does many things in insert mode:
 - Moves to next cell if in an org table
 - Expands the snippet at point if there is one
 - Triggers completion at point if the line is already indented
 - In sgml-mode, triggers XML tag completion instead
 - Indents the line otherwise"
    (interactive)
    (cond
     ((and (derived-mode-p 'org-mode) (org-at-table-p))
      (org-cycle))
     
     ;; tempel returns an error if there is no matching template
     ((condition-case nil
          (progn (call-interactively 'tempel-expand) t)
        (user-error nil)))

     ((save-excursion
      (let ((orig-point (point)))
        (indent-for-tab-command)
        (= (point) orig-point)))
      (if (derived-mode-p 'sgml-mode)
          (bristle--sgml-xml-finish-element-new-line)
        (completion-at-point)))
     
     (t (indent-for-tab-command))))
  
  ;; point facet
  ;; this facet uses registers to quickly jump around between buffers
  
  (gestalt-define-transient-facet
   point
   '("a" . ("add" . bristle--push-register))
   '("j" . ("jump" . consult-register))
   '("c" . ("mod" . bristle--clear-register)))

  (defvar bristle--register-char ?Z
    "The current register character for `bristle--push-register'.")

  (defvar bristle--used-registers nil
    "A list of registers added by `bristle--push-register'.")

  (defun bristle--push-register ()
    "Push the current point into a register. Registers are named Z-A in sequence."
    (interactive)
    (if (char-equal bristle--register-char ?@)
        (error "Points full"))
    (point-to-register bristle--register-char)
    (push bristle--register-char bristle--used-registers)
    (message "Point saved: '%c'" bristle--register-char)
    (setq bristle--register-char (1- bristle--register-char)))

  (defun bristle--clear-register ()
    "Clear all registers added by `bristle--push-register` and reset the sequence."
    (interactive)
    (dolist (reg bristle--used-registers)
      (set-register reg nil))
    (setq bristle--used-registers nil)
    (setq bristle--register-char ?Z)
    (message "Points cleared"))

  ;; lsp facet
  ;; this facet interacts with language servers

  (gestalt-define-transient-facet
   lsp
   '("d" . ("goto def" . xref-find-definitions))
   '("r" . ("find refs" . xref-find-references))
   '("R" . ("rename" . eglot-rename))
   '("h" . ("docs" . eldoc))
   '("e" . ("goto error" . consult-flymake)))
  
  ;; region facet
  ;; this facet expands and acts upon a region of selected text

  (gestalt-define-facet
   region
   "Facet to act on a region."
   :lighter " [RGN]"
   :keymap (gestalt-make-command-map
            '("i" . ("insert" . bristle--region-insert))
            '("p" . ("pair" . gestalt-pair-transient-facet))
            '("." . ("mode specific" . bristle--invoke-mode-region-facet))
            '("y" . ("yank" . bristle--region-yank))
            '(";" . ("summarize" . gptel-quick))
            '("e" . ("exchange" . exchange-point-and-mark))
            '("w" . ("word forward" . bristle--region-forward-word))
            '("W" . ("word backward" . bristle--region-backward-word))
            '("s" . ("sexp forward" . bristle--region-forward-sexp))
            '("S" . ("sexp backward" . bristle--region-backward-sexp))
            '("l" . ("line forward" . bristle--region-forward-line))
            '("L" . ("line backward" . bristle--region-backward-line))
            '("b" . ("buffer" . bristle--mark-buffer))
            '("g" . ("line #" . bristle--expand-line-number))
            '("=" . ("expand" . expreg-expand))
            '("-" . ("contract" . expreg-contract))
            '("[" . ("line beg" . bristle--region-bol))
            '("]" . ("line end" . bristle--region-eol))
            '("{" . ("buffer beg" . bristle--region-bob))
            '("}" . ("buffer end" . bristle--region-eob))
            '("DEL" . ("delete region" . delete-region))
            '("c" . ("clone" . duplicate-dwim))
            '("r" . ("replace" . bristle--replace))
            '("k" . ("kill save" . kill-ring-save))
            '("K" . ("kill" . kill-region))
            '("U" . ("move up" . move-text-up))
            '("D" . ("move down" . move-text-down))
            '("/" . ("comment" . comment-or-uncomment-region))
            '("o" . ("order alpha" . sort-lines))
            '("j" . ("join" . bristle--join-lines-region))
            '(">" . ("rigid indent" . bristle--rigid-indent-region))
            '("<" . ("rigid unindent" . bristle--rigid-unindent-region))
            '("u" . ("upcase" . upcase-region))
            '("d" . ("downcase" . downcase-region))
            '("n" . ("narrow" . narrow-to-region))
            '("f" . ("flush lines" . consult-keep-lines))
            '("SPC" . ("leader" . gestalt-leader-transient-facet))
            '("?" . ("legend" . bristle--legend-toggle))))

  (defun bristle--invoke-mode-region-facet ()
    "Invoke the current mode specific region facet (if any)."
    (interactive)
    (when bristle--mode-region-facet
      (funcall bristle--mode-region-facet)))
  
  (defun bristle--mark-thing (thing &optional backward)
    "Mark a thing at the current point."
    (let ((bounds (bounds-of-thing-at-point thing)))
             (when bounds
               (goto-char (cdr bounds))
               (set-mark (car bounds))
               (when backward
                 (exchange-point-and-mark)))))
  
  (defun bristle--maybe-deactivate (fn)
    "Run FN or deactivate the mark.
The mark is deactivated if point and mark would be inverted."
    (let ((start (region-beginning))
          (end (region-end))
          (backward (= (point) (region-beginning))))
      (if (save-excursion
            (funcall fn)
            (if backward (>= (point) end) (<= (point) start)))
          (progn (deactivate-mark t)
                 (if backward (goto-char end) (goto-char start)))
        (funcall fn))))
  
  (defun bristle--region-forward-word ()
    "Move forward by one word in the context of a region."
    (interactive)
    (if (= (point) (mark))
        (bristle--mark-thing 'word)
      (bristle--maybe-deactivate 'forward-word)))

  (defun bristle--region-backward-word ()
    "Move backward by one word in the context of a region."
    (interactive)
    (if (= (point) (mark))
        (bristle--mark-thing 'word t)
      (bristle--maybe-deactivate 'backward-word)))

  (defun bristle--region-forward-sexp ()
    "Move forward by one sexp in the context of a region."
    (interactive)
    (if (= (point) (mark))
        (bristle--mark-thing 'sexp)
      (bristle--maybe-deactivate 'forward-sexp)))

  (defun bristle--region-backward-sexp ()
    "Move backward by one sexp in the context of a region."
    (interactive)
    (if (= (point) (mark))
        (bristle--mark-thing 'sexp t)
      (bristle--maybe-deactivate 'backward-sexp)))

  (defun bristle--region-forward-line ()
    "Move forward by one line in the context of a region."
    (interactive)
    (if (= (point) (mark))
        (bristle--mark-thing 'line)
      (bristle--maybe-deactivate
       (lambda () 
         (forward-line 1)
         (unless (eobp)
           (beginning-of-line))))))

  (defun bristle--region-backward-line ()
    "Move backward by one line in the context of a region."
    (interactive)
    (if (= (point) (mark))
        (bristle--mark-thing 'line t)
      (bristle--maybe-deactivate
       (lambda () (forward-line -1) (beginning-of-line)))))

  (defun bristle--mark-buffer ()
    "Set the region to be the current buffer."
    (interactive)
    (goto-char (point-max))
    (set-mark 0))

  (defun bristle-line-selected-p ()
  "Return t if exactly one full line is selected."
  (when (region-active-p)
    (let ((start (region-beginning))
          (end (region-end)))
      (save-excursion
        (and
         (progn (goto-char start) (bolp))
         (progn (goto-char end) (or (bolp) (eobp)))
         (= (count-lines start end) 1))))))
  
  (defun bristle--expand-line-number ()
    "Expand the provided number of lines. Relative line numbers are shown."
    (interactive)
    (when (bristle-line-selected-p)
      (unwind-protect (progn
                        (setq display-line-numbers-type 'relative)
                        (display-line-numbers-mode 1)
                        (let ((lnum (read-number "Line number: ")))
                          (if (= (point) (region-beginning))
                              (forward-line (- lnum))
                            (forward-line (+ 1 lnum)))))
        (progn
          (setq display-line-numbers-type t)
          (display-line-numbers-mode -1)))))

  (defun bristle--region-eol ()
    "Move point to end of region, then to end of line."
    (interactive)
    (when (/= (point) (region-end))
      (exchange-point-and-mark))
    (end-of-line))
  
  (defun bristle--region-bol ()
    "Move point to start of region, then to beginning of line."
    (interactive)
    (when (/= (point) (region-beginning))
      (exchange-point-and-mark))
    (beginning-of-line))

  (defun bristle--region-eob ()
    "Move point to end of region, then to end of buffer."
    (interactive)
    (when (/= (point) (region-end))
      (exchange-point-and-mark))
    (end-of-buffer))

  (defun bristle--region-bob ()
    "Move point to start of region, then to beginning of buffer."
    (interactive)
    (when (/= (point) (region-beginning))
      (exchange-point-and-mark))
    (beginning-of-buffer))
  
  (defun bristle--region-insert ()
    "Overwrite the current region using insert mode."
    (interactive)
    (delete-region (region-beginning) (region-end))
    (gestalt-insert-facet-activate))

  (defun bristle--region-yank ()
    "Overwrite the current region using yank."
    (interactive)
    (delete-region (region-beginning) (region-end))
    (yank))
  
  (defun bristle--replace ()
  "Replace every instance of the current region in the current buffer."
  (interactive)
  (when (region-active-p)
    ;; get the target text from the active region
      (let ((target (buffer-substring-no-properties (region-beginning) (region-end)))
            overlays)
        ;; highlight occurrences with overlays
        ;; do this in an excursion to preserve the cursor
        ;; collect the overlays to remove later
        (save-excursion
          (goto-char (point-min))
          (while (search-forward target nil t)
            (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
              (overlay-put overlay 'face 'highlight)
              (push overlay overlays))))
        
        ;; prompt for replacement text
        ;; then replace all matches of the target text with the replacement text
        ;; finally remove all of the overlays
        ;; the cleanup will happen even if the user cancels
        (unwind-protect
            (let ((replacement (read-string "Replace with: " target)))
              (save-excursion
                (goto-char (point-min))
                (while (search-forward target nil t)
                  (replace-match replacement nil nil))))
          (mapc #'delete-overlay overlays)))))

  (defun bristle--join-lines-region (start end)
    "Join all lines in the region specified by START and END."
    (interactive "r")
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\r?\n" end t)
        (replace-match " "))))

  (defun bristle--rigid-indent-region (N)
  "Indent a region by exactly `tab-width'."
    (interactive "p")
    (if (region-active-p)
        (progn (indent-rigidly (region-beginning) (region-end) (* N tab-width))
               (setq deactivate-mark nil))
      (self-insert-command N)))

  (defun bristle--rigid-unindent-region (N)
    "Unindent a region by exactly `tab-width'."
    (interactive "p")
    (if (region-active-p)
        (progn (indent-rigidly (region-beginning) (region-end) (* N (- 0 tab-width)))
               (setq deactivate-mark nil))
      (self-insert-command N)))

  (gestalt-define-transient-facet
   pair
   '("{" . ("{}" . bristle--pair-curly))
   '("'" . ("''" . bristle--pair-single-quote))
   '("\"" . ("\"\"" . bristle--pair-double-quote))
   '("`" . ("``" . bristle--pair-backtick))
   '("(" . ("()" . bristle--pair-parens))
   '("[" . ("[]" . bristle--pair-square)))
  
  (defun bristle--pair (open close)
    "Wrap the current region in a pair."
    (interactive)
    (when (region-active-p)
      (insert-pair 1 open close)
      (left-char)))

  (defun bristle--pair-single-quote ()
    "Wrap the current region in a pair of single quotes."
    (interactive)
    (bristle--pair ?\' ?\'))

  (defun bristle--pair-double-quote ()
    "Wrap the current region in a pair of double quotes."
    (interactive)
    (bristle--pair ?\" ?\"))

  (defun bristle--pair-backtick ()
    "Wrap the current region in a pair of backticks."
    (interactive)
    (bristle--pair ?\` ?\`))

  (defun bristle--pair-parens ()
    "Wrap the current region in a pair of parentheses."
    (interactive)
    (bristle--pair ?\( ?\)))

  (defun bristle--pair-curly ()
    "Wrap the current region in a pair of curly braces"
    (interactive)
    (bristle--pair ?\{ ?\}))

  (defun bristle--pair-square ()
    "Wrap the current region in a pair of square braces"
    (interactive)
    (bristle--pair ?\[ ?\]))

  ;; rectangle facet
  ;; this facet expands and acts upon a rectangle region of selected text

  (gestalt-define-facet
   rectangle
   "Facet to act on a rectangle region."
   :lighter " [RCT]"
   :keymap (gestalt-make-command-map
            '("DEL" . ("delete region" . delete-rectangle))
            '("p" . ("prefix" . string-insert-rectangle))
            '("n" . ("number" . rectangle-number-lines))
            '("?" . ("legend" . bristle--legend-toggle))))

  ;; line facet
  ;; this facet acts up on lines of text

  (gestalt-define-facet
   line
   "Facet to do things with lines."
   :lighter " [LNE]"
   :parent 'move
   :keymap (gestalt-make-command-map
            '("c" . ("clone" . duplicate-line))
            '("DEL" . ("delete" . bristle--delete-line))
            '("/" . ("comment" . bristle--comment-or-uncomment-line))
            '("j" . ("join" . join-line))
            '("U" . ("move down" . move-text-up))
            '("D" . ("move up" . move-text-down))
            '("-" . ("undo" . undo-only))
            '("=" . ("redo" . undo-redo))
            '("?" . ("legend" . bristle--legend-toggle))))
  
  (defun bristle--delete-line ()
    "Delete the current line."
    (interactive)
    (delete-line))
  
  (defun bristle--comment-or-uncomment-line ()
    "Comment or uncomment the current line."
    (interactive)
    (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

  ;; todo facet
  ;; the todo facet is for moving around todos

  (gestalt-define-facet
   todo
   "Facet to do things with TODOs."
   :lighter " [TDO]"
   :parent 'move
   :keymap (gestalt-make-command-map
            '("n" . ("next" . hl-todo-next))
            '("p" . ("previous" . hl-todo-previous))
            '("g" . ("goto todo" . consult-hl-todo))
            '("?" . ("legend" . bristle--legend-toggle))))

  ;; spelling facet
  ;; the spelling facet is for fixing spelling errors
  
  (gestalt-define-facet
   spelling
   "Facet to do things with spelling errors."
   :lighter " [SPL]"
   :parent 'move
   :keymap (gestalt-make-command-map
            '("n" . ("next" . jinx-next))
            '("p" . ("previous" . jinx-previous))
            '("c" . ("correct" . jinx-correct))
            '("!" . ("save" . save-buffer))
            '("-" . ("undo" . undo-only))
            '("=" . ("redo" . undo-redo))
            '("?" . ("legend" . bristle--legend-toggle))))

  ;; debug facet
  ;; the debug facet is used for debugging

  (gestalt-define-transient-facet
   debug
    '("b" . ("toggle breakpoint" . dape-breakpoint-toggle))
    '("l" . ("log breakpoint" . dape-breakpoint-log))
    '("e" . ("expression breakpoint" . dape-breakpoint-expression))
    '("h" . ("hits breakpoint" . dape-breakpoint-hits))
    '("r" . ("remove breakpoint" . dape-breakpoint-remove-at-point))
    '("R" . ("remove all breakpoints" . dape-breakpoint-remove-all)))
  
  ;; dired facet
  ;; the dired facet is used to interact with dired

  (gestalt-define-facet
   dired
   "Facet to work in dired."
   :lighter " [DIR]"
   :keymap (gestalt-make-command-map
            '("i" . ("wdired" . wdired-change-to-wdired-mode))
            '("<left>" . ("left" . ignore))
            '("<right>" . ("right" . ignore))
            '("{" . ("goto buffer beg" . beginning-of-buffer))
            '("}" . ("goto buffer end" . end-of-buffer))
            '("RET" . ("open" . bristle--dired-find-file))
            '("SPC" . ("leader" . gestalt-leader-transient-facet))
            '("+" . ("new dir" . dired-create-directory))
            '("n" . ("new file" . dired-create-empty-file))
            '("R" . ("rename" . dired-do-rename))
            '("D" . ("delete" . dired-do-delete))
            '("C" . ("copy" . dired-do-copy))
            '("M" . ("chmod" . dired-do-chmod))
            '("O" . ("chown" . dired-do-chown))
            '("G" . ("chgrp" . dired-do-chgrp))
            '("S" . ("symlink" . dired-do-symlink))
            '("^" . ("up dir" . dired-up-directory))
            '("f" . ("find in line" . consult-line))
            '("g" . ("revert buffer" . revert-buffer))
            '("m" . ("mark" . dired-mark))
            '("u" . ("unmark" . dired-unmark))
            '("U" . ("unmark all" . dired-unmark-all-marks))
            '("%" . ("mark regex" . dired-mark-files-regexp))
            '("/" . ("mark dirs" . dired-mark-directories))
            '("@" . ("mark symlinks" . dired-mark-symlinks))
            '("*" . ("mark exe" . dired-mark-executables))
            '("s" . ("mark all" . dired-mark-subdir-files))
            '("!" . ("shell" . dired-do-shell-command))
            '("?" . ("legend" . bristle--legend-toggle))))

  (defun bristle--dired-find-file ()
    (interactive)
    (if (pine-selected-p)
        (pine-find-file)
      (dired-find-file)))

  ;; dir tree facet
  ;; the dir tree  facet is used then the sidebar dir tree is active

  (gestalt-define-facet
   dirtree
   "Facet to work in dir tree."
   :lighter " [DTR]"
   :keymap (gestalt-make-command-map
            '("<left>" . ("left" . ignore))
            '("<right>" . ("right" . ignore))
            '("{" . ("goto buffer beg" . beginning-of-buffer))
            '("}" . ("goto buffer end" . end-of-buffer))
            '("RET" . ("open" . bristle--dired-find-file))
            '("TAB" . ("subtree" . dired-subtree-toggle))
            '("SPC" . ("leader" . gestalt-leader-transient-facet))
            '("f" . ("find in line" . consult-line))
            '("^" . ("up dir" . dired-up-directory))
            '("g" . ("revert buffer" . revert-buffer))
            '("?" . ("legend" . bristle--legend-toggle))))
  
  ;; wdired facet
  ;; the wdired facet is used when wdired is active

  (gestalt-define-facet
   wdired
   "Facet to work in wdired."
   :lighter " [WDR]"
   :parent 'dired
   :keymap (gestalt-make-insert-map))

  ;; calc facet
  ;; the calc facet is used to interact with calc

  (gestalt-define-facet
   calc
   "Facet to work in calc."
   :lighter " [CLC]"
   :keymap (gestalt-make-command-map
            '("DEL" . ("pop" . calc-pop))
            '("RET" . ("clone" . calc-enter))
            '("TAB" . ("swap" . calc-roll-down))
            '("u" . ("undo" . calc-undo))
            '("y" . ("yank" . calc-yank))
            '("k" . ("kill save" . calc-copy-as-kill))
            '("'" . ("entry" . calc-algebraic-entry))
            '("+" . ("add" . calc-plus))
            '("-" . ("subtract"  . calc-minus))
            '("*" . ("multiply"  . calc-times))
            '("/" . ("divide"  . calc-divide))
            '("^" . ("power"  . calc-power))
            '("\\" . ("divide (int)"  . calc-idiv))
            '("%" . ("mod"  . calc-mod))
            '("!" . ("factorial"  . calc-factorial))
            '("n" . ("negate" . calc-change-sign))
            '("f" . ("floor" . calc-floor))
            '("c" . ("ceiling" . calc-ceiling))
            '("p" . ("precision" . calc-precision))
            '("v" . ("to vector" . bristle--pack-stack))
            '("r" . ("radix" . gestalt-radix-transient-facet))
            '("b" . ("binary" . gestalt-binary-transient-facet))
            '("SPC" . ("leader" . gestalt-leader-transient-facet))
            '("s" . ("stats" . gestalt-stats-transient-facet))
            '("?" . ("legend" . bristle--legend-toggle))))

  (gestalt-define-transient-facet
   radix
   '("2" . ("binary" . calc-binary-radix))
   '("0" . ("decimal" . calc-decimal-radix))
   '("6" . ("hex" . calc-hex-radix)))

  (gestalt-define-transient-facet
   binary
   '("a" . ("and" . calc-and))
   '("o" . ("or" . calc-or))
   '("x" . ("xor" . calc-xor))
   '("n" . ("not" . calc-not))
   '("l" . ("left shift" . calc-lshift-binary))
   '("r" . ("right shift" . calc-rshift-binary))
   '("w" . ("word" . calc-word-size)))

  (gestalt-define-transient-facet
   stats
   '("a" . ("average" . calc-vector-mean))
   '("m" . ("median" . calc-vector-median))
   '("s" . ("sum" . calc-vector-sum))
   '("l" . ("largest" . calc-vector-max)))

  (defun bristle--pack-stack ()
  "Convert the entire stack to vector."
  (interactive)
    (cond 
     ((< (length calc-stack) 1)
      (error "No elements on stack"))
     ((= (length calc-stack) 1)
      (message "Only one element on stack"))
     (t
      (while (> (length calc-stack) 2)
        (call-interactively 'calc-concat)))))

  ;; mcp facet
  ;; this facet is used to interact with MCP servers

  (gestalt-define-facet
   mcp
   "Facet to work with mcp servers."
   :lighter " [MCP]"
   :keymap (gestalt-make-command-map
            '("l" . ("logs" . mcp-hub-view-log))
            '("s" . ("start server" . mcp-hub-start-server))
            '("k" . ("kill server" . mcp-hub-close-server))
            '("r" . ("restart server" . mcp-hub-restart-server))
            '("S" . ("start all" . mcp-hub-start-all-server))
            '("R" . ("restart all" . mcp-hub-restart-all-server))
            '("K" . ("kill all" . mcp-hub-close-all-server))
            '("?" . ("legend" . bristle--legend-toggle))))

  ;; ibuffer facet
  ;; this facet is used to interact with ibuffer

  (gestalt-define-facet
   ibuffer
   "Facet to work with ibuffer."
   :lighter " [IBF]"
   :keymap (gestalt-make-command-map
            '("SPC" . ("leader" . gestalt-leader-transient-facet))
            '("g" . ("goto buffer" . ibuffer-jump-to-buffer))
            '("m" . ("mark" . ibuffer-mark-forward))
            '("M" . ("mark by" . gestalt-ibuffer-mark-transient-facet))
            '("u" . ("unmark" . ibuffer-unmark-forward))
            '("U" . ("unmark all" . ibuffer-unmark-all-marks))
            '("g" . ("refresh" . ibuffer-update))
            '("s" . ("save" . ibuffer-do-save))
            '("k" . ("kill" . ibuffer-do-delete))
            '("?" . ("legend" . bristle--legend-toggle))))

  (gestalt-define-transient-facet
   ibuffer-mark
   '("d" . ("dirty" . ibuffer-mark-modified-buffers))
   '("n" . ("name" . ibuffer-mark-by-name-regexp))
   '("m" . ("mode" . ibuffer-mark-by-mode-regexp)))

  ;; JS facet
  ;; this facet is for working in JS/TS/JSX/TSX

  (gestalt-define-transient-facet
   js
   '("j" . ("jsdoc" . jsdoc)))

  ;; notmuch
  ;; this facet is for email with notmuch

  (gestalt-define-facet
   notmuch-hello
   "Facet to work with notmuch in hello mode."
   :lighter " [NMH]"
   :keymap (gestalt-make-command-map
            '("SPC" . ("leader" . gestalt-leader-transient-facet))
            '("g" . ("refresh" . notmuch-refresh-this-buffer))
            '("f" . ("fetch" . bristle--notmuch-fetch-mail))
            '("i" . ("inbox" . bristle--notmuch-inbox))
            '("u" . ("unread" . bristle--notmuch-unread))
            '("a" . ("archived" . bristle--notmuch-archived))
            '("d" . ("deleted" . bristle--notmuch-deleted))
            '("s" . ("search" . notmuch-tree))
            '("m" . ("send message" . notmuch-mua-new-mail))
            '("?" . ("legend" . bristle--legend-toggle))))

  (gestalt-define-facet
   notmuch-search
   "Facet to work with notmuch in search mode."
   :lighter " [NMSE]"
   :keymap (gestalt-make-command-map
            '("SPC" . ("leader" . gestalt-leader-transient-facet))
            '("f" . ("find in line" . consult-line))
            '("q" . ("quit" . notmuch-bury-or-kill-this-buffer))
            '("g" . ("refresh" . notmuch-refresh-this-buffer))
            '("{" . ("goto buffer beg" . beginning-of-buffer))
            '("}" . ("goto buffer end" . end-of-buffer))
            '("?" . ("legend" . bristle--legend-toggle))))

  (gestalt-define-facet
   notmuch-tree
   "Facet to work with notmuch in tree mode."
   :lighter " [NMT]"
   :keymap (gestalt-make-command-map
            '("SPC" . ("leader" . gestalt-leader-transient-facet))
            '("f" . ("find in line" . consult-line))
            '("q" . ("quit" . notmuch-bury-or-kill-this-buffer))
            '("g" . ("refresh" . notmuch-refresh-this-buffer))
            '("{" . ("goto buffer beg" . beginning-of-buffer))
            '("}" . ("goto buffer end" . end-of-buffer))
            '("?" . ("legend" . bristle--legend-toggle))))

  (gestalt-define-facet
   notmuch-show
   "Facet to work with notmuch in show mode."
   :lighter " [NMSH]"
   :keymap (gestalt-make-command-map
            '("SPC" . ("leader" . gestalt-leader-transient-facet))
            '("f" . ("find in line" . consult-line))
            '("q" . ("quit" . notmuch-bury-or-kill-this-buffer))
            '("g" . ("refresh" . notmuch-refresh-this-buffer))
            '("*" . ("set tags" .  notmuch-show-tag))
            '("a" . ("archive" .  birstle--notmuch-archive))
            '("d" . ("delete" .  bristle--notmuch-delete))
            '("r" . ("reply" .  notmuch-show-reply))
            '("R" . ("reply all" .  notmuch-show-reply))
            '("?" . ("legend" . notmuch-show-reply-all))))

  (gestalt-define-transient-facet
   notmuch-message
   '("s" . ("send" . bristle--notmuch-send-messsage))
   '("c" . ("cancel" . bristle--notmuch-cancel-messsage))
   '("g" . ("sign" . mml-secure-sign-pgpmime)))
  
  (defun bristle--notmuch-fetch-mail ()
    "Fetch any new mail using `mbsync' + `notmuch'."
    (interactive)
    (shell-command "mbsync -a")
    (shell-command "notmuch new")
    (notmuch-refresh-this-buffer))

  (defun bristle--notmuch-inbox ()
    "Search for `inbox' messages."
    (interactive)
    (notmuch-tree "tag:inbox"))
  
  (defun bristle--notmuch-unread ()
    "Search for `unread' messages."
    (interactive)
    (notmuch-tree "tag:unread"))

  (defun bristle--notmuch-deleted ()
    "Search for `deleted' messages."
    (interactive)
    (notmuch-tree "tag:deleted"))

  (defun bristle--notmuch-archived ()
    "Search for `archived' messages."
    (interactive)
    (notmuch-tree "tag:archived"))

  (defun birstle--notmuch-archive ()
    "Archive the current message."
    (interactive)
    (notmuch-show-tag (list "-inbox" "-deleted" "+archived")))

  (defun bristle--notmuch-delete ()
    "Delete the current message."
    (interactive)
    (notmuch-show-tag (list "-inbox" "-archived" "+deleted")))

  (defun bristle--notmuch-send-messsage ()
    "Send the current message."
    (interactive)
    (message-send)
    (kill-buffer (current-buffer)))

  (defun bristle--notmuch-cancel-messsage ()
    "Send the current message."
    (interactive)
    (set-buffer-modified-p nil)
    (kill-buffer))
  
  ;; leader facet
  ;; this facet is how extended commands are executed

  (gestalt-define-transient-facet
   leader
   '("SPC" . ("exec command" . execute-extended-command))
   '("t" . ("task" . stadion-run))
   '("l" . ("debug" . dape))
   '("d" . ("docker" . docker))
   '("a" . ("ai" . gptel-menu))
   '("m" . ("mcp" . mcp-hub))
   '("f" . ("file" . gestalt-file-transient-facet))
   '("b" . ("buffer" . gestalt-buffer-transient-facet))
   '("h" . ("help" . gestalt-help-transient-facet))
   '("p" . ("project" . gestalt-project-transient-facet))
   '("u" . ("utils" . gestalt-utils-transient-facet))
   '("n" . ("notes" . soma-consult-notes))
   '("s" . ("search" . gestalt-search-transient-facet))
   '("c" . ("dev containers" . gestalt-devcontainer-transient-facet))
   '("i" . ("interfaces" . gestalt-interface-transient-facet)))

  (gestalt-define-transient-facet
   file
   '("f" . ("find file" . find-file))
   '("s" . ("save" . save-buffer))
   '("S" . ("save all" . save-some-buffers)))

  (gestalt-define-transient-facet
   buffer
   '("k" . ("kill buffer" . kill-buffer))
   '("b" . ("list buffers" . consult-buffer)))
  
  (gestalt-define-transient-facet
   help
   '("m" . ("describe mode" . describe-mode))
   '("f" . ("describe function" . describe-function))
   '("v" . ("describe variable" . describe-variable))
   '("c" . ("describe char" . describe-char))
   '("k" . ("describe key" . describe-key))
   '("s" . ("describe symbol" . describe-symbol))
   '("p" . ("describe package" . describe-package))
   '("w" . ("man pages" . woman))
   '("d" . ("devdocs" . devdocs-lookup)))

  (gestalt-define-transient-facet
   project
   '("f" . ("find file" . project-find-file))
   '("d" . ("find dir" . project-find-dir))
   '("D" . ("dired" . project-dired))
   '("e" . ("eshell" . project-eshell))
   '("g" . ("grep" . consult-ripgrep))
   '("l" . ("locate" . consult-fd))
   '("s" . ("switch" . project-switch-project)))

  (gestalt-define-transient-facet
   utils
   '("t" . ("decode jwt" . decode-jwt))
   '("j" . ("format json" . format-json)))

  (gestalt-define-transient-facet
   search
   '("k" . ("kagi" . engine/search-kagi))
   '("g" . ("github" . engine/search-github)))

  (gestalt-define-transient-facet
   devcontainer
   '("s" . ("start" . devcontainer-start))
   '("k" . ("kill" . devcontainer-stop))
   '("b" . ("rebuild" . devcontainer-rebuild)))

  (gestalt-define-transient-facet
   interface
   '("c" . ("calc" . full-calc))
   '("m" . ("magit" . magit))
   '("r" . ("regex builder" . re-builder))
   '("i" . ("ibuffer" . ibuffer))
   '("e" . ("ediff" . ediff-buffers))
   '("g" . ("gptel" . gptel))
   '("d" . ("dired" . dired))
   '("n" . ("notmuch" . notmuch)))

  (defun format-json ()
    "Format the JSON in region or kill ring."
    (interactive)
    (let ((json-content (if (region-active-p)
                            (buffer-substring-no-properties (region-beginning) (region-end))
                          (current-kill 0))))
      (with-current-buffer (get-buffer-create "*json*")
        (erase-buffer)
        (insert json-content)
        (json-ts-mode)
        (json-pretty-print-buffer)
        (goto-char (point-min)))
      (pop-to-buffer "*json*")))
  
  (defun decode-jwt ()
    "Decode a JWT in region or kill ring."
    (interactive)
    (let ((jwt-token (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (current-kill 0))))
      (setq jwt-token (string-trim jwt-token))
      (let ((parts (split-string jwt-token "\\.")))
        (if (not (= (length parts) 3))
            (message "Invalid JWT format - expected 3 parts separated by dots")
          
          (let* ((payload-b64 (nth 1 parts))
                 (padding-needed (% (- 4 (% (length payload-b64) 4)) 4))
                 (padded-payload (concat payload-b64 (make-string padding-needed ?=)))
                 (base64-payload (replace-regexp-in-string "_" "/" 
                                                           (replace-regexp-in-string "-" "+" padded-payload))))
          (condition-case err
              (let* ((decoded-payload (base64-decode-string base64-payload))
                     (buffer (get-buffer-create "*jwt*")))
                (with-current-buffer buffer
                  (erase-buffer)
                  (insert decoded-payload)
                  (json-ts-mode)
                  (json-pretty-print-buffer)
                  (goto-char (point-min)))
                (pop-to-buffer buffer))
            (error (message "Failed to decode JWT: %s" (error-message-string err)))))))))

  ;; smerge facet
  ;; this facet is used to interact with smerge

  (gestalt-define-facet
   smerge
   "Facet to work in smerge."
   :lighter " [MRG]"
   :parent 'move
   :keymap (gestalt-make-command-map
            '("<up>" . ("previous" . smerge-prev))
            '("<down>" . ("next" . smerge-next))
            '("c" . ("keep current" . smerge-keep-current))
            '("m" . ("keep mine" . smerge-keep-mine))
            '("t" . ("keep theirs" . smerge-keep-other))
            '("a" . ("keep all" . smerge-keep-all))
            '("?" . ("legend" . bristle--legend-toggle))))
  
  ;; org facet
  ;; this facet is used to interact with org mode

  (gestalt-define-transient-facet
   org
   '("h" . ("headlines" . gestalt-orgh-facet-activate))
   '("t" . ("tables" . gestalt-orgt-facet-activate))
   '("l" . ("links" . gestalt-orgl-transient-facet))
   '("b" . ("blocks" . gestalt-orgb-transient-facet))
   '("v" . ("visibility" . gestalt-orgv-facet-activate))
   '("x" . ("latex" . gestalt-orgx-transient-facet))
   '("s" . ("source" . gestalt-orgs-transient-facet))
   '("p" . ("plain lists" . gestalt-orgp-facet-activate))
   '("c" . ("http client" . gestalt-orgc-transient-facet))
   '("e" . ("export" . gestalt-orge-transient-facet))
   '("d" . ("slides" . dslide-deck-start)))
  
  (gestalt-define-facet
   orgh
   "Facet to work in org headlines."
   :lighter " [ORG-H]"
   :parent 'move
   :keymap (gestalt-make-command-map
            '("n" . ("new" . (lambda () (interactive) (org-insert-heading))))
            '("d" . ("delete" . bristle--delete-headline))
            '("e" . ("edit" . org-edit-headline))
            '("<left>" . ("promote" . org-do-promote))
            '("<right>" . ("demote" . org-do-demote))
            '("M-<up>" . ("move up" . org-move-subtree-up))
            '("M-<down>" . ("move down" . org-move-subtree-down))
            '("<up>" . ("previous" . outline-previous-heading))
            '("<down>" . ("next" . outline-next-heading))
            '("a" . ("priority A" . bristle--org-priority-a))
            '("b" . ("priority B" . bristle--org-priority-b))
            '("c" . ("priority C" . bristle--org-priority-c))
            '("r" . ("priority remove" . bristle--org-priority-none))
            '("T" . ("status todo" . bristle--org-status-todo))
            '("D" . ("status done" . bristle--org-status-done))
            '("R" . ("status remove" . bristle--org-status-none))
            '("t" . ("tags" . org-set-tags-command))
            '("p" . ("property" . org-set-property))
            '("-" . ("undo" . undo-only))
            '("=" . ("redo" . undo-redo))
            '("?" . ("legend" . bristle--legend-toggle))))

  (defun bristle--delete-headline ()
    "Delete the section at point."
    (interactive)
    (let ((old-kill-ring kill-ring))
      (org-cut-subtree)
      (setq kill-ring old-kill-ring)))
  
  (defun bristle--org-priority-a ()
    "Set headline at points priority to be A."
    (interactive)
    (org-priority ?A))

  (defun bristle--org-priority-b ()
    "Set headline at points priority to be B."
    (interactive)
    (org-priority ?B))

  (defun bristle--org-priority-c ()
    "Set headline at points priority to be C."
    (interactive)
    (org-priority ?C))

  (defun bristle--org-priority-none ()
    "Remove the priority from the headline at point."
    (interactive)
    (org-priority ?\s))

  (defun bristle--org-status-todo ()
    "Set the headline at points status to be TODO."
    (interactive)
    (org-todo "TODO"))

  (defun bristle--org-status-done ()
    "Set the headline at points status to DONE."
    (interactive)
    (org-todo "DONE"))

  (defun bristle--org-status-none ()
    "Remove the status from the headline at point."
    (interactive)
    (org-todo ""))

  (gestalt-define-facet
   orgp
   "Facet to work in org plain lists."
   :lighter " [ORG-P]"
   :parent 'move
   :keymap (gestalt-make-command-map
            '("u" . ("unordered list" . bristle--start-unordered-list))
            '("o" . ("ordered list" . bristle--start-ordered-list))
            '("n" . ("new item" . bristle--insert-item))
            '("d" . ("delete item" . bristle--delete-item))
            '("e" . ("edit item" . bristle--edit-item-text))
            '("c" . ("check item" . bristle--check-item))
            '("r" . ("remove checkbox" . bristle--remove-checkbox))
            '("<right>" . ("indent" . bristle--indent-item))
            '("<left>" . ("unindent" . bristle--unindent-item))
            '("M-<up>" . ("move up" . bristle--move-item-up))
            '("M-<down>" . ("move down" . bristle--move-item-down))
            '("-" . ("undo" . undo-only))
            '("=" . ("redo" . undo-redo))
            '("?" . ("legend" . bristle--legend-toggle))))
  
   (defun bristle--start-unordered-list ()
    "Start a new unordered list."
    (interactive)
    (unless (org-at-item-p)
      (beginning-of-line)
      (insert " - ")
      (bristle--update-statistics)))

  (defun bristle--start-ordered-list ()
    "Start a new ordered list."
    (interactive)
    (unless (org-at-item-p)
      (beginning-of-line)
      (insert " 1. ")
      (bristle--update-statistics)))

  (defun bristle--insert-item ()
    "Insert a new list item."
    (interactive)
    (when (org-at-item-p)
      (end-of-line)
      (org-insert-item)
      (bristle--update-statistics)))
  
    (defun bristle--delete-item ()
    "Delete the current list item."
    (interactive)
    (when (org-at-item-p)
      (let* ((item (org-element-at-point))
             (struct (org-list-struct)))
        (org-list-delete-item (org-element-property :begin item) struct)
        (org-list-write-struct struct (org-list-parents-alist struct))))
    (bristle--update-statistics))
  
    (defun bristle--check-item ()
    "Set the current item as checked."
    (interactive)
    (when (org-at-item-p)
      (save-excursion
        (org-beginning-of-item)
        (unless (org-at-item-checkbox-p)
          (org-toggle-checkbox '(4)))
        (let ((current-state (org-element-property :checkbox (org-element-at-point))))
          (when (not (eq current-state 'on))
            (org-toggle-checkbox)))))
    (bristle--update-statistics))
  
  (defun bristle--remove-checkbox ()
    "Remove checkbox from the current item."
    (interactive)
    (when (org-at-item-p)
      (save-excursion
        (org-beginning-of-item)
        (when (org-at-item-checkbox-p)
          (re-search-forward "\\[[ X-]\\]\\s-*" (line-end-position) t)
          (replace-match ""))))
    (bristle--update-statistics))

  (defun bristle--edit-item-text ()
    "Edit the text content of the current list item."
    (interactive)
    (when (org-at-item-p)
      (save-excursion
        (org-beginning-of-item)
        (re-search-forward "\\(?:[-+*]\\|[0-9]+[.)]\\)\\s-*\\(?:\\[[ X-]\\]\\s-*\\)?" (line-end-position) t)
        (let* ((start (point))
               (line-end (save-excursion (end-of-line) (point)))
               (text-end (save-excursion
                           (if (re-search-forward " \\[[0-9]+\\(?:/[0-9]+\\|%\\)\\]" line-end t)
                               (match-beginning 0)
                             line-end)))
               (current-text (buffer-substring-no-properties start text-end))
               (new-text (read-string "Edit: " current-text)))
          (delete-region start text-end)
          (insert new-text)))))
  
  (defun bristle--indent-item ()
    "Indent the current list item."
    (interactive)
    (org-indent-item)
    (bristle--update-statistics))

  (defun bristle--unindent-item ()
    "Unindent the current list item."
    (interactive)
    (org-outdent-item)
    (bristle--update-statistics))

  (defun bristle--move-item-up ()
    "Move the current list item up."
    (interactive)
    (org-move-item-up)
    (bristle--update-statistics))

  (defun bristle--move-item-down ()
    "Move the current list item down."
    (interactive)
    (org-move-item-down)
    (bristle--update-statistics))

  (defun bristle--update-statistics ()
    "Update statistics cookies in the buffer."
    (when (derived-mode-p 'org-mode)
      (org-update-statistics-cookies 'all)))
  
  (gestalt-define-facet
   orgt
   "Facet to work in org tables."
   :lighter " [ORG-T]"
   :parent 'move
   :keymap (gestalt-make-command-map
            '("n" . ("new" . bristle--org-new-table))
            '("e" . ("edit cell" . bristle--org-set-table-field))
            '("<up>" . ("previous" . org-table-previous-field))
            '("<down>" . ("next" . org-table-next-field))
            '("<left>" . ("" . ignore))
            '("<right>" . ("" . ignore))
            '("r" . ("new row" . org-table-insert-row))
            '("c" . ("new column" . org-table-insert-column))
            '("d" . ("delete row" . bristle--delete-row))
            '("D" . ("delete column" . org-table-delete-column))
            '("-" . ("undo" . undo-only))
            '("=" . ("redo" . undo-redo))
            '("?" . ("legend" . bristle--legend-toggle))))
  
  (defun bristle--org-new-table ()
    (interactive)
    (org-table-create)
    (org-table-next-field))

  (defun bristle--org-set-table-field ()
    (interactive)
    (org-table-get-field
     nil
     (read-string
      "Edit: "
      (string-trim (substring-no-properties (org-table-get-field)))))
    (org-table-align))

  (defun bristle--delete-row ()
    "Delete the row at point."
    (interactive)
    (delete-region (line-beginning-position) 
                   (line-end-position))
    (delete-char 1))

  (gestalt-define-transient-facet
   orgl
    '("h" . ("http" . bristle--insert-http-link))
    '("i" . ("image" . bristle--insert-image-link))
    '("f" . ("file" . bristle--insert-file-link))
    '("n" . ("note" . denote-link)))

  (defun bristle--insert-http-link (file-name)
    "Insert a link  with an HTTP target."
    (interactive (list (read-string "URL: ")))
    (org-insert-link file-name file-name (read-string "Description: ")))

  (defun bristle--insert-file-link (file-name)
    "Insert a link with a file target."
    (interactive (list (read-file-name "File: ")))
    (org-insert-link file-name file-name (read-string "Description: ")))
  
  (defun bristle--insert-image-link (file-name)
    "Insert a link with an image target."
    (interactive (list (read-file-name "File: ")))
    (progn
      (insert (format "[[file:%s]]" file-name))
      (org-redisplay-inline-images)))

  (gestalt-define-transient-facet
   orgf
   '("b" . ("bold" . bristle--bold-region))
   '("i" . ("italic" . bristle--italic-region))
   '("u" . ("underline" . bristle--underline-region))
   '("s" . ("strikethrough" . bristle--strikethrough-region))
   '("c" . ("code" . bristle--code-region))
   '("r" . ("standard" . bristle--standard-region)))

  (defun bristle--bold-region (beg end)
    "Format the current region as bold."
    (interactive "r")
    (org-emphasize ?*))

  (defun bristle--italic-region (beg end)
    "Format the current region as italic."
    (interactive "r")
    (org-emphasize ?/))
  
  (defun bristle--underline-region (beg end)
    "Format the current region as underline."
    (interactive "r")
    (org-emphasize ?_))
  
  (defun bristle--strikethrough-region (beg end)
    "Format the current region as strikethrough."
    (interactive "r")
    (org-emphasize ?+))
  
  (defun bristle--standard-region (beg end)
    "Remove formatting from the current region."
    (interactive "r")
    (org-emphasize ?\s))
  
  (defun bristle--code-region (beg end)
    "Format the current region as code."
    (interactive "r")
    (org-emphasize ?~))

  (gestalt-define-transient-facet
   orgb
   '("q" . ("quote" . bristle--insert-quote-paragraph)))

  (defun bristle--insert-quote-paragraph()
    "Insert a quote paragraph."
    (interactive)
    (progn
      (insert "#+BEGIN_QUOTE\n")
      (newline-and-indent)
      (insert "#+END_QUOTE\n")
      (previous-line 2)))

  (gestalt-define-facet
   orgv
   "Facet to work with org visibility."
   :lighter " [ORG-V]"
   :parent 'move
   :keymap (gestalt-make-command-map
            '("<up>" . ("previous" . outline-previous-heading))
            '("<down>" . ("next" . outline-next-heading))
            '("H" . ("hide all" . outline-hide-body))
            '("S" . ("show all" . outline-show-all))
            '("h" . ("hide" . outline-hide-subtree))
            '("s" . ("show" . outline-show-subtree))
            '("o" . ("olivetti" . olivetti-mode))
            '("?" . ("legend" . bristle--legend-toggle))))

  (gestalt-define-transient-facet
   orgx
   '("t" . ("toggle" . org-latex-preview)))

  (gestalt-define-transient-facet
   orgc
   '("r" . ("send" . verb-send-request-on-point-other-window))
   '("t" . ("tag" . bristle--add-verb-tag))
   '("m" . ("method" . bristle--insert-http-method))
   '("f" . ("file" . bristle--select-file))
   '("u" . ("url form" . bristle--insert-url-form))
   '("v" . ("add variable" . bristle--insert-verb-var))
   '("s" . ("set variable" . verb-set-var))
   '("l" . ("list variables" . verb-show-vars))
   '("p" . ("add prelude" . bristle--add-prelude-property))
   '("e" . ("export" . verb-export-request-on-point)))

  (defun bristle--insert-http-method ()
    "Prompt for an HTTP method and insert it at point."
  (interactive)
  (let ((method (completing-read "HTTP method: " 
                                 '("get" "put" "delete" "post" "options")
                                 nil t)))
    (insert method)))
  
  (defun bristle--add-verb-tag ()
    "Add `verb' tag to current headline."
    (interactive)
    (save-excursion
      (unless (org-back-to-heading t)
        (user-error "Not in or under an Org heading"))
      (let ((current-tags (org-get-tags nil t)))
        (unless (member "verb" current-tags)
          (org-set-tags (cons "verb" current-tags))))))

  (defun bristle--insert-verb-var ()
    "Add a variable at point."
    (interactive)
    (let ((var-name (read-string "Variable name: ")))
      (when (string-empty-p var-name)
        (user-error "Variable name cannot be empty"))
      (insert (format "{{(verb-var %s)}}" var-name))))

  (defun bristle--add-prelude-property ()
    "Prompt for a verb prelude file and add it to the header."
    (interactive)
    (save-excursion
      (unless (org-back-to-heading t)
        (user-error "Not in or under an Org heading"))
      (let ((file (read-file-name "Prelude file: " nil nil t)))
        (let ((filename (file-relative-name file)))
          (org-set-property "Verb-Prelude" filename)))))

  (defun bristle--select-file ()
    "Prompt for a file to use in a verb request."
    (interactive)
    (let ((file (read-file-name "File to send: " nil nil t)))
      (let ((filename (file-relative-name file)))
        (insert (format "{{(verb-read-file \"%s\")}}" filename)))))

  (defun bristle--insert-url-form ()
    "Insert an URL encoded form template at point."
    (interactive)
    (insert "{{(verb-util-form-url-encode '((\"key\" . \"value\")))}}"))

  (gestalt-define-transient-facet
   orge
   '("m" . ("markdown" . org-pandoc-export-as-gfm)))
  
  (gestalt-define-transient-facet
   orgs
   '("n" . ("new" . bristle--insert-src-block))
   '("e" . ("edit" . org-edit-src-code))
   '("r" . ("run" . org-babel-execute-src-block)))

  (defun bristle--insert-src-block (arg)
    "Prompt for language then enter a code block."
    (interactive
     (list
      (completing-read "Source block language: " '("mermaid"
                                                   "octave"
                                                   "go"
                                                   "json"
                                                   "tsx"
                                                   "typescript"
                                                   "yaml"
                                                   "dockerfile"
                                                   "shell"
                                                   "graphql"
                                                   "terraform"
                                                   "verb"
                                                   "xml"
                                                   "duckdb"
                                                   "emacs-lisp"))))
    (let ((begin-src-line 
           (cond ((equal arg "mermaid")
                  (format "#+BEGIN_SRC %s :file \"%s.svg\" :mermaid-config-file \"%smermaid-config.json\" :background-color transparent" 
                          arg 
                          (format-time-string "%s") 
                          (abbreviate-file-name user-emacs-directory)))
                 (t
                  (format "#+BEGIN_SRC %s" arg)))))
      (insert begin-src-line)
      (newline-and-indent)
      (newline-and-indent)
      (insert "#+END_SRC\n")
      (previous-line 2)
      (unless (equal arg "verb")
        (org-edit-src-code)))))

(provide 'init-gestalt)
