;; -*- lexical-binding: t; -*-

(straight-use-package '(ediff :type built-in))

(use-package ediff
  :defer t

  :init
  (add-hook 'ediff-startup-hook 'bristle--ediff-startup))

(straight-use-package '(ediff-wind :type built-in))

(use-package ediff-wind
  :defer t
  
  :custom
  ;; don't have the control panel be a separate frame
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; do side by side diffs if there is enough space
  (ediff-split-window-function (if (> (frame-width) (* 2 80)) 'split-window-horizontally 'split-window-vertically)))

(defun bristle--ediff-startup()
  "Makes ediff work at a char level with auto refining."
  (setq ediff-forward-word-function 'forward-char)
  (setq ediff-highlight-all-diffs t)
  (setq ediff-auto-refine-limit 200000)
  (setq ediff-auto-refine 'on)
  (ediff-toggle-read-only ediff-buffer-A)
  (ediff-toggle-read-only ediff-buffer-B)
  (ediff-next-difference))

(provide 'init-ediff)
