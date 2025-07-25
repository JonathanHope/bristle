;; -*- lexical-binding: t; -*-

(straight-use-package 
 `(legend
   :type nil
   :local-repo ,(concat user-emacs-directory "legend")
   :files ("legend.el")))

(use-package legend
  :after gestalt
  
  :init
  (add-hook 'gestalt-change-keymap-hook #'bristle--legend-update)
  (add-hook 'buffer-list-update-hook #'bristle--legend-update)
  (add-hook 'window-size-change-functions #'bristle--legend-update-on-resize)

  (defun bristle--legend-toggle ()
    "Toggle the legend popup for the current keymap."
    (interactive)
    (legend-toggle (gestalt-active-keymap)))

  (defun bristle--legend-update ()
    "Update the legend popup based on the current keymap."
    (interactive)
    (with-current-buffer (window-buffer)
      (legend-redraw (gestalt-active-keymap))))

  (defun bristle--legend-update-on-resize (frame)
    "Update the legend popup when FRAME size changes."
    (with-selected-frame frame
      (bristle--legend-update)))
  
  :custom
  (legend-filtered-commands '(ignore
                              bristle--contract-char
                              bristle--region-up
                              dired-find-file
                              bristle--smart-tab)))

(provide 'init-legend)
