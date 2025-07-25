;;; consult-hl-todo.el --- Consult integration for hl-todo -*- lexical-binding: t -*-

;; Author: jhope@theflatfield.net
;; Package-Requires: ((emacs "29.1") (consult "2.6") (hl-todo "3.8"))

;;; Commentary:

;; This package provides consult integration for hl-todo.

;;; Code:

(require 'consult)
(require 'hl-todo)

(defun consult-hl-todo--candidates ()
  "Collect TODO items in current buffer."
  (let ((candidates nil))
    (save-excursion
      (goto-char (point-min))
      (while (hl-todo--search)
        (let* ((pos (point))
               (line (line-number-at-pos))
               (line-start (line-beginning-position))
               (line-end (line-end-position))
               (text (string-trim (buffer-substring line-start line-end)))
               (candidate (format "%4d: %s" line text)))
          (put-text-property 0 1 'consult--candidate pos candidate)
          (push candidate candidates))))
    (nreverse candidates)))

;;;###autoload
(defun consult-hl-todo ()
  "Jump to a TODO item in the current buffer."
  (interactive)
  (let ((candidates (consult-hl-todo--candidates)))
    (if candidates
        (consult--read candidates
                       :prompt "TODO: "
                       :category 'consult-location
                       :require-match t
                       :lookup #'consult--lookup-candidate
                       :state (consult--jump-state))
      (user-error "No TODO items found"))))

(provide 'consult-hl-todo)
;;; consult-hl-todo.el ends here
