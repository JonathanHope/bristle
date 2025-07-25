;;; soma.el --- Interface for notes. -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jonathan Hope

;; Author: Jonathan Hope <jhope@theflatfield.net>
;; Version: 1.0
;; Keywords: notes denote
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Interface for notes.
;; Built on denote and consult.

;;; Code:

;; requirements

(eval-when-compile (require 'json))
(unless (require 'denote nil 'noerror)
  (message "denote is required"))
(eval-when-compile (require 'denote))
(unless (require 'consult nil 'noerror)
  (message "consult is required"))
(eval-when-compile (require 'consult))

;; custom

(defgroup soma nil
  "Interface for notes"
  :group 'extensions)

(defface soma-id-face
  '()
  "Face used for ID part of candidate."
  :group 'soma)

(defface soma-title-face
  '()
  "Face used for title part of candidate."
  :group 'soma)

(defface soma-filetags-face
  '()
  "Face used for filetags part of candidate."
  :group 'soma)

(defcustom soma-notes-max-results 50
  "Maximum number of search results."
  :type 'integer
  :group 'soma-notes)

;; private functions

(defun soma--query-notes (query max)
  "Uses `rg' to search the notes in `denote-directory.';
Only org files with a denote identifier in their name are searched.
The search is  performed case insensitive on the title in the front matter.
At most `max' number of results are returned.
Requires `rg' to be in the PATH."
  (split-string
   (shell-command-to-string
    (format "rg -l -i '^\\#\\+title:.*%s.*' %s -g '*.{org}' | rg '[0-9]{8}T[0-9]{6}' | head -n %d" query (denote-directory) max))))

(defun soma--get-notes-meta (notes)
  "Get metadata for notes.
The metadata is parsed from the front matter of the notes."
  (if (not notes)
      '()
    (let ((notes-with-meta '()))
      (dolist (note-path notes)
        (when (file-exists-p note-path)
          (let ((meta (make-hash-table :test 'equal)))
            (with-temp-buffer
              (insert-file-contents note-path)
              (goto-char (point-min))
              (while (and (not (eobp)) (looking-at "^#\\+\\([a-zA-Z_]+\\): *\\(.*\\)$"))
                (let ((key (match-string 1))
                      (value (match-string 2)))
                  (when key
                    (puthash key (if value (string-trim value) "") meta)))
                (forward-line 1)))
            (push (list note-path meta) notes-with-meta))))
      notes-with-meta)))

(defun soma--new-note (cand)
  "Create a note with `denote'."
  (interactive "fNote: ")
  (denote cand (denote-keywords-prompt)))

(defun soma--note-items (input)
  "`consult' source items for notes.
This must be an async source because it is calling out to `rg'."
  (mapcar (lambda (note)
            (list (let* ((meta (car (cdr note)))
                         (title (or (gethash "title" meta) ""))
                         (id (or (gethash "identifier" meta) ""))
                         (filetags (or (gethash "filetags" meta) "")))
                    (concat
                     (propertize id 'face 'soma-id-face)
                     " "
                     (propertize title 'face 'soma-title-face)
                     " "
                     (propertize filetags 'face 'soma-filetags-face)))
                  (car note)))
          (soma--get-notes-meta
           (soma--query-notes input soma-notes-max-results))))

(defun soma--check-external-tools ()
  "Check if required external tools are available."
  (unless (executable-find "rg")
    (user-error "ripgrep (rg) is required but not found in PATH")))

;; public functions

;;;###autoload
(defun soma-consult-notes ()
  "`consult' the available notes."
  (interactive)
  (soma--check-external-tools)
  (consult--multi (list
                   (list :async (consult--dynamic-collection 'soma--note-items)
                         :action (lambda (cand) (find-file (car cand)))
                         :new #'soma--new-note
                         :require-match nil))
                  :prompt "Notes: "))

(provide 'soma)
;;; soma.el ends here
