;;; stadion.el --- Taskfile integration for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Your Name <jhope@theflatfield.net>
;; Keywords: taskfile, compile
;; Package-Requires: ((emacs "29.1"))
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Stadion is a lightweight task runner for Emacs.
;; It gathers up the available tasks, prompts for a task, and runs it.
;; The task is run using `compile'.
;; At this time only `task' is supported.

;;; Code:

(eval-when-compile
  (require 'project)
  (require 'compile))

;;;###autoload
(defun stadion-run ()
  "Run a task in project root using compile mode."
  (interactive)
  (let* ((project-root (project-root (project-current t)))
         (default-directory project-root)
         (taskfile-path (expand-file-name "Taskfile.yml" project-root)))
    
    (unless (file-exists-p taskfile-path)
      (user-error "No Taskfile.yml found in project root: %s" project-root))
    
    (let* ((task-output (shell-command-to-string "task --list-all"))
           (tasks (stadion--parse-taskfile-tasks task-output))
           (task-names (mapcar #'car tasks))
           (completion-extra-properties
            `(:annotation-function 
              ,(lambda (task)
                 (let ((desc (cdr (assoc task tasks))))
                   (if desc (format " - %s" desc) "")))))
           (selected-task (completing-read "Task: " task-names nil t)))
      
      (if selected-task
          (compile (format "task %s" selected-task))
        (message "No task selected")))))

(defun stadion--parse-taskfile-tasks (output)
  "Parse task output into an alist of (task-name . description)."
  (let ((lines (split-string output "\n" t))
        (tasks '()))
    (dolist (line lines)
      (unless (string-match-p "^task:" line)
        (when (string-match "^\\* \\(.+\\):\\s-*\\(.*\\)$" line)
          (let ((task-name (string-trim (match-string 1 line)))
                (description (string-trim (match-string 2 line))))
            (push (cons task-name 
                        (if (string-empty-p description) nil description))
                  tasks)))))
    (nreverse tasks)))

(provide 'stadion)

;;; stadion.el ends here
