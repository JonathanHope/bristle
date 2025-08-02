;;; emacs-mcp-tools.el --- MCP server default tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: jhope@theflatfield.net
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: mcp, tools

;;; Commentary:

;; This file contains the default tool definitions and handlers for the Emacs MCP server:
;; - `get-project-root': get the root directory of the current project
;; - `list-project-files': list files in the current project; supports wildcards
;; - `list-project-directories': list directories in current project
;; - `grep-project': grep in the current project
;; - `regex-replace-project': regex find and replace in the current project
;; - `get-file-info': get metadata for a given file
;; - `read-file': read the contents of a given file
;; - `write-file': write to a file; create if it doesn't exist
;; - `create-directory': create a directory if it doesn't already exist
;; - `delete-file': delete a file if it exists
;; - `delete-directory': delete a directory if it exists; support recursive delete
;; - `fetch-webpage': fetch a web page as plain text
;; - `run-shell-command': execute a shell command
;; - `eval-elisp': evaluate Emacs Lisp code

;; TODO.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'seq)
(require 'url)
(require 'shr)
(require 'project)
(require 'pcre2el)

;; tool definitions

(defvar emacs-mcp-default-tools
  '(("get-project-root" .
     ((title . "Get Project Root")
      (description . "Get the root directory path of the current project")
      (inputSchema . ((type . "object")
                      (additionalProperties . :false)))
      (handler . emacs-mcp--tool-get-project-root)))

    ("list-project-files" .
     ((title . "List Project Files")
      (description . "List files in the current project, optionally filtered by wildcard")
      (inputSchema . ((type . "object")
                      (properties . ((pattern . ((type . "string")
                                                  (description . "Filename wildcard (optional)")))))
                      (additionalProperties . :false)))
      (handler . emacs-mcp--tool-get-project-files)))

    ("list-project-directories" .
     ((title . "List Project Directories")
      (description . "List directories in the current project")
      (inputSchema . ((type . "object")
                      (additionalProperties . :false)))
      (handler . emacs-mcp--tool-get-project-directories)))

    ("grep-project" .
     ((title . "Grep Project")
      (description . "Regex search in project files, optionally limited to specific files")
      (inputSchema . ((type . "object")
                      (properties . ((pattern . ((type . "string")
                                                 (description . "Regular expression pattern")))
                                     (files . ((type . "array")
                                               (items . ((type . "string")))
                                               (description . "Limit search to these files (optional)")))
                                     (case_sensitive . ((type . "boolean")
                                                        (description . "Whether search is case sensitive")
                                                        (default . :false)))))
                      (required . ["pattern"])
                      (additionalProperties . :false)))
      (handler . emacs-mcp--tool-grep-project)))

    ("regex-replace-project" .
     ((title . "Regex Replace in Project")
      (description . "Perform regex find and replace in project files, optionally limited to specific files")
      (inputSchema . ((type . "object")
                      (properties . ((pattern . ((type . "string")
                                                 (description . "Regular expression pattern to find")))
                                     (replacement . ((type . "string")
                                                    (description . "Replacement string (supports capture groups like \\1)")))
                                     (files . ((type . "array")
                                               (items . ((type . "string")))
                                               (description . "Limit replacement to these files (optional)")))
                                     (case_sensitive . ((type . "boolean")
                                                        (description . "Whether replacement is case sensitive")
                                                        (default . :false)))))
                      (required . ["pattern" "replacement"])
                      (additionalProperties . :false)))
      (handler . emacs-mcp--tool-regex-replace-project)))

    ("get-file-info" .
     ((title . "Get File Info")
      (description . "Get metadata information about a file (size, permissions, timestamps, etc.)")
      (inputSchema . ((type . "object")
                      (properties . ((path . ((type . "string")
                                              (description . "Path to the file to get info about")))))
                      (required . ["path"])
                      (additionalProperties . :false)))
      (handler . emacs-mcp--tool-get-file-info)))
    
    ("read-file" .
     ((title . "Read File Contents")
      (description . "Read the contents of a specified file, optionally within a line range")
      (inputSchema . ((type . "object")
                      (properties . ((path . ((type . "string")
                                              (description . "Path to the file to read")))
                                     (start_line . ((type . "integer")
                                                    (description . "Starting line number (optional) (1-based)")
                                                    (minimum . 1)))
                                     (end_line . ((type . "integer")
                                                  (description . "Ending line number (optional) (1-based)")
                                                  (minimum . 1)))))
                      (required . ["path"])
                      (additionalProperties . :false)))
      (handler . emacs-mcp--tool-read-file)))

    ("write-file" .
     ((title . "Write File")
      (description . "Write content to a file, creating it if it doesn't exist or replacing existing content")
      (inputSchema . ((type . "object")
                      (properties . ((path . ((type . "string")
                                              (description . "Path to the file to write")))
                                     (content . ((type . "string")
                                                 (description . "Content to write to the file")))))
                      (required . ["path" "content"])
                      (additionalProperties . :false)))
      (handler . emacs-mcp--tool-write-file)))

    ("create-directory" .
     ((title . "Create Directory")
      (description . "Create a directory if it doesn't already exist")
      (inputSchema . ((type . "object")
                      (properties . ((path . ((type . "string")
                                              (description . "Path to the directory to create")))))
                      (required . ["path"])
                      (additionalProperties . :false)))
      (handler . emacs-mcp--tool-create-directory)))

    ("delete-file" .
     ((title . "Delete File")
      (description . "Delete a file if it exists")
      (inputSchema . ((type . "object")
                      (properties . ((path . ((type . "string")
                                              (description . "Path to the file to delete")))))
                      (required . ["path"])
                      (additionalProperties . :false)))
      (handler . emacs-mcp--tool-delete-file)))

    ("delete-directory" .
     ((title . "Delete Directory")
      (description . "Delete a directory if it exists")
      (inputSchema . ((type . "object")
                      (properties . ((path . ((type . "string")
                                              (description . "Path to the directory to delete")))
                                     (recursive . ((type . "boolean")
                                                   (description . "Whether to delete directory recursively (default: false)")))))
                      (required . ["path"])
                      (additionalProperties . :false)))
      (handler . emacs-mcp--tool-delete-directory)))

    ("fetch-webpage" .
     ((title . "Fetch Webpage")
      (description . "Fetch a webpage and return it as plain text")
      (inputSchema . ((type . "object")
                      (properties . ((url . ((type . "string")
                                             (description . "URL of the webpage to fetch")))))
                      (required . ["url"])
                      (additionalProperties . :false)))
      (handler . emacs-mcp--tool-fetch-webpage)))

    ("run-shell-command" .
     ((title . "Run Shell Command")
      (description . "Execute a shell command and return its output")
      (inputSchema . ((type . "object")
                      (properties . ((command . ((type . "string")
                                                 (description . "Shell command to execute")))
                                     (working_directory . ((type . "string")
                                                           (description . "Working directory to run command in (optional)")))))
                      (required . ["command"])
                      (additionalProperties . :false)))
      (handler . emacs-mcp--tool-run-command)))

    ("eval-elisp" .
     ((title . "Evaluate Emacs Lisp")
      (description . "Evaluate Emacs Lisp code and return the result")
      (inputSchema . ((type . "object")
                      (properties . ((code . ((type . "string")
                                              (description . "Emacs Lisp code to evaluate")))))
                      (required . ["code"])
                      (additionalProperties . :false)))
      (handler . emacs-mcp--tool-eval-elisp))))
  "Default MCP tools.")

;; handler functions

(defun emacs-mcp--tool-get-project-root (args)
  "Get project root tool handler."
  (let ((project (project-current)))
    (if project
        (expand-file-name (project-root project))
      '(error :code -32603 :message "No project found"))))

(defun emacs-mcp--tool-get-project-files (args)
  "Get project files tool handler."
  (let* ((pattern (alist-get 'pattern args))
         (project (project-current)))
    (if project
        (let ((files (project-files project)))
          (if (and pattern (not (string-blank-p pattern)))
              (let* ((regexp (wildcard-to-regexp pattern))
                     (filtered (seq-filter (lambda (file)
                                            (string-match-p regexp
                                                            (file-name-nondirectory file)))
                                          files)))
                (vconcat filtered))
            (vconcat files)))
      '(error :code -32603 :message "No project found"))))

(defun emacs-mcp--tool-get-project-directories (args)
  "Get project directories tool handler."
  (let ((project (project-current)))
    (if project
        (let* ((project-root (project-root project))
               (all-files (project-files project)))
        (cl-remove-duplicates
         (mapcar (lambda (file)
                   (file-name-directory file))
                 all-files)
         :test #'string=))
      '(error :code -32603 :message "No project found"))))

(defun emacs-mcp--tool-grep-project (args)
  "Grep project tool handler."
  (let ((pattern (alist-get 'pattern args))
        (files (alist-get 'files args))
        (case-sensitive (alist-get 'case_sensitive args)))
    (if (not pattern)
        '(error :code -32602 :message "Missing required parameter: pattern")
      (let ((project (project-current)))
        (if project
            (let* ((search-files (if files
                                     (if (vectorp files) (append files nil) files)
                                   (project-files project)))
                   (matches '())
                   (case-fold-search (not case-sensitive))
                   ;; callers will likely use pcre not elisp patterns
                   (elisp-pattern (condition-case nil
                                      (rxt-pcre-to-elisp pattern)
                                    (error pattern))))
              
              (message elisp-pattern)
              (dolist (file search-files)
                (when (and (file-regular-p file) (file-readable-p file))
                  ;; swallow file access errors
                  (condition-case nil
                      (with-temp-buffer
                        (insert-file-contents file)
                        (goto-char (point-min))
                        (while (re-search-forward elisp-pattern nil t)
                          (let* ((line-number (line-number-at-pos))
                                 (match-line (buffer-substring-no-properties 
                                             (line-beginning-position) 
                                             (line-end-position)))
                                 (match-info (format "%s:%d:%s" file line-number match-line)))
                            (push match-info matches))))
                    (error nil))))
              
              (if matches
                  (mapconcat 'identity (nreverse matches) "\n")
                (format "No matches found for pattern \"%s\"" pattern)))
          '(error :code -32603 :message "No project found"))))))

(defun emacs-mcp--tool-regex-replace-project (args)
  "Regex replace in project tool handler."
  (let ((pattern (alist-get 'pattern args))
        (replacement (alist-get 'replacement args))
        (files (alist-get 'files args))
        (case-sensitive (alist-get 'case_sensitive args)))
    (cond
     ((not pattern)
      '(error :code -32602 :message "Missing required parameter: pattern"))
     ((not replacement)
      '(error :code -32602 :message "Missing required parameter: replacement"))
     (t
      (let ((project (project-current)))
        (if project
            (let* ((search-files (if files
                                     (if (vectorp files) (append files nil) files)
                                   (project-files project)))
                   (replacements '())
                   (case-fold-search (not case-sensitive)))
              
              (dolist (file search-files)
                (when (and (file-regular-p file) (file-readable-p file) (file-writable-p file))
                  (condition-case nil
                      (with-temp-buffer
                        (insert-file-contents file)
                        (goto-char (point-min))
                        (let ((file-changed nil)
                              (replace-count 0))
                          (while (re-search-forward pattern nil t)
                            (replace-match replacement)
                            (setq replace-count (1+ replace-count))
                            (setq file-changed t))
                          (when file-changed
                            (write-file file)
                            (push (format "%s: %d replacements" file replace-count) replacements))))
                    (error nil))))
              
              (if replacements
                  (mapconcat 'identity (nreverse replacements) "\n")
                (format "No replacements made for pattern \"%s\"" pattern)))
          '(error :code -32603 :message "No project found")))))))

(defun emacs-mcp--tool-get-file-info (args)
  "Get file info tool handler."
  (let ((file-path (alist-get 'path args)))
    (if (not file-path)
        '(error :code -32602 :message "Missing required parameter: path")
      (condition-case err
          (if (file-exists-p file-path)
              (let ((attrs (file-attributes file-path)))
                `((path . ,file-path)
                  (exists . t)
                  (type . ,(cond ((nth 0 attrs) "directory")
                                ((file-symlink-p file-path) "symlink")
                                (t "file")))
                  (size . ,(nth 7 attrs))
                  (permissions . ,(nth 8 attrs))
                  (readable . ,(file-readable-p file-path))
                  (writable . ,(file-writable-p file-path))
                  (executable . ,(file-executable-p file-path))
                  (modified_time . ,(format-time-string "%Y-%m-%d %H:%M:%S" (nth 5 attrs)))
                  (accessed_time . ,(format-time-string "%Y-%m-%d %H:%M:%S" (nth 4 attrs)))
                  (uid . ,(nth 2 attrs))
                  (gid . ,(nth 3 attrs))
                  (link_count . ,(nth 1 attrs))))
            `((path . ,file-path)
              (exists . :false)))
        (error
         `(error :code -32603 :message ,(format "Error getting file info: %s" (error-message-string err))))))))

(defun emacs-mcp--tool-read-file (args)
  "Get file contents tool handler."
  (let ((file-path (alist-get 'path args))
        (start-line (alist-get 'start_line args))
        (end-line (alist-get 'end_line args)))
    (if (not file-path)
        '(error :code -32602 :message "Missing required parameter: path")
      (condition-case err
          (if (file-exists-p file-path)
              (if (file-readable-p file-path)
                  (with-temp-buffer
                    (insert-file-contents file-path)
                    (if (or start-line end-line)
                        (emacs-mcp--extract-line-range (current-buffer) start-line end-line)
                      (buffer-string)))
                '(error :code -32603 :message "File is not readable"))
            '(error :code -32603 :message "File does not exist"))
        (error
         `(error :code -32603 :message ,(format "Error reading file: %s" (error-message-string err))))))))

(defun emacs-mcp--extract-line-range (buffer start-line end-line)
  "Extract lines from BUFFER between START-LINE and END-LINE (1-based, inclusive)."
  (with-current-buffer buffer
    (let* ((total-lines (count-lines (point-min) (point-max)))
           (actual-start (max 1 (or start-line 1)))
           (actual-end (min total-lines (or end-line total-lines))))
      (if (> actual-start actual-end)
          ""
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- actual-start))
          (let ((start-pos (point)))
            (forward-line (- actual-end actual-start -1))
            (buffer-substring-no-properties start-pos (point))))))))

(defun emacs-mcp--tool-write-file (args)
  "Write file tool handler."
  (let ((file-path (alist-get 'path args))
        (content (alist-get 'content args)))
    (cond
     ((not file-path)
      '(error :code -32602 :message "Missing required parameter: path"))
     ((not content)
      '(error :code -32602 :message "Missing required parameter: content"))
     (t
      (condition-case err
          (progn
            (let ((dir (file-name-directory file-path)))
              (when (and dir (not (file-exists-p dir)))
                (make-directory dir t)))
            (with-temp-buffer
              (insert content)
              (write-file file-path))
            (format "Successfully wrote %d characters to %s" (length content) file-path))
        (error
         `(error :code -32603 :message ,(format "Error writing file: %s" (error-message-string err)))))))))

(defun emacs-mcp--tool-create-directory (args)
  "Create directory tool handler."
  (let ((dir-path (alist-get 'path args)))
    (if (not dir-path)
        '(error :code -32602 :message "Missing required parameter: path")
      (condition-case err
          (if (file-exists-p dir-path)
              (if (file-directory-p dir-path)
                  (format "Directory already exists: %s" dir-path)
                '(error :code -32603 :message "Path exists but is not a directory"))
            (progn
              (make-directory dir-path t)
              (format "Successfully created directory: %s" dir-path)))
        (error
         `(error :code -32603 :message ,(format "Error creating directory: %s" (error-message-string err))))))))

(defun emacs-mcp--tool-delete-file (args)
  "Delete file tool handler."
  (let ((file-path (alist-get 'path args)))
    (if (not file-path)
        '(error :code -32602 :message "Missing required parameter: path")
      (condition-case err
          (if (file-exists-p file-path)
              (if (file-directory-p file-path)
                  '(error :code -32603 :message "Path is a directory, not a file")
                (progn
                  (delete-file file-path)
                  (format "Successfully deleted file: %s" file-path)))
            (format "File does not exist: %s" file-path))
        (error
         `(error :code -32603 :message ,(format "Error deleting file: %s" (error-message-string err))))))))

(defun emacs-mcp--tool-delete-directory (args)
  "Delete directory tool handler."
  (let ((dir-path (alist-get 'path args))
        (recursive (alist-get 'recursive args)))
    (if (not dir-path)
        '(error :code -32602 :message "Missing required parameter: path")
      (condition-case err
          (if (file-exists-p dir-path)
              (if (file-directory-p dir-path)
                  (progn
                    (delete-directory dir-path recursive)
                    (format "Successfully deleted directory: %s" dir-path))
                '(error :code -32603 :message "Path is not a directory"))
            (format "Directory does not exist: %s" dir-path))
        (error
         `(error :code -32603 :message ,(format "Error deleting directory: %s" (error-message-string err))))))))

(defun emacs-mcp--tool-fetch-webpage (args)
  "Fetch webpage tool handler."
  (let ((url (alist-get 'url args)))
    (if (not url)
        '(error :code -32602 :message "Missing required parameter: url")
      (condition-case err
          (progn
            (with-temp-buffer
              (url-insert-file-contents url)
              (let ((html-buffer (current-buffer)))
                (with-temp-buffer
                  (shr-insert-document
                   (with-current-buffer html-buffer
                     (libxml-parse-html-region (point-min) (point-max))))
                  (buffer-string)))))
        (error
         `(error :code -32603 :message ,(format "Error fetching webpage: %s" (error-message-string err))))))))

(defun emacs-mcp--tool-run-command (args)
  "Run command tool handler."
  (let ((command (alist-get 'command args))
        (working-dir (alist-get 'working_directory args)))
    (if (not command)
        '(error :code -32602 :message "Missing required parameter: command")
      (condition-case err
          (let ((default-directory 
                 (cond
                  (working-dir (expand-file-name working-dir))
                  ((project-current)
                   (expand-file-name (project-root (project-current))))
                  (t default-directory))))
            (with-temp-buffer
              (let* ((exit-code (call-process-shell-command command nil t nil))
                     (output (buffer-string)))
                `((command . ,command)
                  (working_directory . ,default-directory)
                  (exit_code . ,exit-code)
                  (output . ,output)))))
        (error
         `(error :code -32603 :message ,(format "Error running command: %s" (error-message-string err))))))))

(defun emacs-mcp--tool-eval-elisp (args)
  "Evaluate Emacs Lisp tool handler."
  (let ((code (alist-get 'code args)))
    (if (not code)
        '(error :code -32602 :message "Missing required parameter: code")
      (condition-case err
          (let ((result (eval (read code))))
            `((code . ,code)
              (result . ,(format "%S" result))))
        (error
         `(error :code -32603 :message ,(format "Error evaluating elisp: %s" (error-message-string err))))))))

(provide 'emacs-mcp-tools)
;;; emacs-mcp-tools.el ends here
