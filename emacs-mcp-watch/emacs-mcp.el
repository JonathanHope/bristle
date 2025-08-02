;;; emacs-mcp.el --- File-based MCP bridge for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: jhope@theflatfield.net
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: mcp

;;; Commentary:

;; TODO

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'filenotify)

;;; private variables

(defvar emacs-mcp--watch-dir "/tmp/emacs-mcp-bridge"
  "Directory to watch for MCP command files.")

(defvar emacs-mcp--watcher nil
  "File watcher for MCP command files.")

(defvar emacs-mcp--debug nil
  "Enable debug logging for file bridge.")

(defvar emacs-mcp--active nil
  "Whether the file bridge is currently active.")

(defcustom emacs-mcp-commands
  '(("get-point" . (lambda (params) (point)))

    ("get-line-number" . (lambda (params) (line-number-at-pos)))

    ("get-buffer-name" . (lambda (params) (buffer-name)))

    ("get-project-name" . (lambda (params) 
                            (if (fboundp 'project-current)
                                (let ((project (project-current)))
                                  (if project
                                      (file-name-nondirectory 
                                       (directory-file-name 
                                        (project-root project)))
                                    "No project"))
                              "Project feature not available")))

    ("list-project-files" . (lambda (params)
                              (if (fboundp 'project-current)
                                  (let ((project (project-current)))
                                    (if project
                                        (project-files project)
                                      '("No project")))
                                '("Project feature not available"))))

    ("read-file" . (lambda (params)
                     (let ((filename (alist-get 'filename params)))
                       (if filename
                           (condition-case err
                               (if (file-exists-p filename)
                                   (with-temp-buffer
                                     (insert-file-contents filename)
                                     (buffer-string))
                                 (format "File not found: %s" filename))
                             (error (format "Error reading file: %s" (error-message-string err))))
                         "No filename provided"))))

    ("fetch-webpage" . (lambda (params)
                         (let ((url (alist-get 'url params)))
                           (if url
                               (condition-case err
                                   (progn
                                     (require 'shr)
                                     (require 'url)
                                     (with-temp-buffer
                                       (url-insert-file-contents url)
                                       (let ((dom (libxml-parse-html-region (point-min) (point-max))))
                                         (erase-buffer)
                                         (shr-insert-document dom)
                                         (buffer-string))))
                                 (error (format "Error fetching webpage: %s" (error-message-string err))))
                             "No URL provided")))))
  "Alist of MCP commands and their handler functions.
Each entry is (COMMAND-NAME . HANDLER-FUNCTION).
HANDLER-FUNCTION should accept PARAMS as argument and return the result."
  :type '(alist :key-type string :value-type function)
  :group 'emacs-mcp)

;; public API

;;;###autoload
(defun emacs-mcp-start (&optional watch-dir)
  "Start the MCP bridge using WATCH-DIR for communication."
  (interactive)
  (when watch-dir
    (setq emacs-mcp--watch-dir (expand-file-name watch-dir)))
  (emacs-mcp-stop)
  (emacs-mcp-ensure-dir)
  (setq emacs-mcp--watcher (file-notify-add-watch emacs-mcp--watch-dir
                                                  '(change) #'emacs-mcp-file-watcher))
  (setq emacs-mcp--active t)
  (message "MCP file bridge started, watching: %s" emacs-mcp--watch-dir))

;;;###autoload
(defun emacs-mcp-stop ()
  "Stop the file-based MCP bridge."
  (interactive)
  (when emacs-mcp--watcher
    (file-notify-rm-watch emacs-mcp--watcher)
    (setq emacs-mcp--watcher nil))
  (setq emacs-mcp--active nil)
  (message "MCP file bridge stopped"))

;;;###autoload
(defun emacs-mcp-active-p ()
  "Whether the MCP bridge is active."
  (interactive)
  emacs-mcp--active)

;;;###autoload
(defun emacs-mcp-toggle-debug ()
  "Toggle debug logging for file bridge."
  (interactive)
  (setq emacs-mcp--debug (not emacs-mcp--debug))
  (message "MCP file bridge debug logging %s" 
           (if emacs-mcp--debug "enabled" "disabled")))

;;;###autoload
(defun emacs-mcp-cleanup ()
  "Clean up any leftover command/response files."
  (interactive)
  (when (file-directory-p emacs-mcp--watch-dir)
    (let ((count 0))
      (dolist (file (directory-files emacs-mcp--watch-dir t "\\(command\\|response\\)-.*\\.json$"))
        (delete-file file)
        (setq count (1+ count)))
      (message "Cleaned up %d files from %s" count emacs-mcp--watch-dir))))

;;; private functions

(defun emacs-mcp-log (format-string &rest args)
  "Log a message if debugging is enabled."
  (when emacs-mcp--debug
    (message "[MCP File Bridge] %s" (apply #'format format-string args))))

(defun emacs-mcp-ensure-dir ()
  "Ensure the watch directory exists."
  (unless (file-directory-p emacs-mcp--watch-dir)
    (make-directory emacs-mcp--watch-dir t))
  emacs-mcp--watch-dir)

(defun emacs-mcp-safe-eval (elisp-string)
  "Safely evaluate ELISP-STRING and return result as serializable data."
  (condition-case err
      (let ((result (eval (read elisp-string))))
        (cond
         ((stringp result) result)
         ((numberp result) result)
         ((symbolp result) (symbol-name result))
         ((listp result) result)
         ((vectorp result) (append result nil))
         (t (format "%S" result))))
    (error (list :error (error-message-string err)))))

(defun emacs-mcp-process-command (command-data)
  "Process a command and return the result."
  (let* ((command (alist-get 'command command-data))
         (params (alist-get 'params command-data))
         (id (alist-get 'id command-data)))
    (emacs-mcp-log "processing command: %s (id: %s)" command id)
    (let ((result (emacs-mcp-dispatch-command command params)))
      (list :id id :result result))))

(defun emacs-mcp-dispatch-command (command params)
  "Dispatch COMMAND with PARAMS to appropriate handler."
  (let ((handler (alist-get command emacs-mcp-commands nil nil #'string=)))
    (if handler
        (funcall handler params)
      (list :error (format "unknown command: %s" command)))))

(defun emacs-mcp-file-watcher (event)
  "Handle file system events for MCP commands."
  (let ((event-type (nth 1 event))
        (file-path (nth 2 event)))
    (when (and (memq event-type '(created changed))
               (string-match "command-\\(.+\\)\\.json$" (file-name-nondirectory file-path)))
      (emacs-mcp-log "processing command file: %s" file-path)
      (run-with-timer 0.05 nil (lambda () (emacs-mcp-process-command-file file-path))))))

(defun emacs-mcp-process-command-file (file-path)
  "Process a command file and write the response."
  (when (file-exists-p file-path)
    (let* ((command-json (with-temp-buffer
                           (insert-file-contents file-path)
                           (buffer-string)))
           (command-data (json-parse-string command-json :object-type 'alist))
           (response (emacs-mcp-process-command command-data))
           (response-file (replace-regexp-in-string "command-" "response-" file-path)))
      (with-temp-file response-file
        (insert (json-encode response)))
      (delete-file file-path)
      (emacs-mcp-log "processed command, wrote response to: %s" response-file))))

(provide 'emacs-mcp)
;;; emacs-mcp.el ends here
