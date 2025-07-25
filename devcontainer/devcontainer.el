;;; devcontainer.el --- Dev container support for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: jhope@theflatfield.net
;; Version: 1.0.0
;; Keywords: devcontainer
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This package provides Dev container support for Emacs.
;; The goal is approximate some of what vscode can do with them.
;; This package is very experimental and I am likely biting the sun here.

;; The first obvious functionality it needs is the ability to build/start/stop dev containers.
;; This is done by invoking the `devcontainer' CLI tool.
;; Only one dev container can be running at a time.

;; Newer versions of Emacs support Docker with tramp natively.
;; This package makes heavy (ab)use of that.

;; vscode has a sidebar to navigate files in the dev container with.
;; This is approximated with `dired' + `tramp'.

;; In vscode you can fuzzy navigate and regex search on files in the workspace.
;; This is approximated with `project.el'.
;; While the the dev container is active it will be used as project root.
;; Thus all of the project-* commands will work with it.

;; vscode will use tools in the container for code intelligence while running.
;; When accessing a file with `tramp' process calls will be made on the remote.
;; This means that `eglot' "just works" provided you have this: `(add-to-list 'tramp-remote-path 'tramp-own-remote-path)'.
;; `apheleia' will also "just work" provided that you this: `(apheleia-remote-algorithm 'remote)'. 

;; vscode will open a terminal in the dev container itself if one is running.
;; This is approximated with `eat' and a call to `docker exec' here.

;; vscode allows you run run tasks, and in a dev container those tasks will use remote tools.
;; The equivalent in Emacs is the `compile' command.
;; This is advised such that it gets wrapped in a `devcontainer exec' so it can invoke the remote tools.

;; In buffer completions with copilot will "just work" provided `node' is installed in the container.
                                       
;;; Code:

(eval-when-compile
  (require 'project)
  (require 'compile))

;; custom

(defgroup devcontainer nil
  "Support for dev containers in Emacs."
  :group 'tools)

(defcustom devcontainer-cli-executable "devcontainer"
  "Path to the devcontainer CLI executable."
  :type 'string
  :group 'devcontainer)

(defcustom devcontainer-start-hook nil
  "Hook run after a dev container is successfully started."
  :type 'hook
  :group 'devcontainer)

(defcustom devcontainer-stop-hook nil
  "Hook run after a dev container is successfully stopped."
  :type 'hook
  :group 'devcontainer)

(defcustom devcontainer-compile-ignored-commands '("docker" "devcontainer")
  "List of command prefixes that should not be wrapped in devcontainer exec."
  :type '(repeat string)
  :group 'devcontainer)

;; private variables

(defvar devcontainer--current-container-id nil
  "The ID of the currently running dev container.")

(defvar devcontainer--current-workspace nil
  "The workspace directory of the currently running dev container.")

(defconst devcontainer--terminal-buffer-name "*dev container terminal*"
  "Name of the buffer used to display the terminal.")

;; public API

;;;###autoload
(defun devcontainer-running-p ()
  "Check if there's already a running dev container."
  devcontainer--current-container-id)

;;;###autoload
(defun devcontainer-start ()
  "Start a dev container.

The container will be built if needed.
Only one container can be running at a time."
  (interactive)
  (when (devcontainer-running-p)
    (user-error "A dev container is already running. Stop it first with `devcontainer-stop'"))

  (let ((dir (read-directory-name "Select workspace directory: " default-directory)))
    (devcontainer--start-internal dir)))

;;;###autoload
(defun devcontainer-stop ()
  "Stop the currently running dev container."
  (interactive)
  (unless devcontainer--current-container-id
    (user-error "No dev container is currently running"))
  
  (when (y-or-n-p (format "Stop dev container %s? " devcontainer--current-workspace))
    (message "Stopping dev container...")

    (devcontainer--kill-container-buffers)
    (when-let* ((buf (get-buffer "*eat*")))
      (when-let* ((proc (get-buffer-process buf)))
        (delete-process proc))
      (kill-buffer buf))

    (let ((default-directory (expand-file-name "~/"))
          (exit-code (call-process "docker" nil nil nil
                                   "stop" devcontainer--current-container-id)))
      (if (zerop exit-code)
          (progn
            (run-hooks 'devcontainer-stop-hook)
            (devcontainer--disable-project-integration)
            (devcontainer--disable-compile-advice)
            (setq devcontainer--current-container-id nil
                  devcontainer--current-workspace nil)
            (message "Dev container stopped successfully"))
        (error "Failed to stop dev container")))))

;;;###autoload
(defun devcontainer-rebuild ()
  "Rebuild the current dev container.

The existing container will be completely removed."
  (interactive)
  (unless (devcontainer-running-p)
    (user-error "No dev container is currently running. Use `devcontainer-start' to start one"))

  (let ((workspace devcontainer--current-workspace)
        (container-id devcontainer--current-container-id))
    (when (y-or-n-p (format "Rebuild dev container for %s? This will remove all cached layers. " workspace))
      (devcontainer-stop)
      (devcontainer--cleanup-container container-id)
      (message "Rebuilding dev container...")
      (devcontainer--build workspace)
      (devcontainer--start-internal workspace)
      (message "Dev container rebuilt successfully"))))

;;;###autoload
(defun devcontainer-tramp-path (&optional path)
  "Return a TRAMP path for accessing files in the current dev container.

PATH is an optional file path within the container. If not provided,
returns the path to the container root.

Returns nil if no dev container is currently running."
  (when devcontainer--current-container-id
    (let ((container-path (or path "/")))
      (format "/docker:%s:%s" 
              devcontainer--current-container-id 
              container-path))))

;;;###autoload
(defun devcontainer-workspace-tramp-path ()
  "Return a TRAMP path for accessing the workspace folder in the current dev container."
  (when devcontainer--current-workspace
    (let ((workspace-name (file-name-nondirectory 
                          (directory-file-name devcontainer--current-workspace))))
      (devcontainer-tramp-path (format "/workspaces/%s" workspace-name)))))

;;;###autoload
(defun devcontainer-terminal ()
  "Open a terminal in the dev container workspace directory."
  (interactive)
  (unless (devcontainer-running-p)
    (user-error "No dev container is currently running. Start one with `devcontainer-start'"))
  (let* ((default-directory (expand-file-name "~/"))
         (detected-shell (devcontainer--detect-default-shell))
         (workspace-name (file-name-nondirectory 
                         (directory-file-name devcontainer--current-workspace)))
         (workspace-path (format "/workspaces/%s" workspace-name))
         (cmd (format "docker exec -it -w %s %s %s" 
                      workspace-path
                      devcontainer--current-container-id 
                      detected-shell)))
    (eat cmd)))

;; helpers

(defun devcontainer--extract-container-id (output-buffer)
  "Get container ID from the dev container start output in OUTPUT-BUFFER."
  (with-current-buffer output-buffer
    (goto-char (point-min))
    (let* ((json-output (condition-case nil
                            (json-parse-buffer :object-type 'alist)
                          (error (error "Failed to parse dev container response as JSON: %s"
                                       (buffer-string)))))
           (outcome (alist-get 'outcome json-output))
           (container-id (alist-get 'containerId json-output)))
      (unless (string= outcome "success")
        (error "Dev container start was not successful. Outcome: %s" outcome))
      (unless container-id
        (error "No container ID found in dev container response"))
      
      container-id)))

(defun devcontainer--cli-available-p ()
  "Check if the dev container CLI tool is available in PATH."
  (executable-find devcontainer-cli-executable))

(defun devcontainer--kill-container-buffers ()
  "Kill all buffers visiting files in the current dev container."
  (when devcontainer--current-container-id
    (let ((container-prefix (format "/docker:%s:" devcontainer--current-container-id))
          (killed-count 0))
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (and buffer-file-name
                     (string-prefix-p container-prefix buffer-file-name))
            (kill-buffer buffer)
            (setq killed-count (1+ killed-count)))))
      (when (> killed-count 0)
        (message "Killed %d buffer(s) from dev container" killed-count)))))

(defun devcontainer--detect-default-shell ()
  "Detect the default shell in the current dev container."
  (let* ((output (shell-command-to-string 
                  (format "docker exec %s sh -c 'echo $SHELL'" 
                          devcontainer--current-container-id)))
         (shell (string-trim output)))
    (if (and shell (not (string-empty-p shell)))
        shell
      "/bin/bash")))

(defun devcontainer--start-internal (workspace-dir)
  "Start a dev container for WORKSPACE-DIR without prompting."
  (unless (devcontainer--cli-available-p)
    (user-error "The dev container CLI was not found. Please install it and ensure it's in your PATH"))

  (let* ((output-buffer (generate-new-buffer "*devcontainer-start*"))
         (expanded-dir (expand-file-name workspace-dir))
         (devcontainer-config (file-name-concat expanded-dir ".devcontainer" "devcontainer.json")))

    (unwind-protect
        (progn
          (unless (file-exists-p devcontainer-config)
            (user-error "There was no dev container config in the selected directory."))

          (let ((exit-code (call-process devcontainer-cli-executable nil (list output-buffer nil) nil 
                                         "up" "--workspace-folder" expanded-dir)))
            (unless (zerop exit-code)
              (error "Failed to start dev container. Exit code: %d\nOutput:\n%s"
                     exit-code
                     (with-current-buffer output-buffer (buffer-string)))))

          (setq devcontainer--current-container-id (devcontainer--extract-container-id output-buffer))
          (setq devcontainer--current-workspace expanded-dir)
          (run-hooks 'devcontainer-start-hook)
          (devcontainer--enable-compile-advice)
          (devcontainer--enable-project-integration))
      (kill-buffer output-buffer))))

(defun devcontainer--build (workspace-dir)
  "Build the dev container for WORKSPACE-DIR."
  (let* ((output-buffer (generate-new-buffer "*devcontainer-build*"))
         (expanded-dir (expand-file-name workspace-dir)))
    (unwind-protect
        (let ((exit-code (call-process devcontainer-cli-executable nil (list output-buffer nil) nil 
                                       "build" "--workspace-folder" expanded-dir)))
          (unless (zerop exit-code)
            (error "Failed to build dev container. Exit code: %d\nOutput:\n%s"
                   exit-code
                   (with-current-buffer output-buffer (buffer-string)))))
      (kill-buffer output-buffer))))

(defun devcontainer--cleanup-container (container-id)
  "Clean up the specified CONTAINER-ID to ensure a fresh rebuild."
  (let ((default-directory (expand-file-name "~/")))
    (message "Cleaning up container %s..." container-id)
    (when container-id
      (call-process "docker" nil nil nil "rm" "-f" container-id))))

(defun devcontainer--command-ignored-p (command)
  "Check if COMMAND should not wrapped in devcontainer exec."
  (cl-some (lambda (prefix)
             (string-prefix-p prefix (string-trim command)))
           devcontainer-compile-ignored-commands))

(defun devcontainer--compile-advice (orig-fun command &optional comint)
  "Advice for `compile' to run commands in the devcontainer when active.
ORIG-FUN is the original compile function.
COMMAND is the compile command to run.
COMINT indicates whether to use comint mode."
  (if (and (devcontainer-running-p)
           (not (devcontainer--command-ignored-p command)))
      (let ((wrapped-command (format "%s exec --workspace-folder %s %s"
                                   devcontainer-cli-executable
                                   devcontainer--current-workspace
                                   command)))
        (funcall orig-fun wrapped-command comint))
    (funcall orig-fun command comint)))

(defun devcontainer--enable-compile-advice ()
  "Enable compile command advice for devcontainer execution."
  (advice-add 'compile :around #'devcontainer--compile-advice))

(defun devcontainer--disable-compile-advice ()
  "Disable compile command advice for devcontainer execution."
  (advice-remove 'compile #'devcontainer--compile-advice))

;; project.el

(cl-defmethod project-root ((project (head devcontainer)))
  "Return the root directory for a devcontainer project."
  (nth 1 project))

(cl-defmethod project-name ((project (head devcontainer)))
  "Return a human-readable name for the devcontainer project."
  (file-name-nondirectory 
   (directory-file-name devcontainer--current-workspace)))

(defun devcontainer--project-try (dir)
  "Resolves the path of a devcontainer project (if any)."
  (when (devcontainer-running-p)
    (list 'devcontainer (devcontainer-workspace-tramp-path) 'container)))

(defun devcontainer--exclude-from-project-list (project)
  "Return non-nil if PROJECT is a devcontainer project.
This prevents devcontainer projects from being saved to the project list."
  (eq (car-safe project) 'devcontainer))

(defun devcontainer--enable-project-integration ()
  "Enable project.el integration for devcontainers.
This adds devcontainer project detection to `project-find-functions'."
  (interactive)
  (add-to-list 'project-list-exclude #'devcontainer--exclude-from-project-list)
  (add-hook 'project-find-functions #'devcontainer--project-try))

(defun devcontainer--disable-project-integration ()
  "Disable project.el integration for devcontainers."
  (interactive)
  (remove-hook 'project-find-functions #'devcontainer--project-try))

(provide 'devcontainer)

;;; devcontainer.el ends here
