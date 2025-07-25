;;; pine.el --- A tiny dired sidebar 🌲 -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Jonathan Hope
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: dired, sidebar

;;; Commentary:
;; Pine is a minimal dired sidebar.
;; It just shows dired in a statically sized window on the left.

;;; Code:

(require 'dired)

;;; custom

(defgroup pine nil
  "Pine dired sidebar."
  :group 'dired)

(defcustom pine-width 35
  "Width of the Pine sidebar window."
  :type 'integer
  :group 'pine)

(defcustom pine-use-project-root t
  "Whether to use project root as sidebar directory.
Falls back to `default-directory' if no project found."
  :type 'boolean
  :group 'pine)

(defcustom pine-root-directory-function nil
  "Function to override the root directory for Pine sidebar."
  :type '(choice (const :tag "Use default logic" nil)
                 (function :tag "Override function"))
  :group 'pine)


;;; internal variables

(defvar pine--buffer-name "*Pine*"
  "Name of the Pine sidebar buffer.")

(defvar-local pine--root-directory nil
  "The root directory for this Pine buffer.")

;;; keymap

(defvar pine-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map dired-mode-map)
    (define-key map (kbd "RET") #'pine-find-file)
    (define-key map (kbd "o") #'pine-find-file)
    map)
  "Keymap for Pine mode.")

;;; major mode

(define-derived-mode pine-mode dired-mode "Pine"
  "Major mode for Pine dired sidebar.
\\{pine-mode-map}"
  ;; this is one of the needed hacks
  ;; from dired.el
  ;; Must first make alist buffer local and set it to nil because
  ;; dired-build-subdir-alist will call dired-clear-alist first
  (setq-local dired-subdir-alist nil)
  (dired-build-subdir-alist)

  ;; another hack
  ;; we store the root directory so for use in revert
  (unless pine--root-directory
    (setq-local pine--root-directory (expand-file-name dired-directory)))
  
  (pine--apply-settings)
  (setq-local revert-buffer-function #'pine--revert)
  (unless (string= (buffer-name) pine--buffer-name)
    (rename-buffer pine--buffer-name)))

;;; internal functions

(defun pine--apply-settings ()
  "Apply Pine-specific dired settings to current buffer."
  (setq-local window-size-fixed 'width)
  (setq-local truncate-lines t)
  (dired-hide-details-mode 1)
  ;; another hack
  ;; this prevents the sidebar buffer from being hijacked by dired
  (setq-local dired-directory nil))

(defun pine--get-root-directory ()
  "Get the root directory for Pine."
  (let ((directory
         (if (and pine-root-directory-function
                  (functionp pine-root-directory-function))
             (condition-case nil
                 (funcall pine-root-directory-function)
               (error nil))
           (when (and pine-use-project-root
                      (fboundp 'project-current))
             (when-let ((project (project-current)))
               (if (fboundp 'project-root)
                   (project-root project)
                 (cdr project)))))))
    (if (and directory
             (stringp directory)
             (file-directory-p directory))
        (expand-file-name directory)
      default-directory)))

(defun pine--create-buffer (directory)
  "Create a dired buffer for DIRECTORY."
  ;; this prevents the sidebar from reusing an existing buffer
  (let ((buffer (cl-letf (((symbol-function 'dired-find-buffer-nocreate)
                           (lambda (&rest _args) nil)))
                  (dired-noselect directory))))
    (with-current-buffer buffer
      (pine-mode)
      buffer)))

(defun pine--revert (&optional _ignore-auto _noconfirm)
  "Revert the Pine buffer."
  (let ((dired-directory pine--root-directory))
    (dired-revert)
    (pine--apply-settings)))

(defun pine--show (buffer)
  "Show BUFFER in a side window."
  (let ((window (display-buffer-in-side-window
                 buffer
                 `((side . left)
                   (slot . -1)
                   (window-width . ,pine-width)))))
    (set-window-dedicated-p window t)
    (with-selected-window window
      (setq window-size-fixed 'width))
    window))

(defun pine--buffer ()
  "Get the Pine buffer if it exists."
  (get-buffer pine--buffer-name))

(defun pine--window ()
  "Get the Pine window if it exists."
  (when-let ((buffer (pine--buffer)))
    (get-buffer-window buffer)))

(defun pine--showing-p ()
  "Return t if Pine is currently showing."
  (and (pine--buffer)
       (pine--window)))

(defmacro pine-with-no-dedication (&rest body)
  "Execute BODY with window dedication temporarily disabled."
  (declare (debug (&rest form)))
  `(progn
     (let ((window (get-buffer-window (current-buffer))))
       (set-window-dedicated-p window nil)
       ,@body
       (set-window-dedicated-p window t))))

;;; public functions

;;;###autoload
(defun pine-toggle ()
  "Toggle Pine dired sidebar."
  (interactive)
  (if (pine--showing-p)
      (pine-hide)
    (pine-show)))

;;;###autoload
(defun pine-show ()
  "Show Pine dired sidebar."
  (interactive)
  (let* ((dir (pine--get-root-directory))
         (existing-buffer (pine--buffer))
         (buffer (if (and existing-buffer
                         (with-current-buffer existing-buffer
                           (string= (expand-file-name pine--root-directory)
                                    (expand-file-name dir))))
                     existing-buffer
                   (progn
                     (when existing-buffer
                       (kill-buffer existing-buffer))
                     (pine--create-buffer dir)))))

    (pine--show buffer)))

;;;###autoload
(defun pine-hide ()
  "Hide Pine dired sidebar."
  (interactive)
  (when-let ((window (pine--window)))
    (delete-window window)))

;;;###autoload
(defun pine-find-file ()
  "Open file at point in a non-sidebar window."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (pine-with-no-dedication
         ;; this prevents the sidebar from reusing an existing buffer
         (cl-letf (((symbol-function 'dired-find-buffer-nocreate)
                    (lambda (&rest _args) nil)))
           (find-alternate-file file)
           (pine-mode)))
      (progn
        (select-window (get-largest-window))
        (find-file file)))))

;;;###autoload
(defun pine-selected-p ()
  "Return t if Pine sidebar is currently selected."
  (when-let ((pine-window (pine--window)))
    (eq (selected-window) pine-window)))

;;;###autoload
(defun pine-change-directory (directory)
  "Change the directory shown in Pine sidebar to DIRECTORY.
If Pine is not currently showing, this function does nothing."
  (interactive "DDirectory: ")
  (when (pine--showing-p)
    (let ((pine-buffer (pine--buffer)))
      (with-current-buffer pine-buffer
        (setq-local pine--root-directory (expand-file-name directory))
        (pine-with-no-dedication
         (revert-buffer))))))

(provide 'pine)
;;; pine.el ends here
