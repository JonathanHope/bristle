;;; ob-go.el --- Org Babel support for Go

;; Copyright (C) 2025

;; Author: jhope@theflatfield.net
;; Keywords: go, babel
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Org-Babel support for evaluating Go source code.

;;; Code:

(require 'ob)
(require 'ob-eval)

(defcustom org-babel-go-command "go"
  "Command to use for executing Go code."
  :group 'org-babel
  :type 'string)

(defun org-babel-execute:go (body params)
  "Execute a block of Go code with org-babel."
  (let* ((processed-params (org-babel-process-params params))
         (flags (cdr (assq :flags processed-params)))
         (args (cdr (assq :args processed-params)))
         (imports (cdr (assq :imports processed-params)))
         (coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8)
         (tmp-file (expand-file-name 
                   (format "tmp_%s.go" (format-time-string "%Y%m%d_%H%M%S"))
                   default-directory))
         (cmd (format "%s run %s %s %s"
                     org-babel-go-command
                     (or flags "")
                     tmp-file
                     (or args "")))
         full-body)
    (setq full-body (org-babel-go-ensure-main-function body))
    (cond
     (imports
      (setq full-body (org-babel-go-add-imports full-body imports)))
     ((and (not (string-match "^package " body))
           (not (string-match "func main()" body)))
      (setq full-body (org-babel-go-add-imports full-body '("fmt")))))
    (unwind-protect
        (progn
          (with-temp-file tmp-file
            (insert full-body))
          (org-babel-eval cmd ""))
      (when (file-exists-p tmp-file)
        (delete-file tmp-file)))))

(defun org-babel-go-ensure-main-function (body)
  "Ensure the Go code has a proper main function and package declaration."
  (cond
   ((string-match "^package " body) body)
   ((string-match "func main()" body)
    (concat "package main\n\n" body))
   (t
    (concat "package main\n\n"
            "func main() {\n"
            body
            "\n}"))))

(defun org-babel-go-as-list (val)
  "Convert VAL to a list if it isn't already one."
  (if (listp val) val (list val)))

(defun org-babel-go-add-imports (body imports)
  "Add import statements to Go code after the package declaration."
  (let* ((package-end (string-match "\n" body))
         (package-line (substring body 0 package-end))
         (rest-body (substring body package-end)))
    (concat package-line
            "\n\n"
            (org-babel-go-format-imports imports)
            rest-body)))

(defun org-babel-go-format-imports (imports)
  "Format imports as a Go import block."
  (let ((import-list (org-babel-go-as-list imports)))
    (if (= (length import-list) 1)
        (format "import %S\n" (car import-list))
      (concat "import (\n"
              (mapconcat (lambda (pkg) (format "\t%S" pkg))
                        import-list
                        "\n")
              "\n)\n"))))

(defun org-babel-prep-session:go (session params)
  "Return an error because Go does not support sessions."
  (error "Go does not support sessions"))

(provide 'ob-go)

;;; ob-go.el ends here
