;;; ob-typescript.el --- Babel Functions for TypeScript -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: jhope@theflatfield.net
;; Keywords: babel, typescript
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Org-Babel support for evaluating TypeScript.
;; This is done with `npx tsx'

;;; Code:

(require 'ob)
(require 'ob-eval)

(defun org-babel-execute:typescript (body params)
  "Execute a BODY of TypeScript code with  PARAMS."
  (let* ((tmp-file (org-babel-temp-file "ob-typescript-" ".ts"))
         (cmd (format "npx tsx %s" tmp-file)))
    
    (unwind-protect
        (progn
          (with-temp-file tmp-file
            (insert body))
          (org-babel-eval cmd ""))
      (when (file-exists-p tmp-file)
        (delete-file tmp-file)))))

(defun ob-typescript-check-npx ()
  "Check if npx is available in PATH."
  (interactive)
  (if (executable-find "npx")
      (message "npx found: %s" (executable-find "npx"))
    (error "npx not found in PATH. Install Node.js to get npx")))

(defun org-babel-prep-session:typescript (session params)
  "Return an error because TypeScript does not support sessions."
  (error "TypeScript does not support sessions"))

(provide 'ob-typescript)

;;; ob-typescript.el ends here
