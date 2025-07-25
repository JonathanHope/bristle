;;; bristle-mode-line.el --- Custom mode line.  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jonathan Hope

;; Author: Jonathan Hope <jonathan.douglas.hope@gmail.com>
;; Version: 1.0
;; Package-Requires ((emacs "29.1") (gestalt "1.0.0") (popper "0.4.8"))
;; Keywords: mode-line

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is the mode line for Bristle Emacs.
;; It shows many indicators:
;; - whether the current window is selected or not
;; - which buffer is being displayed
;; - whether the buffer is dirty
;; - whether the window is a popper
;; - the current project
;; - the current major mode
;; - the current facet
;; - an ASCII scroll bar
;; - the column and row coordinates

(eval-when-compile
  (require 'project))

(declare-function gestalt-active-lighter "gestalt")
(declare-function gestalt-override-facet-p "gestalt")
(declare-function gestalt-transient-facet-p "gestalt")
(declare-function popper-popup-p "popper")

(defvar popper-mode)
(defvar popper-popup-status)

;;; Code:

;; group

(defgroup bristle-mode-line nil
  "Custome modeline theme."
  :group 'mode-line)

;; custom

(defcustom bristle-mode-line-simplified-buffers nil
  "List of buffer names that should use a simplified mode line."
  :group 'bristle-mode-line
  :type '(repeat string))

;; faces

(defface bristle-mode-line
  '()
  "Face used for the inactive mode line."
  :group 'bristle-mode-line)

(defface bristle-mode-line-inactive
  '()
  "Face used for the inactive mode line."
  :group 'bristle-mode-line)

(defface bristle-mode-line-file-status
  '()
  "Face used for the file status in the mode line."
  :group 'bristle-mode-line)

(defface bristle-mode-line-buffer-name
  '()
  "Face used for the buffer name in the mode line."
  :group 'bristle-mode-line)

(defface bristle-mode-line-project
  '()
  "Face used for the project name mode line."
  :group 'bristle-mode-line)

(defface bristle-mode-line-mode
  '()
  "Face used for the major mode in the mode line."
  :group 'bristle-mode-line)

(defface bristle-mode-line-lighter
  '()
  "Face used for the gestalt lighter in the mode line."
  :group 'bristle-mode-line)

(defface bristle-mode-line-row-column
  '()
  "Face used for the row/column in the mode line."
  :group 'bristle-mode-line)

(defface bristle-mode-line-scroll-bar
  '()
  "Face used for the scroll bar in the mode line."
  :group 'bristle-mode-line)

(defface bristle-mode-line-popper
  '()
  "Face used for the popper indicator in the mode line."
  :group 'bristle-mode-line)

(defface bristle-mode-transient-override
  '()
  "Face used for the transient/override facet indicator in the mode line."
  :group 'bristle-mode-line)

;; strings

(defcustom bristle-mode-line-progress-0 "❰          ❱"
  "What characters to show for a progress bar of zero percent."
  :group 'bristle-mode-line
  :type 'string)

(defcustom bristle-mode-line-progress-10 "❰━         ❱"
  "What characters to show for a progress bar of 10 percent."
  :group 'bristle-mode-line
  :type 'string)

(defcustom bristle-mode-line-progress-20 "❰━━        ❱"
  "What characters to show for a progress bar of 20 percent."
  :group 'bristle-mode-line
  :type 'string)

(defcustom bristle-mode-line-progress-30 "❰━━━       ❱"
  "What characters to show for a progress bar of 30 percent."
  :group 'bristle-mode-line
  :type 'string)

(defcustom bristle-mode-line-progress-40 "❰━━━━      ❱"
  "What characters to show for a progress bar of 40 percent."
  :group 'bristle-mode-line
  :type 'string)

(defcustom bristle-mode-line-progress-50 "❰━━━━━     ❱"
  "What characters to show for a progress bar of 50 percent."
  :group 'bristle-mode-line
  :type 'string)

(defcustom bristle-mode-line-progress-60 "❰━━━━━━    ❱"
  "What characters to show for a progress bar of 60 percent."
  :group 'bristle-mode-line
  :type 'string)

(defcustom bristle-mode-line-progress-70 "❰━━━━━━━   ❱"
  "What characters to show for a progress bar of 70 percent."
  :group 'bristle-mode-line
  :type 'string)

(defcustom bristle-mode-line-progress-80 "❰━━━━━━━━  ❱"
  "What characters to show for a progress bar of 80 percent."
  :group 'bristle-mode-line
  :type 'string)

(defcustom bristle-mode-line-progress-90 "❰━━━━━━━━━ ❱"
  "What characters to show for a progress bar of 90 percent."
  :group 'bristle-mode-line
  :type 'string)

(defcustom bristle-mode-line-progress-100 "❰━━━━━━━━━━❱"
  "What characters to show for a progress bar of 100 percent."
  :group 'bristle-mode-line
  :type 'string)

(defcustom bristle-mode-line-dirty-file "●"
  "What characters to show for a dirty file."
  :group 'bristle-mode-line
  :type 'string)

(defcustom bristle-mode-line-missing-project "N/A"
  "What characters to show for a missing project."
  :group 'bristle-mode-line
  :type 'string)

(defcustom bristle-mode-line-bullet "⬢"
  "What characters to use for a bullet in the mode line formatting."
  :group 'bristle-mode-line
  :type 'string)

(defcustom bristle-mode-line-dash "━"
  "What characters to use for a dash in the mode line formatting."
  :group 'bristle-mode-line
  :type 'string)

(defcustom bristle-mode-line-row-column-divider ","
  "What characters to use for a dividing the row and column."
  :group 'bristle-mode-line
  :type 'string)

(defcustom bristle-mode-line-popper-indicator " ⬍"
  "What character to show when the current window is a popper."
  :group 'bristle-mode-line
  :type 'string)

;; track which modeline is active

(defvar bristle-mode-line--selected-window nil
  "The currently selected window.")

(defun bristle-mode-line--update-selected-window (&optional _)
  "Update the selected window tracker."
  (setq bristle-mode-line--selected-window (selected-window)))

(defun bristle-mode-line--active-p ()
  "Return t if the current window is the selected window."
  (eq bristle-mode-line--selected-window (selected-window)))

(defun bristle-mode-line--full-p ()
  "Return t if the current buffer should use the full modeline."
  (not (member (buffer-name) bristle-mode-line-simplified-buffers)))

;; the parts of the mode line

(defun bristle-mode-line--start()
  "The start section of the mode line."
  (propertize bristle-mode-line-dash
              'face (if (bristle-mode-line--active-p)
                        'bristle-mode-line
                      'bristle-mode-line-inactive)))

(defun bristle-mode-line--end ()
  "The end section of the mode line."
  (propertize (make-string 400 (string-to-char bristle-mode-line-dash))
              'face (if (bristle-mode-line--active-p)
                        'bristle-mode-line
                      'bristle-mode-line-inactive)))

(defun bristle-mode-line--section-start ()
  "Start of a section in the mode line."
  (propertize (concat bristle-mode-line-bullet " ")
              'face (if (bristle-mode-line--active-p)
                        'bristle-mode-line
                      'bristle-mode-line-inactive)))

(defun bristle-mode-line--section-end ()
  "End of a section in the mode line."
  (propertize (concat " " bristle-mode-line-bullet bristle-mode-line-dash bristle-mode-line-dash)
              'face (if (bristle-mode-line--active-p)
                        'bristle-mode-line
                      'bristle-mode-line-inactive)))

(defun bristle-mode-line--active-section-start ()
  "Start of a section in the mode line; only shown when active."
  (when (and (bristle-mode-line--active-p) (bristle-mode-line--full-p))
    (bristle-mode-line--section-start)))

(defun bristle-mode-line--active-section-end ()
  "End of a section in the mode line; only shown when active."
  (when (and (bristle-mode-line--active-p) (bristle-mode-line--full-p))
    (bristle-mode-line--section-end)))

(defun bristle-mode-line--buffer-status ()
  "Whether the buffer has been modified since the last save."
  (when (bristle-mode-line--full-p)
    (propertize (if (buffer-modified-p) (concat bristle-mode-line-dirty-file " ") "")
                'face (if (bristle-mode-line--active-p)
                          'bristle-mode-line-file-status
                        'bristle-mode-line-inactive))))

(defun bristle-mode-line--buffer ()
  "The name of the active buffer."
  (propertize "%b"
              'face (if (bristle-mode-line--active-p)
                        'bristle-mode-line-buffer-name
                      'bristle-mode-line-inactive)))

(defun bristle-mode-line--popper-indicator ()
  "Show popper indicator if current buffer is a popper."
  (when (and (bristle-mode-line--active-p)
             (bristle-mode-line--popper-p))
    (propertize bristle-mode-line-popper-indicator
                'face 'bristle-mode-line-popper)))

(defun bristle-mode-line--popper-p ()
  "Return t if the current buffer is a popper."
  (and (bound-and-true-p popper-mode)
       (or 
        (memq popper-popup-status '(popup user-popup))
        (and (not (eq popper-popup-status 'raised))
             (fboundp 'popper-popup-p)
             (popper-popup-p (current-buffer))))))

(defun bristle-mode-line--project ()
  "The currently active project if any."
  (let ((project (project-current))
        (active (bristle-mode-line--active-p))
        (full (bristle-mode-line--full-p)))
    (cond
     ((not full) "")
     ((not active) "") 
     ((not project) (propertize bristle-mode-line-missing-project
                                'face 'bristle-mode-line-project))
     (t (propertize (project-name project)
                    'face 'bristle-mode-line-project)))))

(defun bristle-mode-line--major-mode ()
  "The current major mode."
  (when (and (bristle-mode-line--active-p) (bristle-mode-line--full-p))
    (concat
     (propertize (format-mode-line mode-name) 'face 'bristle-mode-line-mode)
     (propertize (gestalt-active-lighter) 'face 'bristle-mode-line-lighter)
     (propertize (if (gestalt-override-facet-p) " ∀" "") 'face 'bristle-mode-transient-override)
     (propertize (if (gestalt-transient-facet-p) " ∈" "") 'face 'bristle-mode-transient-override))))

(defun bristle-mode-line--scroll-bar ()
  "Build a text-based horizontal scroll bar."
  (when (and (bristle-mode-line--active-p) (bristle-mode-line--full-p))
    (let* ((current-line (line-number-at-pos))
           (total-lines (line-number-at-pos (point-max)))
           (percent (if (= total-lines 1) 0
                      (* 100 (/ (float (1- current-line)) (1- total-lines)))))
           (index (min 10 (floor (/ percent 10))))
           (bars (vector bristle-mode-line-progress-0
                         bristle-mode-line-progress-10  
                         bristle-mode-line-progress-20
                         bristle-mode-line-progress-30
                         bristle-mode-line-progress-40
                         bristle-mode-line-progress-50
                         bristle-mode-line-progress-60
                         bristle-mode-line-progress-70
                         bristle-mode-line-progress-80
                         bristle-mode-line-progress-90
                         bristle-mode-line-progress-100)))
      (propertize (aref bars index)
                  'face 'bristle-mode-line-scroll-bar))))

(defun bristle-mode-line--row-column ()
  "The current row and column numbers."
  (when (and (bristle-mode-line--active-p) (bristle-mode-line--full-p))
    (propertize (concat "%01l" bristle-mode-line-row-column-divider "%01c")
                'face 'bristle-mode-line-row-column)))
;; public API

;;;###autoload
(defun bristle-mode-line-init()
  (bristle-mode-line--update-selected-window)
  (add-hook 'window-selection-change-functions #'bristle-mode-line--update-selected-window)
  (setq-default mode-line-format
                (list
                 '(:eval (bristle-mode-line--start))
                 
                 '(:eval (bristle-mode-line--section-start))
                 '(:eval (bristle-mode-line--buffer-status))
                 '(:eval (bristle-mode-line--buffer))
                 '(:eval (bristle-mode-line--popper-indicator))
                 '(:eval (bristle-mode-line--section-end))

                 '(:eval (bristle-mode-line--active-section-start))
                 '(:eval (bristle-mode-line--project))
                 '(:eval (bristle-mode-line--active-section-end))

                 '(:eval (bristle-mode-line--active-section-start))
                 '(:eval (bristle-mode-line--major-mode))
                 '(:eval (bristle-mode-line--active-section-end))

                 '(:eval (bristle-mode-line--active-section-start))
                 '(:eval (bristle-mode-line--scroll-bar))
                 '(:eval (bristle-mode-line--active-section-end))
                 
                 '(:eval (bristle-mode-line--active-section-start))
                 '(:eval (bristle-mode-line--row-column))
                 '(:eval (bristle-mode-line--active-section-end))
                 
                 '(:eval (bristle-mode-line--end)))))

(provide 'bristle-mode-line)

;; bristle-mode-line.el ends here
