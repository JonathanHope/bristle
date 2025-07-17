;;; legend.el --- Display a legend for a keymap -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Jonathan Hope
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: help, keybindings

;;; Commentary:

;; Legend shows the bindings in a given keymap.
;; They are spaced as concisely as possible.

;; The core idea of this and the available width calc come from from which-key.el - by Justin Burkett

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; custom

(defgroup legend nil
  "Display a keymap's bindings in a legend."
  :group 'extensions)

(defcustom legend-separator " → "
  "What to separate the keys and descriptions with."
  :type 'string
  :group 'legend)

(defcustom legend-column-separator " | "
  "String to separate columns in legend display."
  :type 'string
  :group 'legend)

(defcustom legend-max-description-length nil
  "Maximum length for descriptions in legend display."
  :type '(choice (const :tag "No limit" nil)
                 (integer :tag "Maximum length"))
  :group 'legend)

(defcustom legend-ellipsis-string "…"
  "String to use for ellipsis when truncating descriptions."
  :type 'string
  :group 'legend)

(defcustom legend-filtered-commands '(ignore)
  "List of commands to filter out from legend display."
  :type '(repeat symbol)
  :group 'legend)

(defcustom legend-cache-max-size 500
  "Maximum number of keymap+width combinations to cache.
Set to 0 to disable caching, or nil for unlimited cache size."
  :type '(choice (const :tag "No caching" 0)
                 (const :tag "Unlimited" nil)
                 (integer :tag "Maximum entries"))
  :group 'legend)

(defface legend-key
  '((t :inherit font-lock-constant))
  "Face for keys in legend display."
  :group 'legend)

(defface legend-separator
  '((t :inherit shadow))
  "Face for separator in legend display."
  :group 'legend)

(defface legend-column-separator
  '((t :inherit shadow))
  "Face for column separator in legend display."
  :group 'legend)

(defface legend-description
  '((t :inherit default))
  "Face for descriptions in legend display."
  :group 'legend)

;; private variables

(defconst legend--buffer-name "*Legend*"
  "Name of the buffer used to display the legend.")

(defvar legend--last-keymap nil
  "Last keymap displayed in legend buffer.")

(defvar legend--last-width nil
  "Last window width used for legend buffer.")

(defvar legend--cache (make-hash-table :test 'equal)
  "Cache for rendered legend content.
Keys are (keymap-hash . width) pairs, values are rendered strings.")

(defvar legend--cache-access-order nil
  "List of cache keys in order of access (most recent first).
Used for LRU eviction when cache size limit is reached.")

;; public API

;;;###autoload
(defun legend-show (keymap)
  "Show the legend for KEYMAP.

Example usage:
(legend-show keymap)"
  (interactive)
  (let* ((buffer (get-buffer-create legend--buffer-name))
         (window (get-buffer-window buffer)))
    (unless window
      (setq window (display-buffer buffer)))
     (legend--update-buffer buffer window keymap)))

;;;###autoload
(defun legend-hide ()
  "Hide the legend.

Example usage:
(legend-hide)"
  (interactive)
  (let ((buffer (get-buffer legend--buffer-name)))
    (when buffer
      (let ((window (get-buffer-window buffer)))
        (when window (delete-window window)))
      (setq legend--last-keymap nil
            legend--last-width nil)
      (kill-buffer buffer))))

;;;###autoload
(defun legend-toggle (keymap)
  "Toggle the legend for KEYMAP.

Example usage:
(legend-toggle keymap)"
  (interactive)
  (let ((window (get-buffer-window legend--buffer-name)))
    (if window
        (legend-hide)
      (legend-show keymap))))

;;;###autoload
(defun legend-redraw (keymap)
  "Update the legend for KEYMAP.

Example usage:
(legend-redraw keymap)"
  (let* ((buffer (get-buffer-create legend--buffer-name))
         (window (get-buffer-window buffer)))
    (when (and buffer window)
      (legend--update-buffer buffer window keymap))))

;;;###autoload
(defun legend-clear-cache ()
  "Clear the legend rendering cache."
  (interactive)
  (clrhash legend--cache)
  (setq legend--cache-access-order nil)
  (message "Legend cache cleared"))

;; private functions - caching

(defun legend--cache-key (keymap width)
  "Generate a cache key for KEYMAP and WIDTH."
  (cons (legend--keymap-hash keymap) width))

(defun legend--keymap-hash (keymap)
  "Generate a hash for KEYMAP that changes when the keymap changes."
  (when (keymapp keymap)
    (let ((bindings nil))
      (map-keymap
       (lambda (key binding)
         (push (cons key binding) bindings))
       keymap)
      (sxhash-equal bindings))))

(defun legend--cache-get (keymap width)
  "Get cached content for KEYMAP and WIDTH, or nil if not cached."
  (when (and legend-cache-max-size (not (zerop legend-cache-max-size)))
    (let ((key (legend--cache-key keymap width)))
      (when (gethash key legend--cache)
        (legend--cache-update-access-order key)
        (gethash key legend--cache)))))

(defun legend--cache-put (keymap width content)
  "Cache CONTENT for KEYMAP and WIDTH."
  (when (and legend-cache-max-size (not (zerop legend-cache-max-size)))
    (let ((key (legend--cache-key keymap width)))
      (legend--cache-evict-if-needed)
      (puthash key content legend--cache)
      (legend--cache-update-access-order key))))

(defun legend--cache-update-access-order (key)
  "Update the access order for KEY (move to front)."
  (setq legend--cache-access-order
        (cons key (delq key legend--cache-access-order))))

(defun legend--cache-evict-if-needed ()
  "Evict least recently used cache entries if cache size limit is reached."
  (when (and legend-cache-max-size
             (> (hash-table-count legend--cache) legend-cache-max-size))
    (let ((keys-to-remove (nthcdr legend-cache-max-size legend--cache-access-order)))
      (dolist (key keys-to-remove)
        (remhash key legend--cache))
      (setq legend--cache-access-order
            (cl-subseq legend--cache-access-order 0 legend-cache-max-size)))))

;; private functions - window management

(defun legend--update-buffer (buffer window keymap)
  "Update the legend BUFFER contents with KEYMAP bindings."
  (with-current-buffer buffer
    (let ((current-width (window-width window)))
      (let ((cached-content (legend--cache-get keymap current-width)))
        (if cached-content
            (progn
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert cached-content)
                (goto-char (point-min))
                (setq buffer-read-only t)
                (setq legend--last-keymap keymap
                      legend--last-width current-width)))
          (unless (and (eq keymap legend--last-keymap)
                       (eq current-width legend--last-width))
            (let ((current-bindings (legend--extract-keymap-bindings keymap)))
              (let* ((inhibit-read-only t)
                     (content (legend--render-entries current-bindings current-width)))
                (erase-buffer)
                (insert content)
                (goto-char (point-min))
                (setq buffer-read-only t)
                (setq legend--last-keymap keymap
                      legend--last-width current-width)
                (legend--cache-put keymap current-width content)))))))))

;; private functions - binding extraction

(defun legend--extract-keymap-bindings (keymap)
  "Extract bindings from KEYMAP, and return list of (key . description) pairs sorted by description."
  (when (keymapp keymap)
    (let ((bindings nil))
      (map-keymap
       (lambda (key binding)
         (let ((formatted-key (legend--format-key key))
               (formatted-binding (legend--format-binding binding)))
           (unless (legend--skip-binding-p binding)
             (push (cons formatted-key formatted-binding) bindings))))
       keymap)
      (sort bindings (lambda (a b) 
                       (string< (downcase (cdr a)) 
                                (downcase (cdr b))))))))

(defun legend--skip-binding-p (binding)
  "Returns true if BINDING should not appear in a legend."
  (cond
   ((commandp binding) (memq binding legend-filtered-commands))
   ((consp binding) (memq (cdr binding) legend-filtered-commands))))

(defun legend--format-key (key)
  "Format KEY into a human-readable string."
  (key-description (if (vectorp key) key (vector key))))

(defun legend--format-binding (binding)
  "Format BINDING into a human-readable string."
  (let ((description
         (cond
          ((commandp binding) (symbol-name binding))
          ((keymapp binding) "prefix")
          ((consp binding) (car binding)))))
    (legend--truncate-description description)))

(defun legend--truncate-description (description)
  "Truncate DESCRIPTION to max length if needed, adding ellipsis."
  (if (and legend-max-description-length
           (> (length description) legend-max-description-length))
      (let ((ellipsis-length (length legend-ellipsis-string)))
        (concat
         (substring description 0 (- legend-max-description-length ellipsis-length))
         legend-ellipsis-string))
    description))

;; private functions - layout calculation

(defun legend--calculate-layout (bindings width)
  "Find the optimal number of columns to show BINDINGS in WIDTH solving for most columns.

Results are a plist where
 - :cols is the number of columns
 - :rows is the number of rows
 - :col-widths is the width each column inhabits
 - :key-widths is the width each key in a column inhabits"
  (if (= 0 (length bindings))
      '(:cols 0 :rows 0 :col-widths nil :key-widths nil)
    (legend--walk-layouts
     bindings
     (legend--available-text-width width)
     (legend--guess-layout bindings width)
     :unknown)))

(defun legend--available-text-width (window-width)
  "Get the usable width of a window for legend display."
  (let ((char-width (frame-char-width)))
    (- window-width
       (/ (frame-fringe-width) char-width)
       (/ (frame-scroll-bar-width) char-width)
       1))) ;; fudge factor

(defun legend--guess-layout (bindings width)
  "Estimate the optimal layout to show FORMATTED in WIDTH.

This is done by dividing the available width by the average width of an entry (+ padding).
The thought process here is that most entries will be of pretty similar length.
On the average this calculation should get us close to the optimal layout."
  (let* ((widths (mapcar 'legend--calculate-width bindings))
         (widths-sum (apply '+ widths))
         (average-width (ceiling (/ widths-sum (float (length bindings)))))
         (separator-width (length legend-column-separator))
         (padded-average-width (+ average-width separator-width))
         (available-width (- width separator-width)))
    (max 1 (floor (/ available-width padded-average-width)))))

(defun legend--walk-layouts (bindings width num-cols prev-result)
  "Figure out the optimal layout for the legend by testing different column counts.

BINDINGS - the extracted keybindings
WIDTH - the available width
NUM-COLS - the number of columns we are testing this iteration
PRE-RESULT - the result of the test last iteration

We start from the estimated layout.
We then increase and decrease the number of columns checking if they fit each time.
When we find a number of columns that is too large, but that number less one fits we are optimal."
  (when (<= num-cols 0)
    (error "Window too narrow for legend display"))
  ;; check to see if the layout fits or not
  (let* ((num-bindings (length bindings))
         (num-cols (min num-cols num-bindings))
         (num-rows (ceiling (/ num-bindings (float num-cols))))
         (keys (mapcar 'car bindings))
         (key-widths (mapcar 'length keys))
         (key-columns (legend--distribute-across-columns key-widths num-cols))
         (key-max-widths (mapcar (lambda (col) (apply 'max col)) key-columns))
         (binding-columns (legend--distribute-across-columns bindings num-cols))
         (column-max-widths (cl-mapcar #'legend--column-max-width binding-columns key-max-widths))
         (total-text-width (apply '+ column-max-widths))
         (separator-width (length legend-column-separator))
         (total-spacing-width (* (1- num-cols) separator-width))
         (total-needed-width (+ total-text-width total-spacing-width))
         (result (if (> total-needed-width width) :too-big :fits)))
    (cond
     ;; we cannot reduce the columns below one; this is a hard failure
     ((and (eq result :too-big) (= num-cols 1))
      (error "Not enough space for legend"))
     ;; if the layout fits, and the previous layout was too big this is the optimal layout
     ((and (eq result :fits) (eq prev-result :too-big))
      (list :cols num-cols :rows num-rows :col-widths column-max-widths :key-widths key-max-widths))
     ;; if the layout fits and the number of entries/columns are equal this is the  optimal layout 
     ((and (eq result :fits) (= num-cols num-bindings))
      (list :cols num-cols :rows num-rows :col-widths column-max-widths :key-widths key-max-widths))
     ;; if the layout is too big try reducing the number of columns by one
     ((eq result :too-big)
      (legend--walk-layouts bindings width (1- num-cols) result))
     ;; if the layout fits try increasing the number of columns by one
     ((eq result :fits)
      (legend--walk-layouts bindings width (1+ num-cols) result)))))

(defun legend--calculate-width (binding &optional max-key-length)
  "Calculate the rendered width of BINDING."
  (+ (if max-key-length
         max-key-length
       (length (car binding)))
     (length legend-separator)
     (length (cdr binding))))

(defun legend--column-max-width (bindings max-key-width)
  "Calculate the maximum width needed for a column of BINDINGS with MAX-KEY-WIDTH."
  (apply 'max (mapcar (lambda (binding) 
                        (legend--calculate-width binding max-key-width))
                      bindings)))

(defun legend--distribute-across-columns (bindings num-cols)
  "Distribute ITEMS across NUM-COLS columns in column-major order."
  (let* ((total-items (length bindings))
         (base-size (/ total-items num-cols))
         (extra-items (% total-items num-cols))
         (result nil)
         (start 0))
    (dotimes (col num-cols)
      (let ((col-size (if (< col extra-items)
                          (1+ base-size)
                        base-size)))
        (push (cl-subseq bindings start (+ start col-size)) result)
        (setf start (+ start col-size))))
    (nreverse result)))

;; private functions - rendering

(defun legend--render-entries (bindings window-width)
  "Render BINDINGS into aligned column layout that fit in WINDOW-WIDTH."
  (if (null bindings)
      "No bindings to display.\n"
    (let* ((layout (legend--calculate-layout bindings window-width))
           (cols (plist-get layout :cols))
           (rows (plist-get layout :rows))
           (col-widths (plist-get layout :col-widths))
           (key-widths (plist-get layout :key-widths))
           (binding-columns (legend--distribute-across-columns bindings cols))
           (result ""))
      (dotimes (row rows)
        (dotimes (col cols)
          (let ((column-bindings (nth col binding-columns)))
            (when (< row (length column-bindings))
              (let* ((binding (nth row column-bindings))
                     (key-width (nth col key-widths))
                     (entry (legend--render-keymap-entry binding key-width))
                     (col-width (nth col col-widths))
                     (is-last-col (= col (1- cols))))
                (setq result 
                      (concat result 
                              (legend--render-column-entry entry col-width is-last-col)))))))
        (setq result (concat (string-trim-right result) "\n")))
      result)))

(defun legend--render-column-entry (entry col-width is-last-col)
  "Render a single ENTRY for column display."
  (concat (if is-last-col entry (format (format "%%-%ds" col-width) entry))
          (if is-last-col "" (propertize legend-column-separator 'face 'legend-column-separator))))

(defun legend--render-keymap-entry (binding &optional key-width)
  "Render a single BINDING (key . description) extracted from keymap."
  (let* ((key (car binding))
         (desc (cdr binding))
         (padded-key (if key-width (format (format "%%%ds" key-width) key) key)))
    (concat (propertize padded-key 'face 'legend-key)
            (propertize legend-separator 'face 'legend-separator)
            (propertize desc 'face 'legend-description))))

(provide 'legend)

;;; legend.el ends here
