;;; melitt.el --- Grants the ability to theme org modes headlines and lists.

;; Copyright (C) 2025 Jonathan Hope

;; Author: Jonathan Hope <jonathan.douglas.hope@gmail.com>
;; Version: 1.0
;; Keywords: org

;;; Commentary:
;; This package performs character substitutions in org buffers.
;; These substitutions are done purely for aesthetic reasons.

;;; Code:

;; custom

(defgroup melitt nil
  "Visual tweaks for org mode."
  :group 'org-appearance)

(defcustom melitt-headline-dash ?━
  "What character to use for indenting the org headline character."
  :group 'melitt
  :type 'character)

(defcustom melitt-headline-bullet ?⬢
  "What character to demark a headline with."
  :group 'melitt
  :type 'character)

(defcustom melitt-plain-list-plus-char ?➤
  "What character to demark a plain list using pluses with."
  :group 'melitt
  :type 'character)

(defcustom melitt-plain-list-asterisk-char ?➤
  "What character to demark a plain list using asterisks with."
  :group 'melitt
  :type 'character)

(defcustom melitt-plain-list-minus-char ?➤
  "What character to demark a plain list using minuses with."
  :group 'melitt
  :type 'character)

(defcustom melitt-todo ?⬜
  "What character to demark a TODO in a headline."
  :group 'melitt
  :type 'character)

(defcustom melitt-todo-pad nil
  "Whether to add a padding char to the TODO replacement."
  :group 'melitt
  :type 'boolean)

(defcustom melitt-done ?⬛
  "What character to demark a DONE in a headline."
  :group 'melitt
  :type 'character)

(defcustom melitt-done-pad nil
  "Whether to add a padding char to the DONE replacement."
  :group 'melitt
  :type 'boolean)

(defcustom melitt-checkbox-unchecked ?⚪
  "What character to demark a unchecked checkbox in a plain list."
  :group 'melitt
  :type 'character)

(defcustom melitt-checkbox-unchecked-pad nil
  "Whether to add padding to the checked checkbox replacement."
  :group 'melitt
  :type 'boolean)

(defcustom melitt-checkbox-checked ?⚫
  "What character to demark a checked checkbox in a plain list."
  :group 'melitt
  :type 'character)

(defcustom melitt-checkbox-checked-pad nil
  "Whether to add padding to the unchecked checkbox replacement."
  :group 'melitt
  :type 'boolean)

;; variables

(defvar melitt--headline-cache (make-hash-table :test 'eq)
  "Cache for headline character compositions.")

;; helper functions

(defun melitt--get-headline-char (level)
  "Build headline character sequence for LEVEL."
  (or (gethash level melitt--headline-cache)
      (puthash level
               (let ((result '()))
                 (dotimes (_ level)
                   (push melitt-headline-dash result)
                   (push '(Br . Bl) result))
                 (push melitt-headline-bullet result)
                 (nreverse result))
               melitt--headline-cache)))

(defun melitt--get-char (replacement pad)
  "Build character list with REPLACEMENT and optional PAD."
  (if pad
      (list replacement '(Br . Bl) ?\s)
    (list replacement)))

;;;###autoload
(define-minor-mode melitt-mode
  "Visual tweaks for org mode."
  nil nil nil
  (let* ((headlines
          `(("^\\*+ "
             (0 (let* (( level (- (match-end 0) (match-beginning 0) 1)))
                  (compose-region (- (match-end 0) (+ 2 (- level 1)))
                                  (- (match-end 0) 1)
                                  (melitt--get-headline-char level))
                  nil)))))

         (todo
          `(("^\\*+ TODO "
             (0 (prog1 () (compose-region
                           (- (match-end 0) 5)
                           (- (match-end 0) 1)
                           (melitt--get-char
                            melitt-todo
                            melitt-todo-pad)))))))

         (done
          `(("^\\*+ DONE "
             (0 (prog1 () (compose-region
                           (- (match-end 0) 5)
                           (- (match-end 0) 1)
                           (melitt--get-char
                            melitt-done
                            melitt-done-pad)))))))

         (plain-list-plus
          `(("^ +\\+ "
             (0
              (prog1 () (compose-region
                         (- (match-end 0) 2)
                         (- (match-end 0) 1)
                         melitt-plain-list-plus-char))))))

         (plain-list-asterisk
          `(("^ +\\* "
             (0
              (prog1 () (compose-region
                         (- (match-end 0) 2)
                         (- (match-end 0) 1)
                         melitt-plain-list-asterisk-char))))))

         (plain-list-minus
          `(("^ +\\- "
             (0
              (prog1 () (compose-region
                         (- (match-end 0) 2)
                         (- (match-end 0) 1)
                         melitt-plain-list-minus-char))))))

         ;; Match: "  - [ ] " or "  + [ ] " or "  * [ ] " or "  1. [ ] "
         (checkbox-unchecked
          `(("^ +\\(\\-\\|\\+\\|\\*\\|[0-9]+[\\)\\.]\\) \\[ \\] "
             (0
              (prog1 () (compose-region
                         (- (match-end 0) 4)
                         (- (match-end 0) 1)
                         (melitt--get-char
                          melitt-checkbox-unchecked
                          melitt-checkbox-unchecked-pad)))))))

         ;; Match: "  -  [X] " or "  + [X] " or "  * [X] " or "  1. [X] "
         (checkbox-checked
          `(("^ +\\(\\-\\|\\+\\|\\*\\|[0-9]+[\\)\\.]\\) \\[X\\] "
             (0
              (prog1 () (compose-region
                         (- (match-end 0) 4)
                         (- (match-end 0) 1)
                         (melitt--get-char
                          melitt-checkbox-checked
                          melitt-checkbox-checked-pad))))))))

    (if melitt-mode
        (progn
          (font-lock-add-keywords nil headlines)
          (font-lock-add-keywords nil todo)
          (font-lock-add-keywords nil done)
          (font-lock-add-keywords nil plain-list-plus)
          (font-lock-add-keywords nil plain-list-asterisk)
          (font-lock-add-keywords nil plain-list-minus)
          (font-lock-add-keywords nil checkbox-unchecked)
          (font-lock-add-keywords nil checkbox-checked)
          (font-lock-fontify-buffer))
      (save-excursion
        (goto-char (point-min))
        (font-lock-remove-keywords nil headlines)
        (font-lock-remove-keywords nil todo)
        (font-lock-remove-keywords nil done)
        (font-lock-remove-keywords nil plain-list-plus)
        (font-lock-remove-keywords nil plain-list-asterisk)
        (font-lock-remove-keywords nil plain-list-minus)
        (font-lock-remove-keywords nil checkbox-unchecked)
        (font-lock-remove-keywords nil checkbox-checked)
        (font-lock-fontify-buffer)))))

(provide 'melitt)

;;; melitt.el ends here
