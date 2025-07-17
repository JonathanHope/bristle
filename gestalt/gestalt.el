;;; gestalt.el --- Frictionless and minimal modal editing for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jonathan Hope

;; Author: Jonathan Hope <jhope@theflatfield.net>
;; Version: 1.0
;; Keywords: modal
;; Package-Requires: ((emacs "29.1"))

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

;; Gestalt is a minimal framework to add modal editing to Emacs.
;; It does nothing by default, all behavior is opt in and built by the user.
;; The intention is to add modal editing where and how it makes sense.

;;; Acknowledgments:

;; Some of this comes from meow.el - by Shi Tianshu
;; Another inspiration is hydra.el - by Oleh Krehel

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; custom

(defgroup gestalt nil
  "gestalt"
  :group 'extensions)

(defcustom gestalt-facet-modes
  '()
  "An alist of major mode -> facet mappings.

When a buffers major mode changes, the facet always changes as well.
That facet will be selected using this alist.
If no facet is found for the major mode, then the default facet will be used.
Note that in fundamental mode there is never a facet active.

Example usage:
(setq gestalt-facet-modes
  '((prog-mode . command)))"
  :group 'gestalt
  :type '(alist :key-type (sexp :tag "Major-mode")
                :value-type (symbol :tag "Facet")))

;; hooks

(defvar gestalt-change-facet-hook nil
  "Hook that fires when the facet changes.")

(defvar gestalt-change-keymap-hook nil
  "Hook that fires when the keymap changes.")

(defvar gestalt-change-transient-hook nil
  "Hook that fires when a transient facet activates or deactivates.")

(defvar gestalt-change-override-hook nil
  "Hook that fires when an override facet activates or deactivates.")

;; private variables

(defvar gestalt--facet-mode-alist '()
  "An alist of facet -> minor modes; for internal use.")

(defvar gestalt--facet-keymap-alist '()
  "An alist of facet -> keymap; for internal use.")

(defvar gestalt--facet-parent-alist '()
  "An alist of facet -> parent facet; for internal use.")

(defvar-local gestalt--current-facet nil
  "The facet that is active in the current buffer; for internal use.")

(defvar-local gestalt--current-keymap nil
  "The keymap that is active in the current buffer; for internal use.")

(defvar gestalt--transient-keymap nil
  "The current transient keymap active (if any); for internal use.")

(defvar gestalt--override-keymap nil
  "The current override keymap active (if any); for internal use.")

(defvar gestalt--transient-facet-active nil
  "Whether a transient facet is active; for internal use.")

(defvar gestalt--override-facet-active nil
  "Whether an override face is active; for internal use.")

;; public API - functions

;;;###autoload
(defun gestalt-active-keymap ()
  "Returns the currently active keymap.

Example usage:
(gestalt-active-keymap)"
  (cond (gestalt--transient-facet-active gestalt--transient-keymap)
        (gestalt--override-facet-active gestalt--override-keymap)
        (t gestalt--current-keymap)))

;;;###autoload
(defun gestalt-transient-facet-p ()
  "Returns whether a transient facet is active or not.

Example usage:
(gestalt-transient-facet-p)"
  gestalt--transient-facet-active)

;;;###autoload
(defun gestalt-override-facet-p ()
  "Returns whether an override facet is active or not.

Example usage:
(gestalt-override-facet-p)"
  gestalt--override-facet-active)

;;;###autoload
(defun gestalt-active-lighter ()
  "Returns the lighter for the currently active facet.

Example usage:
(gestalt-active-lighter)"
  (or (when gestalt--current-facet
        (let ((mode-var (alist-get gestalt--current-facet gestalt--facet-mode-alist)))
          (when mode-var
            (let ((lighter-entry (assq mode-var minor-mode-alist)))
              (when lighter-entry
                (let ((lighter (cdr lighter-entry)))
                  (cond
                   ((stringp lighter)
                    lighter)
                   ((and (listp lighter) (= (length lighter) 1) (stringp (car lighter)))
                    (car lighter)))))))))
      ""))

;;;###autoload
(defun gestalt-make-command-map (&rest bindings)
  "Make keymap for a command facet.

In a command facet all non-modifier keys (!-~,SPC) do nothing by default.
However you can provide your own bindings for those keys with BINDINGS.
This is for facets that are akin to the Vi command mode.
While the modifier keys won't be touched, these keymap are _very_ intrusive.
Don't use them for TUIs like dired or calc.

Example usage:
(gestalt-make-command-map
  '(\"w\" . (\"next word\" . forward-word))"
  (let ((map (make-keymap)))
    (dotimes (i 256)
      (let ((char (vector i)))
        (when (or (eq i ? ) (and (>= i ?!) (<= i ?~)))
          (define-key map char #'ignore))))
    (pcase-dolist (`(,key . ,def) bindings)
      (define-key map (kbd key) def))
    map))

;;;###autoload
(defun gestalt-make-insert-map (&rest bindings)
  "Make a keymap for an insert facet.

In an insert facet all keys are untouched by default.
Any changes to key behavior are from BINDINGS.
This is for facets that are akin to the Vi insert mode.
These keymaps generally safe to use.
Just be sure that the bindings you provide don't cause any problems.

Example usage:
(gestalt-make-insert-map
  '(\"C-v\" . (\"paste\" . yank))"
  (let ((map (make-sparse-keymap)))
    (pcase-dolist (`(,key . ,def) bindings)
      (define-key map (kbd key) def))
    map))

;;;###autoload
(defun gestalt-to-parent-facet ()
  "Activate the current facets parent facet (if any).

This is useful for when you've launched a facet from another facet, and want to go back.

Example usage:
(gestalt-to-parent-facet)"
  (interactive)
  (let ((parent (alist-get gestalt--current-facet gestalt--facet-parent-alist)))
    (when parent
      (gestalt--change-facet parent))))

;; facet helpers

(defun gestalt--major-mode-changed ()
  "Change the active facet based on the active major mode."
  (let* ((facet-deactivated (gestalt--deactivate-facet))
         (facet (gestalt--select-facet major-mode))
         (facet-activated (gestalt--activate-facet facet)))
    (when (or facet-deactivated facet-activated)
      (run-hooks 'gestalt-change-facet-hook)
      (run-hooks 'gestalt-change-keymap-hook))))

(defun gestalt--change-facet (facet)
  "Change the currently active facet.

Facets are buffer local, and there can only ever be one active facet active."
  (unless (eq facet gestalt--current-facet)
    (let ((facet-deactivated (gestalt--deactivate-facet))
          (facet-activated (gestalt--activate-facet facet)))
      (when (or facet-deactivated facet-activated)
        (run-hooks 'gestalt-change-facet-hook)
        (run-hooks 'gestalt-change-keymap-hook)))))

(defun gestalt--deactivate-facet ()
  "Deactivate the currently active facet.

If there is no facet active this is a noop."
  (if gestalt--current-facet
    (let ((mode (alist-get gestalt--current-facet gestalt--facet-mode-alist)))
      (funcall mode -1)
      (setq gestalt--current-facet nil)
      (setq gestalt--current-keymap nil)
      t)
    nil))

(defun gestalt--activate-facet (facet)
  "Activate the facet FACET.

If another facet is active this is a noop.
If the facet has not been registered this is a noop."
  (if (and facet (not gestalt--current-facet))
    (let ((mode (alist-get facet gestalt--facet-mode-alist))
          (keymap (alist-get facet gestalt--facet-keymap-alist)))
      (if mode
          (progn
            (funcall mode 1)
            (setq gestalt--current-facet facet)
            (setq gestalt--current-keymap keymap)
            t)
        nil))
    nil))

(defun gestalt--select-facet (mode)
  "Select the correct facet for mode MODE.

This is done by checking for a matching mode in `gestalt-facet-modes'.
Because modes can be derived in Emacs we also check for the modes parents in `gestalt-facet-modes'
If no matching mode is found then the default facet is used.
Finally if the mode is `fundamental-mode' we don't activate any facet."
  (let ((facet (alist-get mode gestalt-facet-modes)))
    (cond (facet facet)
          ((get mode 'derived-mode-parent) (gestalt--select-facet (get mode 'derived-mode-parent)))
          (t (if (eq major-mode 'fundamental-mode)
                 nil
               'default)))))

;; transient facet helpers

(defun gestalt--activate-transient-facet (keymap)
  "Activate a transient facet for KEYMAP.

A transient facet is one that only lasts as long as one keypress.
That is of course exactly what a transient keymap is in Emacs.
There is some extra bookkeeping we do around making sure that current keymap is up to date."
  (setq gestalt--transient-facet-active t)
  (setq gestalt--transient-keymap keymap)
  (run-hooks 'gestalt-change-transient-hook)
  (run-hooks 'gestalt-change-keymap-hook)
  (set-transient-map
   keymap
   nil
   (lambda ()
     (setq gestalt--transient-keymap nil)
     (setq gestalt--transient-facet-active nil)
     (run-hooks 'gestalt-change-keymap-hook)
     (run-hooks 'gestalt-change-transient-hook))))

;; override facet helpers

(defun gestalt--activate-override-facet (keymap)
  "Activate an override facet for KEYMAP.

An override facet lasts until dismissed.
It does this with transient keymaps.
First it activates a transient keymap.
Then it adds a post command hook.
The post command hook will activate KEYMAP as a transient map _again_.
This ensures the keymap is paused for interactive commands while remaining sticky."
  (unless gestalt--override-facet-active
    (setq gestalt--override-facet-active t)
    (setq gestalt--override-keymap keymap)
    (run-hooks 'gestalt-change-override-hook)
    (run-hooks 'gestalt-change-keymap-hook)
    (add-hook 'post-command-hook #'gestalt--post-command)
    (set-transient-map keymap nil)))

(defun gestalt--deactivate-override-facet ()
  "Deactivate the currently active override facet.

The post command hook is removed so the override facet will stop triggering after every command."
  (interactive)
  (setq gestalt--override-facet-active nil)
  (setq gestalt--override-keymap nil)
  (run-hooks 'gestalt-change-override-hook)
  (run-hooks 'gestalt-change-keymap-hook)
  (remove-hook 'post-command-hook #'gestalt--post-command))

(defun gestalt--post-command ()
  "Activate the current keymap as a transient keymap.

This does not occur if a minibuffer is collecting input."
  (when (and gestalt--override-facet-active (not (minibuffer-window-active-p (minibuffer-window))))
    (set-transient-map gestalt--override-keymap nil)))

;; macro helpers

(eval-and-compile
  (defun gestalt--intern (name suffix &optional two-dashes)
    "Intern a string for use in the gestalt package.

The final symbol will be in the form of: gestalt-NAME-SUFFIX.
If TWO-DASHES is then then the form will be: gestalt--NAME-SUFFIX."
    (intern
     (concat "gestalt"
             (if two-dashes "--" "-")
             name suffix)))

  (defun gestalt--define-facet-active-p (name facet)
    "Generate a predicate function to check if a facet is active."
    `(defun ,name ()
       ,(concat "Whether " facet " facet is active.")
       (eq (intern ,facet) gestalt--current-facet)))

  (defun gestalt--define-facet-activate (name facet)
    "Generate a function to activate a facet."
    `(defun ,name ()
       ,(concat "Activate " facet " facet.")
       (interactive)
       (gestalt--change-facet (intern ,facet))))

  (defun gestalt--define-minor-mode (name description keymap lighter form)
    "Define a minor mode for a facet.

A facet is really just a minor mode.
Facets require very little:
 - can be activated/deactivated per buffer
 - has a high priority keymap
Minor modes meet all of these criteria."
    `(define-minor-mode ,name
       ,description
       :lighter ,lighter
       :keymap ,keymap
       ,form))

  (defun gestalt--register-facet (facet mode &optional keymap parent)
    "Register a facet with gestalt."
    (add-to-list 'gestalt--facet-mode-alist `(,facet . ,mode))
    (when keymap
      (add-to-list 'gestalt--facet-keymap-alist `(,facet . ,keymap)))
    (when parent
      (add-to-list 'gestalt--facet-parent-alist `(,facet . ,parent))))

  (defun gestalt--define-transient-facet-keymap (name facet bindings)
    "Define a keymap for a transient facet."
    `(defvar ,name
       (gestalt-make-command-map
        ,@bindings)
       ,(concat "Keymap for " facet " transient facet.")))

  (defun gestalt--define-transient-facet (name facet keymap)
    "Define a function to launch a transient facet."
    `(defun ,name ()
       ,(concat "Activate " facet " transient facet.")
       (interactive)
       (gestalt--activate-transient-facet ,keymap)))

  (defun gestalt--define-override-facet-keymap (name facet bindings)
    "Define a keymap for an override facet."
    `(defvar ,name
       (gestalt-make-command-map
        ,@bindings)
       ,(concat "Keymap for " facet " override facet.")))

  (defun gestalt--define-override-facet (name facet keymap)
    "Define a function to launch an override facet."
    `(defun ,name ()
       ,(concat "Activate " facet " override facet.")
       (interactive)
       (gestalt--activate-override-facet ,keymap))))

;; public macros -- must be after everything they use for byte compilation

;;;###autoload
(defmacro gestalt-define-facet (name-sym description &rest body)
  "Add a facet to your gestalt.

A facet is akin to a mode in the modal editing paradigm.

The syntax for this is derived from `define-minor-mode'.
The facet will be called NAME-SYM and have description DESCRIPTION.
The body can have the following keywords:
:keymap - the keymap to use for the facet
:lighter - the indicator to display in the mode line while the facet is active
:parent - facet that `gestalt-to-parent-facet' will activate
Optionally, after the arguments you can provide a lisp form.
This form will execute each time the facet activates or deactivates.

You can define a command facet by making the keymap with `gestalt-make-command-map'.
Alternatively you can define an insert facet by making the keymap with `gestalt-make-insert-map'.

This will define a number of things:
 - gestalt-NAME-SYM-facet-p: function that can be used to check if a given facet is active.
 - gestalt-NAME-SYM-facet-activate: function that can be used to activate a given facet.
 - gestalt-NAME-SYM-facet-mode: minor mode.
This minor mode is an implementation detail, and should _not_ be enabled or disabled directly.

Example usage:
(gestalt-define-facet
 command
 \"Facet to do things other than insert text.\"
 :lighter \" [C]\"
 :keymap (gestalt-make-command-map
          '(\"w\" . (\"next word\" . forward-word)))
(message \"command mode toggled\"))"
  (let* ((name (symbol-name name-sym))
         (mode-name (gestalt--intern name "-facet-mode"))
         (active-p-name (gestalt--intern name "-facet-p"))
         (facet-activate-name (gestalt--intern name "-facet-activate"))
         (keymap (plist-get body :keymap))
         (lighter (plist-get body :lighter))
         (parent (plist-get body :parent))
         (form (unless (cl-evenp (length body))
                (car (last body)))))
    `(progn
       ,(gestalt--define-facet-active-p active-p-name name)
       ,(gestalt--define-facet-activate facet-activate-name name)
       ,(gestalt--define-minor-mode mode-name description keymap lighter form)
       (gestalt--register-facet
        ',(intern name)
        ',mode-name
        ,keymap
        ,parent))))

;;;###autoload
(defmacro gestalt-define-transient-facet (name-sym &rest bindings)
  "Add a transient facet to your gestalt.

A transient facet is a facet that only lasts as long as one keypress.
Defining these can allow chains of keys to imitate modifiers.
I.e. you could map C-x C-f to SPC x f.

This will define a number of things:
 - gestalt-NAME-SYM-transient-facet-keymap: the keymap the transient facet will use
 - gestalt-NAME-SYM-transient-facet: a function that activates the transient facet; is interactive

Example usage:
(gestalt-define-transient-facet
   mod
   `(\"x\" . (\"exec command\" . execute-extended-command)))"
  (let* ((name (symbol-name name-sym))
         (fn-name (gestalt--intern name "-transient-facet"))
         (keymap-name (gestalt--intern name "-transient-facet-keymap")))
    `(progn
       ,(gestalt--define-transient-facet-keymap keymap-name name bindings)
       ,(gestalt--define-transient-facet fn-name name keymap-name))))

;;;###autoload
(defmacro gestalt-define-override-facet (name-sym quit &rest bindings)
  "Add an override facet to your gestalt.

An override facet is a facet that will last until explicitly dismissed with QUIT.
It will take priority over _everything_.
These can be used for hydra/transient like functionality.
These should be used sparingly and triggered from the global map.

This will define a number of things:
 - gestalt-NAME-SYM-override-facet-keymap: the keymap the override facet will use
 - gestalt-NAME-SYM-override-facet: a function that activates the override facet; is interactive

Example usage:
(gestalt-define-override-facet
   system \"<escape>\"
   '(\"|\" . (\"split ver\" . split-window-horizontally))
   '(\"-\" . (\"split hor\" . split-window-vertically))))"
  (let* ((name (symbol-name name-sym))
         (keymap-name (gestalt--intern name "-override-facet-keymap"))
         (fn-name (gestalt--intern name "-override-facet")))
    `(progn
       ,(gestalt--define-override-facet-keymap keymap-name name bindings)
       (define-key
        ,keymap-name
        (kbd ,quit)
        '("quit" . gestalt--deactivate-override-facet))
       ,(gestalt--define-override-facet fn-name name keymap-name))))

;; init function - calls a macro so it must be after the macros for byte compilation

;;;###autoload
(defun gestalt-init ()
  "One time setup for gestalt.

This should be called during package configuration.
A default facet will be registered.
The default facet does nothing, so it can't cause any keybinding conflicts.
This also adds a function to change the active facet to the `after-change-major-mode-hook' hook.

Example usage:
(gestalt-init)"
  (gestalt-define-facet
   default
   "A facet lacking any behavior."
   :lighter " [DEF]")
  (add-hook 'after-change-major-mode-hook #'gestalt--major-mode-changed))

(provide 'gestalt)
;;; gestalt.el ends here
