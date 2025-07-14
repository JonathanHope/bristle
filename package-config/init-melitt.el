;; -*- lexical-binding: t; -*-

(straight-use-package 
 `(melitt
   :type nil
   :local-repo ,(concat user-emacs-directory "melitt")
   :files ("melitt.el")))

(use-package melitt
  :defer t

  :custom
  (melitt-done ?)
  (melitt-done-pad nil)
  (melitt-todo ?)
  (melitt-todo-pad nil)
  (melitt-checkbox-unchecked ?)
  (melitt-checkbox-unchecked-pad nil)
  (melitt-checkbox-checked ?)
  (melitt-checkbox-checked-pad nil)
  (melitt-headline-dash ?━)
  (melitt-headline-bullet ?⬢)
  (melitt-plain-list-plus-char ?➤)
  (melitt-plain-list-asterisk-char ?➤)
  (melitt-plain-list-minus-char ?➤))

(provide 'init-melitt)
