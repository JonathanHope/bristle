;; -*- lexical-binding: t; -*-

(straight-use-package '(ibuffer :type built-in))

(use-package ibuffer
  :defer t

  :init
  (defface ibuffer-title-face
    '((t))
    "Face for ibuffer title."
    :group 'ibuffer)

  (defface ibuffer-marked-face
    '((t))
    "Face for ibuffer marked buffers."
    :group 'ibuffer)
  
  :custom
  (ibuffer-title-face 'ibuffer-title-face)
  (ibuffer-marked-face 'ibuffer-marked-face)
  (ibuffer-expert t)
  (ibuffer-display-summary nil)
  (ibuffer-modified-char ?✏)
  (ibuffer-marked-char ?✓)
  (ibuffer-formats '((mark
                      " "
                      modified
                      " "
                      (name 28 28 :left :elide)
                      " "
                      (mode 16 16 :left :elide)
                      " "
                      filename))))

(provide 'init-ibuffer)
