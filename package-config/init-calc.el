;; -*- lexical-binding: t; -*-

(straight-use-package '(calc :type built-in))

(use-package calc
  :defer t

  :init
  (setq calc-show-banner nil)
  (setq calc-line-numbering nil)

  :config
  (calc-trail-display 0))

(straight-use-package '(calc-bin :type built-in))

(use-package calc-bin
  :defer t
  :commands (calc-binary-radix
             calc-hex-radix
             calc-decimal-radix
             calc-and
             calc-or
             calc-xor
             calc-not
             calc-word-size
             calc-lshift-binary
             calc-rshift-binary))

(straight-use-package '(calc-ext :type built-in))

(use-package calc-ext
  :defer t
  :commands (calc-precision))

(provide 'init-calc)
