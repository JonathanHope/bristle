;; -*- lexical-binding: t; -*-

(use-package pcre2el
  :straight t

  :config
  (setq-default reb-re-syntax 'pcre))

(provide 'init-pcre2el)
