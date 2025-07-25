;; -*- lexical-binding: t; -*-

(use-package dape
  :defer t
  :straight t

  :commands (dape-breakpoint-toggle
             dape-breakpoint-log
             dape-breakpoint-expression
             dape-breakpoint-hits
             dape-breakpoint-remove-at-point
             dape-breakpoint-remove-all)
  
  :custom
  (dape-buffer-window-arrangement nil)
  (dape-start-hook '(dape-repl))
  (dape-breakpoint-margin-string "✪"))

;; this is tricky to configure or projects
;; create a `dir-locals.el' at the top level with the debug configs
;; here is an example:

;; ((nil . ((dape-configs . ((cli
;;                                modes (go-mode go-ts-mode)
;;                                ensure dape-ensure-command
;;                                command "dlv"
;;                                command-args ("dap" "--listen" "127.0.0.1::autoport")
;;                                command-cwd dape-command-cwd
;;                                port :autoport
;;                                :request "launch"
;;                                :type "go"
;;                                :cwd "."
;;                                :program "./cmd/cli"
;;                                :args ["list" "all"])))
;;              (dape-command . (cli)))))

(provide 'init-dape)
