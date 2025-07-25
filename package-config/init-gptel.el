;; -*- lexical-binding: t; -*-

(use-package gptel
  :straight t
  :defer t

  ;; TODO: I would like to remove the prompt when I launch it
  ;; https://github.com/karthink/gptel/issues/318

  :custom

  (gptel-directives
    '((default . "Be terse. Don't use headings in responses.")))
  (gptel-default-mode 'org-mode)
  (gptel-display-buffer-action '(display-buffer-same-window))
  (gptel-confirm-tool-calls t)

  :config

  (require 'gptel-integrations)
  
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "** 🗣️ Prompt\n\n")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "** 🤖 Response\n\n")
  (setq gptel-model 'claude-sonnet-4-20250514)
  (setq gptel-backend (gptel-make-anthropic "Claude"
                        :stream t
                        :models '(claude-sonnet-4-20250514)
                        :key (lookup-password "claude")
                        :request-params '(:thinking
                                          (:type "enabled" :budget_tokens 1024)
                                          :max_tokens 2048))))

(provide 'init-gptel)
