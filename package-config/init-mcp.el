;; -*- lexical-binding: t; -*-

(use-package mcp
  :straight t
  :after gptel)

(use-package mcp-hub
  :after mcp

  :custom (mcp-hub-servers
           `(;; used to interact with Emacs
             ("emacs" . (:url "http://127.0.0.1:8080"))
             ;; used to access the local filesystem
             ("filesystem" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" ,(expand-file-name "~/Projects"))))
             ;; used to fetch resources on the internet
             ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
             ;; filesystem and cli support
             ("desktop commander" . (:command "npx" :args ("-y" "@wonderwhy-er/desktop-commander")))))

  :init
  (defun bristle--mcp-hub-start-server-advice (orig-fun server &optional inited-callback)
    "Handle Emacs MCP server startup."
    (let ((server-name (car server)))
      (if (string= server-name "emacs")
          (progn
              (emacs-mcp-start)
              (funcall orig-fun server inited-callback))
        (funcall orig-fun server inited-callback))))
  
    (defun bristle--mcp-hub-stop-server-advice (orig-fun server-name)
      "Handle Emacs MCP server shutdown."
      (when (string= server-name "emacs")
        (emacs-mcp-stop))
      (funcall orig-fun server-name))

    (advice-add 'mcp-hub--start-server :around #'bristle--mcp-hub-start-server-advice)
    (advice-add 'mcp-stop-server :around #'bristle--mcp-hub-stop-server-advice))

(provide 'init-mcp)
