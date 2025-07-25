;; -*- lexical-binding: t; -*-

(use-package mcp
  :straight t
  :after gptel)

(use-package mcp-hub
  :after mcp
  :custom
  (mcp-hub-servers
   ;; used to access the local filesystem
   `(("filesystem" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem" ,(expand-file-name "~/Projects"))))
     ;; used to fetch resources on the internet
     ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
     ;; filesystem and cli support
     ("desktop commander" . (:command "npx" :args ("-y" "@wonderwhy-er/desktop-commander"))))))

(provide 'init-mcp)
