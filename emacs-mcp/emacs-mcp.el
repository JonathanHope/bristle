;;; emacs-mcp.el --- MCP server to expose Emacs functionality -*- lexical-binding: t; -*-

;; Right now the built in Emacs mcp client doesn't work with this.
;; TODO: https://github.com/lizqwerscott/mcp.el/issues/29

;; Copyright (C) 2025
;; Author: jhope@theflatfield.net
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: mcp

;;; Commentary:
;; This is a rough implementation of an MCP server for Emacs itself.
;; This was primarily an exercise to learn the MCP standard.

;; MCP servers use JSON RPC as their message format: 
;; https://www.jsonrpc.org/specification.
;; It's a pretty simple spec.

;; The request message has the fields:
;; - `jsonrpc': always 2.0
;; - `id': unique ID of the message
;; - `method': the name of the method being invoked
;; - `params': parameters to provide to the method; optional
;; A notification message doesn't include the `id' field.

;; The response message has the fields:
;; - `jsonrpc': always 2.0
;; - `id': same ID as the request message
;; - `result': result of the method; required for success
;; - `error': the error that occurred; required for failure

;; The error object has the fields:
;; - `code': number that identifies the error
;; - `message': description of the error
;; - `data': additional data about the error; optional

;; There are some standard error codes:
;; - `32700': parse error
;; - `32600': invalid request
;; - `32601': method not found
;; - `32602': invalid parameter(s)
;; - `32603': internal error

;; JSON RPC batch support is not used.

;; The MPC spec itself is  bit more complex.
;; It is also evolving _very_ fast.

;; See here:
;; https://modelcontextprotocol.io/specification/2025-03-26/basic
;; https://modelcontextprotocol.io/specification/2025-03-26/basic/lifecycle
;; https://modelcontextprotocol.io/specification/2025-03-26/basic/transports

;; MPC servers have a lifecycle they must adhere to.

;; First the client application sends an `initialize' message.
;; That message must include:
;; - supported protocol version
;; - client capabilities
;; - client implementation information

;; The server will then respond with similar information.
;; Once the client receives the response it sends a `notifications/initialized' notification.
;; At that point the handshake is complete and the server and client can communicate.

;; There are many capabilities that an MCP server can provide.
;; See here:
;; https://modelcontextprotocol.io/specification/2025-06-18/basic/lifecycle#capability-negotiation
;; However this server will only provide `tools.'

;; Here is the flow this all enables:
;; - client requests available tools with `tools/list'
;; - client sends a request to an LLM with the tools descriptions
;; - LLM decides it wants to execute a tool
;; - LLM sends a request to the client to execute a tool
;; - client invokes tool with `tools/call'
;; - client returns the result to the LLM

;; This takes us to the trickiest part: the transport.
;; MCP can use either stdio or HTTP for its transport.
;; Using stdio is much easier, but it won't work since we are exposing in-process info.

;; Emacs of course has a built in network server with `make-network-process'.
;; It can do both TCP and UDP.
;; Since the transport is HTTP we just need TCP support.

;; HTTP is a pretty big standard these days:
;; https://www.rfc-editor.org/rfc/rfc9110.html
;; Thankfully we only need HTTP 1.1, and we don't need TLS.
;; On top of that we only need to support enough of the spec for the server.
;; We are not building a full production grade general purpose server here.

;; The server only ever has to listen on localhost.
;; It also must only accept connections from localhost.
;; The server should never be exposed publicly for any reason.

;; The client calls as single endpoint on the server with `POST'.
;; The accept header will be `application/json' or `text/event-stream'.
;; The body of the message must be a single JSON RPC request.
;; The server will then respond with a JSON RPC message of its own.

;; The server doesn't have session support at this time.
;; The server also doesn't support SSE at this time.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'emacs-mcp-tools)

;; custom

(defcustom emacs-mcp-server-port 8080
  "Port for the MCP HTTP server."
  :type 'integer
  :group 'emacs-mcp)

(defcustom emacs-mcp-server-host "127.0.0.1"
  "Host for the MCP HTTP server."
  :type 'string
  :group 'emacs-mcp)

(defcustom emacs-mcp-allowed-origins '("http://localhost" "https://localhost")
  "List of allowed Origin headers for CORS protection."
  :type '(repeat string)
  :group 'emacs-mcp)

(defcustom emacs-mcp-connection-timeout 30
  "Connection timeout in seconds."
  :type 'integer
  :group 'emacs-mcp)

(defcustom emacs-mcp-tool-timeout 10
  "Tool execution timeout in seconds."
  :type 'integer
  :group 'emacs-mcp)

(defcustom emacs-mcp-tools emacs-mcp-default-tools
  "Available MCP tools.

Each tool is an alist entry with the format:
  (\"tool-name\" . ((title . \"Tool Title\")
                   (description . \"Tool description\")
                   (inputSchema . json-schema)
                   (handler . handler-function)))

The handler function should accept a single argument (the tool arguments)
and return either a string result or an error list of the form:
  (error :code error-code :message \"error message\" :data optional-data)"
  :type '(alist :key-type string
                :value-type (plist :key-type symbol
                                  :value-type sexp))
  :group 'emacs-mcp)


;; private variables

(defvar emacs-mcp--http-server nil
  "The HTTP server process.")

(defvar emacs-mcp--supported-protocol-versions '("2025-06-18" "2025-03-26")
  "List of supported MCP protocol versions.

Versions older than 2025-03-26 had a different transport.")

(defvar emacs-mcp--server-capabilities
  `((tools . ,(make-hash-table)))
  "Server capabilities.")

(defvar emacs-mcp--server-info
  '((name . "emacs")
    (version . "0.1.0"))
  "Server information.")

;; public API

;;;###autoload
(defun emacs-mcp-start (&optional port host)
  "Start the Emacs MCP server."
  (interactive)
  (when emacs-mcp--http-server
    (emacs-mcp-stop))

  (let ((server-port (or port emacs-mcp-server-port))
        (server-host (or host emacs-mcp-server-host)))
    (setq emacs-mcp--http-server
          (make-network-process
           :name "emacs-mcp-complete"
           :service server-port
           :host server-host
           :server t
           :family 'ipv4
           :sentinel #'emacs-mcp--http-sentinel
           :coding 'utf-8))

    (message "Emacs MCP server started on %s:%d" server-host server-port)))

;;;###autoload
(defun emacs-mcp-stop ()
  "Stop the Emacs MCP server."
  (interactive)
  (when emacs-mcp--http-server
    (delete-process emacs-mcp--http-server)
    (setq emacs-mcp--http-server nil))

  (message "Emacs MCP server stopped"))

;; server routing

(defun emacs-mcp--http-sentinel (proc event)
  "HTTP server sentinel."
  (when (string-match "open" event)
    (set-process-filter proc #'emacs-mcp--http-filter)
    (emacs-mcp--set-connection-timeout proc)))

(defun emacs-mcp--http-filter (proc string)
  "HTTP server process filter."
  (condition-case err
      (let* ((parsed (emacs-mcp--parse-http-request string))
             (method (plist-get parsed :method))
             (path (plist-get parsed :path))
             (headers (plist-get parsed :headers))
             (body (plist-get parsed :body)))
        
        (cond
         ;; TODO: this seems to break the inspector
         ;; https://github.com/modelcontextprotocol/inspector/issues/574
         ;; https://modelcontextprotocol.io/specification/2025-03-26/basic/transports#security-warning
         ;; ((not (emacs-mcp--validate-origin headers))
         ;;  (emacs-mcp--send-jsonrpc-error-response proc -32003 "Request forbidden - Invalid Origin")
         ;;  (delete-process proc))

         ;; handle JSON RPC
         ((and (string= method "POST") (string= path "/"))
          (emacs-mcp--handle-jsonrpc-post proc body headers))

         ;; handle SSE
         ;; the server doesn't support SSE so we send a 405.
         ((and (string= method "GET") (string= path "/"))
          (emacs-mcp--send-http-response proc "405 Method Not Allowed"
                                        '(("Content-Type" . "text/plain")
                                          ("Allow" . "POST"))
                                        "Method Not Allowed")
          (delete-process proc))

         ;; otherwise 404
         (t
          (emacs-mcp--send-http-response proc "404 Not Found"
                                        '(("Content-Type" . "text/plain"))
                                        "Not Found")
          (delete-process proc))))
    (error
     (message "Error in HTTP filter: %s" (error-message-string err))
     (delete-process proc))))

(defun emacs-mcp--handle-jsonrpc-post (proc body headers)
  "Handle JSON-RPC POST request."
  (if (string-empty-p body)
      (emacs-mcp--handle-empty-body proc)
    (let ((json-data (condition-case nil
                         (json-parse-string body :object-type 'alist :array-type 'list)
                       (error nil))))
      (if json-data
          (let* ((id (alist-get 'id json-data))
                 (method-name (alist-get 'method json-data))
                 (params (alist-get 'params json-data)))
            (if (null id)
                (emacs-mcp--handle-jsonrpc-notification proc method-name params)
              (emacs-mcp--handle-jsonrpc-request proc id method-name params)))
        (emacs-mcp--handle-invalid-json proc)))))

(defun emacs-mcp--handle-jsonrpc-notification (proc method-name params)
  "Handle JSON-RPC notification."
  (emacs-mcp--notification-dispatcher (intern method-name) params)
  (emacs-mcp--send-http-response proc "202 Accepted" nil "")
  (delete-process proc))

(defun emacs-mcp--handle-jsonrpc-request (proc id method-name params)
  "Handle JSON-RPC request."
  (let ((result (emacs-mcp--request-dispatcher (intern method-name) params)))
    (if (and (listp result) (eq (car result) 'error))
        (let* ((error-data (cdr result))
               (error-code (plist-get error-data :code))
               (error-message (plist-get error-data :message))
               (error-additional-data (plist-get error-data :data))
               (error-response (emacs-mcp--make-jsonrpc-error-response id error-code error-message error-additional-data)))
          (emacs-mcp--send-jsonrpc-response proc error-response))
      (let ((success-response (emacs-mcp--make-jsonrpc-success-response id result)))
        (emacs-mcp--send-jsonrpc-response proc success-response)))))

(defun emacs-mcp--request-dispatcher (method params)
  "Main request dispatcher for MCP protocol."
  (pcase method
    ('initialize
     (emacs-mcp--handle-initialize params))
    
    ('ping
     (emacs-mcp--handle-ping params))
    
    ('tools/list
     (emacs-mcp--handle-tools-list params))
    
    ('tools/call
     (emacs-mcp--handle-tools-call params))
    
    (_
     '(error :code -32601 :message "Method not found"))))

(defun emacs-mcp--notification-dispatcher (method params)
  "Handle MCP notifications.

These are all noops at the moment.
The key here is that we return a 202 for them."
  (pcase method
    ('notifications/initialized
     (message "MCP client initialized"))
    
    ('notifications/cancelled
     nil)
    
    (_
     nil)))

;; server handlers

(defun emacs-mcp--handle-ping (params)
  "Handle MCP ping request."
  (make-hash-table))

(defun emacs-mcp--handle-initialize (params)
  "Handle MCP initialize request."
  (let ((client-version (alist-get 'protocolVersion params)))
    
    (if (member client-version emacs-mcp--supported-protocol-versions)
        `((protocolVersion . ,client-version)
          (capabilities . ,emacs-mcp--server-capabilities)
          (serverInfo . ,emacs-mcp--server-info))
      `(error :code -32602 
              :message "Invalid params - Unsupported protocol version"
              :data ((received . ,client-version)
                     (supported . ,emacs-mcp--supported-protocol-versions))))))

(defun emacs-mcp--handle-tools-list (params)
  "Handle tools/list request."
  (let ((tools (mapcar (lambda (tool-entry)
                        (let ((name (car tool-entry))
                              (spec (cdr tool-entry)))
                          `((name . ,name)
                            (title . ,(alist-get 'title spec))
                            (description . ,(alist-get 'description spec))
                            (inputSchema . ,(alist-get 'inputSchema spec)))))
                      emacs-mcp-tools)))
    `((tools . ,(vconcat tools)))))

(defun emacs-mcp--handle-tools-call (params)
  "Handle tools/call request."
  (let* ((tool-name (alist-get 'name params))
         (arguments (alist-get 'arguments params))
         (tool-spec (alist-get tool-name emacs-mcp-tools nil nil #'string=)))
    
    (if (not tool-spec)
        `(error :code -32602 
                :message "Invalid params - Unknown tool"
                :data ((tool-name . ,tool-name)
                       (available-tools . ,(mapcar #'car emacs-mcp-tools))))
      
      (let* ((handler (alist-get 'handler tool-spec))
             (result (emacs-mcp--call-tool-with-timeout handler arguments)))
        (if (and (listp result) (eq (car result) 'error))
            result
          `((content . [((type . "text")
                        (text . ,(if (stringp result)
                                    result
                                  (json-encode result))))])))))))

;; JSON-RPC 2.0 protocol support

(defun emacs-mcp--make-jsonrpc-success-response (id result)
  "Create a JSON-RPC 2.0 success response."
  `((jsonrpc . "2.0")
    (id . ,id)
    (result . ,result)))

(defun emacs-mcp--make-jsonrpc-error-response (id error-code error-message &optional error-data)
  "Create a JSON-RPC 2.0 error response."
  (let ((error-obj `((code . ,error-code)
                    (message . ,error-message)
                    ,@(when error-data `((data . ,error-data))))))
    `((jsonrpc . "2.0")
      (id . ,id)
      (error . ,error-obj))))

(defun emacs-mcp--make-jsonrpc-error-object (error-code error-message &optional error-data)
  "Create a JSON-RPC 2.0 error object."
  `((code . ,error-code)
    (message . ,error-message)
    ,@(when error-data `((data . ,error-data)))))

;; HTTP protocol support

(defun emacs-mcp--parse-http-request (request-string)
  "Parse HTTP REQUEST-STRING into components."
  (let* ((lines (split-string request-string "\r?\n"))
         (request-line (car lines))
         (headers '())
         (body-start nil)
         (body ""))
    
    (unless (string-match "^\\(GET\\|POST\\) \\([^ ]*\\) HTTP/1\\.[01]" request-line)
      (error "Invalid HTTP request line: %s" request-line))
    
    (let ((method (match-string 1 request-line))
          (path (let ((p (match-string 2 request-line)))
                  (if (or (null p) (string-empty-p p)) "/" p))))
      
      (dolist (line (cdr lines))
        (if (string-empty-p line)
            (setq body-start t)
          (if body-start
              (setq body (concat body line "\n"))
            (when (string-match "^\\([^:]+\\):\\s-*\\(.+\\)$" line)
              (push (cons (downcase (match-string 1 line))
                         (match-string 2 line))
                    headers)))))
      
      (list :method method
            :path path
            :headers (nreverse headers)
            :body (string-trim body)))))

(defun emacs-mcp--send-http-response (proc status headers body)
  "Send HTTP response to PROC with STATUS, HEADERS, and BODY."
  (let* ((header-string (mapconcat (lambda (header)
                                    (format "%s: %s\r\n" (car header) (cdr header)))
                                  headers ""))
         (response (concat
                   (format "HTTP/1.1 %s\r\n" status)
                   header-string
                   (format "Content-Length: %d\r\n" (length body))
                   "\r\n"
                   body)))
    (process-send-string proc response)))

(defun emacs-mcp--send-jsonrpc-response (proc json-obj)
  "Send JSON response to HTTP process."
  (let ((json-string (json-encode json-obj)))
    (emacs-mcp--send-http-response proc "200 OK"
                                  '(("Content-Type" . "application/json; charset=utf-8")
                                    ("Connection" . "close"))
                                  json-string)
    (delete-process proc)))

(defun emacs-mcp--send-jsonrpc-error-response (proc error-code error-message &optional error-data id)
  "Send a proper JSON-RPC 2.0 error response."
  (let* ((response-obj (emacs-mcp--make-jsonrpc-error-response (or id :null) error-code error-message error-data))
         (json-string (json-encode response-obj)))
    (emacs-mcp--send-http-response proc "400 Bad Request"
                                  '(("Content-Type" . "application/json; charset=utf-8"))
                                  json-string)))

;; timeout support

(defun emacs-mcp--set-connection-timeout (proc)
  "Set a timeout for connection PROC."
  (run-with-timer emacs-mcp-connection-timeout nil
                  (lambda ()
                    (when (process-live-p proc)
                      (message "Connection timeout for process %s" proc)
                      (delete-process proc)))))

(defun emacs-mcp--call-tool-with-timeout (handler arguments)
  "Call HANDLER with ARGUMENTS with timeout protection."
  (with-timeout (emacs-mcp-tool-timeout
                 '(error :code -32603 :message "Tool execution timeout"))
    (funcall handler arguments)))

;; other helpers

;; TODO
;; (defun emacs-mcp--validate-origin (headers)
;;   "Validate Origin header against allowed origins per MCP spec."
;;   (let ((origin (alist-get "origin" headers)))
;;     (and origin  ; Origin header is required by MCP spec
;;          (or (member origin emacs-mcp-allowed-origins)
;;              (string-match-p "^https?://\\(localhost\\|127\\.0\\.0\\.1\\)\\(:[0-9]+\\)?$" origin)))))

(defun emacs-mcp--handle-empty-body (proc)
  "Handle request with empty body."
  (emacs-mcp--send-jsonrpc-error-response proc -32700 "Parse error - Empty request body")
  (delete-process proc))

(defun emacs-mcp--handle-invalid-json (proc)
  "Handle request with invalid JSON."
  (emacs-mcp--send-jsonrpc-error-response proc -32700 "Parse error - Invalid JSON")
  (delete-process proc))

(provide 'emacs-mcp)
;;; emacs-mcp.el ends here
