;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTTP SERVER
;;
;; Based on "echo server" from the EmacsWiki:
;; https://www.emacswiki.org/emacs/EmacsEchoServer

(defvar http-server-port 5000
  "port of the http server")

(defvar http-server-clients '() 
  "alist where KEY is a client process and VALUE is the string")

(defun status (num msg)
  (format "HTTP/1.1 %d %s\r\n\r\n" num msg))

(defun keys (plist)
  "Get the keys of a plist"
  (loop for key in plist by 'cddr
	collecting key))

(defvar routes
  `(("/redirect" .
     (GET  ,(status 301 "Moved")
      HEAD ,(status 405 "Not Allowed")))

    ("/method_options" .
     (OPTIONS ,(status 200 "OK"))))
  "Alist of routes that the server will handle")

(defun find-route (path method)
  (let ((route (assoc path routes))) ; look for the route
    (if route
	(let ((meth (getf (cdr route) method))) ; route supports method?
	  (if meth
	      meth
	    (status 405 "Not Allowed")))
      (status 404 "Not Found"))))

(defun http-server-start nil
  "starts an emacs http server"
  (interactive)
  (unless (process-status "http-server")
    (make-network-process
     :name "http-server"
     :buffer "*http-server*"
     :family 'ipv4
     :service http-server-port
     :sentinel 'http-server-sentinel
     :filter 'http-server-filter
     :server 't) 
    (setq http-server-clients '())
    (print "Server started...")))

(defun http-server-stop nil
  "stop an emacs http server"
  (interactive)
  (while  http-server-clients
    (delete-process (car (car http-server-clients)))
    (setq http-server-clients (cdr http-server-clients)))
  (delete-process "http-server")
  (print "Server stopped."))

(defun handle-http-request (request)
  "Handles an HTTP request. Return value is the response sent to the client"
  "HTTP/1.1 200 OK\r\n\r\n")

(defun http-server-filter (proc string)
  (let ((resp (handle-http-request string)))
    (http-server-log string)
    (http-server-log resp)
    (process-send-string proc resp)
    (delete-process proc)))

(defun http-server-sentinel (proc msg)
  (when (string= msg "connection broken by remote peer\n")
    (setq http-server-clients (assq-delete-all proc http-server-clients))
    (http-server-log (format "client %s has quit" proc))))

;;from server.el
(defun http-server-log (string &optional client)
  "If a *http-server* buffer exists, write STRING to it for logging purposes."
  (if (get-buffer "*http-server*")
      (with-current-buffer "*http-server*"
        (goto-char (point-max))
        (insert (current-time-string)
                (if client (format " %s: " client) " ")
                string)
        (or (bolp) (newline)))))
