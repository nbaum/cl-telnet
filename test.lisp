
(require :telnet)

(telnet:with-server (server 1701)
  (telnet:with-client (client stream server)
    (format stream "Hello. What is your name?~%")
    (let ((name (read-line stream)))
      (format stream "Hello ~A. Would you like some onions?~%" name)
      (let ((onion-mad (read-line stream)))
        
        ))))

(quit)
