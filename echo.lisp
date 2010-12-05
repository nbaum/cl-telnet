;; RFC 857: TELNET ECHO OPTION
;; 
;; When ECHO is enabled, the server will echo the client's input back.

(in-package :telnet)

(defconstant +ECHO+ (code-char 1))

(defmethod telnet-process-do ((stream telnet-stream) (option (eql +ECHO+)))
  (telnet-will stream option)
  (setf (telnet-echoing? stream) t))

(defmethod telnet-process-dont ((stream telnet-stream) (option (eql +ECHO+)))
  (telnet-wont stream option)
  (setf (telnet-echoing? stream) nil))

(defmethod stream-read-char :around ((stream telnet-stream))
  (let ((c (call-next-method)))
    (when (telnet-echoing? stream)
      (write-char c stream))
    c))
