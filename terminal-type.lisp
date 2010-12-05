
;; RFC 1091: Telnet Terminal-Type Option
;;
;; The server will respond to notifications about the client's window
;; size. Since the server itself doesn't have a window size, it won't
;; send a report of that itself.

(in-package :telnet)

(defconstant +TERMINAL-TYPE+ (code-char 24))

(defmethod telnet-process-will ((stream telnet-stream) (option (eql +TERMINAL-TYPE+)))
  (telnet-do stream option)
  (telnet-negotiate stream +TERMINAL-TYPE+ +SEND+))

(defmethod telnet-process-negotiation ((stream telnet-stream) (option (eql +TERMINAL-TYPE+)) parameter)
  (let ((parameter (subseq parameter 1)))
    (format stream "~A~%" parameter)))
