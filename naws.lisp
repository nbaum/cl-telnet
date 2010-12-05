;; RFC 1073: Telnet Window Size Option
;;
;; The server will respond to notifications about the client's window
;; size. Since the server itself doesn't have a window size, it won't
;; send a report of that itself.

(in-package :telnet)

(defconstant +NAWS+ (code-char 31))

(defmethod telnet-process-negotiation ((stream telnet-stream) (option (eql +NAWS+)) parameter)
  (setf (telnet-width stream) (+ (* 256 (char-code (elt parameter 0)))
                           (char-code (elt parameter 1)))
        (telnet-height stream) (+ (* 256 (char-code (elt parameter 2)))
                           (char-code (elt parameter 3)))))

(defmethod telnet-process-will ((stream telnet-stream) (option (eql +NAWS+)))
  (telnet-do stream option))
