;; RFC 858: Suppress Go Ahead Option
;;

(in-package :telnet)

(defconstant +SUPPRESS-GO-AHEAD+ (code-char 3))

(defmethod telnet-process-will ((stream telnet-stream) (option (eql +SUPPRESS-GO-AHEAD+)))
  (telnet-do stream option))

(defmethod telnet-process-do ((stream telnet-stream) (option (eql +SUPPRESS-GO-AHEAD+)))
  (telnet-will stream option)
  (setf (telnet-suppress-go-ahead? stream) t))

(defmethod telnet-process-dont ((stream telnet-stream) (option (eql +SUPPRESS-GO-AHEAD+)))
  (telnet-wont stream option)
  (setf (telnet-suppress-go-ahead? stream) nil))

(defmethod stream-read-char :around ((stream telnet-stream))
 (let ((c (read-char-no-hang (telnet-stream stream))))
    (when (and (null c)
               (telnet-suppress-go-ahead? stream))
      (peek-char nil (telnet-stream stream)))
    (when c
      (unread-char c (telnet-stream stream)))
    (call-next-method stream)))
