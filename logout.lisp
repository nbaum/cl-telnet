;; RFC 727: Logout Option

(in-package :telnet)

(defconstant +LOGOUT+ (code-char 18))

(define-condition telnet-logout-warning (telnet-warning)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Remote host asks you to stop"))))

(defmethod telnet-process-do ((stream telnet-stream) (option (eql +LOGOUT+)))
  (telnet-will stream option)
  (warn (make-condition 'telnet-logout-warning :stream stream)))
