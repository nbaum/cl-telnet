
;; RFC 854: Telnet Protocol

;; This file implements the Telnet protocol. Several other files
;; implement useful Telnet options.

;; Options implemented:
;;
;;   Option        File
;;   ECHO          echo.lisp
;;   NAWS          naws.lisp
;;   TERMINAL-TYPE terminal-type.lisp

(in-package :telnet)

(defconstant +se+        (code-char 240))
(defconstant +nop+       (code-char 241))
(defconstant +data-mark+ (code-char 242))
(defconstant +break+     (code-char 243))
(defconstant +ip+        (code-char 244))
(defconstant +ao+        (code-char 245))
(defconstant +ayt+       (code-char 246))
(defconstant +ec+        (code-char 247))
(defconstant +el+        (code-char 248))
(defconstant +ga+        (code-char 249))
(defconstant +sb+        (code-char 250))
(defconstant +will+      (code-char 251))
(defconstant +wont+      (code-char 252))
(defconstant +do+        (code-char 253))
(defconstant +dont+      (code-char 254))
(defconstant +iac+       (code-char 255))

(defconstant +SEND+ (code-char 1))
(defconstant +IS+ (code-char 0))

(defclass telnet-stream (fundamental-character-output-stream
                         fundamental-character-input-stream)
  ((stream :initform nil :initarg :stream :accessor telnet-stream)
   (echoing? :initform nil :accessor telnet-echoing?)
   (width :initform nil :accessor telnet-width)
   (height :initform nil :accessor telnet-height)
   (suppress-go-ahead? :initform nil :accessor telnet-suppress-go-ahead?)))

(defgeneric telnet-process-command (stream command))
(defgeneric telnet-process-negotiation (stream command parameter))

(defgeneric telnet-process-will (stream option))
(defgeneric telnet-process-wont (stream option))
(defgeneric telnet-process-do   (stream option))
(defgeneric telnet-process-dont (stream option))

(defmethod stream-read-char ((stream telnet-stream))
  (let ((c (read-char-no-hang (telnet-stream stream))))
    (cond
     ((null c)
      (telnet-send-command stream +ga+)
      (finish-output (telnet-stream stream))
      (peek-char nil (telnet-stream stream))
      (stream-read-char stream))
     ((eql c +IAC+)
      ;; If we read an IAC, go deal with it.  If
      ;; process-telnet-command returns something, return that;
      ;; otherwise read the next character.
      (let ((c (telnet-process-command stream (read-char (telnet-stream stream)))))
        (or c
            (stream-read-char stream))))
     ((eql c #\Return)
      ;; Ignore CRs.
      (stream-read-char stream))
     (t
      c))))

(defmethod stream-write-char ((stream telnet-stream) char)
  (case char
    ((+IAC+ )
     (write-char +IAC+ (telnet-stream stream))
     (write-char +IAC+ (telnet-stream stream)))
    ((#\Newline)
     (write-char #\Return (telnet-stream stream))
     (write-char #\Newline (telnet-stream stream)))
    ((#\Return))
    (t
     (write-char char (telnet-stream stream)))))

(defmethod stream-finish-output ((stream telnet-stream))
  (finish-output (telnet-stream stream)))

(defvar *negotiating-suboption?* nil)

(defun telnet-send-command (stream &rest args)
  (format t "~A~%" (mapcar #'char-code args))
  (format (telnet-stream stream) "~A~{~A~}" +IAC+ args))

(defun telnet-negotiate (stream option &rest args)
  (format (telnet-stream stream) "~A~A~A~{~A~}~A~A" +IAC+ +SB+ option args +IAC+ +SE+))

(defun telnet-will (stream option)
  (telnet-send-command stream +WILL+ option))

(defun telnet-wont (stream option)
  (telnet-send-command stream +WONT+ option))

(defun telnet-do (stream option)
  (telnet-send-command stream +do+ option))

(defun telnet-dont (stream option)
  (telnet-send-command stream +dont+ option))

(defmethod telnet-process-will (stream option)
  (telnet-dont stream option))

(defmethod telnet-process-do (stream option)
  (telnet-wont stream option))

(defmethod telnet-process-command (stream (command (eql +IAC+)))
  command)

(defmethod telnet-process-command (stream (command (eql +WILL+)))
  (telnet-process-will stream (read-char (telnet-stream stream))))

(defmethod telnet-process-command (stream (command (eql +AYT+)))
  (format stream "/================[ AYT Received ]============\\~%~
                  | You talkin' to me?                         |~%~
                  | You talkin' to me?                         |~%~
                  | Then who the hell else are you talkin' to? |~%~
                  | You talkin' to me?                         |~%~
                  | Well I'm the only one here.                |~%~
                  \\============================================/~%"))

(defmethod telnet-process-command (stream (command (eql +DO+)))
  (telnet-process-do stream (read-char (telnet-stream stream))))

(defmethod telnet-process-command (stream (command (eql +SB+)))
  (let ((c (read-char (telnet-stream stream)))
        (*negotiating-suboption?* t))
    (with-output-to-string (parameter)
      (loop
       (let ((c (read-char stream)))
         (when (eq c :end-of-suboption)
           (return))
         (write-char c parameter)))
      (telnet-process-negotiation stream c (get-output-stream-string parameter)))))

(defmethod telnet-process-command (stream (command (eql +SE+)))
  (when *negotiating-suboption?*
    :end-of-suboption))

(define-condition telnet-condition (condition)
  ((stream :initarg :stream :reader telnet-condition-stream)))

(define-condition telnet-warning (warning)
  ((stream :initarg :stream :reader telnet-warning-stream)))

(define-condition telnet-interrupt-condition (telnet-condition)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Telnet session interrupted by remote host"))))

(defmethod telnet-process-command (stream (command (eql +IP+)))
  (error (make-condition 'telnet-interrupt-condition :stream stream)))
