
(in-package :telnet)

(defun make-server (&key (host #(127 0 0 1))
                         (port (error "Server port not specified."))
                         (backlog 10))
  (let ((socket (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (setf (sockopt-reuse-address socket) t)
    (socket-bind socket host port)
    (socket-listen socket backlog)
    socket))

(defmacro with-open-socket ((socket) &body forms)
  `(unwind-protect
       (progn
         ,@forms)
     (socket-close ,socket)))

(defmacro with-server ((name port &rest args) &body forms)
  `(let ((,name (make-server :port ,port ,@args)))
     (with-open-socket (,name)
       ,@forms)))

(defmacro with-client ((socket stream server) &body forms)
  `(let ((,socket (socket-accept ,server)))
     (with-open-socket (,socket)
        (with-open-stream (,stream
                           (make-instance 'telnet-stream
                                          :stream (socket-make-stream ,socket
                                                                      :input t
                                                                      :output t
                                                                      :element-type :default
                                                                      :external-format :iso-8859-1)))
          ,@forms))))

(defvar *server* nil)
