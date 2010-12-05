
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-bsd-sockets)
  (require :cl-ppcre)
  (require :split-sequence)
  (require :parse-number))

(defpackage :telnet
  (:use :common-lisp :sb-bsd-sockets :cl-ppcre :parse-number :split-sequence :sb-gray :sb-thread)
  (:export :with-server :with-client))
