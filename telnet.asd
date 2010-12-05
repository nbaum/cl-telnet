
(defsystem "telnet"
  :description "telnet: a Telnet client/server library."
  :version "1.0"
  :author "Nathan Baum <nathan_baum@btinternet.com>"
  :licence "GPL"
  :components
  ((:system "cl-ppcre")
   (:file "package" :depends-on ("cl-ppcre"))
   (:file "telnet-stream" :depends-on ("package"))
   (:file "echo" :depends-on ("telnet-stream"))
   (:file "naws" :depends-on ("telnet-stream"))
   (:file "terminal-type" :depends-on ("telnet-stream"))
   (:file "suppress-go-ahead" :depends-on ("telnet-stream"))
   (:file "logout" :depends-on ("telnet-stream"))
   (:file "server" :depends-on ("telnet-stream"))))
