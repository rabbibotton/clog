(defpackage #:test-clog-connection
  (:use #:cl)
  (:export test on-connect))

(in-package :test-clog-connection)

(defun on-connect (id)
  (format t "Connection ~A is valid? ~A~%" id (clog-connection:validp id))
  (dotimes (n 10)
    (clog-connection:put id "<b>connection-write</b>")
    (clog-connection:put-line id "<i>connection-writeln</i>")
    (sleep .2))
  (clog-connection:put id "<br><b>Query Result : </b>")
  (clog-connection:put-line id (clog-connection:query id "navigator.appVersion"))
  (clog-connection:new-line id)
  (clog-connection:put id "<hr>simulate network interupt")
  (clog-connection:cclose id)
  (sleep .2)
  (clog-connection:put id "<br><b>reconnected</b>")
  (sleep .2)
  (clog-connection:put id "<br><b>shutting down connection</b>")
  (sleep .2)
  ;; It is generally uneccessary to shutdown the connection
  (clog-connection:shutdown id))

(defun test ()
  (print "Init connection")
  (clog-connection:initialize #'on-connect
                              :static-root (merge-pathnames "./static-files/"
                                             (asdf:system-source-directory :clog))
                              :boot-file "/debug.html")
  (print "Open browser")
  (clog:open-browser)
)
