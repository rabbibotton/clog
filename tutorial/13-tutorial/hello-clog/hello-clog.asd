;;;; hello-clog

(asdf:defsystem #:hello-clog
  :description "Hello Clog - Flying Solo"

  :author "someone@someplace.com"
  :license  "BSD"
  :version "0.0.0"
  :serial t
  :depends-on (#:clog)
  :components ((:file "hello-clog")))

