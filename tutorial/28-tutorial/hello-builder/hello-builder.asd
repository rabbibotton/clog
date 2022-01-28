;;;; hello-builder

(asdf:defsystem #:hello-builder
  :description "Hello Builder - Easy apps with CLOG Builder"

  :author "david@botton.com"
  :license  "BSD"
  :version "0.0.0"
  :serial t
  :depends-on (#:clog)
  :components ((:file "hello-builder")
	       (:file "hello")))

