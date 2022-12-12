;;;; hello-builder

(asdf:defsystem #:hello-builder
  :description "Hello Builder - Easy apps with CLOG Builder"

  :author "david@botton.com"
  :license  "BSD"
  :version "0.0.0"
  :serial t
  :depends-on (#:clog)
  :entry-point "hello-builder:start-app"
  :components ((:file "hello-builder")
               (:file "hello")))

(asdf:defsystem #:hello-builder/tools
  :defsystem-depends-on (:clog)
  :depends-on (#:hello-builder #:clog/tools) ; add clog plugins here as #:plugin/tools for design time
  :components ((:clog-file "hello")))
