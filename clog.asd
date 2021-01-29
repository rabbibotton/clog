;;;; clog.asd

(asdf:defsystem #:clog
  :description "The Common Lisp Omnificent GUI"

  :author "David Botton <david@botton.com>"
  :license  "BSD"
  :version "0.8.0"
  :serial t
  :depends-on (#:clack #:websocket-driver #:alexandria #:hunchentoot #:cl-ppcre
		       #:bordeaux-threads #:trivial-open-browser
		       #:lack-middleware-static #:lack-middleware-session
                       #:mgl-pax #:quri)
  :components ((:file "clog-connection")
	       (:file "clog")
	       (:file "clog-docs")
	       (:file "clog-system")
	       (:file "clog-utilities")
	       (:file "clog-base")
	       (:file "clog-element")
	       (:file "clog-element-common")
	       (:file "clog-canvas")
	       (:file "clog-form")
	       (:file "clog-multimedia")
	       (:file "clog-window")
	       (:file "clog-document")
	       (:file "clog-location")
	       (:file "clog-navigator")	       
	       (:file "clog-body")
	       (:file "clog-helpers")))
