;;;; clog.asd

(asdf:defsystem #:clog
  :description "The Common Lisp Omnificent GUI"

  :author "David Botton <david@botton.com>"
  :license  "BSD"
  :version "0.0.1"
  :serial t
  :depends-on (#:clack #:websocket-driver #:alexandria #:hunchentoot #:cl-ppcre
		       #:bordeaux-threads #:trivial-open-browser
		       #:lack-middleware-static #:lack-middleware-session
                       #:mgl-pax)
  :components ((:file "clog-connection")
	       (:file "clog")))
