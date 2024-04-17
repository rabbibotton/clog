;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2024 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog.asd                                                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(asdf:defsystem #:clog
  :description "CLOG - The Common Lisp Omnificent GUI"
  :author "David Botton <david@botton.com>"
  :license  "BSD"
  :version "1.9.0"
  :serial t
  :depends-on (#:clack #:websocket-driver #:alexandria #:hunchentoot #:cl-ppcre
                       #:bordeaux-threads #:trivial-open-browser #:parse-float #:quri
                       #:lack-middleware-static #:lack-request #:lack-util-writer-stream
                       #:trivial-gray-streams #:closer-mop #:mgl-pax #:cl-template #:atomics
                       #:cl-indentify
                       #:sqlite #:cl-dbi #:cl-pass #-(or mswindows win32 cormanlisp) #:cl-isaac)
  :components ((:module "static-files"
                :components ((:static-file "js/boot.js")))
               (:module "source"
                :components (;; ASDF Extension for CLOG Panel files
                             (:file "asdf-ext")
                             ;; Connectivity
                             (:file "clog-connection")
                             (:file "clog-connection-websockets")
                             ;; CLOG Framework
                             (:file "clog")
                             (:file "clog-system")
                             (:file "clog-utilities")
                             ;; Base System
                             (:file "clog-base")
                             (:file "clog-element")
                             (:file "clog-jquery")
                             ;; DOM Elements
                             (:file "clog-body")
                             (:file "clog-document")
                             (:file "clog-window")
                             (:file "clog-location")
                             (:file "clog-navigator")
                             (:file "clog-style")
                             ;; HTML Elements
                             (:file "clog-element-common")
                             (:file "clog-form")
                             (:file "clog-multimedia")
                             (:file "clog-canvas")
                             (:file "clog-webgl")
                             ;; CLOG Extensions
                             (:file "clog-panel")
                             (:file "clog-presentations")
                             (:file "clog-data")
                             (:file "clog-dbi")
                             (:file "clog-auth")
                             ;; W3CSS Bindings
                             (:file "clog-web")
                             (:file "clog-web-dbi")
                             (:file "clog-web-themes")
                             ;; Desktop Environment
                             (:file "clog-gui")
                             ;; CLOG Programming Tools
                             (:file "clog-helpers")))))

(asdf:defsystem #:clog/docs
  :depends-on (#:clog #:3BMD #:colorize #:print-licenses)
  :pathname "source/"
  :components (;; CLOG documentation creation utils and additional documentation
               ;; use (print-licenses:print-licenses :print-licenses) to check
               ;; dependency licenses.
               (:file "clog-docs")))

(asdf:defsystem #:clog/tools
  :depends-on (#:clog #:clog-ace #:clog-terminal #:s-base64 #:swank
               #:definitions #:parenscript)
  :pathname "tools/"
  :components (;; clog-db-admin app
               (:file "clog-db-admin")
               ;; clog-builder code
               (:file "clog-builder-api")
               (:file "clog-builder-settings")
               (:file "clog-builder-settings-controls")
               (:file "clog-builder")
               (:file "clog-builder-control-events")
               (:file "clog-builder-control-properties")
               (:file "clog-builder-control-list")
               (:file "clog-builder-eval")
               (:file "clog-builder-files")
               (:file "clog-builder-panels")
               (:file "clog-builder-render")
               (:file "clog-builder-ace")
               (:file "clog-builder-templates")
               (:file "clog-builder-projects")
               (:file "clog-builder-asdf-browser")               
               (:file "clog-builder-sys-browser")
               (:file "clog-builder-dir-win")
               (:file "clog-builder-repl")
               (:file "clog-builder-shell")
               (:file "clog-builder-images")
               (:file "preferences-tabs")
               ;; clog-builder panels (post-render)
               (:file "panel-clog-templates")
               (:file "panel-image-to-data")
               (:file "panel-quick-start")
               (:file "panel-threads")
               (:file "panel-systems")
               (:file "panel-sys-browser")
               (:file "panel-projects")
               (:file "panel-project-directory")
               (:file "panel-clog-builder-repl")
               (:file "panel-shell")
               (:file "panel-dir-view")))
