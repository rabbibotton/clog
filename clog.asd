;;;; clog.asd

(asdf:defsystem #:clog
  :description "The Common Lisp Omnificent GUI"
  :author "David Botton <david@botton.com>"
  :license  "BSD"
  :version "1.6.0"
  :serial t
  :depends-on (#:clack #:websocket-driver #:alexandria #:hunchentoot #:cl-ppcre
                       #:bordeaux-threads #:trivial-open-browser #:parse-float #:quri
                       #:lack-middleware-static #:lack-request #:lack-util-writer-stream
                       #:closer-mop #:mgl-pax #:cl-template #:atomics
                       #:sqlite #:cl-dbi #:cl-pass #-(or mswindows win32 cormanlisp) #:cl-isaac)
  :components ((:module "static-files"
                :components ((:static-file "js/boot.js")))
               (:module "source"
                :components ((:file "asdf-ext")
                             (:file "clog-connection")
                             (:file "clog")
                             (:file "clog-utilities")
                             (:file "clog-base")
                             (:file "clog-element")
                             (:file "clog-jquery")
                             (:file "clog-element-common")
                             (:file "clog-style")
                             (:file "clog-canvas")
                             (:file "clog-webgl")
                             (:file "clog-form")
                             (:file "clog-multimedia")
                             (:file "clog-window")
                             (:file "clog-document")
                             (:file "clog-location")
                             (:file "clog-navigator")
                             (:file "clog-body")
                             (:file "clog-system")
                             (:file "clog-panel")
                             (:file "clog-presentations")
                             (:file "clog-data")
                             (:file "clog-dbi")
                             (:file "clog-auth")
                             (:file "clog-gui")
                             (:file "clog-web")
                             (:file "clog-web-dbi")
                             (:file "clog-web-themes")
                             (:file "clog-helpers")))))

(asdf:defsystem #:clog/docs
  :depends-on (#:clog #:3BMD #:colorize)
  :pathname "source/"
  :components ((:file "clog-docs")))

(asdf:defsystem #:clog/tools
  :depends-on (#:clog #:clog-ace #:clog-terminal #:s-base64 #:swank
               #:definitions)
  :pathname "tools/"
  :components (;; clog-db-admin app
               (:file "clog-db-admin")
               ;; clog-builder generated clode
               (:file "clog-templates")
               (:file "image-to-data")
               (:file "quick-start")
               (:file "threads")
               (:file "systems")
               (:file "sys-browser")
               (:file "projects")
               (:file "project-directory")
               (:file "clog-builder-repl")
               (:file "dir-view")
               ;; clog-builder code
               (:file "clog-builder-settings")
               (:file "clog-builder")
               (:file "clog-builder-templates")
               (:file "clog-builder-projects")
               (:file "clog-builder-asdf-browser")               
               (:file "clog-builder-sys-browser")
               (:file "clog-builder-dir-win")
               (:file "clog-builder-images")))
