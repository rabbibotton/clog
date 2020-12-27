;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2021 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog.lisp                                                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports - clog
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mgl-pax:define-package :clog
  (:documentation "The Common List Omnificent GUI - Parent package")  
  (:local-nicknames (:cc :clog-connection))
  (:use #:cl #:mgl-pax))

(in-package :clog)

(defsection @clog-manual (:title "The CLOG manual")
  "The Common Lisp Omnificient GUI, CLOG for short, uses web technology
to produce graphical user interfaces for applications locally or
remotely. The CLOG package starts up the connectivity to the browser
or other websocket client (often a browser embedded in a native
application."

  (clog asdf:system)

  (@clog-system    section)
  (@clog-utilities section)
  (@clog-obj       section)
  (@clog-body      section)
  (@clog-window    section)
  (@clog-document  section)
  (@clog-location  section)
  (@clog-navigator section))

(defsection @clog-system (:title "CLOG System")
  "CLOG Startup and Shutdown"
  (initialize function)
  (shutdown   function))

(defsection @clog-utilities (:title "CLOG Utilities")
  "CLOG utilities"
  (js-true-p    function)
  (open-browser function))

(defsection @clog-obj (:title "CLOG Objects")
  "CLOG-Obj - Base class for CLOG Objects"
  (clog-obj class)

  "CLOG-Obj - General Properties"
  (property  generic-function)
  (style     generic-function)
  (attribute generic-function)
  
  "CLOG-Obj - General Methods"
  (height generic-function)
  (width  generic-function)
  (focus  generic-function)
  (blur   generic-function)
  
  "CLOG-Obj - Placement"
  (place-after            generic-function)
  (place-before           generic-function)
  (place-inside-top-of    generic-function)
  (place-inside-bottom-of generic-function)

  "CLOG-Obj - Low Level"
  (create-child    generic-function)
  (attach-as-child generic-function)
  (connection-data generic-function)
  (validp          generic-function)

  "CLOG-Obj - Event Handling"
  (set-on-resize             generic-function)
  (set-on-focus              generic-function)
  (set-on-blur               generic-function)
  (set-on-change             generic-function)
  (set-on-focus-in           generic-function)
  (set-on-focus-out          generic-function)
  (set-on-reset              generic-function)
  (set-on-search             generic-function)
  (set-on-select             generic-function)
  (set-on-submit             generic-function)
  (set-on-select             generic-function)
  (set-on-context-menu       generic-function)
  (set-on-click              generic-function)
  (set-on-double-click       generic-function)
  (set-on-mouse-click        generic-function)
  (set-on-mouse-double-click generic-function)
  (set-on-mouse-right-click  generic-function)
  (set-on-mouse-enter        generic-function)
  (set-on-mouse-leave        generic-function)
  (set-on-mouse-over         generic-function)
  (set-on-mouse-out          generic-function)
  (set-on-mouse-down         generic-function) 
  (set-on-mouse-up           generic-function)
  (set-on-mouse-move         generic-function)
  (set-on-character          generic-function)
  (set-on-key-down           generic-function)
  (set-on-key-up             generic-function)
  (set-on-key-press          generic-function)
  (set-on-copy               generic-function)
  (set-on-cut                generic-function)
  (set-on-paste              generic-function))
;; need to add drag and drop events

(defsection @clog-body (:title "CLOG Body Objects")
  "CLOG-Body - CLOG Body Objects"
  (clog-body class)

  "CLOG-Body - Properties"
  (window        generic-function)
  (html-document generic-function)
  (location      generic-function)
  (navigator     generic-function))

(defsection @clog-window (:title "CLOG Window Objects")
  "CLOG-Window - CLOG Window Objects"
  (clog-window class)

  "CLOG-Window - Properties"
  (window-name  generic-function)
  (inner-height generic-function)
  (inner-width  generic-function)
  (outer-height generic-function)
  (outer-width  generic-function)
  (x-offset     generic-function)
  (y-offset     generic-function)
  (left         generic-function)
  (top          generic-function)

  "CLOG-Window - Methods"
  (alert            generic-function)
  (log-console      generic-function)
  (log-error        generic-function)
  (print-window     generic-function)
  (scroll-by        generic-function)
  (scroll-to        generic-function)
  ;;(close-window     generic-function)
  ;;(close-connection generic-function)

  "CLOG-Window - Events"
  ;;  (set-on-abort              generic-function)
  ;;  (set-on-error              generic-function)
  ;;  (set-on-before-unload      generic-function)
  ;;  (set-on-hash-change        generic-function)
  ;;  (set-on-orientation-change generic-function)
  ;;  (set-on-storage            generic-function)

  ;; These have no use in most modern browsers
  ;; (move-by    generic-function)
  ;; (move-to    generic-function)
  ;; (resize-by  generic-function)
  ;; (resize-to  generic-function)
  )

(defsection @clog-document (:title "CLOG Document Objects")
  "CLOG-Document - CLOG Document Objects"
  (clog-document class))

(defsection @clog-location (:title "CLOG Location Objects")
  "CLOG-Location - CLOG Location Objects"
  (clog-location class))

(defsection @clog-navigator (:title "CLOG Navigator Objects")
  "CLOG-Navigator - CLOG Navigator Objects"
  (clog-navigator class))

(defsection @clog-location (:title "CLOG Location Objects")
  "CLOG-Location - CLOG Location Objects"
  (clog-location class))

(export 'make-markup)
(defun make-markup ()
  (load "clog.lisp")
  (load "clog-base.lisp")
  (load "clog-window.lisp")
  (load "clog-navigator.lisp")
  (load "clog-document.lisp")
  (load "clog-location.lisp")
  (load "clog-system.lisp")
  (load "clog-utilities.lisp")
  (load "clog-body.lisp")
  (describe clog:@CLOG-MANUAL))
  
(export 'make-html)
(defun make-html ()
  (load "clog.lisp")
  (load "clog-base.lisp")
  (load "clog-window.lisp")
  (load "clog-navigator.lisp")
  (load "clog-document.lisp")
  (load "clog-location.lisp")
  (load "clog-system.lisp")
  (load "clog-utilities.lisp")
  (load "clog-body.lisp")
  (mgl-pax:update-asdf-system-html-docs clog:@CLOG-MANUAL :clog))

