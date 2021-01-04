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
  (@clog-element   section)
  (@clog-body      section)
  (@clog-window    section)
  (@clog-document  section)
  (@clog-location  section)
  (@clog-navigator section))

(defsection @clog-system (:title "CLOG System")
  "CLOG Startup and Shutdown"
  (initialize        function)
  (set-on-new-window function)
  (shutdown          function))

(defsection @clog-utilities (:title "CLOG Utilities")
  "CLOG utilities"
  (js-true-p     function)
  (p-true-js     function)
  (open-browser  function)
  (escape-string function))

(defsection @clog-obj (:title "CLOG Objects")
  "CLOG-Obj - Base class for CLOG Objects"
  (clog-obj class)

  "CLOG-Obj - General Properties"
  (property  generic-function)
  
  "CLOG-Obj - General Methods"
  (height generic-function)
  (width  generic-function)
  (focus  generic-function)
  (blur   generic-function)
  
  "CLOG-Obj - Low Level"
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

(defsection @clog-element (:title "CLOG Elements")
  "CLOG-Element - Base class for CLOG Elements"
  (clog-element class)

  "CLOG-Element - Low Level Creation"
  (create-child    generic-function)
  (attach-as-child generic-function)

  "CLOG-Element - Placement"
  (place-after            generic-function)
  (place-before           generic-function)
  (place-inside-top-of    generic-function)
  (place-inside-bottom-of generic-function)

  "CLOG-Element - General Properties"
  (style     generic-function)
  (attribute generic-function)

  "CLOG-Element - Properties"
  (access-key          generic-function)
  (advisory-title      generic-function)
  (class-name          generic-function)
  (editablep           generic-function)
  (draggablep          generic-function)
  (hiddenp             generic-function)
  (inner-html          generic-function)
  (outer-html          generic-function)
  (spellcheckp         generic-function)
  (tab-index           generic-function)
  (text                generic-function)
  (text-direction-type type)
  (text-direction      generic-function)
  (language-code       generic-function)
  (client-left         generic-function)
  (client-top          generic-function)
  (client-bottom       generic-function)
  (client-right        generic-function)
  (offset-left         generic-function)
  (offset-top          generic-function)
  (offset-bottom       generic-function)
  (offset-right        generic-function)
  (html-tag            generic-function)

  "CLOG-Element - Styles"
  (box-sizing-type        type)
  (box-sizing             generic-function)
  (clear-side-type        type)
  (clear-side             generic-function)
  (float-wrap-type        type)
  (float-wrap             generic-function)
  (display-type           type)
  (display                generic-function)
  (overflow-type          type)
  (overflow               generic-function)
  (overflow-x-type        type)
  (overflow-x             generic-function)
  (overflow-y-type        type)
  (overflow-y             generic-function)
  (z-index                generic-function)
  (resizable-type         type)
  (resizable              generic-function)
  (position-type          type)
  (positioning            generic-function)
  (position-top           generic-function)
  (position-left          generic-function)
  (offset-top             generic-function)
  (offset-left            generic-function)
  (left                   generic-function)
  (top                    generic-function)
  (right                  generic-function)
  (bottom                 generic-function)
  (box-height             generic-function)
  (box-width              generic-function)
  (maximum-height         generic-function)
  (maximum-width          generic-function)
  (minimum-height         generic-function)
  (minimum-width          generic-function)
  (visiblep               generic-function)
  (inner-height           generic-function)
  (inner-width            generic-function)
  (outer-height           generic-function)
  (outer-width            generic-function)
  (outer-height-to-margin generic-function)
  (outer-width-to-margin  generic-function)
  (color                  generic-function)
  (opacity                generic-function)
  (background-attachment  generic-function)
  (background-color       generic-function)
  (background-image       generic-function)
  (background-position    generic-function)
  (background-origin      generic-function)
  (background-repeat      generic-function)
  (background-clip        generic-function)
  (background-size        generic-function)
  (border                 generic-function)
  (border-radius          generic-function)
  (box-shadow             generic-function)
  (outline                generic-function)
  (margin                 generic-function)
  (padding                generic-function)
  (cursor                 generic-function)
  (font                   generic-function)
  (vertical-align-type    type)
  (vertical-align         generic-function)

  "CLOG-Element - Methods"
  (addClass        generic-function)
  (removeClass     generic-function)
  (toggleClass     generic-function)
  (remove-from-dom generic-function)
  (click           generic-function))

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
  (window-name             generic-function)
  (inner-height            generic-function)
  (inner-width             generic-function)
  (outer-height            generic-function)
  (outer-width             generic-function)
  (x-offset                generic-function)
  (y-offset                generic-function)
  (left                    generic-function)
  (top                     generic-function)
  (pixel-ratio             generic-function)
  (screen-width            generic-function)
  (screen-height           generic-function)
  (screen-available-width  generic-function)
  (screen-available-height generic-function)
  (screen-available-top    generic-function)
  (screen-available-left   generic-function)
  (screen-color-depth      generic-function)

  "CLOG-Window - Methods"
  (alert            generic-function)
  (log-console      generic-function)
  (log-error        generic-function)
  (print-window     generic-function)
  (scroll-by        generic-function)
  (scroll-to        generic-function)
  (close-window     generic-function)
  (close-connection generic-function)

  "CLOG-Window - Events"
  (set-on-abort              generic-function)
  (set-on-error              generic-function)
  (set-on-before-unload      generic-function)
  (set-on-hash-change        generic-function)
  (set-on-orientation-change generic-function)
  (set-on-storage            generic-function)
  (set-on-resize             generic-function)
  (move-by                   generic-function)
  (move-to                   generic-function)
  (resize-by                 generic-function)
  (resize-to                 generic-function))

(defsection @clog-document (:title "CLOG Document Objects")
  "CLOG-Document - CLOG Document Objects"
  (clog-document class)

  (domain           generic-function)
  (input-encoding   generic-function)
  (last-modified    generic-function)
  (referer          generic-function)
  (title            generic-function)
  (url              generic-function)
  (head-element     generic-function)
  (body-element     generic-function)
  (document-element generic-function)
  (ready-state      generic-function)
  (load-css         generic-function)
  (put              generic-function)
  (put-line         generic-function)
  (put-br           generic-function)
  (new-line         generic-function))

(defsection @clog-navigator (:title "CLOG Navigator Objects")
  "CLOG-Navigator - CLOG Navigator Objects"
  (clog-navigator class)

  "CLOG-Navigator - Properties"
  (cookie-enabled-p generic-function)
  (language         generic-function)
  (user-agent       generic-function)
  (vendor           generic-function))
  
(defsection @clog-location (:title "CLOG Location Objects")
  "CLOG-Location - CLOG Location Objects"
  (clog-location class)

  "CLOG-Location - Properties"
  (url         generic-function)
  (hash        generic-function)
  (host        generic-function)
  (host-name   generic-function)
  (origin      generic-function)
  (path-name   generic-function)
  (port        generic-function)
  (protocol    generic-function)
  (url-search  generic-function)
  
  "CLOG-Location - Methods"
  (reload      generic-function)
  (url-replace generic-function)
  (url-assign  generic-function))

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

(export 'make-world)
(defun make-world ()
  (make-html)
  (asdf:compile-system :clog))
