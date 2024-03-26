;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG Builder - UI Design tool for CLOG                                ;;;;
;;;; (c) David Botton                                                      ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :clog-tools)

;; These are defaults, if the file preferences.lisp exists
;; the values set there will be used instead

;; Open panels and files in new browser tabs by default
(defparameter *open-external* nil)
;; Open files in browser popups instead of tabs if browser allows
(defparameter *open-external-in-popup* nil)
;; Open panel editors in browser popus instead of tabs if browser allows
(defparameter *open-external-panels-in-popup* nil)
;; Open panels as popups by default
(defparameter *open-panels-as-popups* nil)
;; Use emacs instead of the source-editor when openning external
(defparameter *open-external-with-emacs* nil)
;; Best Light Theme for Lisp
(defparameter *editor-theme* "ace/theme/iplastic")
;; Best Dark Theme for Lisp
;;(defparameter *editor-theme* "ace/theme/terminal")
(defparameter *editor-mode* "ace/mode/lisp")
;;(defparameter *editor-keybinding* "ace/keyboard/emacs")
(defparameter *editor-keybinding* "ace/keyboard/ace")
(defparameter *editor-tab-size* 2)
(defparameter *editor-renderer-options*
"fontSize : 14,
 showInvisibles : false,
 displayIndentGuides : true,
 printMarginColumn : 80,
 showPrintMargin : true,
 showLineNumbers : true,
 showGutter : true,
 enableBasicAutocompletion: true,
 enableLiveAutocompletion : true")
