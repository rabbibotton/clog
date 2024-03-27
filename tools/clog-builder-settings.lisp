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

;; Builder Look and Feel

(defparameter *builder-window-desktop-class* "w3-blue-grey")
(defparameter *builder-window-show-static-root-class* "w3-grey")
(defparameter *builder-show-callers-class* "w3-orange")
(defparameter *builder-show-callees-class* "w3-orange")
(defparameter *builder-menu-button-class* "w3-input w3-grey w3-button w3-ripple")
(defparameter *builder-pallete-class* "w3-light-grey w3-small")
(defparameter *builder-event-list-class* "w3-light-grey w3-small")

;; Menus
(defparameter *builder-menu-bar-class* "w3-bar w3-round w3-small w3-blue-grey w3-card-4")
(defparameter *builder-menu-bar-drop-down-class* "w3-dropdown-content w3-bar-block w3-card-4")
(defparameter *builder-menu-item-class* "w3-bar-item w3-blue-grey w3-button")
(defparameter *builder-menu-window-select-class* "w3-grey w3-bar-item w3-button")

;; Window treatements
(defparameter *builder-title-class* "w3-blue-grey w3-round")
(defparameter *builder-border-class* "w3-card-4 w3-white w3-border w3-round")
(defparameter *builder-package-class* "w3-white w3-round")
(defparameter *builder-icons-class* "w3-button w3-white w3-round w3-border w3-border-black w3-ripple")
