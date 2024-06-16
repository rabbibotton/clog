;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG Builder - UI Design tool for CLOG                                ;;;;
;;;; (c) David Botton                                                      ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :clog-tools)

;; These are defaults, if the file preferences.lisp exists
;; the values set there will be used instead

;; To passowrod protect use of the IDE set to a password or a function that
;; returns a password.
(defparameter *password-protect* nil)
;; Open panels and files in new browser tabs by default
(defparameter *open-external* nil)
;; Use clog-popup and extend desktop to popups
(defparameter *open-external-using-clog-popups* t)
;; Open files in browser popups instead of tabs if browser allows
(defparameter *open-external-source-in-popup* nil)
;; Open panel editors in browser popus instead of tabs if browser allows
(defparameter *open-external-panels-in-popup* nil)
;; Open panels as popups by default
(defparameter *open-panels-as-popups* nil)
;; Use console for evals instead of capture
(defparameter *editor-use-console-for-evals* nil)
;; Use TAB key as in emacs (can always use ctl/alt-t)
(defparameter *editor-use-tab-as-tabbify* t)
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
(defparameter *editor-delay-on-eval-sel* 15)
(defparameter *editor-delay-on-eval-form* 30)
(defparameter *editor-delay-on-eval-file* 60)

;; CLOG Panels
(defparameter *project-tree-sticky-open* t)
(defparameter *project-tree-dir-filter* "(\\\\|\\/)\\..*(\\\\|\\/)$")
(defparameter *project-tree-file-filter* "(^\\..*)|(.*~$)|(.*\\.bak$)")

;; CLOG Builder REPL
(defparameter *clog-repl-use-console* t)
(defparameter *clog-repl-open-console-on-start* nil)
(defparameter *clog-repl-send-result-to-console* nil)
(defparameter *clog-repl-private-console* t)
;; eval on main thread so (break) works for sbcl.
(defparameter *clog-repl-eval-on-main-thread* nil)

;; Panel Builder

(defparameter *builder-render-right-margin* 80)
(defparameter *builder-render-case* :downcase)

;; Builder Look and Feel

;; Builder Interface
(defparameter *builder-left-panel-size* 300)
(defparameter *builder-window-desktop-class* "w3-blue-grey")
(defparameter *builder-window-show-static-root-class* "w3-grey")
(defparameter *builder-show-callers-class* "w3-orange")
(defparameter *builder-show-callees-class* "w3-orange")
(defparameter *builder-menu-button-class* "w3-input w3-grey w3-button w3-ripple")
(defparameter *builder-pallete-class* "w3-light-grey w3-small")
(defparameter *builder-event-list-class* "w3-light-grey w3-small")
(defparameter *builder-panel-class* "w3-white w3-sans-serif w3-medium")
(defparameter *builder-menu-search-class* "w3-right w3-gray w3-bar-item")

;; Menus
(defparameter *builder-menu-bar-class* "w3-bar w3-round w3-small w3-blue-grey w3-card-4")
(defparameter *builder-menu-bar-drop-down-class* "w3-dropdown-content w3-bar-block w3-card-4")
(defparameter *builder-menu-item-class* "w3-bar-item w3-blue-grey w3-button")
(defparameter *builder-menu-window-select-class* "w3-grey w3-bar-item w3-button")
(defparameter *builder-menu-context-item-class* "w3-button w3-bar")

;; Window treatements
(defparameter *builder-title-class* "w3-blue-grey w3-round")
(defparameter *builder-border-class* "w3-card-4 w3-white w3-border w3-round")
(defparameter *builder-package-class* "w3-white w3-round")
(defparameter *builder-status-bar-class* "w3-tiny w3-border")
(defparameter *builder-icons-class* "w3-button w3-white w3-round w3-border w3-border-black w3-ripple")
