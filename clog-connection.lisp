;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2021 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-connection.lisp                                                  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports - clog-connection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mgl-pax:define-package :clog-connection
  (:documentation "The Common List Omnificent GUI - Connection")
  (:use #:cl #:mgl-pax))

(in-package :clog-connection)

(defsection @clog-connection (:title "CLOG Connection")
  "Low level connectivity to the web client and boot file script."

  "CLOG connections"
  
  (message        function)
  (execute-script function)
  (validp         function)
  (cclose         function)
  (shutdown       function)
  (cwrite         function)
  (cwriteln       function))

;;;;;;;;;;;;;
;; message ;;
;;;;;;;;;;;;;

(defun message (connection-id message)
  "Send MESSAGE to CONNECTION-ID."
  (let ((con (clog::get-connection connection-id)))
    (when con
      (websocket-driver:send con message))))

;;;;;;;;;;;;;;;;;;;;
;; execute-script ;;
;;;;;;;;;;;;;;;;;;;;

(defun execute-script (connection-id script)
  "Execute SCRIPT on CONNECTION-ID, disregard return value."
  (message connection-id script))

;;;;;;;;;;;;
;; validp ;;
;;;;;;;;;;;;

(defun validp (connection-id)
  "Check if CONNECTION-ID is valid."
  (if (clog::get-connection connection-id)
      t
      nil))

;;;;;;;;;;;;
;; cclose ;;
;;;;;;;;;;;;

(defun cclose (connection-id)
  "Close connection to CONNECTION-ID. The boot file may try to reistablish
 connectivity."
  (execute-script connection-id "ws.close()"))

;;;;;;;;;;;;;;
;; shutdown ;;
;;;;;;;;;;;;;;

(defun shutdown (connection-id)
  "Shutdown connection to CONNECTION-ID. The boot file may not try to
reistablish connectivity."
  (execute-script connection-id "Shutdown_ws(event.reason='user')"))

;;;;;;;;;;;;
;; cwrite ;;
;;;;;;;;;;;;

(defun cwrite (connection-id text)
  "Write TEXT raw to document object of CONNECTION-ID with out new line."
  (message connection-id (format nil "document.write('~A');" text)))

;;;;;;;;;;;;;;
;; cwriteln ;;
;;;;;;;;;;;;;;

(defun cwriteln (connection-id text)
  "Write TEXT raw to document object of CONNECTION-ID with new line."
  (message connection-id (format nil "document.writeln('~A');" text)))
