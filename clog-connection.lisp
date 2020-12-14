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
  
  (execute  function)
  (query    function)
  (validp   function)
  (cclose   function)
  (shutdown function)
  (cwrite   function)
  (cwriteln function))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implemetation - clog-connection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;
;; execute ;;
;;;;;;;;;;;;;

(defun execute (connection-id message)
  "Execute SCRIPT on CONNECTION-ID, disregard return value."
  (let ((con (clog::get-connection connection-id)))
    (when con
      (websocket-driver:send con message))))

;;;;;;;;;;;
;; query ;;
;;;;;;;;;;;

(defun query (connection-id script)
  "Execute SCRIPT on CONNECTION-ID, return value."
  (let ((uid (clog::generate-connection-id)))
    (clog::prep-query uid nil)
    (execute connection-id
	     (format nil "ws.send (\"~A:\"+eval(\"~A\"));" uid script))
    (clog::wait-for-answer uid)))

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
  (execute connection-id "ws.close()"))

;;;;;;;;;;;;;;
;; shutdown ;;
;;;;;;;;;;;;;;

(defun shutdown (connection-id)
  "Shutdown connection to CONNECTION-ID. The boot file may not try to
reistablish connectivity."
  (execute connection-id "Shutdown_ws(event.reason='user')"))

;;;;;;;;;;;;
;; cwrite ;;
;;;;;;;;;;;;

(defun cwrite (connection-id text)
  "Write TEXT raw to document object of CONNECTION-ID with out new line."
  (execute connection-id (format nil "document.write('~A');" text)))

;;;;;;;;;;;;;;
;; cwriteln ;;
;;;;;;;;;;;;;;

(defun cwriteln (connection-id text)
  "Write TEXT raw to document object of CONNECTION-ID with new line."
  (execute connection-id (format nil "document.writeln('~A');" text)))
