;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2021 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-location.lisp                                                    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-location
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-location (clog-obj)()
  (:documentation "CLOG Location Objects encapsulate the location."))

;;;;;;;;;;;;;;;;;;;;;;;;
;; make-clog-location ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-clog-location (connection-id)
  "Construct a new clog-location. (Private)"
  (make-instance 'clog-location :connection-id connection-id :html-id "location"))

;;;;;;;;;
;; url ;;
;;;;;;;;;

(defgeneric url (clog-location)
  (:documentation "Get/Setf full url. Setting the URL causes navgation to URL."))

(defmethod url ((obj clog-location))
  (property obj "href"))

(defgeneric set-url (clog-location value)
  (:documentation "Set url VALUE for CLOG-LOCATION"))

(defmethod set-url ((obj clog-location) value)
  (setf (property obj "href") value))
(defsetf url set-url)

;;;;;;;;;;
;; hash ;;
;;;;;;;;;;

(defgeneric hash (clog-location)
  (:documentation "Get/Setf url hash."))

(defmethod hash ((obj clog-location))
  (property obj "hash"))

(defgeneric set-hash (clog-location value)
  (:documentation "Set hash VALUE for CLOG-LOCATION"))

(defmethod set-hash ((obj clog-location) value)
  (setf (property obj "hash") value))
(defsetf hash set-hash)

;;;;;;;;;;
;; host ;;
;;;;;;;;;;

(defgeneric host (clog-location)
  (:documentation "Get/Setf url host."))

(defmethod host ((obj clog-location))
  (property obj "host"))

(defgeneric set-host (clog-location value)
  (:documentation "Set host VALUE for CLOG-LOCATION"))

(defmethod set-host ((obj clog-location) value)
  (setf (property obj "host") value))
(defsetf host set-host)

;;;;;;;;;;;;;;;
;; host-name ;;
;;;;;;;;;;;;;;;

(defgeneric host-name (clog-location)
  (:documentation "Get/Setf url host name."))

(defmethod host-name ((obj clog-location))
  (property obj "hostname"))

(defgeneric set-host-name (clog-location value)
  (:documentation "Set host-name VALUE for CLOG-LOCATION"))

(defmethod set-host-name ((obj clog-location) value)
  (setf (property obj "hostname") value))
(defsetf host-name set-host-name)

;;;;;;;;;;;;
;; origin ;;
;;;;;;;;;;;;

(defgeneric origin (clog-location)
  (:documentation "Get url origin."))

(defmethod origin ((obj clog-location))
  (property obj "origin"))

;;;;;;;;;;;;;;;
;; path-name ;;
;;;;;;;;;;;;;;;

(defgeneric path-name (clog-location)
  (:documentation "Get/Setf url path-name."))

(defmethod path-name ((obj clog-location))
  (property obj "pathname"))

(defgeneric set-path-name (clog-location value)
  (:documentation "Set path-name VALUE for CLOG-LOCATION"))

(defmethod set-path-name ((obj clog-location) value)
  (setf (property obj "pathname") value))
(defsetf path-name set-path-name)

;;;;;;;;;;
;; port ;;
;;;;;;;;;;

(defgeneric port (clog-location)
  (:documentation "Get/Setf url port."))

(defmethod port ((obj clog-location))
  (property obj "port"))

(defgeneric set-port (clog-location value)
  (:documentation "Set port VALUE for CLOG-LOCATION"))

(defmethod set-port ((obj clog-location) value)
  (setf (property obj "port") value))
(defsetf port set-port)

;;;;;;;;;;;;;;
;; protocol ;;
;;;;;;;;;;;;;;

(defgeneric protocol (clog-location)
  (:documentation "Get/Setf url protocol."))

(defmethod protocol ((obj clog-location))
  (property obj "protocol"))

(defgeneric set-protocol (clog-location value)
  (:documentation "Set protocol VALUE for CLOG-LOCATION"))

(defmethod set-protocol ((obj clog-location) value)
  (setf (property obj "protocol") value))
(defsetf protocol set-protocol)

;;;;;;;;;;;;;;;;
;; url-search ;;
;;;;;;;;;;;;;;;;

(defgeneric url-search (clog-location)
  (:documentation "Get/Setf url-search."))

(defmethod url-search ((obj clog-location))
  (property obj "search"))

(defgeneric set-url-search (clog-location value)
  (:documentation "Set url-search VALUE for CLOG-LOCATION"))

(defmethod set-url-search ((obj clog-location) value)
  (setf (property obj "search") value))
(defsetf url-search set-url-search)

;;;;;;;;;;;;
;; reload ;;
;;;;;;;;;;;;

(defgeneric reload (clog-location)
  (:documentation "Reload browser window."))

(defmethod reload ((obj clog-location))
  (execute obj "reload()"))

;;;;;;;;;;;;;;;;;
;; url-replace ;;
;;;;;;;;;;;;;;;;;

(defgeneric url-replace (clog-window replace-url)
  (:documentation "Replace browser url, ie a redirection and current URL not
saved in session history and back button will not return to it."))

(defmethod url-replace ((obj clog-window) replace-url)
  (execute obj (format nil "replace('~A')" replace-url)))

;;;;;;;;;;;;;;;;
;; url-assign ;;
;;;;;;;;;;;;;;;;

(defgeneric url-assign (clog-window assign-url)
  (:documentation "Assign browser url."))

(defmethod url-assign ((obj clog-window) assign-url)
  (execute obj (format nil "assign('~A')" assign-url)))
