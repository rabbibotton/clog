;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2022 David Botton                                            ;;;;
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

(defgeneric (setf url) (value clog-location)
  (:documentation "Set url VALUE for CLOG-LOCATION"))

(defmethod (setf url) (value (obj clog-location))
  (setf (property obj "href") value))

;;;;;;;;;;
;; hash ;;
;;;;;;;;;;

(defgeneric hash (clog-location)
  (:documentation "Get/Setf url hash."))

(defmethod hash ((obj clog-location))
  (property obj "hash"))

(defgeneric (setf hash) (value clog-location)
  (:documentation "Set hash VALUE for CLOG-LOCATION"))

(defmethod (setf hash) (value (obj clog-location))
  (setf (property obj "hash") value))

;;;;;;;;;;
;; host ;;
;;;;;;;;;;

(defgeneric host (clog-location)
  (:documentation "Get/Setf url host."))

(defmethod host ((obj clog-location))
  (property obj "host"))

(defgeneric (setf host) (value clog-location)
  (:documentation "Set host VALUE for CLOG-LOCATION"))

(defmethod (setf host) (value (obj clog-location))
  (setf (property obj "host") value))

;;;;;;;;;;;;;;;
;; host-name ;;
;;;;;;;;;;;;;;;

(defgeneric host-name (clog-location)
  (:documentation "Get/Setf url host name."))

(defmethod host-name ((obj clog-location))
  (property obj "hostname"))

(defgeneric (setf host-name) (value clog-location)
  (:documentation "Set host-name VALUE for CLOG-LOCATION"))

(defmethod (setf host-name) (value (obj clog-location))
  (setf (property obj "hostname") value))

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

(defgeneric (setf path-name) (value clog-location)
  (:documentation "Set path-name VALUE for CLOG-LOCATION"))

(defmethod (setf path-name) (value (obj clog-location))
  (setf (property obj "pathname") value))

;;;;;;;;;;
;; port ;;
;;;;;;;;;;

(defgeneric port (clog-location)
  (:documentation "Get/Setf url port."))

(defmethod port ((obj clog-location))
  (property obj "port"))

(defgeneric (setf port) (value clog-location)
  (:documentation "Set port VALUE for CLOG-LOCATION"))

(defmethod (setf port) (value (obj clog-location))
  (setf (property obj "port") value))

;;;;;;;;;;;;;;
;; protocol ;;
;;;;;;;;;;;;;;

(defgeneric protocol (clog-location)
  (:documentation "Get/Setf url protocol."))

(defmethod protocol ((obj clog-location))
  (property obj "protocol"))

(defgeneric (setf protocol) (value clog-location)
  (:documentation "Set protocol VALUE for CLOG-LOCATION"))

(defmethod (setf protocol) (value (obj clog-location))
  (setf (property obj "protocol") value))

;;;;;;;;;;;;;;;;
;; url-search ;;
;;;;;;;;;;;;;;;;

(defgeneric url-search (clog-location)
  (:documentation "Get/Setf url-search."))

(defmethod url-search ((obj clog-location))
  (property obj "search"))

(defgeneric (setf url-search) (value clog-location)
  (:documentation "Set url-search VALUE for CLOG-LOCATION"))

(defmethod (setf url-search) (value (obj clog-location))
  (setf (property obj "search") value))

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

(defgeneric url-replace (clog-location replace-url)
  (:documentation "Replace browser url, i.e. a redirection and current URL not
saved in session history and back button will not return to it."))

(defmethod url-replace ((obj clog-location) replace-url)
  (execute obj (format nil "replace('~A')" replace-url)))

;;;;;;;;;;;;;;;;
;; url-assign ;;
;;;;;;;;;;;;;;;;

(defgeneric url-assign (clog-location assign-url)
  (:documentation "Assign browser url, i.e. a redirection to assign-url
that will be saved in session histoy and back button will return to it."))

(defmethod url-assign ((obj clog-location) assign-url)
  (execute obj (format nil "assign('~A')" assign-url)))
