;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2021 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-webgl.lisp                                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)

;; Use clog-canvas to create the html element and then use clog-webgl
;; to obtain the WebGL2 context

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-webgl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-webgl (clog-obj)())

;;;;;;;;;;;;;;;;;;
;; create-webgl ;;
;;;;;;;;;;;;;;;;;;

(defgeneric create-webgl (clog-canvas)
  (:documentation "Create a new CLOG-WebGL from a CLOG-Canvas"))


(defmethod create-webgl ((obj clog-canvas))
  (let ((web-id (clog-connection:generate-id)))
    (clog-connection:execute (connection-id obj)
                (format nil "clog['~A']=clog['~A'].getContext('webgl2')"
                        web-id
                        (html-id obj)))
    (make-instance 'clog-context2d
                   :connection-id (connection-id obj)
                   :html-id web-id)))

