;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2022 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-auth.lisp                                                        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CLOG-Auth an authorization abstraction for CLOG

(mgl-pax:define-package :clog-auth
  (:documentation "CLOG-AUTH an authorization abstraction for CLOG")
  (:use #:cl #:clog #:mgl-pax))

(cl:in-package :clog-auth)

(defsection @clog-auth (:title "CLOG Auth Objects")
  "CLOG-AUTH - authorization abstraction for CLOG"
  (get-authentication-token    function)
  (store-authentication-token  function)
  (remove-authentication-token function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-auth
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *clog-auth-key* "clog-auth-token"
  "Key used for local storage of authentication token")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-authentication-token ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-authentication-token (body &key auth-path)
  "Retrieve the stored authorization token"
  (let ((token (storage-element (window body) :local *clog-auth-key*)))
    (when (equalp token "null")
      (setf token nil))
    (unless token
      (when auth-path	
	(url-assign (window body) auth-path)))
    token))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; store-authentication-token ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun store-authentication-token (body token)
  (setf (storage-element (window body) :local *clog-auth-key*) token))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; remove-authentication-token ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remove-authentication-token (body)
  (storage-remove (window body) :local *clog-auth-key*))  

  

