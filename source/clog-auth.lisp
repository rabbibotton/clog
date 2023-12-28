;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2022 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-auth.lisp                                                        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CLOG-Auth an authorization abstraction for CLOG

(mgl-pax:define-package :clog-auth
  (:documentation "CLOG-AUTH an authenticationa and authorization abstraction
for CLOG")
  (:use #:cl #:clog #:mgl-pax))

(cl:in-package :clog-auth)

(defsection @clog-auth (:title "CLOG Auth Objects")
  "CLOG-AUTH - Authentication"
  (get-authentication-token     function)
  (store-authentication-token   function)
  (remove-authentication-token  function)
  (set-on-authentication-change function)

  "CLOG-AUTH - Authorization"
  (add-authorization function)
  (is-authorized-p   function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-auth - Authenitcation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *clog-auth-key* "clog-auth-token"
  "Key used for local storage of authentication token")

(defparameter *authorization-hash* (make-hash-table* :test #'equalp)
  "Hash table of roles to actions")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-authentication-token ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-authentication-token (body &key auth-path)
  "Retrieve the stored authentication token"
  (check-type body clog-body)
  (let ((token (storage-element (window body) :local *clog-auth-key*)))
    (when (equalp token "null")
      (setf token nil))
    (unless token
      (when auth-path
        (url-assign (location body) auth-path)))
    token))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; store-authentication-token ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun store-authentication-token (body token)
  (check-type body clog-body)
  (setf (storage-element (window body) :local *clog-auth-key*) token))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; remove-authentication-token ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remove-authentication-token (body)
  (check-type body clog-body)
  (storage-remove (window body) :local *clog-auth-key*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-authentication-change ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-on-authentication-change (body handler)
  (check-type body clog-body)
  (set-on-storage (window body) (lambda (obj data)
                                  (declare (ignore obj))
                                  (set-on-storage (window body) nil)
                                  (when (equalp (getf data :key)
                                                *clog-auth-key*)
                                    (funcall handler body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-auth - Authorization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *authorization-hash* (make-hash-table* :test #'equalp)
  "Hash table of role to action")

;;;;;;;;;;;;;;;;;;;;;;;
;; add-authorization ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun add-authorization (role-list action-list)
  "Add to each role in ROLE-LIST every action in ACTION-LIST"
  (dolist (role role-list)
    (dolist (action action-list)
      (setf (gethash role *authorization-hash*)
       (adjoin action (gethash role *authorization-hash*))))))

;;;;;;;;;;;;;;;;;;;;;
;; is-authorized-p ;;
;;;;;;;;;;;;;;;;;;;;;

(defun is-authorized-p (role-list action)
  "Given ROLE-LIST is action authorized. If action is nil returns t."
  (if action
      (dolist (role role-list nil)
        (when (member action (gethash role *authorization-hash*))
          (return t)))
      t))
