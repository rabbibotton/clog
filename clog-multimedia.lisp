;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2021 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-mulitmedia.lisp                                                  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-multimedia
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-multimedia (clog-element)()
  (:documentation "CLOG Multimedia base class."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-audio
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-audio (clog-multimedia)()
  (:documentation "CLOG Audio class."))

;;;;;;;;;;;;;;;;;;
;; create-audio ;;
;;;;;;;;;;;;;;;;;;

(defgeneric create-audio (clog-obj &key
				     source
				     controls
				     preload
				     autoplay
				     autoloop
				     muted
				     auto-place)
  (:documentation "Create a CLOG Audio control"))

(defmethod create-audio ((obj clog-obj)
			 &key (source "")
			   (controls t)
			   (preload  nil)
			   (autoplay nil)
			   (autoloop nil)
			   (muted    nil)
			   (auto-place t))
  (create-child obj (format nil "<audio~A~A~A~A~A~A/>"
			    (if (equal source "")
				""
				(format nil " src='~A'" (escape-string source)))
			    (if controls
				" controls"
				"")
			    (if preload
				" preload='auto'"
				"")
			    (if autoplay
				" autoplay"
				"")
			    (if autoloop
				" loop"
				"")
			    (if muted
				" muted"
				""))
		:clog-type 'clog-audio :auto-place auto-place))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-video
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-video (clog-multimedia)()
  (:documentation "CLOG Video class."))

(defgeneric create-video (clog-obj &key
				     source
				     controls
				     preload
				     poster
				     autoplay
				     autoloop
				     muted
				     auto-place)
(:documentation "Create a CLOG video control"))

(defmethod create-video ((obj clog-obj)
			&key (source "")
			  (controls t)
			  (preload  nil)
			  (poster   "")
			  (autoplay nil)
			  (autoloop nil)
			  (muted    nil)
			  (auto-place t))
  (create-child obj (format nil "<video~A~A~A~A~A~A~A/>"
			    (if (equal source "")
				""
				(format nil " src='~A'" (escape-string source)))
			    (if controls
				" controls"
				"")
			    (if preload
				" preload='auto'"
				"")
			    (if (equal poster "")
				""
				(format nil " poster='~A'" (escape-string poster)))
			    (if autoplay
				" autoplay"
				"")
			    (if autoloop
				" loop"
				"")
			    (if muted
				" muted"
				""))			    
		:clog-type 'clog-video :auto-place auto-place))

