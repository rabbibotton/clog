;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2022 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-style.lisp                                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-style-block
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-style-block (clog-element)()
  (:documentation "CLOG style-blocks for applying css styles."))

;;;;;;;;;;;;;;;;;;;;;;;;
;; create-style-block ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-style-block (clog-obj
                                &key content media html-id auto-place)
  (:documentation "Ideally style blocks should be created in the (head body)
clog-element but can be placed anywhere on a document and are applied as found
in the document. Although they are not 'scoped'. Media is a css media query
defaulting to all. To load CSS style sheets from files see LOAD-CSS in
clog-document. The add-style method can be used or can directly use the
TEXT method to access blocks content."))

(defmethod create-style-block ((obj clog-obj)
                               &key
                                 (content "")
                                 (media "all")
                                 (html-id nil) (auto-place t))
  (create-child obj (format nil "<style media='~A'>~A</style>"
                            (escape-to-single-quote-in-tag media)
                            content)
                :clog-type  'clog-style-block
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;;;;;
;; add-style ;;
;;;;;;;;;;;;;;;

(deftype selector-type () '(member :element :id :class))

(defgeneric add-style (clog-style-block selector-type selector style-alist)
  (:documentation "Add to style-block an alist of css styles to affect
selector. For example:
     (add-style :element \"a\" '((\"text-decoration\" :none)))"))

(defmethod add-style ((obj clog-style-block) selector-type selector style-alist)
  (let ((old-text (text obj)))
    (setf (text obj) (format nil "~A ~A~A\{~{~A~}\}"
                             (if old-text
                                 old-text
                                 "")
                             (cond ((eq selector-type :id) "#")
                                   ((eq selector-type :element) "")
                                   ((eq selector-type :class) ".")
                                   (t ""))
                             selector
                             (mapcar (lambda (s)
                                       (format nil "~A:~A;"
                                               (car s)
                                               (cadr s)))
                                     style-alist)))))
