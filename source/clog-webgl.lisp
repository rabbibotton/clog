;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2021 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-webgl.lisp                                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mgl-pax:define-package :clog-webgl
  (:documentation "CLOG-WEBGL bindings to WebGL")
  (:use #:cl #:parse-float #:clog #:mgl-pax))

(cl:in-package :clog-webgl)

(defsection @clog-webgl (:title "CLOG WebGL Objects")
  "CLOG-WebGL - Class for CLOG WebGL objects"
  (clog-webgl                    class)
  (create-webgl                  generic-function)
  (compile-shader-source         generic-function)
  (compile-webgl-program         generic-function)
  (clear-color                   generic-function)
  (clear-webgl                   generic-function)
  (viewport                      generic-function)
  (enable-vertex-attribute-array generic-function)
  (vertex-attribute-pointer      generic-function)
  (draw-arrays                   generic-function)

  "CLOG-WebGL-Shader - Class for CLOG WebGL-Shader objects"
  (clog-webgl-shader class)
  (create-shader     generic-function)

  (shader-source     generic-function)
  (shader-parameter  generic-function)
  (shader-info-log   generic-function)
  (compile-shader    generic-function)
  (delete-shader     generic-function)

  "CLOG-WebGL-Program - Class for CLOG WebGL-Program objects"
  (clog-webgl-program class)
  (create-program     generic-function)

  (attach-shader      generic-function)
  (program-parameter  generic-function)
  (attribute-location generic-function)
  (program-info-log   generic-function)
  (link-program       generic-function)
  (use-program        generic-function)
  (delete-program     generic-function)

  "CLOG-WebGL-Buffer - Class for CLOG WebGL-Buffer objects"
  (clog-webgl-buffer   class)
  (create-webgl-buffer generic-function)
  (bind-buffer         generic-function)
  (buffer-data         generic-function)
  (delete-buffer       generic-function)

  "CLOG-WebGL-Vertex-Array - Class for CLOG WebGL-Vertex-Array objects"
  (clog-vertex-array   class)
  (create-vertex-array generic-function)
  (bind-vertex-array   generic-function)
  (delete-vertex-array generic-function))

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
    (js-execute obj (format nil "clog['~A']=clog['~A'].getContext('webgl2')"
                            web-id
                            (html-id obj)))
    (make-instance 'clog-webgl
                   :connection-id (clog::connection-id obj)
                   :html-id web-id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-webgl-shader
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-webgl-shader (clog-obj)
  ((gl :accessor gl :initarg :clog-webgl)))

(defgeneric create-shader (clog-webgl glenum-type)
  (:documentation "Create a clog-webgl-shader for type :GLENUM.
See https://github.com/KhronosGroup/WebGL/blob/main/specs/latest/2.0/webgl2.idl
For :GLENUM values"))

(defmethod create-shader ((obj clog-webgl) glenum-type)
  (let ((web-id (clog-connection:generate-id)))
    (js-execute obj (format nil "clog['~A']=~A.createShader(~A.~A)"
                            web-id
                            (script-id obj) (script-id obj) glenum-type))
    (make-instance 'clog-webgl-shader
                   :connection-id (clog::connection-id obj)
                   :html-id web-id
                   :clog-webgl obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Properties - clog-webgl-shader
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf shader-source) (source (obj clog-webgl-shader))
  (execute (gl obj) (format nil "shaderSource(~A, '~A')"
                            (script-id obj)
                            (escape-string source)))
  source)

(defmethod shader-parameter ((obj clog-webgl-shader) glenum-param)
  (query (gl obj) (format nil "getShaderParameter(~A, ~A.~A)"
                          (script-id obj)
                          (script-id (gl obj)) glenum-param)))

(defmethod shader-info-log ((obj clog-webgl-shader))
  (query (gl obj) (format nil "getShaderInfoLog(~A)"
                          (script-id obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods - clog-webgl-shader
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod compile-shader ((obj clog-webgl-shader))
  (execute (gl obj) (format nil "compileShader(~A)"
                            (script-id obj))))

(defmethod delete-shader ((obj clog-webgl-shader))
  (execute (gl obj) (format nil "deleteShader(~A)"
                            (script-id obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-webgl-program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-webgl-program (clog-obj)
  ((gl :accessor gl :initarg :clog-webgl)))

(defgeneric create-program (clog-webgl)
  (:documentation "Create a clog-webgl-program"))

(defmethod create-program ((obj clog-webgl))
  (let ((web-id (clog-connection:generate-id)))
    (js-execute obj (format nil "clog['~A']=~A.createProgram()"
                            web-id
                            (script-id obj)))
    (make-instance 'clog-webgl-program
                   :connection-id (clog::connection-id obj)
                   :html-id web-id
                   :clog-webgl obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parameters - clog-webgl-program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod program-parameter ((obj clog-webgl-program) glenum-param)
  (query (gl obj) (format nil "getProgramParameter(~A, ~A.~A)"
                          (script-id obj)
                          (script-id (gl obj)) glenum-param)))

(defmethod attribute-location ((obj clog-webgl-program) name)
  (query (gl obj) (format nil "getAttribLocation(~A, '~A')"
                          (script-id obj) name)))

(defmethod program-info-log ((obj clog-webgl-program))
  (query (gl obj) (format nil "getProgramInfoLog(~A)"
                          (script-id obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods - clog-webgl-program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod attach-shader ((obj clog-webgl-program) (shader clog-webgl-shader))
  (execute (gl obj) (format nil "attachShader(~A, ~A)"
                            (script-id obj)
                            (script-id shader))))

(defmethod link-program ((obj clog-webgl-program))
  (execute (gl obj) (format nil "linkProgram(~A)" (script-id obj))))

(defmethod use-program ((obj clog-webgl-program))
  (execute (gl obj) (format nil "useProgram(~A)" (script-id obj))))

(defmethod delete-program ((obj clog-webgl-program))
  (execute (gl obj) (format nil "deleteProgram(~A)"
                            (script-id obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methdods - clog-webgl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod clear-color ((obj clog-webgl) red green blue alpha)
  (execute obj (format nil "clearColor(~A,~A,~A,~A)"
                       red green blue alpha)))

(defmethod clear-webgl ((obj clog-webgl) glenum-mask)
  (execute obj (format nil "clear(~A.~A)"
                       (script-id obj)
                       glenum-mask)))

(defmethod viewport ((obj clog-webgl) x y width height)
  (execute obj (format nil "viewport(~A,~A,~A,~A)"
                       x y width height)))

(defmethod draw-arrays ((obj clog-webgl) primitive-type offset count)
  (execute obj (format nil "drawArrays(~A.~A,~A,~A)"
                       (script-id obj) primitive-type
                       offset count)))

(defmethod enable-vertex-attribute-array ((obj clog-webgl) attribute-location)
  (execute obj (format nil "enableVertexAttribArray(~A)"
                       attribute-location)))

(defmethod vertex-attribute-pointer ((obj clog-webgl) attribute-location size type normalize stride offset)
  (execute obj (format nil "vertexAttribPointer(~A,~A,~A.~A,~A,~A,~A)"
                       attribute-location size (script-id obj) type
                       (p-true-js normalize) stride offset)))

(defmethod compile-shader-source ((obj clog-webgl) glenum-type source)
  (let ((shader (create-shader obj glenum-type)))
    (setf (shader-source shader) source)
    (compile-shader shader)
    (let ((result (shader-parameter shader :COMPILE_STATUS)))
      (cond ((js-true-p result)
             shader)
            (t
             (setf result (shader-info-log shader))
             (delete-shader shader)
             (error result))))))

(defmethod compile-webgl-program ((obj clog-webgl) vertex-shader fragment-shader)
  (let ((program (create-program obj)))
    (attach-shader program vertex-shader)
    (attach-shader program fragment-shader)
    (link-program program)
    (let ((result (program-parameter program :LINK_STATUS)))
      (cond ((js-true-p result)
             program)
            (t
             (setf result (program-info-log program))
             (delete-program program)
             (error result))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-webgl-buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-webgl-buffer (clog-obj)
  ((gl      :accessor gl      :initarg :clog-webgl)
   (gl-type :accessor gl-type :initarg :gl-type)))

(defgeneric create-webgl-buffer (clog-webgl &key bind-type)
  (:documentation "Create a clog-webgl-buffer"))

(defmethod create-webgl-buffer ((obj clog-webgl) &key bind-type)
  (let ((web-id (clog-connection:generate-id)))
    (js-execute obj (format nil "clog['~A']=~A.createBuffer()"
                            web-id
                            (script-id obj)))
    (let ((new-obj (make-instance 'clog-webgl-buffer
                                  :connection-id (clog::connection-id obj)
                                  :html-id web-id
                                  :clog-webgl obj
                                  :gl-type bind-type)))
      (when bind-type
        (bind-buffer new-obj bind-type))
      new-obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods - clog-webgl-buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod bind-buffer ((obj clog-webgl-buffer) glenum-target)
  (execute (gl obj) (format nil "bindBuffer(~A.~A,~A)"
                            (script-id (gl obj)) glenum-target
                            (script-id obj)))
  (setf (gl-type obj) glenum-target))

(defmethod buffer-data ((obj clog-webgl-buffer) data-list data-type hint)
  (execute (gl obj) (format nil "bufferData(~A.~A, new ~A([~{~A~^,~}]), ~A.~A)"
                            (script-id (gl obj)) (gl-type obj)
                            data-type data-list
                            (script-id (gl obj)) hint)))

(defmethod delete-buffer ((obj clog-webgl-buffer))
  (execute (gl obj) (format nil "deleteBuffer(~A)"
                            (script-id obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-webgl-vertex-array
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-webgl-vertex-array (clog-obj)
  ((gl :accessor gl :initarg :clog-webgl)))

(defgeneric create-vertex-array (clog-webgl)
  (:documentation "Create a clog-webgl-vertex-array"))

(defmethod create-vertex-array ((obj clog-webgl))
  (let ((web-id (clog-connection:generate-id)))
    (js-execute obj (format nil "clog['~A']=~A.createVertexArray()"
                            web-id
                            (script-id obj)))
    (make-instance 'clog-webgl-vertex-array
                   :connection-id (clog::connection-id obj)
                   :html-id web-id
                   :clog-webgl obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods - clog-webgl-vertex-array
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod bind-vertex-array ((obj clog-webgl-vertex-array))
  (execute (gl obj) (format nil "bindVertexArray(~A)"
                            (script-id obj))))

(defmethod delete-vertex-array ((obj clog-webgl-vertex-array))
  (execute (gl obj) (format nil "deleteVertexArray(~A)"
                            (script-id obj))))
