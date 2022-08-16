(defpackage #:clog-tut-35
  (:use #:cl #:clog #:clog-webgl)
  (:export start-tutorial))

(in-package :clog-tut-35)

;; https://github.com/mdn/dom-examples/blob/master/webgl-examples/tutorial/sample5/webgl-demo.js
;; This example uses WegGL v1
(defparameter *vertex-shader-source*
  " attribute vec4 aVertexPosition;
    attribute vec4 aVertexColor;
    uniform mat4 uModelViewMatrix;
    uniform mat4 uProjectionMatrix;
    varying lowp vec4 vColor;
    void main(void) {
      gl_Position = uProjectionMatrix * uModelViewMatrix * aVertexPosition;
      vColor = aVertexColor;
    }")

(defparameter *fragment-shader-source*
  " varying lowp vec4 vColor;
    void main(void) {
      gl_FragColor = vColor;
    }")

(defun on-new-window (body)
  (debug-mode body)
  (setf (title (html-document body)) "Tutorial 35")
  (let* ((canvas          (create-canvas body :width 500 :height 500))
         (gl              (create-webgl canvas))
         (vertex-shader   (compile-shader-source gl :VERTEX_SHADER *vertex-shader-source*))
         (fragment-shader (compile-shader-source gl :FRAGMENT_SHADER *fragment-shader-source*))
         (program         (compile-webgl-program gl vertex-shader fragment-shader))
         ;; Attribute locations
         (vertex-position (attribute-location program "aVertexPosition"))
         (vertex-color    (attribute-location program "aVertexColor"))
         ;; Uniform locations
         (model-view-matrix (uniform-location program "uModelViewMatrix"))
         (projection-matrix (uniform-location program "uProjectionMatrix"))
         ;; Bufffers
         (position-buffer (create-webgl-buffer gl))
         (color-buffer    (create-webgl-buffer gl))
         (index-buffer    (create-webgl-buffer gl)))
    (bind-buffer position-buffer :ARRAY_BUFFER)
    (buffer-data position-buffer
                 '(;; Front face
                   -1.0 -1.0 1.0 1.0 -1.0 1.0 1.0 1.0 1.0 -1.0 1.0 1.0
                   ;; Back face
                   -1.0 -1.0 -1.0 -1.0 1.0 -1.0 1.0 1.0 -1.0 1.0 -1.0 -1.0
                   ;; Top face
                   -1.0 1.0 -1.0 -1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 -1.0
                   ;; Bottom face
                   -1.0 -1.0 -1.0 1.0 -1.0 -1.0 1.0 -1.0 1.0 -1.0 -1.0 1.0
                   ;; Right face
                   1.0 -1.0 -1.0 1.0 1.0 -1.0 1.0 1.0 1.0 1.0 -1.0 1.0
                   ;; Left face
                   -1.0 -1.0 -1.0 -1.0 -1.0 1.0 -1.0 1.0 1.0 -1.0 1.0 -1.0)
                 "Float32Array"
                 :STATIC_DRAW)
    (print (is-buffer position-buffer))
    (bind-buffer color-buffer :ARRAY_BUFFER)
    (buffer-data color-buffer
                 ;; Repeat each color four times for the four vertices of the face
                 '(;; Front face - white
                   1.0 1.0 1.0 1.0
                   1.0 1.0 1.0 1.0
                   1.0 1.0 1.0 1.0
                   1.0 1.0 1.0 1.0
                   ;; Back face - red
                   1.0 0.0 0.0 1.0
                   1.0 0.0 0.0 1.0
                   1.0 0.0 0.0 1.0
                   1.0 0.0 0.0 1.0
                   ;; Top face - green
                   0.0 1.0 0.0 1.0
                   0.0 1.0 0.0 1.0
                   0.0 1.0 0.0 1.0
                   0.0 1.0 0.0 1.0
                   ;; Bottom face - blue
                   0.0 0.0 1.0 1.0
                   0.0 0.0 1.0 1.0
                   0.0 0.0 1.0 1.0
                   0.0 0.0 1.0 1.0
                   ;; Right face - yellow
                   1.0 1.0 0.0 1.0
                   1.0 1.0 0.0 1.0
                   1.0 1.0 0.0 1.0
                   1.0 1.0 0.0 1.0
                   ;; Left face
                   1.0 0.0 1.0 1.0
                   1.0 0.0 1.0 1.0
                   1.0 0.0 1.0 1.0
                   1.0 0.0 1.0 1.0)
                 "Float32Array"
                 :STATIC_DRAW)
    (print (is-buffer color-buffer))
    ;; This array defines each face as two triangles, using the
    ;; indices into the vertex array to specify each triangle's
    ;; position.
    (bind-buffer index-buffer :ELEMENT_ARRAY_BUFFER)
    (buffer-data index-buffer
                 `(0
                   1
                   2
                   0
                   2
                   3 ; front
                   4
                   5
                   6
                   4
                   6
                   7 ; back
                   8
                   9
                   10
                   8
                   10
                   11 ; top
                   12
                   13
                   14
                   12
                   14
                   15 ; bottom
                   16
                   17
                   18
                   16
                   18
                   19 ; right
                   20
                   21
                   22
                   20
                   22
                   23) ; left
                 "Uint16Array"
                 :STATIC_DRAW)
    (print (is-buffer index-buffer))
    (labels ((draw-scene ()
               (clear-color gl 0 0 0 1)
               (clear-depth gl 1)
               (enable-capability gl :DEPTH_TEST)
               (depth-function gl :LEQUAL)
               (clear-webgl gl :COLOR_BUFFER_BIT)
               (clear-webgl gl :DEPTH_BUFFER_BIT)
               ;; Tell WebGL how to pull out the positions from the position
               ;; buffer into the vertexPosition attribute
               (bind-buffer position-buffer :ARRAY_BUFFER)
               (vertex-attribute-pointer gl vertex-position
                                         3
                                         :FLOAT
                                         nil
                                         0
                                         0)
               (enable-vertex-attribute-array gl vertex-position)
               ;; Tell WebGL how to pull out the colors from the color buffer
               ;; into the vertexColor attribute.
               (bind-buffer color-buffer :ARRAY_BUFFER)
               (vertex-attribute-pointer gl vertex-color
                                         4
                                         :FLOAT
                                         nil
                                         0
                                         0)
               (enable-vertex-attribute-array gl vertex-color)
               ;; Indices
               (bind-buffer index-buffer :ELEMENT_ARRAY_BUFFER)
               (use-program program)
               (uniform-matrix gl 4 projection-matrix nil
                               (list 2.4142136573791504 0 0 0
                                     0 2.4142136573791504 0 0
                                     0 0 -1.0002000331878662 -1
                                     0 0 -0.020002000033855438 0))
               (uniform-matrix gl 4 model-view-matrix nil
                               (list 0.24186034500598907 0.6221014261245728 -0.7446430921554565 0
                                     -0.7772392630577087 0.5836206674575806 0.2351299524307251 0
                                     0.5808637738227844 0.5218972563743591 0.6246763467788696 0
                                     0 0 -6 1))
               (draw-elements gl :TRIANGLES 36 :UNSIGNED_SHORT 0)))
      (draw-scene))))

(defun start-tutorial ()
  "Start turtorial."
  (initialize 'on-new-window)
  (open-browser))
