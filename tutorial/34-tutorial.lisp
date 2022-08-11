(defpackage #:clog-tut-34
  (:use #:cl #:clog #:clog-webgl)
  (:export start-tutorial))

(in-package :clog-tut-34)

;; example based on - https://webgl2fundamentals.org/webgl/lessons/webgl-fundamentals.html

;; "#version 300 es" MUST BE THE VERY FIRST LINE OF YOUR SHADER.
;; No comments or blank lines are allowed before it
(defparameter *vertex-shader-source* "#version 300 es

// an attribute is an input (in) to a vertex shader.
// It will receive data from a buffer
in vec4 a_position;

// all shaders have a main function
void main() {

  // gl_Position is a special variable a vertex shader
  // is responsible for setting
  gl_Position = a_position;
}")

(defparameter *fragment-shader-source* "#version 300 es

// fragment shaders don't have a default precision so we need
// to pick one. highp is a good default. It means \"high precision\"
precision highp float;

// we need to declare an output for the fragment shader
out vec4 outColor;

void main() {
  // Just set the output to a constant reddish-purple
  outColor = vec4(1, 0, 0.5, 1);
}")

(defun on-new-window (body)
  (debug-mode body)
  (setf (title (html-document body)) "Tutorial 34")
  (let* ((canvas          (create-canvas body :width 1000 :height 500))
         (gl              (create-webgl canvas))
         (vertex-shader   (compile-shader-source gl :VERTEX_SHADER *vertex-shader-source*))
         (fragment-shader (compile-shader-source gl :FRAGMENT_SHADER *fragment-shader-source*))
         (program         (compile-webgl-program gl vertex-shader fragment-shader))
         (pos             (attribute-location program "a_position"))
         (pos-buffer      (create-webgl-buffer gl))
         (vao             (create-vertex-array gl)))
    (print (drawing-buffer-width gl))
    (print (drawing-buffer-height gl))
    (bind-buffer pos-buffer :ARRAY_BUFFER)
    (buffer-data pos-buffer `(0   0
                              0   0.5
                              0.7 0)
                 "Float32Array" :STATIC_DRAW)
    (bind-vertex-array vao)
    (enable-vertex-attribute-array gl pos)
    (vertex-attribute-pointer gl pos 2 :FLOAT nil 0 0)
    (viewport gl 0 0 (width canvas) (height canvas))
    (clear-color gl 0.0 0.0 0.0 1.0)
    (clear-webgl gl :COLOR_BUFFER_BIT)
    (use-program program)
    (bind-vertex-array vao)
    (draw-arrays gl :TRIANGLES 0 3)))

(defun start-tutorial ()
  "Start turtorial."
  (initialize 'on-new-window)
  (open-browser))
