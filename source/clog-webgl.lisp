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
  (clog-webgl                     class)
  (create-webgl                   generic-function)

  (drawing-buffer-width           generic-function)
  (drawing-buffer-height          generic-function)

  (active-texture                 generic-function)
  (blend-color                    generic-function)
  (blend-equation                 generic-function)
  (blend-equation-seperate        generic-function)
  (blend-function                 generic-function)
  (blend-function-seperate        generic-function)
  (check-frame-buffer-status      generic-function)
  (clear-color                    generic-function)
  (clear-depth                    generic-function)
  (clear-webgl                    generic-function)
  (clear-stencil                  generic-function)
  (color-mask                     generic-function)
  (depth-function                 generic-function)
  (depth-mask                     generic-function)
  (depth-range                    generic-function)
  (disable                        generic-function)
  (disable-vertex-attribute-array generic-function)
  (draw-arrays                    generic-function)
  (draw-elements                  generic-function)
  (enable                         generic-function)
  (enable-vertex-attribute-array  generic-function)
  (finish                         generic-function)
  (flush                          generic-function)
  (frame-buffer-render-buffer     generic-function)
  (frame-buffer-texture-2d        generic-function)
  (front-face                     generic-function)
  (generate-mipmap                generic-function)
  (viewport                       generic-function)
  (vertex-attribute-pointer       generic-function)

  (compile-shader-source         generic-function)
  (compile-webgl-program         generic-function)

  "CLOG-WebGL-Active-Info - Class for CLOG WebGL Active Info objects"
  (clog-webgl-active-info class)
  (info-name              generic-function)
  (info-size              generic-function)
  (info-type              generic-function)

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

  (attach-shader           generic-function)
  (detach-shader           generic-function)
  (bind-attribute-location generic-function)
  (program-parameter       generic-function)
  (attribute-location      generic-function)
  (uniform-location        generic-function)
  (active-attribute        generic-function)
  (active-uniform          generic-function)
  (program-info-log        generic-function)
  (link-program            generic-function)
  (use-program             generic-function)
  (delete-program          generic-function)

  "CLOG-WebGL-Buffer - Class for CLOG WebGL-Buffer objects"
  (clog-webgl-buffer   class)
  (create-webgl-buffer generic-function)
  (bind-buffer         generic-function)
  (buffer-data         generic-function)
  (buffer-sub-data     generic-function)
  (delete-buffer       generic-function)

  "CLOG-WebGL-Vertex-Array - Class for CLOG WebGL-Vertex-Array objects"
  (clog-vertex-array   class)
  (create-vertex-array generic-function)
  (bind-vertex-array   generic-function)
  (delete-vertex-array generic-function)

  "CLOG-WebGL-Frame-Buffer - Class for CLOG WebGL-Frame-Buffer objects"
  (clog-webgl-frame-buffer   class)
  (create-webgl-frame-buffer generic-function)
  (bind-frame-buffer         generic-function)
  (delete-frame-buffer       generic-function)

  "CLOG-WebGL-Render-Buffer - Class for CLOG WebGL-Render-Buffer objects"
  (clog-webgl-render-buffer   class)
  (create-webgl-render-buffer generic-function)
  (bind-render-buffer         generic-function)
  (delete-render-buffer       generic-function)

  "CLOG-WebGL-Texture - Class for CLOG WebGL-Texture objects"
  (clog-webgl-texture   class)
  (create-webgl-texture generic-function)
  (bind-texture         generic-function)
  (delete-texture       generic-function))

;; Use clog-canvas to create the html element and then use clog-webgl
;; to obtain the WebGL2 context

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-webgl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-webgl (clog-obj)())

;;;;;;;;;;;;;;;;;;
;; create-webgl ;;
;;;;;;;;;;;;;;;;;;

(defgeneric create-webgl (clog-canvas &key context)
  (:documentation "Create a new CLOG-WebGL from a CLOG-Canvas. Context
can be webgl (version 1) or webgl2 (default)"))


(defmethod create-webgl ((obj clog-canvas) &key (context "webgl2"))
  (let ((web-id (clog-connection:generate-id)))
    (js-execute obj (format nil "clog['~A']=clog['~A'].getContext('~A')"
                            web-id
                            (html-id obj) context))
    (make-instance 'clog-webgl
                   :connection-id (clog::connection-id obj)
                   :html-id web-id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Properties - clog-webgl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric drawing-buffer-width (clog-webgl)
  (:documentation "Drawing are of buffer width. returns float"))

(defmethod drawing-buffer-width ((obj clog-webgl))
  (parse-float (query obj "drawingBufferWidth")))

(defgeneric drawing-buffer-height (clog-webgl)
  (:documentation "Drawing are of buffer height. returns float"))

(defmethod drawing-buffer-height ((obj clog-webgl))
  (parse-float (query obj "drawingBufferHeight")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methdods - clog-webgl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric active-texture (clog-webgl glenum-texture)
  (:documentation "Sets the active textcture in gl context based on :TEXTUREI
where I is the texture number."))

(defmethod active-texture ((obj clog-webgl) glenum-texture)
  (execute obj (format nil "activeTexture(~A.~A)"
                       (script-id obj) glenum-texture)))

(defgeneric blend-color (clog-webgl red green blue alpha)
  (:documentation "Used to set the source and destination blending factors."))

(defmethod blend-color ((obj clog-webgl) red green blue alpha)
  (execute obj (format nil "blendColor(~A,~A,~A,~A)"
                       red green blue alpha)))

(defgeneric blend-equation (clog-webgl glenum-mode)
  (:documentation "Used to set both the RGB blend equation and alpha blend equation to a single equation.
:FUNC_ADD : source + destination (default value)
:FUNC_SUBTRACT : source - destination
:FUNC_REVERSE_SUBTRACT : destination - source

When using a WebGL 2 context, the following values are available additionally:

:MIN : Minimum of source and destination
:MAX : Maximum of source and destination"))

(defmethod blend-equation ((obj clog-webgl) glenum-mode)
  (execute obj (format nil "blendEquation(~A.~A)" (script-id obj) glenum-mode)))

(defgeneric blend-equation-separate (clog-webgl glenum-mode-rgb glenum-mode-alpha)
  (:documentation "Used to set both the RGB blend equation and alpha blend equation to a single equation.
:FUNC_ADD : source + destination (default value)
:FUNC_SUBTRACT : source - destination
:FUNC_REVERSE_SUBTRACT : destination - source

When using a WebGL 2 context, the following values are available additionally:

:MIN : Minimum of source and destination
:MAX : Maximum of source and destination"))

(defmethod blend-equation-separate ((obj clog-webgl) glenum-mode-rgb glenum-mode-alpha)
  (execute obj (format nil "blendEquationSeparate(~A.~A,~A.~A)"
                       (script-id obj) glenum-mode-rgb
                       (script-id obj) glenum-mode-alpha)))

(defgeneric blend-function (clog-webgl glenum-source-factor glenum-destination-factor)
  (:documentation "Defines which function is used for blending pixel arithmetic.
See - https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContext/blendFunc"))

(defmethod blend-function ((obj clog-webgl) glenum-sfactor glenum-dfactor)
  (execute obj (format nil "blendFunc(~A.~A,~A.~A)"
                       (script-id obj) glenum-sfactor
                       (script-id obj) glenum-dfactor)))

(defgeneric blend-function-seperate
    (clog-webgl
     glenum-source-factor-rgb      glenum-destination-factor-rgb
     glenum-source-factor-alpha    glenum-destination-factor-alpha)
  (:documentation "Defines which function is used for blending pixel arithmetic.
See - https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContext/blendFunc"))

(defmethod blend-function-seperate
    ((obj clog-webgl)
     glenum-sfactor-rgb   glenum-dfactor-rgb
     glenum-sfactor-alpha glenum-dfactor-alpha)
  (execute obj (format nil "blendFunc(~A.~A,~A.~A,~A.~A,~A.~A)"
                       (script-id obj) glenum-sfactor-rgb
                       (script-id obj) glenum-dfactor-rgb
                       (script-id obj) glenum-sfactor-alpha
                       (script-id obj) glenum-dfactor-alpha)))

(defgeneric check-frame-buffer-status (clog-webgl target)
  (:documentation "Completeness status of frame buffer. Target can be:
:FRAMEBUFFER
Collection buffer data storage of color, alpha, depth and stencil buffers used to render an image.

When using a WebGL 2 context, the following values are available additionally:

:DRAW_FRAMEBUFFER
Equivalent to :FRAMEBUFFER. Used as a destination for drawing, rendering, clearing, and writing operations.

:READ_FRAMEBUFFER
Used as a source for reading operations."))

(defmethod check-frame-buffer-status ((obj clog-webgl) target)
  (parse-integer (query obj (format nil "checkFrameBufferStatus(~A.~A)"
                                    (script-id obj) target))
                 :junk-allowed t))

(defgeneric clear-color (clog-webgl red green blue alpha)
  (:documentation "Specifies the color values used when clearing color buffers
with CLEAR-WEBGL."))

(defmethod clear-color ((obj clog-webgl) red green blue alpha)
  (execute obj (format nil "clearColor(~A,~A,~A,~A)"
                       red green blue alpha)))

(defgeneric clear-depth (clog-webgl depth)
  (:documentation "Specifying the depth value used when the depth buffer is cleared."))

(defmethod clear-depth ((obj clog-webgl) depth)
  (execute obj (format nil "clearDepth(~A)"
                       depth)))

(defgeneric clear-stencil (clog-webgl stencil)
  (:documentation "Specifying the stencil index used when the stencil buffer is cleared."))

(defmethod clear-stencil ((obj clog-webgl) stencil)
  (execute obj (format nil "clearStencil(~A)"
                       stencil)))

(defgeneric clear-webgl (clog-webgl glenum-mask)
  (:documentation "Clears buffers to preset values. GLENUM-MASK can be:
:COLOR_BUFFER_BIT
:DEPTH_BUFFER_BIT
:STENCIL_BUFFER_BIT"))

(defmethod clear-webgl ((obj clog-webgl) glenum-mask)
  (execute obj (format nil "clear(~A.~A)"
                       (script-id obj) glenum-mask)))

(defgeneric color-mask (clog-webgl red green blue alpha)
  (:documentation "Sets which color components to enable or to disable when drawing or rendering
red green blue alpha are nil or true"))

(defmethod color-mask ((obj clog-webgl) red green blue alpha)
  (execute obj (format nil "colorMask(~A,~A,~A,~A)"
                       (p-true-js red) (p-true-js green)
                       (p-true-js blue) (p-true-js alpha))))

;; WebGLRenderingContext.compressedTexImage2D() and WebGL2RenderingContext.compressedTexImage3D()
;; WebGLRenderingContext.compressedTexSubImage2D()
;; WebGLRenderingContext.copyTexImage2D()
;; WebGLRenderingContext.copyTexSubImage2D()

(defgeneric cull-face (clog-webgl glenum-mode)
  (:documentation "Specifies whether or not front- and/or back-facing polygons
can be culled. GLENUM-MODE can be:
:FRONT
:BACK
:FRONT_AND_BACK"))

(defmethod cull-face ((obj clog-webgl) glenum-mode)
  (execute obj (format nil "cullFace(~A.~A)"
                       (script-id obj) glenum-mode)))

(defgeneric depth-function (clog-webgl glenum-func)
  (:documentation "Specifies a function that compares incoming pixel depth to
the current depth buffer value. GLENUM-FUNC can be:
:NEVER (never pass)
:LESS (pass if the incoming value is less than the depth buffer value)
:EQUAL (pass if the incoming value equals the depth buffer value)
:LEQUAL (pass if the incoming value is less than or equal to the depth buffer value)
:GREATER (pass if the incoming value is greater than the depth buffer value)
:NOTEQUAL (pass if the incoming value is not equal to the depth buffer value)
:GEQUAL (pass if the incoming value is greater than or equal to the depth buffer value)
:ALWAYS (always pass)"))

(defmethod depth-function ((obj clog-webgl) glenum-func)
  (execute obj (format nil "depthFunc(~A.~A)"
                       (script-id obj) glenum-func)))

(defgeneric depth-mask (clog-webgl enablep)
  (:documentation "Specifying whether or not writing into the depth buffer is
enabled"))

(defmethod depth-mask ((obj clog-webgl) enablep)
  (execute obj (format nil "depthMask(~A)"
                       (p-true-js enablep))))

(defgeneric depth-range (clog-webgl znear zfar)
  (:documentation "Specifies the depth range mapping from normalized device
coordinates to window or viewport coordinates."))

(defmethod depth-range ((obj clog-webgl) znear zfar)
  (execute obj (format nil "depthRange(~A,~A)"
                       znear zfar)))

(defgeneric enable-capability (clog-webgl glenum-capability)
  (:documentation "Enables specific WebGL capabilities for this context.
For GLENUM-CAPABILITY see:
https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContext/enable"))

(defmethod enable-capability ((obj clog-webgl) glenum-capability)
  (execute obj (format nil "enable(~A.~A)"
                       (script-id obj) glenum-capability)))

(defgeneric disable-capability (clog-webgl glenum-capability)
  (:documentation "Disables specific WebGL capabilities for this context.
For GLENUM-CAPABILITY see:
https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContext/disable"))

(defmethod disable-capability ((obj clog-webgl) glenum-capability)
  (execute obj (format nil "disable(~A.~A)"
                       (script-id obj) glenum-capability)))

(defgeneric draw-arrays (clog-webgl mode offset count)
  (:documentation "Renders primitives from array data. MODE can be:
:POINTS Draws a single dot.
:LINE_STRIP Draws a straight line to the next vertex.
:LINE_LOOP Draws a straight line to the next vertex, and connects the last vertex back to the first.
:LINES Draws a line between a pair of vertices.
:TRIANGLE_STRIP
:TRIANGLE_FAN
:TRIANGLES Draws a triangle for a group of three vertices."))

(defmethod draw-arrays ((obj clog-webgl) primitive-type offset count)
  (execute obj (format nil "drawArrays(~A.~A,~A,~A)"
                       (script-id obj) primitive-type
                       offset count)))

(defgeneric draw-elements (clog-webgl mode count type offset)
  (:documentation "Renders primitives from array data.
MODE can be:
:POINTS Draws a single dot.
:LINE_STRIP Draws a straight line to the next vertex.
:LINE_LOOP Draws a straight line to the next vertex, and connects the last vertex back to the first.
:LINES Draws a line between a pair of vertices.
:TRIANGLE_STRIP
:TRIANGLE_FAN
:TRIANGLES Draws a triangle for a group of three vertices.

TYPE can be:
:UNSIGNED_BYTE
:UNSIGNED_SHORT"))

(defmethod draw-elements ((obj clog-webgl) mode count type offset)
  (execute obj (format nil "drawElements(~A.~A,~A,~A.~A,~A)"
                       (script-id obj) mode
                       count
                       (script-id obj) type
                       offset)))

(defgeneric enable-vertex-attribute-array (clog-webgl attribute-location)
  (:documentation "Turns the generic vertex attribute array on at a given index
position."))

(defmethod enable-vertex-attribute-array ((obj clog-webgl) attribute-location)
  (execute obj (format nil "enableVertexAttribArray(~A)"
                       attribute-location)))

(defgeneric finish (clog-webgl)
  (:documentation "Blocks execution until all previously called commands are
finished. [this needs to be written to fire an event when done to work fully
with CLOG]"))

(defmethod finish ((obj clog-webgl))
  (execute obj "finish()"))

(defgeneric flush (clog-webgl)
  (:documentation "Empties different buffer commands, causing all commands to
be executed as quickly as possible"))

(defmethod flush ((obj clog-webgl))
  (execute obj "flush()"))

(defgeneric frame-buffer-render-buffer (clog-webgl target attachment renderbuffertarget
                                        clog-webgl-render-buffer)
  (:documentation "Attaches a clog-Render-buffer object to a clog-frame-buffer object"))

(defmethod frame-buffer-render-buffer ((obj clog-webgl) target attachment renderbuffertarget
                                       renderbuffer)
  (execute obj (format nil "framebufferRenderbuffer(~A.~A,~A.~A,~A.~A,~A)"
                       (script-id obj) target
                       (script-id obj) attachment
                       (script-id obj) renderbuffertarget
                       (script-id renderbuffer))))

(defgeneric frame-buffer-texture-2d (clog-webgl target attachment
                                     textarget clog-frame-buffer level)
  (:documentation "attaches a texture to a clog-frame-buffer"))

(defmethod frame-buffer-texture-2d ((obj clog-webgl) target attachment
                                    textarget texture level)
    (execute obj (format nil "framebufferTexture2D(~A.~A,~A.~A,~A.~A,~A~A)"
                       (script-id obj) target
                       (script-id obj) attachment
                       (script-id obj) textarget
                       (script-id texture)
                       level)))

(defgeneric front-face (clog-webgl glenum-mode)
  (:documentation "Specifies whether polygons are front- or back-facing by
setting a winding orientation. GLENUM-MODE can be:
:CW Clock-wise winding.
:CCW Counter-clock-wise winding."))

(defmethod front-face ((obj clog-webgl) glenum-mode)
  (execute obj (format nil "frontFace(~A.~A)"
                       (script-id obj) glenum-mode)))

(defgeneric generate-mipmap (clog-webgl glenum-target)
  (:documentation "Generates a set of mipmaps for a WebGLTexture object.
GLENUM-TARGET can be:
:TEXTURE_2D A two-dimensional texture.
:TEXTURE_CUBE_MAP A cube-mapped texture.
When using a WebGL 2 context, the following values are available additionally:

:TEXTURE_3D A three-dimensional texture.
:TEXTURE_2D_ARRAY A two-dimensional array texture"))

(defmethod generate-mipmap ((obj clog-webgl) glenum-target)
  (execute obj (format nil "generateMipmap(~A.~A)"
                       (script-id obj) glenum-target)))

(defgeneric disable-vertex-attribute-array (clog-webgl attribute-location)
  (:documentation "Turns the generic vertex attribute array off at a given index
position."))

(defmethod disable-vertex-attribute-array ((obj clog-webgl) attribute-location)
  (execute obj (format nil "desableVertexAttribArray(~A)"
                       attribute-location)))

(defmethod viewport ((obj clog-webgl) x y width height)
  (execute obj (format nil "viewport(~A,~A,~A,~A)"
                       x y width height)))

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
;; Implementation - clog-webgl-active-info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-webgl-active-info (clog-obj)())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Properties - clog-webgl-active-info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric info-name (clog-webgl-active-info)
  (:documentation "Active Info Name"))

(defmethod info-name ((obj clog-webgl-active-info))
  (query obj "name"))

(defgeneric info-size (clog-webgl-active-info)
  (:documentation "Active Info Size"))

(defmethod info-size ((obj clog-webgl-active-info))
  (query obj "size"))

(defgeneric info-type (clog-webgl-active-info)
  (:documentation "Active Info Type"))

(defmethod info-type ((obj clog-webgl-active-info))
  (query obj "type"))

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

(defgeneric attribute-location (clog-webgl-program name)
  (:documentation "Returns the location of an attribute variable in clog-program"))

(defmethod attribute-location ((obj clog-webgl-program) name)
  (query (gl obj) (format nil "getAttribLocation(~A, '~A')"
                          (script-id obj) name)))

(defgeneric uniform-location (clog-webgl-program name)
  (:documentation "Returns the location of an uniform variable in clog-program"))

(defmethod uniform-location ((obj clog-webgl-program) name)
  (query (gl obj) (format nil "getUniformLocation(~A, '~A')"
                          (script-id obj) name)))

(defmethod program-info-log ((obj clog-webgl-program))
  (query (gl obj) (format nil "getProgramInfoLog(~A)"
                          (script-id obj))))

(defgeneric active-attribute (clog-webgl-program index)
  (:documentation "Query about unknown attributes"))

(defmethod active-attribute ((obj clog-webgl-program) index)
  (let ((web-id (clog-connection:generate-id)))
    (js-execute obj (format nil "clog['~A']=~A.getActiveAttrib(~A,~A)"
                            web-id
                            (script-id (gl obj)) (script-id obj) index))
    (make-instance 'clog-webgl-active-info
                   :connection-id (clog::connection-id obj)
                   :html-id web-id)))

(defgeneric active-uniform (clog-webgl-program index)
  (:documentation "Query about unknown uniforms"))

(defmethod active-uniform ((obj clog-webgl-program) index)
  (let ((web-id (clog-connection:generate-id)))
    (js-execute obj (format nil "clog['~A']=~A.getActiveUniform(~A,~A)"
                            web-id
                            (script-id (gl obj)) (script-id obj) index))
    (make-instance 'clog-webgl-active-info
                   :connection-id (clog::connection-id obj)
                   :html-id web-id)))

;; WebGLRenderingContext.getAttachedShaders()

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods - clog-webgl-program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric attach-shader (clog-webgl-program clog-webgl-shader)
  (:documentation "Attaches either a fragment or vertex CLOG-WEBGL-SHADER"))

(defmethod attach-shader ((obj clog-webgl-program) (shader clog-webgl-shader))
  (execute (gl obj) (format nil "attachShader(~A, ~A)"
                            (script-id obj)
                            (script-id shader))))

(defgeneric detach-shader (clog-webgl-program clog-webgl-shader)
  (:documentation "Detaches a CLOG-WEBGL-SHADER"))

(defmethod detach-shader ((obj clog-webgl-program) (shader clog-webgl-shader))
  (execute (gl obj) (format nil "detachShader(~A, ~A)"
                            (script-id obj)
                            (script-id shader))))

(defgeneric bind-attribute-location (clog-webgl-program index name)
  (:documentation "Binds a generic vertex INDEX to an attribute variable
called NAME."))

(defmethod bind-attribute-location ((obj clog-webgl-program) index name)
  (execute (gl obj) (format nil "bindAttribLocation(~A, ~A, '~A')"
                            (script-id obj) index name)))

(defmethod link-program ((obj clog-webgl-program))
  (execute (gl obj) (format nil "linkProgram(~A)" (script-id obj))))

(defmethod use-program ((obj clog-webgl-program))
  (execute (gl obj) (format nil "useProgram(~A)" (script-id obj))))

(defmethod delete-program ((obj clog-webgl-program))
  (execute (gl obj) (format nil "deleteProgram(~A)"
                            (script-id obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-webgl-buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-webgl-buffer (clog-obj)
  ((gl      :accessor gl      :initarg :clog-webgl)
   (gl-type :accessor gl-type :initarg :gl-type)))

(defgeneric create-webgl-buffer (clog-webgl &key bind-type)
  (:documentation "Create a clog-webgl-buffer. If BIND-TYPE
is set binds the buffer to either :ARRAY_BUFFER or :ELEMENT_ARRAY_BUFFER
in WebGL2 the following added:
:COPY_READ_BUFFER : Buffer for copying from one buffer object to another.
:COPY_WRITE_BUFFER : Buffer for copying from one buffer object to another.
:TRANSFORM_FEEDBACK_BUFFER : Buffer for transform feedback operations.
:UNIFORM_BUFFER : Buffer used for storing uniform blocks.
:PIXEL_PACK_BUFFER : Buffer used for pixel transfer operations.
:PIXEL_UNPACK_BUFFER : Buffer used for pixel transfer operations."))

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

(defgeneric bind-buffer (clog-webgl-buffer glenum-target)
  (:documentation "Set BIND-TYPE of buffer to either :ARRAY_BUFFER or
:ELEMENT_ARRAY_BUFFER. WebGL2 adds:
:COPY_READ_BUFFER : Buffer for copying from one buffer object to another.
:COPY_WRITE_BUFFER : Buffer for copying from one buffer object to another.
:TRANSFORM_FEEDBACK_BUFFER : Buffer for transform feedback operations.
:UNIFORM_BUFFER : Buffer used for storing uniform blocks.
:PIXEL_PACK_BUFFER : Buffer used for pixel transfer operations.
:PIXEL_UNPACK_BUFFER : Buffer used for pixel transfer operations."))

(defmethod bind-buffer ((obj clog-webgl-buffer) glenum-target)
  (execute (gl obj) (format nil "bindBuffer(~A.~A,~A)"
                            (script-id (gl obj)) glenum-target
                            (script-id obj)))
  (setf (gl-type obj) glenum-target))

(defgeneric buffer-data (clog-webgl-buffer data-list data-type glenum-usage)
  (:documentation "Initializes and creates the buffer object's data store.
DATA-LIST is a Lisp list of data elements.
DATA-TYPE is the WebGL data type as a string \"Float32Array\"
GLENUM-USAGE us a usage hint like :STATIC_DRAW"))

(defmethod buffer-data ((obj clog-webgl-buffer) data-list data-type hint)
  (execute (gl obj) (format nil "bufferData(~A.~A, new ~A([~{~A~^,~}]), ~A.~A)"
                            (script-id (gl obj)) (gl-type obj)
                            data-type data-list
                            (script-id (gl obj)) hint)))

(defgeneric buffer-sub-data (clog-webgl-buffer offset data-list data-type)
  (:documentation "Initializes and creates the buffer object's data store.
OFFSET element index offset where to start reading the buffer.
DATA-LIST is a Lisp list of data elements.
DATA-TYPE is the WebGL data type as a string \"Float32Array\""))

(defmethod buffer-sub-data ((obj clog-webgl-buffer) offset data-list data-type)
  (execute (gl obj) (format nil "bufferSubData(~A.~A, ~A, new ~A([~{~A~^,~}]))"
                            (script-id (gl obj)) (gl-type obj)
                            offset
                            data-type data-list)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-webgl-frame-buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-webgl-frame-buffer (clog-obj)
  ((gl      :accessor gl      :initarg :clog-webgl)
   (gl-type :accessor gl-type :initarg :gl-type)))

(defgeneric create-webgl-frame-buffer (clog-webgl &key bind-type)
  (:documentation "Create a clog-webgl-frame-buffer. If BIND-TYPE
is set binds the frame-buffer to :FRAMEBUFFER in WebGL2 the following are
added:
:DRAW_FRAMEBUFFER : Used as a destination for drawing operations such as draw*,
clear* and blit-frame-buffer.
:READ_FRAMEBUFFER : Used as a source for reading operations such as readPixels
and blit-frame-buffer."))

(defmethod create-webgl-frame-buffer ((obj clog-webgl) &key bind-type)
  (let ((web-id (clog-connection:generate-id)))
    (js-execute obj (format nil "clog['~A']=~A.createFrameBuffer()"
                            web-id
                            (script-id obj)))
    (let ((new-obj (make-instance 'clog-webgl-frame-buffer
                                  :connection-id (clog::connection-id obj)
                                  :html-id web-id
                                  :clog-webgl obj
                                  :gl-type bind-type)))
      (when bind-type
        (bind-frame-buffer new-obj bind-type))
      new-obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods - clog-webgl-frame-buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric bind-frame-buffer (clog-webgl-frame-buffer glenum-target)
  (:documentation "Set BIND-TYPE of frame-buffer to :FRAMEBUFFER
in WebGL2 the following are added:
:DRAW_FRAMEBUFFER : Used as a destination for drawing operations such as draw*,
clear* and blit-frame-buffer.
:READ_FRAMEBUFFER : Used as a source for reading operations such as readPixels
and blit-frame-buffer"))

(defmethod bind-frame-buffer ((obj clog-webgl-frame-buffer) glenum-target)
  (execute (gl obj) (format nil "bindFrameBuffer(~A.~A,~A)"
                            (script-id (gl obj)) glenum-target
                            (script-id obj)))
  (setf (gl-type obj) glenum-target))

(defmethod delete-frame-buffer ((obj clog-webgl-frame-buffer))
  (execute (gl obj) (format nil "deleteFrameBuffer(~A)"
                            (script-id obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-webgl-render-buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-webgl-render-buffer (clog-obj)
  ((gl      :accessor gl      :initarg :clog-webgl)
   (gl-type :accessor gl-type :initarg :gl-type)))

(defgeneric create-webgl-render-buffer (clog-webgl &key bind-type)
  (:documentation "Create a clog-webgl-render-buffer. If BIND-TYPE
is set binds the render-buffer to :RENDERBUFFER"))

(defmethod create-webgl-render-buffer ((obj clog-webgl) &key bind-type)
  (let ((web-id (clog-connection:generate-id)))
    (js-execute obj (format nil "clog['~A']=~A.createRenderBuffer()"
                            web-id
                            (script-id obj)))
    (let ((new-obj (make-instance 'clog-webgl-render-buffer
                                  :connection-id (clog::connection-id obj)
                                  :html-id web-id
                                  :clog-webgl obj
                                  :gl-type bind-type)))
      (when bind-type
        (bind-render-buffer new-obj bind-type))
      new-obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods - clog-webgl-render-buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric bind-render-buffer (clog-webgl-render-buffer glenum-target)
  (:documentation "Set BIND-TYPE of render-buffer to :RENDERBUFFER"))

(defmethod bind-render-buffer ((obj clog-webgl-render-buffer) glenum-target)
  (execute (gl obj) (format nil "bindRenderBuffer(~A.~A,~A)"
                            (script-id (gl obj)) glenum-target
                            (script-id obj)))
  (setf (gl-type obj) glenum-target))

(defmethod delete-render-buffer ((obj clog-webgl-render-buffer))
  (execute (gl obj) (format nil "deleteRenderBuffer(~A)"
                            (script-id obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-webgl-texture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-webgl-texture (clog-obj)
  ((gl      :accessor gl      :initarg :clog-webgl)
   (gl-type :accessor gl-type :initarg :gl-type)))

(defgeneric create-webgl-texture (clog-webgl &key bind-type)
  (:documentation "Create a clog-webgl-texture. If BIND-TYPE
is set binds the texture to:
:TEXTURE_2D : A two-dimensional texture.
:TEXTURE_CUBE_MAP : A cube-mapped texture.
in WebGL 2 also:
:TEXTURE_3D : A three-dimensional texture.
:TEXTURE_2D_ARRAY : A two-dimensional array texture."))

(defmethod create-webgl-texture ((obj clog-webgl) &key bind-type)
  (let ((web-id (clog-connection:generate-id)))
    (js-execute obj (format nil "clog['~A']=~A.createTexture()"
                            web-id
                            (script-id obj)))
    (let ((new-obj (make-instance 'clog-webgl-texture
                                  :connection-id (clog::connection-id obj)
                                  :html-id web-id
                                  :clog-webgl obj
                                  :gl-type bind-type)))
      (when bind-type
        (bind-texture new-obj bind-type))
      new-obj)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods - clog-webgl-texture
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric bind-texture (clog-webgl-texture glenum-target)
  (:documentation "Set BIND-TYPE of texture to:
:TEXTURE_2D : A two-dimensional texture.
:TEXTURE_CUBE_MAP : A cube-mapped texture.
in WebGL 2 also:
:TEXTURE_3D : A three-dimensional texture.
:TEXTURE_2D_ARRAY : A two-dimensional array texture."))

(defmethod bind-texture ((obj clog-webgl-texture) glenum-target)
  (execute (gl obj) (format nil "bindTexture(~A.~A,~A)"
                            (script-id (gl obj)) glenum-target
                            (script-id obj)))
  (setf (gl-type obj) glenum-target))

(defmethod delete-texture ((obj clog-webgl-texture))
  (execute (gl obj) (format nil "deleteTexture(~A)"
                            (script-id obj))))
