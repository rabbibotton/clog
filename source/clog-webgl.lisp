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

;; Use clog-canvas to create the html element and then use
;; clog-webgl:create-webgl to obtain the WebGL2 context


(defsection @clog-webgl (:title "CLOG WebGL Objects")
  "CLOG-WebGL - Class for CLOG WebGL objects"
  (clog-webgl                     class)
  (create-webgl                   generic-function)

  (drawing-buffer-width              generic-function)
  (drawing-buffer-height             generic-function)
  (buffer-parameter                  generic-function)
  (context-attributes                generic-function)
  (webgl-error                       generic-function)
  (frame-buffer-attachment-parameter generic-function)
  (parameter                         generic-function)
  (render-buffer-parameter           generic-function)
  (texture-paramenter                generic-function)
  (vertex-attribute                  generic-function)

  (active-texture                    generic-function)
  (blend-color                       generic-function)
  (blend-equation                    generic-function)
  (blend-equation-seperate           generic-function)
  (blend-function                    generic-function)
  (blend-function-seperate           generic-function)
  (check-frame-buffer-status         generic-function)
  (clear-color                       generic-function)
  (clear-depth                       generic-function)
  (clear-webgl                       generic-function)
  (clear-stencil                     generic-function)
  (color-mask                        generic-function)
  (depth-function                    generic-function)
  (depth-mask                        generic-function)
  (depth-range                       generic-function)
  (disable-capability                generic-function)
  (disable-vertex-attribute-array    generic-function)
  (draw-arrays                       generic-function)
  (draw-elements                     generic-function)
  (enable-capability                 generic-function)
  (enable-vertex-attribute-array     generic-function)
  (finish                            generic-function)
  (flush                             generic-function)
  (frame-buffer-render-buffer        generic-function)
  (frame-buffer-texture-2d           generic-function)
  (front-face                        generic-function)
  (generate-mipmap                   generic-function)
  (hint                              generic-function)
  (is-capability-enabled             generic-function)
  (is-context-lost                   generic-function)
  (pixel-store-integer               generic-function)
  (polygon-offset                    generic-function)
  (render-buffer-storage             generic-function)
  (sample-coverage                   generic-function)
  (scissor                           generic-function)
  (stencil-function                  generic-function)
  (stencil-function-seperate         generic-function)
  (stencil-mask                      generic-function)
  (stencil-mask-seperate             generic-function)
  (stencil-operation                 generic-function)
  (stencil-operation-seperate        generic-function)
  (texture-parameter-float           generic-function)
  (texture-parameter-integer         generic-function)
  (uniform-matrix                    generic-function)
  (viewport                          generic-function)
  (vertex-attribute-pointer          generic-function)

  (compile-shader-source             generic-function)
  (compile-webgl-program             generic-function)

  (clog-webgl-uniform                class)

  "CLOG-WebGL-Active-Info - Class for CLOG WebGL Active Info objects"
  (clog-webgl-active-info class)
  (info-name              generic-function)
  (info-size              generic-function)
  (info-type              generic-function)

  "CLOG-WebGL-Shader - Class for CLOG WebGL-Shader objects"
  (clog-webgl-shader class)
  (create-shader     generic-function)
  (is-shader         generic-function)

  (shader-source     generic-function)
  (shader-parameter  generic-function)
  (shader-info-log   generic-function)
  (compile-shader    generic-function)
  (delete-shader     generic-function)

  "CLOG-WebGL-Program - Class for CLOG WebGL-Program objects"
  (clog-webgl-program class)
  (create-program     generic-function)
  (is-program         generic-function)

  (attach-shader           generic-function)
  (detach-shader           generic-function)
  (bind-attribute-location generic-function)
  (program-parameter       generic-function)
  (attribute-location      generic-function)
  (uniform-location        generic-function)
  (active-attribute        generic-function)
  (active-uniform          generic-function)
  (uniform                 generic-function)
  (program-info-log        generic-function)
  (link-program            generic-function)
  (use-program             generic-function)
  (delete-program          generic-function)

  "CLOG-WebGL-Buffer - Class for CLOG WebGL-Buffer objects"
  (clog-webgl-buffer   class)
  (create-webgl-buffer generic-function)
  (is-buffer           generic-function)

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
  (is-frame-buffer           generic-function)

  (bind-frame-buffer         generic-function)
  (delete-frame-buffer       generic-function)

  "CLOG-WebGL-Render-Buffer - Class for CLOG WebGL-Render-Buffer objects"
  (clog-webgl-render-buffer   class)
  (create-webgl-render-buffer generic-function)
  (is-render-buffer           generic-function)

  (bind-render-buffer         generic-function)
  (delete-render-buffer       generic-function)

  "CLOG-WebGL-Texture - Class for CLOG WebGL-Texture objects"
  (clog-webgl-texture   class)
  (create-webgl-texture generic-function)
  (is-texture           generic-function)

  (bind-texture         generic-function)
  (delete-texture       generic-function))

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

(defgeneric buffer-parameter (clog-webgl glenum-target glenum-pname)
  (:documentation "Returns information about the buffer.
target
------
A GLenum specifying the target buffer object. Possible values:

:ARRAY_BUFFER
Buffer containing vertex attributes, such as vertex coordinates, texture coordinate data, or vertex color data.

:ELEMENT_ARRAY_BUFFER
Buffer used for element indices.

When using a WebGL 2 context, the following values are available additionally:

:COPY_READ_BUFFER
Buffer for copying from one buffer object to another.

:COPY_WRITE_BUFFER
Buffer for copying from one buffer object to another.

:TRANSFORM_FEEDBACK_BUFFER
Buffer for transform feedback operations.

:UNIFORM_BUFFER
Buffer used for storing uniform blocks.

:PIXEL_PACK_BUFFER
Buffer used for pixel transfer operations.

:PIXEL_UNPACK_BUFFER
Buffer used for pixel transfer operations.

pname
-----
A GLenum specifying information to query. Possible values:

:BUFFER_SIZE
Returns a GLint indicating the size of the buffer in bytes.

:BUFFER_USAGE
Returns a GLenum indicating the usage pattern of the buffer. One of the followings:

:STATIC_DRAW
:DYNAMIC_DRAW
:STREAM_DRAW

When using a WebGL 2 context, the following values are available additionally:

:STATIC_READ
:DYNAMIC_READ
:STREAM_READ
:STATIC_COPY
:DYNAMIC_COPY
:STREAM_COPY"))

(defmethod buffer-parameter ((obj clog-webgl) glenum-target glenum-pname)
  (query obj (format nil "getBufferParameter(~A.~A,~A.~A)"
                     (script-id obj) glenum-target
                     (script-id obj) glenum-pname)))

(defmethod context-attributes ((obj clog-webgl))
  (query obj "getContextAttributes()"))

(defmethod webgl-error ((obj clog-webgl))
  (parse-integer (query obj "getError()")))

;; WebGLRenderingContext.getExtension()

(defmethod frame-buffer-attachment-parameter ((obj clog-webgl) glenum-target
                                              glenum-attachment
                                              glenum-pname)
  (query obj (format nil "getFramebufferAttachmentParameter(~A.~A,~A.~A,~A.~A)"
                     (script-id obj) glenum-target
                     (script-id obj) glenum-attachment
                     (script-id obj) glenum-pname)))

(defmethod parameter ((obj clog-webgl) glenum-pname)
  (query obj (format nil "getParameter(~A.~A)"
                     (script-id obj) glenum-pname)))

(defmethod render-buffer-parameter ((obj clog-webgl) glenum-target glenum-pname)
  (query obj (format nil "getRenderbufferParameter(~A.~A,~A.~A)"
                     (script-id obj) glenum-target
                     (script-id obj) glenum-pname)))

;; WebGLRenderingContext.getShaderPrecisionFormat()

;; WebGLRenderingContext.getShaderSource()

;; WebGLRenderingContext.getSupportedExtensions()

(defmethod texture-parameter ((obj clog-webgl) glenum-target glenum-pname)
  (query obj (format nil "getTexParameter(~A.~A,~A.~A)"
                     (script-id obj) glenum-target
                     (script-id obj) glenum-pname)))

(defgeneric vertex-attribute (clog-webgl index glenum-pname)
  (:documentation "Information about a vertex attribute at a given position.
pname
-----
A GLenum specifying the information to query. Possible values:

:VERTEX_ATTRIB_ARRAY_BUFFER_BINDING
Returns the currently bound WebGLBuffer.

:VERTEX_ATTRIB_ARRAY_ENABLED
Returns a GLboolean that is true if the vertex attribute is enabled at this index. Otherwise false.

:VERTEX_ATTRIB_ARRAY_SIZE
Returns a GLint indicating the size of an element of the vertex array.

:VERTEX_ATTRIB_ARRAY_STRIDE
Returns a GLint indicating the number of bytes between successive elements in the array. 0 means that the elements are sequential.

:VERTEX_ATTRIB_ARRAY_TYPE
Returns a GLenum representing the array type. One of

:BYTE
:UNSIGNED_BYTE
:SHORT,
:UNSIGNED_SHORT
:FLOAT
:VERTEX_ATTRIB_ARRAY_NORMALIZED
Returns a GLboolean that is true if fixed-point data types are normalized for the vertex attribute array at the given index.

:CURRENT_VERTEX_ATTRIB
Returns a Float32Array (with 4 elements) representing the current value of the vertex attribute at the given index.

When using a WebGL 2 context, the following values are available additionally:

:VERTEX_ATTRIB_ARRAY_INTEGER
Returns a GLboolean indicating whether an integer data type is in the vertex attribute array at the given index.

:VERTEX_ATTRIB_ARRAY_DIVISOR
Returns a GLint describing the frequency divisor used for instanced rendering."))


(defmethod vertex-attribute ((obj clog-webgl) index glenum-pname)
  (query obj (format nil "getVertexAttrib(~A,~A.~A)"
                     index
                     (script-id obj) glenum-pname)))

;; WebGLRenderingContext.getVertexAttribOffset()

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

(defgeneric is-capability-enabled (clog-webgl glenum-capability)
  (:documentation "Return true if glenum-capability is enabled.
https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContext/isEnabled"))

(defmethod is-capability-enabled ((obj clog-webgl) glenum-capability)
  (js-true-p (query obj (format nil "isEnabled(~A.~A)"
                                (script-id obj) glenum-capability))))

(defgeneric disable-vertex-attribute-array (clog-webgl attribute-location)
  (:documentation "Turns the generic vertex attribute array off at a given index
position."))

(defmethod disable-vertex-attribute-array ((obj clog-webgl) attribute-location)
  (execute obj (format nil "desableVertexAttribArray(~A)"
                       attribute-location)))

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

(defgeneric hint (clog-webgl glenum-target glenum-mode)
  (:documentation "Hints for certain behaviors. The interpretation of these
hints depend on the implementation.

target
------
Sets which behavior to be controlled. Possible values:

:GENERATE_MIPMAP_HINT
Quality of filtering when generating mipmap images with WebGLRenderingContext.generateMipmap().

When using the OES_standard_derivatives extension:

ext.FRAGMENT_SHADER_DERIVATIVE_HINT_OES
Accuracy of the derivative calculation for the GLSL built-in functions: dFdx, dFdy, and fwidth.

When using a WebGL 2 context, the following values are available additionally:

:FRAGMENT_SHADER_DERIVATIVE_HINT
Same as ext.FRAGMENT_SHADER_DERIVATIVE_HINT_OES

mode
----
Sets the behavior. The default value is :DONT_CARE. The possible values are:

:FASTEST : The most efficient behavior should be used.
:NICEST : The most correct or the highest quality option should be used.
:DONT_CARE : There is no preference for this behavior."))

(defmethod hint ((obj clog-webgl) glenum-target glenum-mode)
  (execute obj (format nil "hint(~A.~A,~A.~A)"
                       (script-id obj) glenum-target
                       (script-id obj) glenum-mode)))

(defgeneric is-context-lost (clog-webgl)
  (:documentation "Return true if context was lost and needs to be re-established"))

(defmethod is-context-lost ((obj clog-webgl))
  (js-true-p (query obj  "isContextLost()")))

;; WebGLRenderingContext.lineWidth() - lineWidth only 1.0 works, so has no effect anymore since 2017

(defmethod pixel-store-integer ((obj clog-webgl) glenum-pname value)
  (execute obj (format nil "pixelStorei(~A.~A,~A)"
                       (script-id obj) glenum-pname
                       value)))

(defmethod polygon-offset ((obj clog-webgl) factor units)
  (execute obj (format nil "polygonOffset(~A,~A)"
                       factor units)))

;; WebGLRenderingContext.readPixels()

(defmethod render-buffer-storage ((obj clog-webgl) glenum-target
                                  glenum-internal-format
                                  width height)
  (execute obj (format nil "renderbufferStorage(~A.~A,~A.~A,~A,~A)"
                       (script-id obj) glenum-target
                       (script-id obj) glenum-internal-format
                       width height)))

(defmethod sample-coverage ((obj clog-webgl) value invertp)
  (execute obj (format nil "sampleCoverage(~A,~A)"
                       value (p-true-js invertp))))

(defmethod scissor ((obj clog-webgl) x y width height)
  (execute obj (format nil "scissor(~A,~A,~A,~A)"
                       x y width height)))

(defmethod stencil-function ((obj clog-webgl) glenum-func ref mask)
  (execute obj (format nil "stencilFunc(~A.~A,~A,~A)"
                       (script-id obj) glenum-func
                       ref mask)))

(defmethod stencil-function-seperate ((obj clog-webgl) glenum-face
                                      glenum-func ref mask)
  (execute obj (format nil "stencilFuncSeperate(~A.~A,~A.~A,~A,~A)"
                       (script-id obj) glenum-face
                       (script-id obj) glenum-func
                       ref mask)))

(defmethod stencil-mask ((obj clog-webgl) mask)
  (execute obj (format nil "stencilMask(~A)" mask)))

(defmethod stencil-mask-seperate ((obj clog-webgl) glenum-face mask)
  (execute obj (format nil "stencilFuncSeperate(~A.~A,~A)"
                       (script-id obj) glenum-face
                       mask)))

(defmethod stencil-operation ((obj clog-webgl) fail zfail zpass)
  (execute obj (format nil "stencilOp(~A,~A,~A)"
                       fail zfail zpass)))

(defmethod stencil-operation-seperate ((obj clog-webgl) glenum-face
                                       fail zfail zpass)
  (execute obj (format nil "stencilOpSeperate(~A.~A,~A,~A,~A)"
                       (script-id obj) glenum-face
                       fail zfail zpass)))

;; WebGLRenderingContext.texImage2D()

(defmethod texture-parameter-integer ((obj clog-webgl) glenum-target
                                      glenum-pname value)
  (execute obj (format nil "textParameteri(~A.~A,~A.~A,~A)"
                       (script-id obj) glenum-target
                       (script-id obj) glenum-pname
                       value)))

(defmethod texture-parameter-float ((obj clog-webgl) glenum-target
                                    glenum-pname value)
  (execute obj (format nil "textParameterf(~A.~A,~A.~A,~A)"
                       (script-id obj) glenum-target
                       (script-id obj) glenum-pname
                       value)))

;; WebGLRenderingContext.texSubImage2D()
;; WebGL2RenderingContext.uniform[1234][uif][v]()

;; WebGLRenderingContext.uniformMatrix[234]fv()
(defmethod uniform-matrix ((obj clog-webgl) size location normalize matrix)
  (execute obj (format nil "uniformMatrix~Afv(~A,~A,[~{~A~^,~}])"
                       size (script-id location) (p-true-js normalize) matrix)))

;; WebGLRenderingContext.vertexAttrib[1234]f[v]()

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
;; Implementation - clog-webgl-uniform
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-webgl-uniform (clog-obj)())

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

(defgeneric is-shader (clog-webgl-shader)
  (:documentation "Return true if is valid"))

(defmethod is-shader ((obj clog-webgl-shader))
  (js-true-p (query (gl obj) (format nil "isShader(~A)" (script-id obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Properties - clog-webgl-shader
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf shader-source) (source (obj clog-webgl-shader))
  (execute (gl obj) (format nil "shaderSource(~A, '~A')"
                            (script-id obj)
                            (escape-string source)))
  source)

(defgeneric shader-parameter (clog-webgl-shader glenum-param)
  (:documentation "Returns information about the given shader.
:DELETE_STATUS
Returns a GLboolean indicating whether or not the shader is flagged for deletion.

:COMPILE_STATUS
Returns a GLboolean indicating whether or not the last shader compilation was successful.

:SHADER_TYPE
Returns a GLenum indicating whether the shader is a vertex shader (gl.VERTEX_SHADER) or fragment shader (gl.FRAGMENT_SHADER) object.
"))

(defmethod shader-parameter ((obj clog-webgl-shader) glenum-param)
  (query (gl obj) (format nil "getShaderParameter(~A, ~A.~A)"
                          (script-id obj)
                          (script-id (gl obj)) glenum-param)))

(defgeneric shader-info-log (clog-webgl-shader)
  (:documentation "It contains warnings, debugging and compile information."))

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

(defgeneric is-program (clog-webgl-program)
  (:documentation "Return true if is valid"))

(defmethod is-program ((obj clog-webgl-program))
  (js-true-p (query (gl obj) (format nil "isProgram(~A)" (script-id obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parameters - clog-webgl-program
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric program-parameter (clog-webgl-program glenum-param)
  (:documentation "Information about the given program.
:DELETE_STATUS
Returns a GLboolean indicating whether or not the program is flagged for deletion.

:LINK_STATUS
Returns a GLboolean indicating whether or not the last link operation was successful.

:VALIDATE_STATUS
Returns a GLboolean indicating whether or not the last validation operation was successful.

:ATTACHED_SHADERS
Returns a GLint indicating the number of attached shaders to a program.

:ACTIVE_ATTRIBUTES
Returns a GLint indicating the number of active attribute variables to a program.

:ACTIVE_UNIFORMS
Returns a GLint indicating the number of active uniform variables to a program.

When using a WebGL 2 context, the following values are available additionally:

:TRANSFORM_FEEDBACK_BUFFER_MODE
Returns a GLenum indicating the buffer mode when transform feedback is active. May be :SEPARATE_ATTRIBS or :INTERLEAVED_ATTRIBS.

:TRANSFORM_FEEDBACK_VARYINGS
Returns a GLint indicating the number of varying variables to capture in transform feedback mode.

:ACTIVE_UNIFORM_BLOCKS
Returns a GLint indicating the number of uniform blocks containing active uniforms."))

(defmethod program-parameter ((obj clog-webgl-program) glenum-param)
  (query (gl obj) (format nil "getProgramParameter(~A, ~A.~A)"
                          (script-id obj)
                          (script-id (gl obj)) glenum-param)))

(defgeneric attribute-location (clog-webgl-program name)
  (:documentation "Returns the location of an attribute variable in clog-webgl-program"))

(defmethod attribute-location ((obj clog-webgl-program) name)
  (query (gl obj) (format nil "getAttribLocation(~A, '~A')"
                          (script-id obj) name)))

(defgeneric uniform-location (clog-webgl-program name)
  (:documentation "Returns the location of an uniform variable in clog-webgl-program"))

(defmethod uniform-location ((obj clog-webgl-program) name)
  (let ((web-id (clog-connection:generate-id)))
    (js-execute obj (format nil "clog['~A']=~A.getUniformLocation(~A,'~A')"
                            web-id
                            (script-id (gl obj)) (script-id obj) name))
    (make-instance 'clog-webgl-uniform
                   :connection-id (clog::connection-id obj)
                   :html-id web-id)))

(defgeneric uniform (clog-webgl-program location)
  (:documentation "Returns the value of uniform at LOCATION in clog-webgl-program"))

(defmethod uniform ((obj clog-webgl-program) location)
  (query (gl obj) (format nil "getUniform(~A, ~A)"
                          (script-id obj) (script-id location))))

(defgeneric program-info-log (clog-webgl-program)
    (:documentation "Contains errors that occurred during failed linking or
validation of WebGLProgram objects."))

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

(defgeneric is-buffer (clog-webgl-buffer)
  (:documentation "Return true if is valid"))

(defmethod is-buffer ((obj clog-webgl-buffer))
  (js-true-p (query (gl obj) (format nil "isBuffer(~A)" (script-id obj)))))

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
    (js-execute obj (format nil "clog['~A']=~A.createFramebuffer()"
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

(defgeneric is-frame-buffer (clog-webgl-frame-buffer)
  (:documentation "Return true if is valid"))

(defmethod is-frame-buffer ((obj clog-webgl-frame-buffer))
  (js-true-p (query (gl obj) (format nil "isFramebuffer(~A)" (script-id obj)))))


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
  (execute (gl obj) (format nil "bindFramebuffer(~A.~A,~A)"
                            (script-id (gl obj)) glenum-target
                            (script-id obj)))
  (setf (gl-type obj) glenum-target))

(defmethod delete-frame-buffer ((obj clog-webgl-frame-buffer))
  (execute (gl obj) (format nil "deleteFramebuffer(~A)"
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
    (js-execute obj (format nil "clog['~A']=~A.createRenderbuffer()"
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

(defgeneric is-render-buffer (clog-webgl-render-buffer)
  (:documentation "Return true if is valid"))

(defmethod is-render-buffer ((obj clog-webgl-render-buffer))
  (js-true-p (query (gl obj) (format nil "isRenderbuffer(~A)" (script-id obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods - clog-webgl-render-buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric bind-render-buffer (clog-webgl-render-buffer glenum-target)
  (:documentation "Set BIND-TYPE of render-buffer to :RENDERBUFFER"))

(defmethod bind-render-buffer ((obj clog-webgl-render-buffer) glenum-target)
  (execute (gl obj) (format nil "bindRenderbuffer(~A.~A,~A)"
                            (script-id (gl obj)) glenum-target
                            (script-id obj)))
  (setf (gl-type obj) glenum-target))

(defmethod delete-render-buffer ((obj clog-webgl-render-buffer))
  (execute (gl obj) (format nil "deleteRenderbuffer(~A)"
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

(defgeneric is-texture (clog-webgl-texture)
  (:documentation "Return true if is valid"))

(defmethod is-texture ((obj clog-webgl-texture))
  (js-true-p (query (gl obj) (format nil "isTexture(~A)" (script-id obj)))))

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
