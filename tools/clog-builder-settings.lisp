;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG Builder - UI Design tool for CLOG                                ;;;;
;;;; (c) 2020-2022 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :clog-tools)

(defparameter *import-types*
  (list '(:tag "label"
          :control "label")
        '(:tag "input"
          :control "input")
        '(:tag "form"
          :control "form")
        '(:tag "button"
          :control "button")
        '(:tag "a"
          :control "link")
        '(:tag "img"
          :control "image")
        '(:tag "meter"
          :control "meter")
        '(:tag "progress"
          :control "progress")
        '(:tag "ol"
          :control "ol")
        '(:tag "ul"
          :control "ul")
        '(:tag "li"
          :control "li")
        '(:tag "li"
          :control "li")
        '(:tag "table"
          :control "table")
        '(:tag "tr"
          :control "tr")
        '(:tag "td"
          :control "td")
        '(:tag "th"
          :control "th")
        '(:tag "thead"
          :control "thead")
        '(:tag "tbody"
          :control "tbody")
        '(:tag "tfoot"
          :control "tfoot")
        '(:tag "caption"
          :control "tcaption")
        '(:tag "colgroup"
          :control "tcolgroup")
        '(:tag "col"
          :control "tcol")
        '(:tag "span"
          :control "span")
        '(:tag "textarea"
          :control "textarea")
        '(:tag "style"
          :control "style-block")
        '(:tag "fieldset"
          :control "fieldset")
        '(:tag "legend"
          :control "legend")
        '(:tag "datalist"
          :control "datalist")
        '(:tag "select"
          :control "dropdown")
        '(:tag "option"
          :control "option")
        '(:tag "optgroup"
          :control "optgroup")
        '(:tag "dialog"
          :control "dialog")
        '(:tag "p"
          :control "p")
        '(:tag "br"
          :control "br")
        '(:tag "hr"
          :control "hr")
        '(:tag "dl"
          :control "dl")
        '(:tag "dt"
          :control "dt")
        '(:tag "dd"
          :control "dd")
        '(:tag "details"
          :control "details")
        '(:tag "summary"
          :control "summary")
        '(:tag "div"
          :control "div")))

(defparameter *props-location*
  `((:name "top"
     :setup ,(lambda (control td1 td2)
               (declare (ignore control td1))
               (add-class td2 "clog-prop-top"))
     :get ,(lambda (control)
             (if (equal (positioning control) "static")
                 "n/a"
                 (top control)))
     :set ,(lambda (control obj)
             (setf (top control) (text obj))))
    (:name "left"
     :setup ,(lambda (control td1 td2)
               (declare (ignore control td1))
               (add-class td2 "clog-prop-left"))
     :get  ,(lambda (control)
              (if (equal (positioning control) "static")
                  "n/a"
                  (left control)))
     :set  ,(lambda (control obj)
              (setf (left control) (text obj))))
    (:name "bottom"
     :setup ,(lambda (control td1 td2)
               (declare (ignore control td1))
               (add-class td2 "clog-prop-bottom"))
     :get ,(lambda (control)
             (if (equal (positioning control) "static")
                 "n/a"
                 (bottom control)))
     :set ,(lambda (control obj)
             (setf (bottom control) (text obj))))
    (:name "right"
     :setup ,(lambda (control td1 td2)
               (declare (ignore control td1))
               (add-class td2 "clog-prop-right"))
     :get  ,(lambda (control)
              (if (equal (positioning control) "static")
                  "n/a"
                  (right control)))
     :set  ,(lambda (control obj)
              (setf (right control) (text obj))))
    (:name "positioning"
     :setup ,(lambda (control td1 td2)
               (declare (ignore td1))
               (let ((dd (create-select td2))
                     (v  (string-downcase (positioning control))))
                 (add-select-options dd `(,v
                                          "absolute"
                                          "static"
                                          "relative"
                                          "sticky"
                                          "fixed"))
                 (set-on-change dd (lambda (obj)
                                     (declare (ignore obj))
                                     (when (equalp (value dd) "static")
                                       (setf (top control) "")
                                       (setf (left control) ""))
                                     (setf (positioning control) (value dd))
                                     (set-geometry (get-placer control)
                                                   :top (position-top control)
                                                   :left (position-left control)
                                                   :width (client-width control)
                                                   :height (client-height control))
                                     (on-populate-control-properties-win obj)))
                 nil)))))

(defparameter *props-with-height*
  `((:name "width"
     :setup ,(lambda (control td1 td2)
               (declare (ignore control td1))
               (add-class td2 "clog-prop-width"))
     :set  ,(lambda (control obj)
              (setf (width control) (text obj)))
     :get  ,(lambda (control)
              (width control)))
    (:name "height"
     :setup ,(lambda (control td1 td2)
               (declare (ignore control td1))
               (add-class td2 "clog-prop-height"))
     :set  ,(lambda (control obj)
              (setf (height control) (text obj)))
     :get  ,(lambda (control)
              (height control)))))

(defparameter *props-form-values*
  `((:name "value"
     :prop "value")
    (:name "default value"
     :prop "defaultValue")
    (:name "place holder"
     :prop "placeholder")
    (:name "name on form"
     :prop "name")
    (:name "size"
     :prop "size")
    (:name "image url"
     :prop "src")
    (:name "image alt"
     :prop "alt")
    (:name "checked"
     :get  ,(lambda (control)
              (property control "checked"))
     :set  ,(lambda (control obj)
             (if (equalp (text obj) "true")
                 (setf (checkedp control) t)
                 (setf (checkedp control) nil))
              (property control "required")))
    (:name "read only"
     :get  ,(lambda (control)
              (property control "readonly"))
     :set  ,(lambda (control obj)
             (if (equalp (text obj) "true")
                 (setf (read-only-p control) t)
                 (setf (read-only-p control) nil))
             (property control "readonly")))
    (:name "disabled"
     :get  ,(lambda (control)
              (property control "disabled"))
     :set  ,(lambda (control obj)
             (if (equalp (text obj) "true")
                 (setf (disabledp control) t)
                 (setf (disabledp control) nil))
             (property control "disabled")))
    (:name "required"
     :get  ,(lambda (control)
              (property control "required"))
     :set  ,(lambda (control obj)
             (if (equalp (text obj) "true")
                 (setf (requiredp control) t)
                 (setf (requiredp control) nil))
              (property control "required")))
    (:name "pattern"
     :prop "pattern")
    (:name "minimum"
     :prop "min")
    (:name "maximum"
     :prop "max")
    (:name "element step"
     :prop "step")
    (:name "minimum length"
     :prop "minlength")
    (:name "maximum length"
     :prop "maxlength")
    (:name "multiple"
     :get  ,(lambda (control)
              (property control "multiple"))
     :set  ,(lambda (control obj)
              (if (equalp (text obj) "true")
                 (setf (multiplep control) t)
                 (setf (multiplep control) nil))
              (property control "multiple")))
    (:name "files accepted"
     :prop "accept")))

(defparameter *props-colors*
  `((:name "color"
     :setup ,(lambda (control td1 td2)
               (declare (ignore td1))
               (let ((d1 (create-form-element td2 :text  :value (color control)))
                     (dd (create-form-element td2 :color :value (color control))))
                 (make-data-list dd '("#ffffff"
                                      "#ff0000"
                                      "#00ff00"
                                      "#0000ff"
                                      "#ff00ff"))
                 (set-on-change dd (lambda (obj)
                                     (declare (ignore obj))
                                     (setf (value d1) (value dd))
                                     (setf (color control) (value d1))))
                 (set-on-change d1 (lambda (obj)
                                     (declare (ignore obj))
                                     (setf (color control) (value d1)))))
               nil))
    (:name "opacity"
     :style "opacity")
    (:name "background color"
     :setup ,(lambda (control td1 td2)
               (declare (ignore td1))
               (let ((d1 (create-form-element td2 :text  :value (background-color control)))
                     (dd (create-form-element td2 :color :value (background-color control))))
                 (make-data-list dd '("#ffffff"
                                      "#ff0000"
                                      "#00ff00"
                                      "#0000ff"
                                      "#ff00ff"))
                 (set-on-change dd (lambda (obj)
                                     (declare (ignore obj))
                                     (setf (value d1) (value dd))
                                     (setf (background-color control) (value d1))))
                 (set-on-change d1 (lambda (obj)
                                     (declare (ignore obj))
                                     (setf (background-color control) (value d1))))
                 nil)))
    (:name "background attachment"
     :style "background-attachment")
    (:name "background image"
     :style "background-image")
    (:name "background position"
     :style "background-position")
    (:name "background origin"
     :style "background-origin")
    (:name "background repeat"
     :style "background-repeat")
    (:name "background clip"
     :style "background-clip")
    (:name "background size"
     :style "background-size")
    (:name "border"
     :style "border")
    (:name "border radius"
     :style "border-radius")
    (:name "box shadow"
     :style "box-shadow")
    (:name "text shadow"
     :style "text-shadow")
    (:name "outline"
     :style "outline")
    (:name "margin"
     :style "margin")
    (:name "padding"
     :style "padding")
    (:name "cursor"
     :style "cursor")
    (:name "font"
     :style "font")
    (:name "text alignment"
     :style "text-align")
    (:name "vertical align"
     :style "vertical-align")))

(defparameter *props-contents*
  `((:name "contents"
     :setup ,(lambda (control td1 td2)
               (declare (ignore td1))
               (let ((d1 (create-text-area td2 :value (inner-html control))))
                 (set-on-change d1 (lambda (obj)
                                     (declare (ignore obj))
                                     (setf (inner-html control) (value d1)))))
               nil))))

(defparameter *props-raw-contents*
  `((:name "commands"
     :setup ,(lambda (control td1 td2)
               (declare (ignore td1))
               (let ((exp (create-button td2 :content "convert to div")))
                 (set-on-click exp (lambda (obj)
                                     (setf (attribute control "data-clog-type") "div")
                                     (remove-attribute control "data-original-html")
                                     (remove-attribute control "data-clog-composite-control")
                                     (clog-web-alert obj
                                                     "Convert to Div"
                                                     "Save and reload panel to access child controls."
                                                     :color-class "w3-yellow"))))))
    (:name "html contents"
     :setup ,(lambda (control td1 td2)
               (declare (ignore td1))
               (let ((app (connection-data-item td1 "builder-app-data"))
                     (d1  (create-text-area td2 :value (escape-string (attribute control "data-original-html") :html t))))
                 (set-on-change d1 (lambda (obj)
                                     (declare (ignore obj))
                                     (setf (attribute control "data-original-html") (value d1))
                                     (setf (inner-html control) (value d1)))))
               nil))))

(defparameter *props-text*
  `((:name "text"
     :get  ,(lambda (control)
              (text-value control))
     :set  ,(lambda (control obj)
              (setf (text-value control) (text obj))))))

(defparameter *props-css*
  `((:name "css classes"
     :setup ,(lambda (control td1 td2)
               (declare (ignore control td1))
               (add-class td2 "clog-prop-class"))
     :get  ,(lambda (control)
              (property control "className"))
     :set  ,(lambda (control obj)
              (setf (property control "className") (text obj))))))

(defparameter *props-display*
  `((:name "hidden"
     :get  ,(lambda (control)
              (property control "hidden"))
     :set  ,(lambda (control obj)
              (if (equalp (text obj) "true")
                 (setf (hiddenp control) t)
                 (setf (hiddenp control) nil))
              (property control "hidden")))
    (:name "visible"
     :get  ,(lambda (control)
              (style control "visibility"))
     :set  ,(lambda (control obj)
              (if (or (equalp (text obj) "true")
                      (equalp (text obj) "visible"))
                 (setf (visiblep control) t)
                 (setf (visiblep control) nil))
              (style control "visibility")))
    (:name "display"
     :style "display")
    (:name "editable"
     :prop "contenteditable")
    (:name "spell check"
     :prop "spellcheck")
    (:name "text direction"
     :prop "test-direction")
    (:name "language code"
     :prop "lang")
    (:name "overflow"
     :style "overflow")
    (:name "resizable"
     :style "resize")
    (:name "minimum width"
     :style "min-width")
    (:name "minimum height"
     :style "min-height")
    (:name "maximum width"
     :style "max-width")
    (:name "maximum height"
     :style "max-height")))

(defparameter *props-nav*
  '((:name "access key"
     :prop "access key")
    (:name "tool tip"
     :prop "title")
    (:name "tab index"
     :prop "tabindex")
    (:name "z index"
     :style "z-index")))

(defparameter *props-flex*
`((:name "flex-direction"
   :style "flex-direction")
  (:name "flex-wrap"
   :style "flex-wrap")
  (:name "flex-flow"
   :style "flex-flow")
  (:name "justify-content"
   :style "justify-content")
  (:name "align-items"
   :style "align-items")
  (:name "align-content"
   :style "align-content")))

(defparameter *props-flex-item*
`((:name "flex-grow"
   :style "flex-grow")
  (:name "flex-shrink"
   :style "flex-shrink")
  (:name "flex-basis"
   :style "flex-basis")
  (:name "align-self"
   :style "align-self")
  (:name "order"
   :style "order")))

(defparameter *props-base*
  `(,@*props-location*
    ,@*props-with-height*
    ,@*props-css*
    ,@*props-colors*
    ,@*props-display*
    ,@*props-flex-item*
    ,@*props-nav*))

(defparameter *props-element*
  `(,@*props-location*
    ,@*props-with-height*
    ,@*props-text*
    ,@*props-css*
    ,@*props-colors*
    ,@*props-display*
    ,@*props-flex-item*
    ,@*props-nav*))

(defparameter *props-form-element*
  `(,@*props-location*
    ,@*props-with-height*
    (:name "type"
     :setup ,(lambda (control td1 td2)
               (declare (ignore td1))
               (let ((dd (create-select td2))
                     (v  (string-downcase (attribute control "type"))))
                 (add-select-options dd `(,v
                                          "button" "checkbox" "color" "date"
                                          "datetime" "datetime-local" "email"
                                          "image" "file" "hidden"
                                          "month" "number" "password" "radio"
                                          "range" "reset" "search" "submit"
                                          "tel" "text" "time" "url" "week"))
                 (set-on-change dd (lambda (obj)
                                   (declare (ignore obj))
                                   (setf (attribute control "type") (value dd))
                                   (set-geometry (get-placer control)
                                                 :top (position-top control)
                                                 :left (position-left control)
                                                 :width (client-width control)
                                                 :height (client-height control))))
                 nil)))
     (:name "data list"
                       :get ,(lambda (control)
                               (clog::js-query control (format nil "$('#~A').attr('data-clog-name')"
                                                               (attribute control "list"))))
                       :set ,(lambda (control obj)
                               (setf (attribute control "list")
                                     (clog::js-query control (format nil "$(\"[data-clog-name='~A']\").attr('id')"
                                                                     (text obj))))))
    ,@*props-form-values*
    ,@*props-css*
    ,@*props-colors*
    ,@*props-display*
    ,@*props-flex-item*
    ,@*props-nav*))

(defparameter *props-w3css*
  `((:name "Add Color Class"
     :setup ,(lambda (control td1 td2)
               (declare (ignore td1))
               (let ((dd (create-select td2)))
                 (add-select-options dd `("" "w3-amber" "w3-aqua" "w3-blue" "w3-light-blue"
                                             "w3-brown" "w3-cyan" "w3-blue-grey" "w3-green"
                                             "w3-light-green" "w3-indigo" "w3-khaki" "w3-lime"
                                             "w3-orange" "w3-deep-orange" "w3-pink" "w3-purple"
                                             "w3-deep-purple" "w3-red" "w3-sand" "w3-teal"
                                             "w3-yellow" "w3-white" "w3-black" "w3-grey"
                                             "w3-light-grey" "w3-dark-grey" "w3-pale-red"
                                             "w3-pale-green" "w3-pale-yellow" "w3-pale-blue"))
                 (set-on-change dd (lambda (obj)
                                     (declare (ignore obj))
                                     (add-class control (value dd))
                                     (set-property-display control "class" (css-class-name control))))
                 nil)))
    (:name "Add Text Color Class"
     :setup ,(lambda (control td1 td2)
               (declare (ignore td1))
               (let ((dd (create-select td2)))
                 (add-select-options dd `("" "w3-text-amber" "w3-text-aqua" "w3-text-blue" "w3-text-light-blue"
                                             "w3-text-brown" "w3-text-cyan" "w3-text-blue-grey" "w3-text-green"
                                             "w3-text-light-green" "w3-text-indigo" "w3-text-khaki" "w3-text-lime"
                                             "w3-text-orange" "w3-text-deep-orange" "w3-text-pink" "w3-text-purple"
                                             "w3-text-deep-purple" "w3-text-red" "w3-text-sand" "w3-text-teal"
                                             "w3-text-yellow" "w3-text-white" "w3-text-black" "w3-text-grey"
                                             "w3-text-light-grey" "w3-text-dark-grey" "w3-text-pale-red"
                                             "w3-text-pale-green" "w3-text-pale-yellow" "w3-text-pale-blue"))
                 (set-on-change dd (lambda (obj)
                                     (declare (ignore obj))
                                     (add-class control (value dd))
                                     (set-property-display control "class" (css-class-name control))))
                 nil)))
    (:name "Add Hover Text Color Class"
     :setup ,(lambda (control td1 td2)
               (declare (ignore td1))
               (let ((dd (create-select td2)))
                 (add-select-options dd `("" "w3-hover-text-amber" "w3-hover-text-aqua" "w3-hover-text-blue" "w3-hover-text-light-blue"
                                             "w3-hover-text-brown" "w3-hover-text-cyan" "w3-hover-text-blue-grey" "w3-hover-text-green"
                                             "w3-hover-text-light-green" "w3-hover-text-indigo" "w3-hover-text-khaki" "w3-hover-text-lime"
                                             "w3-hover-text-orange" "w3-hover-text-deep-orange" "w3-hover-text-pink" "w3-hover-text-purple"
                                             "w3-hover-text-deep-purple" "w3-hover-text-red" "w3-hover-text-sand" "w3-hover-text-teal"
                                             "w3-hover-text-yellow" "w3-hover-text-white" "w3-hover-text-black" "w3-hover-text-grey"
                                             "w3-hover-text-light-grey" "w3-hover-text-dark-grey" "w3-hover-text-pale-red"
                                             "w3-hover-text-pale-green" "w3-hover-text-pale-yellow" "w3-hover-text-pale-blue"))
                 (set-on-change dd (lambda (obj)
                                     (declare (ignore obj))
                                     (add-class control (value dd))
                                     (set-property-display control "class" (css-class-name control))))
                 nil)))
    (:name "Add Border Class"
     :setup ,(lambda (control td1 td2)
               (declare (ignore td1))
               (let ((dd (create-select td2)))
                 (add-select-options dd `("" "w3-border" "w3-border-top" "w3-border-right" "w3-border-bottom"
                                             "w3-border-left" "w3-border-0" "w3-bottombar" "w3-leftbar"
                                             "w3-rightbar" "w3-topbar"))
                 (set-on-change dd (lambda (obj)
                                     (declare (ignore obj))
                                     (add-class control (value dd))
                                     (set-property-display control "class" (css-class-name control))))
                 nil)))
    (:name "Add Border Color Class"
     :setup ,(lambda (control td1 td2)
               (declare (ignore td1))
               (let ((dd (create-select td2)))
                 (add-select-options dd `("" "w3-border-amber" "w3-border-aqua" "w3-border-blue" "w3-border-light-blue"
                                             "w3-border-brown" "w3-border-cyan" "w3-border-blue-grey" "w3-border-green"
                                             "w3-border-light-green" "w3-border-indigo" "w3-border-khaki" "w3-border-lime"
                                             "w3-border-orange" "w3-border-deep-orange" "w3-border-pink" "w3-border-purple"
                                             "w3-border-deep-purple" "w3-border-red" "w3-border-sand" "w3-border-teal"
                                             "w3-border-yellow" "w3-border-white" "w3-border-black" "w3-border-grey"
                                             "w3-border-light-grey" "w3-border-dark-grey" "w3-border-pale-red"
                                             "w3-border-pale-green" "w3-border-pale-yellow" "w3-border-pale-blue"))
                 (set-on-change dd (lambda (obj)
                                     (declare (ignore obj))
                                     (add-class control (value dd))
                                     (set-property-display control "class" (css-class-name control))))
                 nil)))
    (:name "Add Hover Border Color Class"
     :setup ,(lambda (control td1 td2)
               (declare (ignore td1))
               (let ((dd (create-select td2)))
                 (add-select-options dd `("" "w3-hover-border-amber" "w3-hover-border-aqua" "w3-hover-border-blue" "w3-hover-border-light-blue"
                                             "w3-hover-border-brown" "w3-hover-border-cyan" "w3-hover-border-blue-grey" "w3-hover-border-green"
                                             "w3-hover-border-light-green" "w3-hover-border-indigo" "w3-hover-border-khaki" "w3-hover-border-lime"
                                             "w3-hover-border-orange" "w3-hover-border-deep-orange" "w3-hover-border-pink" "w3-hover-border-purple"
                                             "w3-hover-border-deep-purple" "w3-hover-border-red" "w3-hover-border-sand" "w3-hover-border-teal"
                                             "w3-hover-border-yellow" "w3-hover-border-white" "w3-hover-border-black" "w3-hover-border-grey"
                                             "w3-hover-border-light-grey" "w3-hover-border-dark-grey" "w3-hover-border-pale-red"
                                             "w3-hover-border-pale-green" "w3-hover-border-pale-yellow" "w3-hover-border-pale-blue"))
                 (set-on-change dd (lambda (obj)
                                     (declare (ignore obj))
                                     (add-class control (value dd))
                                     (set-property-display control "class" (css-class-name control))))
                 nil)))
    (:name "Add Round Class"
     :setup ,(lambda (control td1 td2)
               (declare (ignore td1))
               (let ((dd (create-select td2)))
                 (add-select-options dd `("" "w3-circle" "w3-round-small" "w3-round" "w3-round-medium"
                                             "w3-round-large" "w3-round-xlarge" "w3-round-xxlarge"))
                 (set-on-change dd (lambda (obj)
                                     (declare (ignore obj))
                                     (add-class control (value dd))
                                     (set-property-display control "class" (css-class-name control))))
                 nil)))
    (:name "Add 3D Class"
     :setup ,(lambda (control td1 td2)
               (declare (ignore td1))
               (let ((dd (create-select td2)))
                 (add-select-options dd `("" "w3-card" "w3-card-2" "w3-card-4" "w3-hover-shadow" "w3-hoverable" "w3-hover-none"))
                 (set-on-change dd (lambda (obj)
                                     (declare (ignore obj))
                                     (add-class control (value dd))
                                     (set-property-display control "class" (css-class-name control))))
                 nil)))
    (:name "Add Visibility Class"
     :setup ,(lambda (control td1 td2)
               (declare (ignore td1))
               (let ((dd (create-select td2)))
                 (add-select-options dd `("" "w3-opacity" "w3-opacity-min" "w3-opacity-max"
                                             "w3-grayscale" "w3-grayscale-min" "w3-grayscale-max"
                                             "w3-sepia" "w3-sepia-min" "w3-sepia-max"
                                             "w3-hover-opacity" "w3-hover-grayscale" "w3-hover-sepia"
                                             "w3-hover-opacity-off"))
                 (set-on-change dd (lambda (obj)
                                     (declare (ignore obj))
                                     (add-class control (value dd))
                                     (set-property-display control "class" (css-class-name control))))
                 nil)))
    (:name "Add Font Class"
     :setup ,(lambda (control td1 td2)
               (declare (ignore td1))
               (let ((dd (create-select td2)))
                 (add-select-options dd `("" "w3-serif" "w3-sans-serif" "w3-cursive" "w3-monospace"
                                             "w3-wide"))
                 (set-on-change dd (lambda (obj)
                                     (declare (ignore obj))
                                     (add-class control (value dd))
                                     (set-property-display control "class" (css-class-name control))))
                 nil)))
    (:name "Add Size Class"
     :setup ,(lambda (control td1 td2)
               (declare (ignore td1))
               (let ((dd (create-select td2)))
                 (add-select-options dd `("" "w3-tiny" "w3-small" "w3-medium" "w3-large"
                                             "w3-xlarge" "w3-xxlarge" "w3-xxxlarge" "w3-jumbo"))
                 (set-on-change dd (lambda (obj)
                                     (declare (ignore obj))
                                     (add-class control (value dd))
                                     (set-property-display control "class" (css-class-name control))))
                 nil)))
    (:name "Add Alignmnet Class"
     :setup ,(lambda (control td1 td2)
               (declare (ignore td1))
               (let ((dd (create-select td2)))
                 (add-select-options dd `("" "w3-mobile" "w3-responsive"
                                             "w3-left-align" "w3-right-align" "w3-justify"
                                             "w3-center" "w3-right" "w3-left" "w3-top" "w3-bottom" "w3-block"
                                             "w3-bar" "w3-bar-block" "w3-bar-item" "w3-sidebar"
                                             "w3-show-inline-block" "w3-dropdown-hover"
                                             "w3-dropdown-click" "w3-collapse"
                                             "w3-hide-small" "w3-hide-medium" "w3-hide-large"))
                 (set-on-change dd (lambda (obj)
                                     (declare (ignore obj))
                                     (add-class control (value dd))
                                     (set-property-display control "class" (css-class-name control))))
                 nil)))
    (:name "Add Margins/Padding Class"
     :setup ,(lambda (control td1 td2)
               (declare (ignore td1))
               (let ((dd (create-select td2)))
                 (add-select-options dd `("" "w3-margin" "w3-margin-top" "w3-margin-right"
                                             "w3-margin-bottom" "w3-margin-left" "w3-section"
                                             "w3-padding" "w3-padding-small" "w3-padding-large"
                                             "w3-padding-16" "w3-padding-24" "w3-padding-32"
                                             "w3-padding-48" "w3-padding-64"))
                 (set-on-change dd (lambda (obj)
                                     (declare (ignore obj))
                                     (add-class control (value dd))
                                     (set-property-display control "class" (css-class-name control))))
                 nil)))
    (:name "Add Animation Class"
     :setup ,(lambda (control td1 td2)
               (declare (ignore td1))
               (let ((dd (create-select td2)))
                 (add-select-options dd `("" "w3-animate-top" "w3-animate-bottom" "w3-animate-left"
                                             "w3-animate-right" "w3-animate-opacity" "w3-animate-zoom"
                                             "w3-animate-fading" "w3-animate-input" "w3-spin"))
                 (set-on-change dd (lambda (obj)
                                     (declare (ignore obj))
                                     (add-class control (value dd))
                                     (set-property-display control "class" (css-class-name control))))
                 nil)))))

(defparameter *events-multimedia*
  '((:name "on-media-abort"
     :parameters "target")
    (:name "on-media-error"
     :parameters "target")
    (:name "on-can-play"
     :parameters "target")
    (:name "on-can-play-through"
     :parameters "target")
    (:name "on-duration-change"
     :parameters "target")
    (:name "on-emptied"
     :parameters "target")
    (:name "on-ended"
     :parameters "target")
    (:name "on-loaded-data"
     :parameters "target")
    (:name "on-loaded-meta-data"
     :parameters "target")
    (:name "on-load-start"
     :parameters "target")
    (:name "on-play"
     :parameters "target")
    (:name "on-pause"
     :parameters "target")
    (:name "on-playing"
     :parameters "target")
    (:name "on-progress"
     :parameters "target")
    (:name "on-rate-change"
     :parameters "target")
    (:name "on-seek"
     :parameters "target")
    (:name "on-seeked"
     :parameters "target")
    (:name "on-seeking"
     :parameters "target")
    (:name "on-stalled"
     :parameters "target")
    (:name "on-suspend"
     :parameters "target")
    (:name "on-time-updat"
     :parameters "target")
    (:name "on-volume-change"
     :parameters "target")
    (:name "on-waiting"
     :parameters "target")))

(defparameter *events-element*
  '((:name        "on-create"
     :parameters  "target")
    (:name        "on-click"
     :parameters  "target")
    (:name        "on-focus"
     :parameters  "target")
    (:name        "on-blur"
     :parameters  "target")
    (:name        "on-change"
     :parameters  "target")
    (:name        "on-input"
     :parameters  "target")
    (:name        "on-focus-in"
     :parameters  "target")
    (:name        "on-focus-out"
     :parameters  "target")
    (:name        "on-reset"
     :parameters  "target")
    (:name        "on-search"
     :parameters  "target")
    (:name        "on-select"
     :parameters  "target")
    (:name        "on-submit"
     :parameters  "target")
    (:name        "on-context-menu"
     :parameters  "target")
    (:name        "on-double-click"
     :parameters  "target")
    (:name        "on-mouse-click"
     :parameters  "target data")
    (:name        "on-mouse-double-click"
     :parameters  "target data")
    (:name        "on-mouse-right-click"
     :parameters  "target data")
    (:name        "on-mouse-enter"
     :parameters  "target")
    (:name        "on-mouse-leave"
     :parameters  "target")
    (:name        "on-mouse-over"
     :parameters  "target")
    (:name        "on-mouse-out"
     :parameters  "target")
    (:name        "on-mouse-down"
     :parameters  "target data")
    (:name        "on-mouse-up"
     :parameters  "target data")
    (:name        "on-mouse-move"
     :parameters  "target data")
    (:name        "on-pointer-enter"
     :parameters  "target")
    (:name        "on-pointer-leave"
     :parameters  "target")
    (:name        "on-pointer-over"
     :parameters  "target")
    (:name        "on-pointer-out"
     :parameters  "target")
    (:name        "on-pointer-down"
     :parameters  "target data")
    (:name        "on-pointer-up"
     :parameters  "target data")
    (:name        "on-pointer-move"
     :parameters  "target data")
    (:name        "on-touch-start"
     :parameters  "target data")
    (:name        "on-touch-move"
     :parameters  "target data")
    (:name        "on-touch-end"
     :parameters  "target data")
    (:name        "on-touch-cancel"
     :parameters  "target data")
    (:name        "on-character"
     :parameters  "target data")
    (:name        "on-key-down"
     :parameters  "target data")
    (:name        "on-key-up"
     :parameters  "target data")
    (:name        "on-key-press"
     :parameters  "target data")
    (:name        "on-copy"
     :parameters  "target")
    (:name        "on-cut"
     :parameters  "target")
    (:name        "on-paste"
     :parameters  "target")
    (:name        "on-resize"
     :parameters  "target")
    (:name        "on-drag-start"
     :parameters  "target")
    (:name        "on-drag"
     :parameters  "target")
    (:name        "on-drag-end"
     :parameters  "target")
    (:name        "on-drag-enter"
     :parameters  "target")
    (:name        "on-drag-leave"
     :parameters  "target")
    (:name        "on-drag-over"
     :parameters  "target")
    (:name        "on-drop"
     :parameters  "target data")))

(defparameter *supported-controls*
  (list
   '(:name           "group"
     :description    "Tools"
     :create         nil
     :create-type    nil
     :events         nil
     :properties     nil)
   '(:name           "select"
     :description    "Selection Tool"
     :create         nil
     :create-type    nil
     :events         nil
     :properties     nil)
   `(:name           "custom"
     :description    "Custom HTML"
     :clog-type      clog:clog-element
     :create         clog:create-child
     :create-type    :custom-query
     :create-content "<div></div>"
     :events         (,@*events-element*)
     :properties     (,@*props-element*))
   `(:name           "block"
     :description    "Custom HTML Block"
     :clog-type      clog:clog-div
     :create         clog:create-div
     :setup          ,(lambda (control content control-record)
                        (declare (ignore content control-record))
                        (setf (attribute control "data-clog-composite-control") "b"))
     :create-type    :custom-block
     :create-content ""
     :events         (,@*events-element*)
     :properties     (,@*props-element*
                      ,@*props-raw-contents*))
   `(:name           "style-block"
     :description    "Style Block"
     :clog-type      clog:clog-style-block
     :create         clog:create-style-block
     :create-type    :base
     :positioning    :static
     :events         (,@*events-element*)
     :properties     ((:name "media"
                       :attr "media")
                      (:name "type"
                       :prop "type")
                      ,@*props-contents*))
   '(:name           "group"
     :description    "Alignment Controls"
     :create         nil
     :create-type    nil
     :events         nil
     :properties     nil)
   `(:name           "flex-row"
     :description    "Row Align"
     :clog-type      clog:clog-div
     :create         clog:create-div
     :create-type    :element
     :create-content ""
     :setup          ,(lambda (control content control-record)
                        (declare (ignore content  control-record))
                        (set-geometry control :width 200 :height 28)
                        (setf (display control) :flex)
                        (setf (flex-direction control) :row))
     :events         (,@*events-element*)
     :properties     (,@*props-flex*
                      ,@*props-element*))
   `(:name           "flex-row-rev"
     :description    "Row Reverse Align"
     :clog-type      clog:clog-div
     :create         clog:create-div
     :create-type    :element
     :create-content ""
     :setup          ,(lambda (control content control-record)
                        (declare (ignore content  control-record))
                        (set-geometry control :width 200 :height 28)
                        (setf (display control) :flex)
                        (setf (flex-direction control) :row-reverse))
     :events         (,@*events-element*)
     :properties     (,@*props-flex*
                      ,@*props-element*))
   `(:name           "flex-col"
     :description    "Column Align"
     :clog-type      clog:clog-div
     :create         clog:create-div
     :create-type    :element
     :create-content ""
     :setup          ,(lambda (control content control-record)
                        (declare (ignore content  control-record))
                        (set-geometry control :width 100 :height 200)
                        (setf (display control) :flex)
                        (setf (flex-direction control) :column))
     :events         (,@*events-element*)
     :properties     (,@*props-flex*
                      ,@*props-element*))
   `(:name           "flex-col-rev"
     :description    "Column Reverse Align"
     :clog-type      clog:clog-div
     :create         clog:create-div
     :create-type    :element
     :create-content ""
     :setup          ,(lambda (control content control-record)
                        (declare (ignore content  control-record))
                        (set-geometry control :width 100 :height 200)
                        (setf (display control) :flex)
                        (setf (flex-direction control) :column-reverse))
     :events         (,@*events-element*)
     :properties     (,@*props-flex*
                      ,@*props-element*))
   `(:name           "flex-center"
     :description    "Center Align"
     :clog-type      clog:clog-div
     :create         clog:create-div
     :create-type    :element
     :create-content ""
     :setup          ,(lambda (control content control-record)
                        (declare (ignore content  control-record))
                        (set-geometry control :width 200 :height 200)
                        (setf (display control) :flex)
                        (setf (align-items control) :center)
                        (setf (justify-content control) :center))
     :events         (,@*events-element*)
     :properties     (,@*props-flex*
                      ,@*props-element*))
   '(:name           "group"
     :description    "Basic HTML Controls"
     :create         nil
     :create-type    nil
     :events         nil
     :properties     nil)
   `(:name           "label"
     :description    "Label"
     :clog-type      clog:clog-label
     :create         clog:create-label
     :create-type    :element
     :create-content "Label"
     :events         (,@*events-element*)
     :setup          ,(lambda (control content control-record)
                        (declare (ignore content control-record))
                        (setf (attribute control "data-clog-for") ""))
     :on-setup       ,(lambda (control control-record)
                        (declare (ignore control-record))
                        (unless (equal (attribute control "data-clog-for") "")
                          (format nil
                            "(setf (attribute target \"for\") ~
                               (clog:js-query target \"$('[data-clog-name=\\\\'~A\\\\']\').attr('id')\"))"
                                  (attribute control "data-clog-for"))))
     :properties     ((:name "for"
                       :get ,(lambda (control)
                               (attribute control "data-clog-for"))
                       :set ,(lambda (control obj)
                               (setf (attribute control "data-clog-for") (text obj))
                               (setf (attribute control "for")
                                     (js-query control (format nil "$(\"[data-clog-name='~A']\").attr('id')"
                                                               (text obj))))))
                      ,@*props-element*))
   `(:name           "button"
     :description    "Button"
     :clog-type      clog:clog-button
     :create         clog:create-button
     :create-type    :element
     :create-content "Button"
     :events         (,@*events-element*)
     :properties     (,@*props-element*))
   `(:name           "div"
     :description    "Div"
     :clog-type      clog:clog-div
     :create         clog:create-div
     :create-type    :element
     :create-content "div"
     :events         (,@*events-element*)
     :properties     (,@*props-element*))
   `(:name           "textarea"
     :description    "Text Area"
     :clog-type      clog:clog-text-area
     :create         clog:create-text-area
     :create-type    :textarea
     :create-value   ""
     :events         (,@*events-element*)
     :properties     ((:name "rows"
                       :prop "rows")
                      (:name "columns"
                       :prop "columns")
                      (:name "word wrap"
                       :prop "wrap")
                      ,@*props-form-element*))
   `(:name           "dropdown"
     :description    "Drop down select"
     :clog-type      clog:clog-select
     :create         clog:create-select
     :create-type    :base
     :events         (,@*events-element*)
     :properties     ((:name "multiple select"
                       :get  ,(lambda (control)
                                (property control "multiple"))
                       :set  ,(lambda (control obj)
                                (if (or (equalp (text obj) "true") (equalp (text obj) "multiple"))
                                    (setf (attribute control "multiple") t)
                                    (remove-attribute control "multiple"))
                                (property control "multiple")))
                      ,@*props-form-element*))
   `(:name           "listbox"
     :description    "Listbox select"
     :clog-type      clog:clog-select
     :create         clog:create-select
     :create-type    :base
     :setup          ,(lambda (control content control-record)
                        (declare (ignore content) (ignore control-record))
                        (setf (size control) "4"))
     :events         (,@*events-element*)
     :properties     ((:name "multiple select"
                       :get  ,(lambda (control)
                                (property control "multiple"))
                       :set  ,(lambda (control obj)
                                (if (or (equalp (text obj) "true") (equalp (text obj) "multiple"))
                                    (setf (attribute control "multiple") t)
                                    (remove-attribute control "multiple"))
                                (property control "multiple")))
                      ,@*props-form-element*))
   `(:name           "option"
     :description    "Option Item"
     :clog-type      clog:clog-option
     :create         clog:create-option
     :create-content "option item"
     :create-type    :element
     :events         (,@*events-element*)
     :properties     ((:name "value"
                       :prop "value")
                      (:name "selected"
                       :get  ,(lambda (control)
                                (property control "selected"))
                       :set  ,(lambda (control obj)
                                (if (or (equalp (text obj) "true") (equalp (text obj) "selected"))
                                    (setf (attribute control "selected") t)
                                    (remove-attribute control "selected"))
                                (property control "selected")))
                      (:name "disabled"
                       :get  ,(lambda (control)
                                (property control "disabled"))
                       :set  ,(lambda (control obj)
                                (if (or (equalp (text obj) "true") (equalp (text obj) "disabled"))
                                    (setf (attribute control "disabled") t)
                                    (remove-attribute control "disabled"))
                                (property control "disabled")))
                      ,@*props-element*))
   `(:name           "optgroup"
     :description    "Option Group"
     :clog-type      clog:clog-optgroup
     :create         clog:create-optgroup
     :create-content "option group"
     :create-type    :element
     :events         (,@*events-element*)
     :properties     ((:name "disabled"
                       :get  ,(lambda (control)
                                (property control "disabled"))
                       :set  ,(lambda (control obj)
                                (if (or (equalp (text obj) "true") (equalp (text obj) "disabled"))
                                    (setf (attribute control "disabled") t)
                                    (remove-attribute control "disabled"))
                                (property control "disabled")))
                      ,@*props-element*))
   `(:name           "image"
     :description    "Image"
     :clog-type      clog:clog-img
     :create         clog:create-img
     :create-type    :base
     :setup          ,(lambda (control content control-record)
                        (declare (ignore content) (ignore control-record))
                        (setf (url-src control) "/img/clogicon.png")
                        (setf (alt-text control) "Add image url"))
     :events         (,@*events-element*)
     :properties     ((:name "image url"
                       :prop "src")
                      (:name "alternative text"
                       :prop "alt")
                      ,@*props-base*))
   `(:name           "meter"
     :description    "Meter"
     :clog-type      clog:clog-meter
     :create         clog:create-meter
     :create-type    :base
     :events         (,@*events-element*)
     :properties     ((:name "value"
                       :prop "value")
                      (:name "high"
                       :prop "high")
                      (:name "low"
                       :prop "low")
                      (:name "maximum"
                       :prop "max")
                      (:name "minimum"
                       :prop "min")
                      (:name "optimum"
                       :prop "optimum")
                      ,@*props-base*))
   `(:name           "progress"
     :description    "Progress Bar"
     :clog-type      clog:clog-progress-bar
     :create         clog:create-progress-bar
     :create-type    :base
     :events         (,@*events-element*)
     :properties     ((:name "value"
                       :prop "value")
                      (:name "maximum"
                       :prop "max")
                      ,@*props-base*))
   `(:name           "dialog"
     :description    "Dialog"
     :clog-type      clog:clog-dialog
     :create         clog:create-dialog
     :create-type    :element
     :create-content ""
     :events         (,@*events-element*)
     :properties     ((:name "open"
                       :get  ,(lambda (control)
                                (property control "open"))
                       :set  ,(lambda (control obj)
                                (if (or (equalp (text obj) "true") (equalp (text obj) "open"))
                                    (setf (attribute control "open") t)
                                    (remove-attribute control "open"))
                                (property control "open")))
                      (:name "return value"
                       :prop "returnValue")
                      ,@*props-element*))
   '(:name           "group"
     :description    "Form Controls"
     :create         nil
     :create-type    nil
     :events         nil
     :properties     nil)
   `(:name           "form"
     :description    "Form"
     :clog-type      clog:clog-form
     :create         clog:create-form
     :create-type    :base
     :events         (,@*events-element*)
     :properties     ((:name "action"
                       :attr "action")
                      (:name "target"
                       :attr "target")
                      (:name "method"
                       :attr "method")
                      (:name "encoding"
                       :prop "encoding")
                      (:name "form element count"
                       :get ,(lambda (control) (form-element-count control)))
                      ,@*props-element*))
   `(:name           "input"
     :description    "Form Input"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :text
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "fbutton"
     :description    "Form Button"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :button
     :create-value   "Button"
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "reset"
     :description    "Form Reset"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :reset
     :create-value   "Reset"
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "submit"
     :description    "Form Submit"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :submit
     :create-value   "Submit"
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "checkbox"
     :description    "Form Checkbox"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :checkbox
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "radio"
     :description    "Form Radio Button"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :radio
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "color"
     :description    "Form Color Picker"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :color
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "date"
     :description    "Form Date Picker"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :date
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "datetime"
     :description    "Form Datetime Picker"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :datetime
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "datetime-local"
     :description    "Form Datetime Local Picker"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :datetime-local
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "email"
     :description    "Form Email Input"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :email
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "file"
     :description    "Form File Input"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :file
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "hidden"
     :description    "Form Hidden Value"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :hidden
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "fimage"
     :description    "Form Image Input"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :image
     :create-value   ""
     :setup          ,(lambda (control content control-record)
                        (declare (ignore content) (ignore control-record))
                        (setf (url-src control) "/img/clogicon.png")
                        (setf (alt-text control) "Add image url"))
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "month"
     :description    "Form Month and Year Picker"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :month
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "number"
     :description    "Form Number Picker"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :number
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "password"
     :description    "Form Password Input"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :password
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "range"
     :description    "Form Range Picker"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :range
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "search"
     :description    "Form Search Input"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :search
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "tel"
     :description    "Form Tel Input"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :tel
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "time"
     :description    "Form Time Picker"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :time
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "url"
     :description    "Form URL Input"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :url
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "week"
     :description    "Form Week Input"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :week
     :create-value   ""
     :events         (,@*events-element*)
     :properties     (,@*props-form-element*))
   `(:name           "fieldset"
     :description    "Fieldset"
     :clog-type      clog:clog-fieldset
     :create         clog:create-fieldset
     :create-type    :base
     :events         (,@*events-element*)
     :properties     (,@*props-base*))
   `(:name           "legend"
     :description    "Fieldset Legend"
     :clog-type      clog:clog-legend
     :create         clog:create-legend
     :create-content "Legend here"
     :create-type    :element
     :events         (,@*events-element*)
     :properties     (,@*props-element*))
   `(:name           "datalist"
     :description    "Data list"
     :clog-type      clog:clog-data-list
     :create         clog:create-data-list
     :create-type    :base
     :events         (,@*events-element*)
     :properties     (,@*props-base*))
   '(:name           "group"
     :description    "Text Display Elements"
     :create         nil
     :create-type    nil
     :events         nil
     :properties     nil)
   `(:name           "span"
     :description    "Span"
     :clog-type      clog:clog-span
     :create         clog:create-span
     :create-type    :element
     :create-content "span"
     :events         (,@*events-element*)
     :properties     (,@*props-contents*
                      ,@*props-element*))
   `(:name           "link"
     :description    "Link"
     :clog-type      clog:clog-a
     :create         clog:create-a
     :create-type    :element
     :create-content "HTML Link"
     :events         (,@*events-element*)
     :properties     ((:name "href link"
                       :prop "href")
                      (:name "target"
                       :prop "target")
                      ,@*props-element*))
   `(:name           "hr"
     :description    "Horizontal Rule"
     :clog-type      clog:clog-hr
     :create         clog:create-hr
     :create-type    :base
     :events         (,@*events-element*)
     :properties     (,@*props-base*))
   `(:name           "br"
     :description    "Line Break"
     :clog-type      clog:clog-br
     :create         clog:create-br
     :create-type    :base
     :events         (,@*events-element*)
     :properties     (,@*props-base*))
   `(:name           "p"
     :description    "Paragraph"
     :clog-type      clog:clog-p
     :create         clog:create-p
     :create-content "Paragraph"
     :create-type    :element
     :events         (,@*events-element*)
     :properties     (,@*props-element*))
   `(:name           "ol"
     :description    "Ordered List"
     :clog-type      clog:clog-ordered-list
     :create         clog:create-ordered-list
     :create-type    :base
     :events         (,@*events-element*)
     :properties     ((:name "list kind"
                       :prop "list-style-type")
                      (:name "list location"
                       :prop "list-style-position")
                      ,@*props-element*))
   `(:name           "ul"
     :description    "Unordered List"
     :clog-type      clog:clog-unordered-list
     :create         clog:create-unordered-list
     :create-type    :base
     :events         (,@*events-element*)
     :properties     ((:name "value"
                       :prop "value")
                      ,@*props-element*))
   `(:name           "li"
     :description    "List Item"
     :clog-type      clog:clog-list-item
     :create         clog:create-list-item
     :create-type    :element
     :create-content "List Item"
     :positioning    :static
     :events         (,@*events-element*)
     :properties     (,@*props-element*))
   `(:name           "table"
     :description    "Table"
     :clog-type      clog:clog-table
     :create         clog:create-table
     :create-type    :base
     :setup          ,(lambda (control content control-record)
                        (declare (ignore content control-record))
                        (set-geometry control :width 200 :height 100))
     :events         (,@*events-element*)
     :properties     (,@*props-base*))
   `(:name           "tr"
     :description    "Table Row"
     :clog-type      clog:clog-table-row
     :create         clog:create-table-row
     :create-type    :base
     :positioning    :static
     :events         (,@*events-element*)
     :properties     (,@*props-base*))
   `(:name           "td"
     :description    "Table Column"
     :clog-type      clog:clog-table-column
     :create         clog:create-table-column
     :create-type    :element
     :create-content "Column"
     :positioning    :static
     :events         (,@*events-element*)
     :properties     ((:name "column span"
                       :attr "colspan")
                      (:name "row span"
                       :attr "rowspan")
                      ,@*props-element*))
   `(:name           "th"
     :description    "Table Heading"
     :clog-type      clog:clog-table-heading
     :create         clog:create-table-heading
     :create-type    :element
     :create-content "Heading"
     :positioning    :static
     :events         (,@*events-element*)
     :properties     ((:name "column span"
                       :attr "colspan")
                      (:name "row span"
                       :attr "rowspan")
                      (:name "abbreviated version"
                       :attr "abbr")
                      (:name "scope"
                       :attr "rowspan")
                      ,@*props-element*))
   `(:name           "tcolgroup"
     :description    "Table Column Group"
     :clog-type      clog:clog-table-column-group
     :create         clog:create-table-column-group
     :create-type    :base
     :positioning    :static
     :events         (,@*events-element*)
     :properties     ((:name "span"
                       :attr "span")
                      ,@*props-base*))
   `(:name           "tcol"
     :description    "Table Column Group Item"
     :clog-type      clog:clog-table-column-group-item
     :create         clog:create-table-column-group-item
     :create-type    :base
     :create-content "Column Group Item"
     :positioning    :static
     :events         (,@*events-element*)
     :properties     ((:name "span"
                       :attr "span")
                      ,@*props-base*))
   `(:name           "thead"
     :description    "Table Head"
     :clog-type      clog:clog-table-head
     :create         clog:create-table-head
     :create-type    :base
     :positioning    :static
     :events         (,@*events-element*)
     :properties     (,@*props-base*))
   `(:name           "tbody"
     :description    "Table Body"
     :clog-type      clog:clog-table-body
     :create         clog:create-table-body
     :create-type    :base
     :positioning    :static
     :events         (,@*events-element*)
     :properties     (,@*props-base*))
   `(:name           "tfoot"
     :description    "Table Footer"
     :clog-type      clog:clog-table-footer
     :create         clog:create-table-footer
     :create-type    :base
     :positioning    :static
     :events         (,@*events-element*)
     :properties     (,@*props-base*))
   `(:name           "tcaption"
     :description    "Table Caption"
     :clog-type      clog:clog-table-caption
     :create         clog:create-table-caption
     :create-type    :element
     :create-content "Caption"
     :positioning    :static
     :events         (,@*events-element*)
     :properties     ((:name "caption side"
                       :style "caption-side")
                      ,@*props-element*))
   `(:name           "dl"
     :description    "Definition List"
     :clog-type      clog:clog-definition-list
     :create         clog:create-definition-list
     :create-type    :base
     :events         (,@*events-element*)
     :properties     (,@*props-base*))
   `(:name           "dt"
     :description    "Definition Term"
     :clog-type      clog:clog-term
     :create         clog:create-term
     :create-content "Term"
     :create-type    :element
     :positioning    :static
     :events         (,@*events-element*)
     :properties     (,@*props-element*))
   `(:name           "dd"
     :description    "Definition Description"
     :clog-type      clog:clog-description
     :create         clog:create-description
     :create-content "Description"
     :create-type    :element
     :positioning    :static
     :events         (,@*events-element*)
     :properties     (,@*props-element*))
   `(:name           "details"
     :description    "Details Block"
     :clog-type      clog:clog-details
     :create         clog:create-details
     :create-type    :element
     :create-content "Details"
     :events         (,@*events-element*)
     :properties     ((:name "open"
                       :get  ,(lambda (control)
                                (property control "open"))
                       :set  ,(lambda (control obj)
                                (if (or (equalp (text obj) "true") (equalp (text obj) "open"))
                                    (setf (attribute control "open") t)
                                    (remove-attribute control "open"))
                                (property control "open")))
                      ,@*props-element*))
   `(:name           "summary"
     :description    "Summary Block"
     :clog-type      clog:clog-summary
     :create         clog:create-summary
     :create-content "Summary"
     :create-type    :element
     :events         (,@*events-element*)
     :properties     (,@*props-element*))
   '(:name           "group"
     :description    "W3.CSS Controls"
     :create         nil
     :create-type    nil
     :events         nil
     :properties     nil)
   `(:name           "w3-button"
     :description    "W3-Button"
     :clog-type      clog:clog-button
     :create         clog:create-button
     :create-type    :element
     :create-content "w3-button"
     :setup          ,(lambda (control content control-record)
                        (declare (ignore content control-record))
                        (setf (css-class-name control) "w3-button w3-ripple w3-border"))
     :events         (,@*events-element*)
     :properties     (,@*props-w3css*
                      ,@*props-element*))
   `(:name           "w3-btn"
     :description    "W3-Btn"
     :clog-type      clog:clog-button
     :create         clog:create-button
     :create-type    :element
     :create-content "w3-btn"
     :setup          ,(lambda (control content control-record)
                        (declare (ignore content control-record))
                        (setf (css-class-name control) "w3-btn w3-ripple w3-border"))
     :events         (,@*events-element*)
     :properties     (,@*props-w3css*
                      ,@*props-element*))
   `(:name           "w3-image"
     :description    "W3-Image"
     :clog-type      clog:clog-img
     :create         clog:create-img
     :create-type    :base
     :setup          ,(lambda (control content control-record)
                        (declare (ignore content) (ignore control-record))
                        (setf (url-src control) "/img/clogicon.png")
                        (setf (alt-text control) "Add image url")
                        (setf (css-class-name control) "w3-image"))
     :events         (,@*events-element*)
     :properties     ((:name "image url"
                       :prop "src")
                      (:name "alternative text"
                       :prop "alt")
                      ,@*props-w3css*
                      ,@*props-base*))
   `(:name           "w3-input"
     :description    "W3-Form Input"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :text
     :create-value   ""
     :setup          ,(lambda (control content control-record)
                        (declare (ignore content control-record))
                        (set-geometry control :width 200)
                        (setf (css-class-name control) "w3-input"))
     :events         (,@*events-element*)
     :properties     (,@*props-w3css*
                      ,@*props-form-element*))
   `(:name           "w3-checkbox"
     :description    "W3-Form Checkbox"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :checkbox
     :create-value   ""
     :setup          ,(lambda (control content control-record)
                        (declare (ignore content control-record))
                        (setf (css-class-name control) "w3-check"))
     :events         (,@*events-element*)
     :properties     (,@*props-w3css*
                      ,@*props-form-element*))
   `(:name           "w3-radio"
     :description    "W3-Form Radio Button"
     :clog-type      clog:clog-form-element
     :create         clog:create-form-element
     :create-type    :form
     :create-param   :radio
     :create-value   ""
     :setup          ,(lambda (control content control-record)
                        (declare (ignore content control-record))
                        (setf (css-class-name control) "w3-radio"))
     :events         (,@*events-element*)
     :properties     (,@*props-w3css*
                      ,@*props-form-element*))
   `(:name           "w3-dropdown"
     :description    "W3-Drop down select"
     :clog-type      clog:clog-select
     :create         clog:create-select
     :create-type    :base
     :setup          ,(lambda (control content control-record)
                        (declare (ignore content control-record))
                        (set-geometry control :width 200)
                        (setf (css-class-name control) "w3-select"))
     :events         (,@*events-element*)
     :properties     ((:name "multiple select"
                       :get  ,(lambda (control)
                                (property control "multiple"))
                       :set  ,(lambda (control obj)
                                (if (or (equalp (text obj) "true") (equalp (text obj) "multiple"))
                                    (setf (attribute control "multiple") t)
                                    (remove-attribute control "multiple"))
                                (property control "multiple")))
                      ,@*props-w3css*
                      ,@*props-form-element*))
   `(:name           "w3-table"
     :description    "W3-Table"
     :clog-type      clog:clog-table
     :create         clog:create-table
     :create-type    :base
     :setup          ,(lambda (control content control-record)
                        (declare (ignore content control-record))
                        (set-geometry control :width 200 :height 100)
                        (setf (css-class-name control) "w3-table w3-striped w3-border w3-bordered w3-hoverable"))
     :events         (,@*events-element*)
     :properties     (,@*props-w3css*
                      ,@*props-base*))
   `(:name           "w3-ul"
     :description    "W3-Unordered List"
     :clog-type      clog:clog-unordered-list
     :create         clog:create-unordered-list
     :create-type    :base
     :setup          ,(lambda (control content control-record)
                        (declare (ignore content control-record))
                        (set-geometry control :width 200 :height 100)
                        (setf (css-class-name control) "w3-ul w3-hoverable"))
     :events         (,@*events-element*)
     :properties     ((:name "value"
                       :prop "value")
                      ,@*props-w3css*
                      ,@*props-element*))
   `(:name           "w3-badge"
     :description    "W3-Badge"
     :clog-type      clog:clog-span
     :create         clog:create-span
     :create-type    :element
     :create-content "7"
     :setup          ,(lambda (control content control-record)
                        (declare (ignore content control-record))
                        (setf (css-class-name control) "w3-badge"))
     :events         (,@*events-element*)
     :properties     (,@*props-w3css*
                      ,@*props-contents*
                      ,@*props-element*))
   `(:name           "w3-tag"
     :description    "W3-Tag"
     :clog-type      clog:clog-span
     :create         clog:create-span
     :create-type    :element
     :create-content "tag"
     :setup          ,(lambda (control content control-record)
                        (declare (ignore content control-record))
                        (setf (css-class-name control) "w3-tag"))
     :events         (,@*events-element*)
     :properties     (,@*props-w3css*
                      ,@*props-contents*
                      ,@*props-element*))
   `(:name           "w3-container"
     :description    "W3-Container"
     :clog-type      clog:clog-div
     :create         clog:create-div
     :create-type    :element
     :create-content "w3-container"
     :setup          ,(lambda (control content control-record)
                        (declare (ignore content control-record))
                        (set-geometry control :width 200 :height 100)
                        (setf (css-class-name control) "w3-container w3-card-2"))
     :events         (,@*events-element*)
     :properties     (,@*props-w3css*
                      ,@*props-element*))
   `(:name           "w3-code-div"
     :description    "W3-Code-Div"
     :clog-type      clog:clog-div
     :create         clog:create-div
     :create-type    :element
     :create-content "code"
     :setup          ,(lambda (control content control-record)
                        (declare (ignore content control-record))
                        (set-geometry control :width 200 :height 100)
                        (setf (css-class-name control) "w3-code w3-border"))
     :events         (,@*events-element*)
     :properties     (,@*props-w3css*
                      ,@*props-element*))
   `(:name           "w3-codespan"
     :description    "W3-Codespan"
     :clog-type      clog:clog-span
     :create         clog:create-span
     :create-type    :element
     :create-content "code span"
     :setup          ,(lambda (control content control-record)
                        (declare (ignore content control-record))
                        (setf (css-class-name control) "w3-codespan"))
     :events         (,@*events-element*)
     :properties     (,@*props-w3css*
                      ,@*props-contents*
                      ,@*props-element*))
   '(:name           "group"
     :description    "Multi-Media Controls"
     :create         nil
     :create-type    nil
     :events         nil
     :properties     nil)
   `(:name           "audio"
     :description    "Audio Player"
     :clog-type      clog:clog-audio
     :create         clog:create-audio
     :create-type    :base
     :events         (,@*events-multimedia*
                      ,@*events-element*)
     :properties     ((:name "media url"
                       :prop "src")
                      (:name "volume"
                       :prop "volume")
                      (:name "controls"
                       :attr "controls")
                      (:name "preload"
                       :attr "preload")
                      (:name "autoplay"
                       :attr "autoplay")
                      (:name "muted"
                       :prop "muted")
                      (:name "loop"
                       :prop "loop")
                      ,@*props-base*))
   `(:name           "video"
     :description    "Video Player"
     :clog-type      clog:clog-video
     :create         clog:create-video
     :create-type    :base
     :events         (,@*events-multimedia*
                      ,@*events-element*)
     :properties     ((:name "media url"
                       :prop "src")
                      (:name "volume"
                       :prop "volume")
                      (:name "controls"
                       :attr "controls")
                      (:name "preload"
                       :attr "preload")
                      (:name "autoplay"
                       :attr "autoplay")
                      (:name "muted"
                       :prop "muted")
                      (:name "loop"
                       :prop "loop")
                      ,@*props-base*))
   '(:name           "group"
     :description    "Graphics Controls"
     :create         nil
     :create-type    nil
     :events         nil
     :properties     nil)
   `(:name           "canvas"
     :description    "Canvas"
     :clog-type      clog:clog-canvas
     :create         clog:create-canvas
     :create-type    :base
     :events         (,@*events-element*)
     :properties     (,@*props-base*))
   '(:name           "group"
     :description    "Database Controls"
     :create         nil
     :create-type    nil
     :events         nil
     :properties     nil)
   `(:name           "database"
     :description    "Database"
     :clog-type      clog:clog-database
     :create         clog:create-database
     :create-type    :base
     :positioning    :static
     :setup          ,(lambda (control content control-record)
                        (declare (ignore content) (ignore control-record))
                        (setf (attribute control "data-clog-dbi-dbtype") ":sqlite3")
                        (setf (attribute control "data-clog-dbi-dbname") ":memory:")
                        (setf (attribute control "data-clog-dbi-dbparams") ""))
     :on-setup       ,(lambda (control control-record)
                        (declare (ignore control-record))
                        (format nil "(setf (database-connection target) ~
                                       (dbi:connect ~A ~A :database-name ~A))"
                                (attribute control "data-clog-dbi-dbtype")
                                (attribute control "data-clog-dbi-dbparams")
                                (let ((dbi-name (attribute control "data-clog-dbi-dbname")))
                                  (if (equal (char dbi-name 0) #\*)
                                      dbi-name
                                      (format nil "\"~A\""
                                              dbi-name)))))
     :events         (,@*events-element*)
     :properties     ((:name "database type"
                       :attr "data-clog-dbi-dbtype")
                      (:name "database params"
                       :attr "data-clog-dbi-dbparams")
                      (:name "database name"
                       :attr "data-clog-dbi-dbname")
                      ,@*props-element*))
   `(:name           "one-row"
     :description    "One Row"
     :clog-type      clog:clog-one-row
     :create         clog:create-one-row
     :create-type    :base
     :positioning    :static
     :setup          ,(lambda (control content control-record)
                        (declare (ignore content) (ignore control-record))
                        (setf (attribute control "data-clog-one-row-db") "")
                        (setf (attribute control "data-clog-one-row-table") "")
                        (setf (attribute control "data-clog-one-row-where") "")
                        (setf (attribute control "data-clog-one-row-order") "")
                        (setf (attribute control "data-clog-one-row-limit") "")
                        (setf (attribute control "data-clog-one-row-master") "")
                        (setf (attribute control "data-clog-one-row-id-name") "rowid")
                        (setf (attribute control "data-clog-one-row-columns") "rowid"))
     :on-setup       ,(lambda (control control-record)
                        (declare (ignore control-record))
                        (let ((parent (attribute (parent-element control) "data-clog-name"))
                              (cdb    (attribute control "data-clog-one-row-db"))
                              (master (attribute control "data-clog-one-row-master")))
                          (if (or (equal cdb "")
                                  (equal cdb "undefined"))
                              (setf cdb parent))
                          (when (equal master "")
                            (setf master nil))
                          (format nil "(setf (clog-database target) (clog-database (~A panel))) ~
                                       ~A ~
                                       (setf (table-name target) \"~A\") ~
                                       (setf (where-clause target) \"~A\") ~
                                       (setf (order-by target) \"~A\") ~
                                       (setf (limit target) \"~A\") ~
                                       (setf (row-id-name target) \"~A\") ~
                                       (setf (table-columns target) '(~A))"
                                  cdb
                                  (if master
                                      (format nil "(set-master-one-row target (~A panel) \"~A\")"
                                              cdb master)
                                      "")
                                  (attribute control "data-clog-one-row-table")
                                  (attribute control "data-clog-one-row-where")
                                  (attribute control "data-clog-one-row-order")
                                  (attribute control "data-clog-one-row-limit")
                                  (attribute control "data-clog-one-row-id-name")
                                  (attribute control "data-clog-one-row-columns"))))
     :events         ((:name        "on-fetch"
                       :parameters  "target")
                       ,@*events-element*)
     :properties     ((:name "database control"
                       :attr "data-clog-one-row-db")
                      (:name "table name"
                       :attr "data-clog-one-row-table")
                      (:name "table row id name"
                       :attr "data-clog-one-row-id-name")
                      (:name "table columns"
                       :attr "data-clog-one-row-columns")
                      (:name "where clause (optional)"
                       :attr "data-clog-one-row-where")
                      (:name "order by (optional)"
                       :attr "data-clog-one-row-order")
                      (:name "limit (optional)"
                       :attr "data-clog-one-row-limit")
                      (:name "join to slot-name (optional)"
                       :attr "data-clog-one-row-master")
                      ,@*props-element*))
   `(:name           "db-table"
     :description    "Table Many Rows"
     :clog-type      clog:clog-db-table
     :create         clog:create-db-table
     :create-type    :base
     :positioning    :static
     :setup          ,(lambda (control content control-record)
                        (declare (ignore content) (ignore control-record))
                        (setf (attribute control "data-clog-one-row-db") "")
                        (setf (attribute control "data-clog-one-row-table") "")
                        (setf (attribute control "data-clog-one-row-where") "")
                        (setf (attribute control "data-clog-one-row-order") "")
                        (setf (attribute control "data-clog-one-row-limit") "")
                        (setf (attribute control "data-clog-one-row-master") "")
                        (setf (attribute control "data-clog-one-row-id-name") "rowid")
                        (setf (attribute control "data-clog-one-row-columns") "rowid"))
     :on-setup       ,(lambda (control control-record)
                        (declare (ignore control-record))
                        (let ((parent (attribute (parent-element control) "data-clog-name"))
                              (cdb    (attribute control "data-clog-one-row-db"))
                              (master (attribute control "data-clog-one-row-master")))
                          (if (or (equal cdb "")
                                  (equal cdb "undefined"))
                              (setf cdb parent))
                          (when (equal master "")
                            (setf master nil))
                          (format nil "(setf (clog-database target) (clog-database (~A panel))) ~
                                       ~A ~
                                       (setf (table-name target) \"~A\") ~
                                       (setf (where-clause target) \"~A\") ~
                                       (setf (order-by target) \"~A\") ~
                                       (setf (limit target) \"~A\") ~
                                       (setf (row-id-name target) \"~A\") ~
                                       (setf (table-columns target) '(~A))"
                                  cdb
                                  (if master
                                      (format nil "(set-master-one-row target (~A panel) \"~A\")"
                                              cdb master)
                                      "")
                                  (attribute control "data-clog-one-row-table")
                                  (attribute control "data-clog-one-row-where")
                                  (attribute control "data-clog-one-row-order")
                                  (attribute control "data-clog-one-row-limit")
                                  (attribute control "data-clog-one-row-id-name")
                                  (attribute control "data-clog-one-row-columns"))))
     :events         ((:name        "on-fetch"
                       :parameters  "target")
                      (:name        "on-header"
                       :parameters  "target")
                      (:name        "on-footer"
                       :parameters  "target")
                      (:name        "on-row"
                       :parameters  "target table-row")
                      (:name        "on-column"
                       :parameters  "target column table-column")
                       ,@*events-element*)
     :properties     ((:name "database control"
                       :attr "data-clog-one-row-db")
                      (:name "table name"
                       :attr "data-clog-one-row-table")
                      (:name "table row id name"
                       :attr "data-clog-one-row-id-name")
                      (:name "table columns"
                       :attr "data-clog-one-row-columns")
                      (:name "where clause (optional)"
                       :attr "data-clog-one-row-where")
                      (:name "order by (optional)"
                       :attr "data-clog-one-row-order")
                      (:name "limit (optional)"
                       :attr "data-clog-one-row-limit")
                      (:name "join to slot-name (optional)"
                       :attr "data-clog-one-row-master")
                      ,@*props-element*))
   `(:name           "lookup-drop"
     :description    "Drop down table lookup"
     :clog-type      clog:clog-lookup
     :create         clog:create-lookup
     :create-type    :base
     :setup          ,(lambda (control content control-record)
                        (declare (ignore content) (ignore control-record))
                        (setf (attribute control "data-clog-one-row-db") "")
                        (setf (attribute control "data-clog-one-row-table") "")
                        (setf (attribute control "data-clog-lookup-value") "")
                        (setf (attribute control "data-clog-lookup-option") "")
                        (setf (attribute control "data-clog-one-row-where") "")
                        (setf (attribute control "data-clog-one-row-order") "")
                        (setf (attribute control "data-clog-one-row-limit") "")
                        (setf (attribute control "data-clog-one-row-master") "")
                        (setf (attribute control "data-clog-one-row-id-name") "rowid")
                        (setf (attribute control "data-clog-one-row-columns") "rowid"))
     :on-setup       ,(lambda (control control-record)
                        (declare (ignore control-record))
                        (let ((parent (attribute (parent-element control) "data-clog-name"))
                              (cdb    (attribute control "data-clog-one-row-db"))
                              (master (attribute control "data-clog-one-row-master")))
                          (if (or (equal cdb "")
                                  (equal cdb "undefined"))
                              (setf cdb parent))
                          (when (equal master "")
                            (setf master nil))
                          (format nil "(setf (clog-database target) (clog-database (~A panel))) ~
                                       ~A ~
                                       (setf (table-name target) \"~A\") ~
                                       (setf (value-field target) :|~A|) ~
                                       (setf (option-field target) :|~A|) ~
                                       (setf (where-clause target) \"~A\") ~
                                       (setf (order-by target) \"~A\") ~
                                       (setf (limit target) \"~A\") ~
                                       (setf (row-id-name target) \"~A\") ~
                                       (setf (table-columns target) '(~A))"
                                  cdb
                                  (if master
                                      (format nil "(set-master-one-row target (~A panel) \"~A\")"
                                              cdb master)
                                      "")
                                  (attribute control "data-clog-one-row-table")
                                  (attribute control "data-clog-lookup-value")
                                  (attribute control "data-clog-lookup-option")
                                  (attribute control "data-clog-one-row-where")
                                  (attribute control "data-clog-one-row-order")
                                  (attribute control "data-clog-one-row-limit")
                                  (attribute control "data-clog-one-row-id-name")
                                  (attribute control "data-clog-one-row-columns"))))
     :events         ((:name        "on-fetch"
                       :parameters  "target")
                       ,@*events-element*)
     :properties     ((:name "multiple select"
                       :get  ,(lambda (control)
                                (property control "multiple"))
                       :set  ,(lambda (control obj)
                                (if (or (equalp (text obj) "true") (equalp (text obj) "multiple"))
                                    (setf (attribute control "multiple") t)
                                    (remove-attribute control "multiple"))
                                (property control "multiple")))
                      (:name "database control"
                       :attr "data-clog-one-row-db")
                      (:name "table name"
                       :attr "data-clog-one-row-table")
                      (:name "table row id name"
                       :attr "data-clog-one-row-id-name")
                      (:name "table columns"
                       :attr "data-clog-one-row-columns")
                      (:name "value field"
                       :attr "data-clog-lookup-value")
                      (:name "value display field"
                       :attr "data-clog-lookup-option")
                      (:name "where clause (optional)"
                       :attr "data-clog-one-row-where")
                      (:name "order by (optional)"
                       :attr "data-clog-one-row-order")
                      (:name "limit (optional)"
                       :attr "data-clog-one-row-limit")
                      (:name "join to slot-name (optional)"
                       :attr "data-clog-one-row-master")
                      ,@*props-form-element*))
   `(:name           "lookup-list"
     :description    "Listbox table lookup"
     :clog-type      clog:clog-lookup
     :create         clog:create-lookup
     :create-type    :base
     :setup          ,(lambda (control content control-record)
                        (declare (ignore content) (ignore control-record))
                        (setf (size control) "4")
                        (setf (attribute control "data-clog-one-row-db") "")
                        (setf (attribute control "data-clog-one-row-table") "")
                        (setf (attribute control "data-clog-lookup-value") "")
                        (setf (attribute control "data-clog-lookup-option") "")
                        (setf (attribute control "data-clog-one-row-where") "")
                        (setf (attribute control "data-clog-one-row-order") "")
                        (setf (attribute control "data-clog-one-row-limit") "")
                        (setf (attribute control "data-clog-one-row-master") "")
                        (setf (attribute control "data-clog-one-row-id-name") "rowid")
                        (setf (attribute control "data-clog-one-row-columns") "rowid"))
     :on-setup       ,(lambda (control control-record)
                        (declare (ignore control-record))
                        (let ((parent (attribute (parent-element control) "data-clog-name"))
                              (cdb    (attribute control "data-clog-one-row-db"))
                              (master (attribute control "data-clog-one-row-master")))
                          (if (or (equal cdb "")
                                  (equal cdb "undefined"))
                              (setf cdb parent))
                          (when (equal master "")
                            (setf master nil))
                          (format nil "(setf (clog-database target) (clog-database (~A panel))) ~
                                       ~A ~
                                       (setf (table-name target) \"~A\") ~
                                       (setf (value-field target) :|~A|) ~
                                       (setf (option-field target) :|~A|) ~
                                       (setf (where-clause target) \"~A\") ~
                                       (setf (order-by target) \"~A\") ~
                                       (setf (limit target) \"~A\") ~
                                       (setf (row-id-name target) \"~A\") ~
                                       (setf (table-columns target) '(~A))"
                                  cdb
                                  (if master
                                      (format nil "(set-master-one-row target (~A panel) \"~A\")"
                                              cdb master)
                                      "")
                                  (attribute control "data-clog-one-row-table")
                                  (attribute control "data-clog-lookup-value")
                                  (attribute control "data-clog-lookup-option")
                                  (attribute control "data-clog-one-row-where")
                                  (attribute control "data-clog-one-row-order")
                                  (attribute control "data-clog-one-row-limit")
                                  (attribute control "data-clog-one-row-id-name")
                                  (attribute control "data-clog-one-row-columns"))))
     :events         ((:name        "on-fetch"
                       :parameters  "target")
                       ,@*events-element*)
     :properties     ((:name "multiple select"
                       :get  ,(lambda (control)
                                (property control "multiple"))
                       :set  ,(lambda (control obj)
                                (if (or (equalp (text obj) "true") (equalp (text obj) "multiple"))
                                    (setf (attribute control "multiple") t)
                                    (remove-attribute control "multiple"))
                                (property control "multiple")))
                      (:name "database control"
                       :attr "data-clog-one-row-db")
                      (:name "table name"
                       :attr "data-clog-one-row-table")
                      (:name "table row id name"
                       :attr "data-clog-one-row-id-name")
                      (:name "table columns"
                       :attr "data-clog-one-row-columns")
                      (:name "value field"
                       :attr "data-clog-lookup-value")
                      (:name "value display field"
                       :attr "data-clog-lookup-option")
                      (:name "where clause (optional)"
                       :attr "data-clog-one-row-where")
                      (:name "order by (optional)"
                       :attr "data-clog-one-row-order")
                      (:name "limit (optional)"
                       :attr "data-clog-one-row-limit")
                      (:name "join to slot-name (optional)"
                       :attr "data-clog-one-row-master")
                      ,@*props-form-element*))))

(defparameter *supported-templates*
  (list
   '(:name    "CLOG Builder - Panel Projects"
     :code    :group)
   '(:name    "New Builder Panel Project"
     :code    "nbp"
     :type    :system
     :www     "templates/www/"
     :loc     "templates/projects/clog-panel/")
   '(:name    "CLOG - General Projects"
     :code    :group)
   '(:name    "New CLOG Basic HTML Project"
     :code    "ncp"
     :type    :system
     :www     "templates/www/"
     :loc     "templates/projects/clog/")
   '(:name    "New CLOG-GUI Project"
     :code    "ncgp"
     :type    :system
     :www     "templates/www/"
     :loc     "templates/projects/clog-gui/")
   '(:name    "New CLOG-WEB Project"
     :code    "ncwp"
     :type    :system
     :www     "templates/www/"
     :loc     "templates/projects/clog-web/")
   '(:name    "New CLOG-WEB-SITE Project"
     :code    "ncws"
     :type    :system
     :www     "templates/www/"
     :loc     "templates/projects/clog-web-site/")
   '(:name    "CLOG/CLOG Builder - Extension Projects"
     :code    :group)
   '(:name    "New CLOG/CLOG-Builder Plugin Project"
     :code    "ncplug"
     :type    :system
     :www     "templates/www/"
     :loc     "templates/projects/clog-plugin/")))
