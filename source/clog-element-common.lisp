;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) David Botton                                                      ;;;;
;;;;                                                                       ;;;;
;;;; clog-element-commont.lisp                                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)

;; Common HTML Elements

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-a
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-a (clog-element)()
  (:documentation "CLOG A, anchor, Objects."))

;;;;;;;;;;;;;;
;; create-a ;;
;;;;;;;;;;;;;;

(defgeneric create-a (clog-obj
                      &key link content target
                        style hidden class html-id auto-place)
  (:documentation "Create a new CLOG-A as child of CLOG-OBJ with :LINK and
:CONTENT (default \"\") and :TARGET (\"_self\") and if :AUTO-PLACE (default t)
place-inside-bottom-of CLOG-OBJ.

   Target of link, name of a frame or:
   _blank  = new window
   _top    = top most frame (full browser window)
   _parent = parent frame or window
   _self   = current frame or window"))

(defmethod create-a ((obj clog-obj)
                     &key (link "#")
                       (content "")
                       (target "_self")
                       (download nil)
                       (style nil)
                       (hidden nil)
                       (class nil)
                       (html-id nil) (auto-place t))
  (create-child obj (format nil "<a~@[~a~]~@[~a~]~@[~a~] target='~A' href='~A'>~A</a>"
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style))
                            (when download
                              (format nil " download='~A'"
                                      (escape-string download :html t)))
                            (escape-string target :html t)
                            (escape-string link :html t)
                            content)
                :clog-type  'clog-a
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;
;; link ;;
;;;;;;;;;;

(defgeneric link (clog-a)
  (:documentation "Get/Setf the HREF link of the anchor."))

(defmethod link ((obj clog-a))
  (property obj "href"))

(defgeneric (setf link) (value clog-a)
  (:documentation "Set link VALUE for CLOG-A"))

(defmethod (setf link) (value (obj clog-a))
  (setf (property obj "href") value))

;;;;;;;;;;;;
;; target ;;
;;;;;;;;;;;;

(defgeneric target (clog-a)
  (:documentation "Get/Setf the link target of the anchor."))

(defmethod target ((obj clog-a))
  (property obj "target"))

(defgeneric (setf target) (value clog-a)
  (:documentation "Set target VALUE for CLOG-A"))

(defmethod (setf target) (value (obj clog-a))
  (setf (property obj "target") value))

;;;;;;;;;;;;;;
;; download ;;
;;;;;;;;;;;;;;

(defgeneric download (clog-a)
  (:documentation "Get/Setf the download name of the anchor."))

(defmethod download ((obj clog-a))
  (property obj "download"))

(defgeneric (setf download) (value clog-a)
  (:documentation "Set download VALUE for CLOG-A"))

(defmethod (setf download) (value (obj clog-a))
  (setf (property obj "download") value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-br
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-br (clog-element)()
  (:documentation "CLOG BR Objects for line breaks."))

;;;;;;;;;;;;;;;
;; create-br ;;
;;;;;;;;;;;;;;;

(defgeneric create-br (clog-obj &key style hidden class html-id auto-place)
  (:documentation "Create a new CLOG-BR as child of CLOG-OBJ that creates a
line break and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-br ((obj clog-obj) &key (style nil)
                                       (hidden nil)
                                       (class nil)
                                       (html-id nil)
                                       (auto-place t))
  (create-child obj (format nil "<br~@[~A~]~@[~A~]/>"
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style)))
                :clog-type  'clog-br
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-button
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-button (clog-element)()
  (:documentation "CLOG Button Objects."))

;;;;;;;;;;;;;;;;;;;
;; create-button ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric create-button (clog-obj &key content
                                      style hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Button as child of CLOG-OBJ with :CONTENT
(default \"\") and if :AUTO-PLACE (default t) place-inside-bottom-of
CLOG-OBJ"))

(defmethod create-button ((obj clog-obj) &key (content "")
                                           (style nil)
                                           (hidden nil)
                                           (class nil)
                                           (html-id nil)
                                           (auto-place t))
  (create-child obj (format nil "<button~@[~A~]~@[~A~]>~A</button>"
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style))
                            content)
                :clog-type  'clog-button
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;;;;;
;; disabledp ;;
;;;;;;;;;;;;;;;

(defgeneric disabledp (clog-button)
  (:documentation "Get/Setf disabled status of button."))

(defmethod disabledp ((obj clog-button))
  (js-true-p (property obj "disabled")))

(defgeneric (setf disabledp) (value clog-button)
  (:documentation "Set editable VALUE for CLOG-BUTTON"))

(defmethod (setf disabledp) (value (obj clog-button))
  (if value
      (setf (property obj "disabled") (p-true-js value))
      (remove-attribute obj "disabled")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-div
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-div (clog-element)()
  (:documentation "CLOG Div Objects."))

;;;;;;;;;;;;;;;;
;; create-div ;;
;;;;;;;;;;;;;;;;

(defgeneric create-div (clog-obj &key content
                                   style hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Div as child of CLOG-OBJ with :CONTENT
(default \"\") and if :AUTO-PLACE (default t) place-inside-bottom-of
CLOG-OBJ. If hidden is true visiblep is set to nil."))

(defmethod create-div ((obj clog-obj) &key (content "")
                                        (style nil)
                                        (hidden nil)
                                        (class nil)
                                        (html-id nil)
                                        (auto-place t))
  (create-child obj (format nil "<div~@[~A~]~@[~A~]>~A</div>"
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style))
                            content)
                :clog-type  'clog-div
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-dialog
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-dialog (clog-element)()
  (:documentation "CLOG Dialog Objects."))

;;;;;;;;;;;;;;;;;;;
;; create-dialog ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric create-dialog (clog-obj &key content
                                      style hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Dialog as child of CLOG-OBJ with :CONTENT
(default \"\") and if :AUTO-PLACE (default t) place-inside-bottom-of
CLOG-OBJ. If hidden is true visiblep is set to nil. Modal does not work on
firefox and dialog does not work at all on IE."))

(defmethod create-dialog ((obj clog-obj) &key (content "")
                                           (style nil)
                                           (hidden nil)
                                           (class nil)
                                           (html-id nil)
                                           (auto-place t))
  (create-child obj (format nil "<dialog~@[~A~]~@[~A~]>~A</dialog>"
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style))
                            content)
                :clog-type  'clog-dialog
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;;;;;;;;
;; return-value ;;
;;;;;;;;;;;;;;;;;;

(defgeneric return-value (clog-dialog)
  (:documentation "Get/Setf return-value of dialog."))

(defmethod return-value ((obj clog-dialog))
  (property obj "returnValue"))

(defgeneric (setf return-value) (value clog-dialog)
  (:documentation "Set return-value VALUE for CLOG-DIALOG"))

(defmethod (setf return-value) (value (obj clog-dialog))
  (setf (property obj "returnValue") value))

;;;;;;;;;;;;;;;;;;
;; dialog-openp ;;
;;;;;;;;;;;;;;;;;;

(defgeneric dialog-openp (clog-dialog)
  (:documentation "Get/Setf dialog-openp. Will show dialog "))

(defmethod dialog-openp ((obj clog-dialog))
  (unless (equalp (attribute obj "open") "undefined")
    t))

(defgeneric (setf dialog-openp) (value clog-dialog)
  (:documentation "Set dialog-openp VALUE for CLOG-DIALOG"))

(defmethod (setf dialog-openp) (value (obj clog-dialog))
  (if value
      (setf (attribute obj "open") t)
      (remove-attribute obj "open")))

;;;;;;;;;;;;;;;;;
;; show-dialog ;;
;;;;;;;;;;;;;;;;;

(defgeneric show-dialog (clog-dialog &key modal)
  (:documentation "Show dialog."))

(defmethod show-dialog ((obj clog-dialog) &key (modal nil))
  (if modal
      (jquery-execute obj (format nil "get(0).showModal()"))
      (jquery-execute obj (format nil "get(0).show()"))))

;;;;;;;;;;;;;;;;;;
;; close-dialog ;;
;;;;;;;;;;;;;;;;;;

(defgeneric close-dialog (clog-dialog)
  (:documentation "Close dialog."))

(defmethod close-dialog ((obj clog-dialog))
  (jquery-execute obj (format nil "get(0).close()")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-hr
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-hr (clog-element)()
  (:documentation "CLOG HR Objects for horizontal rules."))

;;;;;;;;;;;;;;;
;; create-hr ;;
;;;;;;;;;;;;;;;

(defgeneric create-hr (clog-obj &key hidden
                                  style class html-id auto-place)
  (:documentation "Create a new CLOG-HR as child of CLOG-OBJ that creates a
horizontal rule (line) and if :AUTO-PLACE (default t) place-inside-bottom-of
CLOG-OBJ"))

(defmethod create-hr ((obj clog-obj) &key (hidden nil)
                                       (style nil)
                                       (class nil)
                                       (html-id nil)
                                       (auto-place t))
  (create-child obj (format nil "<hr~@[~A~]~@[~A~]/>"
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style)))
                :clog-type  'clog-hr
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-img
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-img (clog-element)()
  (:documentation "CLOG Img Objects."))

;;;;;;;;;;;;;;;;
;; create-img ;;
;;;;;;;;;;;;;;;;

(defgeneric create-img (clog-obj
                        &key url-src alt-text
                          style hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Img as child of CLOG-OBJ with :URL-SRC
(default \"\") and :ALT-TEXT (default \"\") if :AUTO-PLACE (default t)
place-inside-bottom-of CLOG-OBJ. Use width and height properties before
placing image to constrain image size."))

(defmethod create-img ((obj clog-obj) &key
                                        (url-src "")
                                        (alt-text "")
                                        (style nil)
                                        (hidden nil)
                                        (class nil)
                                        (html-id nil)
                                        (auto-place t))
  (create-child obj (format nil "<img~@[~A~]~@[~A~] src='~A' alt='~A'>)"
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style))
                            (escape-string url-src :html t)
                            (escape-string alt-text :html t))
                :clog-type  'clog-img
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;;;
;; url-src ;;
;;;;;;;;;;;;;

(defgeneric url-src (clog-img)
  (:documentation "Get/Setf the url-src of the img."))

(defmethod url-src ((obj clog-img))
  (property obj "src"))

(defgeneric (setf url-src) (value clog-img)
  (:documentation "Set url-src VALUE for CLOG-IMG"))

(defmethod (setf url-src) (value (obj clog-img))
  (setf (property obj "src") value))

;;;;;;;;;;;;;;
;; alt-text ;;
;;;;;;;;;;;;;;

(defgeneric alt-text (clog-img)
  (:documentation "Get/Setf the alt-text of the img."))

(defmethod alt-text ((obj clog-img))
  (attribute obj "alt"))

(defgeneric (setf alt-text) (value clog-img)
  (:documentation "Set alt-text VALUE for CLOG-IMG"))

(defmethod (setf alt-text) (value (obj clog-img))
  (setf (attribute obj "alt") value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-meter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-meter (clog-element)()
  (:documentation "CLOG Meter Objects."))

;;;;;;;;;;;;;;;;;;
;; create-meter ;;
;;;;;;;;;;;;;;;;;;

(defgeneric create-meter (clog-obj &key value high low maximum minimum optimum
                                     style
                                     hidden
                                     class
                                     html-id
                                     auto-place)
  (:documentation "Create a new CLOG-Meter as child of CLOG-OBJ with VALUE
(default 0) HIGH (default 100) LOW (default 0) MAXIMUM (default 100) MINIMUM
(default 0) OPTIMUM (default 50) and if :AUTO-PLACE (default t)
place-inside-bottom-of CLOG-OBJ."))

(defmethod create-meter ((obj clog-obj) &key
                                          (value 0)
                                          (high 100)
                                          (low 0)
                                          (maximum 100)
                                          (minimum 0)
                                          (optimum 50)
                                          (style nil)
                                          (hidden nil)
                                          (class nil)
                                          (html-id nil)
                                          (auto-place t))
  (create-child obj (format nil
            "<meter value=~A high=~A low=~A max=~A min=~A optimum=~A~@[~A~]~@[~A~]/>"
                            value high low maximum minimum optimum
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style)))
                :clog-type  'clog-meter
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;
;; value ;;
;;;;;;;;;;;

(defmethod value ((obj clog-meter))
  (property obj "value"))

(defmethod (setf value) (value (obj clog-meter))
  (setf (property obj "value") value))

;;;;;;;;;;;;;;;;
;; text-value ;;
;;;;;;;;;;;;;;;;

(defmethod text-value ((obj clog-meter))
  (property obj "value"))

(defmethod (setf text-value) (value (obj clog-meter))
  (setf (property obj "value") value))

;;;;;;;;;;
;; high ;;
;;;;;;;;;;

(defgeneric high (clog-meter)
  (:documentation "Get/Setf the high of the meter."))

(defmethod high ((obj clog-meter))
  (property obj "high"))

(defgeneric (setf high) (high clog-meter)
  (:documentation "Set HIGH for CLOG-METER"))

(defmethod (setf high) (high (obj clog-meter))
  (setf (property obj "high") high))

;;;;;;;;;
;; low ;;
;;;;;;;;;

(defgeneric low (clog-meter)
  (:documentation "Get/Setf the low of the meter."))

(defmethod low ((obj clog-meter))
  (property obj "low"))

(defgeneric (setf low) (low clog-meter)
  (:documentation "Set LOW for CLOG-METER"))

(defmethod (setf low) (low (obj clog-meter))
  (setf (property obj "low") low))

;;;;;;;;;;;;;
;; maximum ;;
;;;;;;;;;;;;;

(defgeneric maximum (clog-meter)
  (:documentation "Get/Setf the maximum of the meter."))

(defmethod maximum ((obj clog-meter))
  (property obj "max"))

(defgeneric (setf maximum) (maximum clog-meter)
  (:documentation "Set maximum MAXIMUM for CLOG-METER"))

(defmethod (setf maximum) (maximum (obj clog-meter))
  (setf (property obj "max") maximum))

;;;;;;;;;;;;;
;; minimum ;;
;;;;;;;;;;;;;

(defgeneric minimum (clog-meter)
  (:documentation "Get/Setf the minimum of the meter."))

(defmethod minimum ((obj clog-meter))
  (property obj "min"))

(defgeneric (setf minimum) (minimum clog-meter)
  (:documentation "Set minimum MINIMUM for CLOG-METER"))

(defmethod (setf minimum) (minimum (obj clog-meter))
  (setf (property obj "min") minimum))


;;;;;;;;;;;;;
;; optimum ;;
;;;;;;;;;;;;;

(defgeneric optimum (clog-meter)
  (:documentation "Get/Setf the optimum of the meter."))

(defmethod optimum ((obj clog-meter))
  (property obj "optimum"))

(defgeneric (setf optimum) (optimum clog-meter)
  (:documentation "Set optimum OPTIMUM for CLOG-METER"))

(defmethod (setf optimum) (optimum (obj clog-meter))
  (setf (property obj "optimum") optimum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-progress-bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-progress-bar (clog-element)()
  (:documentation "CLOG Progress-Bar Objects."))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-progress-bar ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-progress-bar (clog-obj
                                 &key value maximum
                                   style hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Progress-Bar as child of CLOG-OBJ with
VALUE (default 0) MAXIMUM (default 100) and if :AUTO-PLACE (default t)
place-inside-bottom-of CLOG-OBJ."))

(defmethod create-progress-bar ((obj clog-obj) &key (value 0)
                                                 (maximum 100)
                                                 (style nil)
                                                 (hidden nil)
                                                 (class nil)
                                                 (html-id nil)
                                                 (auto-place t))
  (create-child obj (format nil "<progress value=~A max=~A ~@[~A~]~@[~A~]/>"
                            value maximum
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style)))
                :clog-type  'clog-progress-bar
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;
;; value ;;
;;;;;;;;;;;

(defmethod value ((obj clog-progress-bar))
  (property obj "value"))

(defmethod (setf value) (value (obj clog-progress-bar))
  (setf (property obj "value") value))

;;;;;;;;;;;;;;;;
;; text-value ;;
;;;;;;;;;;;;;;;;

(defmethod text-value ((obj clog-progress-bar))
  (property obj "value"))

(defmethod (setf text-value) (value (obj clog-progress-bar))
  (setf (property obj "value") value))

;;;;;;;;;;;;;
;; maximum ;;
;;;;;;;;;;;;;

(defgeneric maximum (clog-progress-bar)
  (:documentation "Get/Setf the maximum of the progress-bar."))

(defmethod maximum ((obj clog-progress-bar))
  (property obj "max"))

(defgeneric (setf maximum) (maximum clog-progress-bar)
  (:documentation "Set maximum MAXIMUM for CLOG-PROGRESS-BAR"))

(defmethod (setf maximum) (maximum (obj clog-progress-bar))
  (setf (property obj "max") maximum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-p
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-p (clog-element)()
  (:documentation "CLOG P Objects."))

;;;;;;;;;;;;;;
;; create-p ;;
;;;;;;;;;;;;;;

(defgeneric create-p (clog-obj &key content
                                 style hidden class html-id auto-place)
  (:documentation "Create a new CLOG-P as child of CLOG-OBJ with :CONTENT
(default \"\") and if :AUTO-PLACE (default t) place-inside-bottom-of
CLOG-OBJ"))

(defmethod create-p ((obj clog-obj) &key (content "")
                                      (style nil)
                                      (hidden nil)
                                      (class nil)
                                      (html-id nil)
                                      (auto-place t))
  (create-child obj (format nil "<p~@[~A~]~@[~A~]>~A</p>"
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style))
                            content)
                :clog-type  'clog-p
                :html-id    html-id
                :auto-place auto-place))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-span
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-span (clog-element)()
  (:documentation "CLOG Span Objects."))

;;;;;;;;;;;;;;;;;
;; create-span ;;
;;;;;;;;;;;;;;;;;

(defgeneric create-span (clog-obj &key content
                                    style hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Span as child of CLOG-OBJ with CONTENT
and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ. A span is
an inline element while a div is a block element (one that takes up the entire
browser width)."))

(defmethod create-span ((obj clog-obj) &key (content "")
                                         (style nil)
                                         (hidden nil)
                                         (class nil)
                                         (html-id nil)
                                         (auto-place t))
  (create-child obj (format nil "<span~@[~A~]~@[~A~]>~A</span>"
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style))
                            content)
                :clog-type  'clog-span
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-section (clog-element)()
  (:documentation "CLOG Section Objects."))

;;;;;;;;;;;;;;;;;;;;
;; create-section ;;
;;;;;;;;;;;;;;;;;;;;

(deftype section-type () '(member :address :article :aside :header :main :nav
                           :p :pre :section :blockquote :h1 :h2 :h3 :h4 :h5 :h6
                           :hgroup))

(defgeneric create-section (clog-obj section
                            &key content
                              style hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Section of section type as child of
CLOG-OBJ with CONTENT and if :AUTO-PLACE (default t) place-inside-bottom-of
CLOG-OBJ.

SECTION-TYPE -
   :address :article :aside :header :main :nav
   :p :pre :section :blockquote :h1 :h2 :h3 :h4 :h5 :h6
   :hgroup"))

(defmethod create-section ((obj clog-obj) section
                           &key (content "")
                             (style nil)
                             (hidden nil)
                             (class nil)
                             (html-id nil) (auto-place t))
  (create-child obj (format nil "<~A~@[~A~]~@[~A~]>~A</~A>"
                            section
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style))
                            content
                            section)
                :clog-type  'clog-section
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-phrase
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-phrase (clog-element)()
  (:documentation "CLOG Phrase Objects."))

;;;;;;;;;;;;;;;;;;;
;; create-phrase ;;
;;;;;;;;;;;;;;;;;;;

(deftype phrase-type () '(member :abbr :code :strong :em :dfn :samp :kbd :var
                          :marked :del :ins :s :q :big :small :time :tt :cite
                          :i :b :u :sub :su :center))

(defgeneric create-phrase (clog-obj phrase
                           &key content
                             style hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Phrase of phrase type  as child of
CLOG-OBJ with CONTENT and if :AUTO-PLACE (default t) place-inside-bottom-of
CLOG-OBJ

PHRASE-TYPE -
  :abbr :code :strong :em :dfn :samp :kbd :var
  :marked :del :ins :s :q :big :small :time :tt :cite
  :i :b :u :sub :su :center"))

(defmethod create-phrase ((obj clog-obj) phrase
                          &key (content "")
                            (style nil)
                            (hidden nil)
                            (class nil)
                            (html-id nil)
                            (auto-place t))
  (create-child obj (format nil "<~A~@[~A~]~@[~A~]>~A</~A>"
                            phrase
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style))
                            content
                            phrase)
                :clog-type  'clog-phrase
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-ordered-list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-ordered-list (clog-element)()
  (:documentation "CLOG Ordered-List Objects."))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-ordered-list ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-ordered-list (clog-obj &key style
                                            hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Ordered-List as child of CLOG-OBJ
and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-ordered-list ((obj clog-obj)
                                &key (hidden nil)
                                  (style nil)
                                  (class nil)
                                  (html-id nil)
                                  (auto-place t))
  (create-child obj (format nil "<ol~@[~A~]~@[~A~]/>"
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style)))
                :clog-type  'clog-ordered-list
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;;;;;
;; list-kind ;;
;;;;;;;;;;;;;;;

(deftype list-kind-type () '(member :disc :armenian :circle :cjk-ideographic
                             :decimal :decimal-leading-zero :georgian :hebrew
                             :hiragana :hiragana-iroha :katakana
                             :katakana-iroha :lower-alpha :lower-greek
                             :lower-latin :lower-roman :none :square
                             :upper-alpha :upper-latin :upper-roman))

(defgeneric list-kind (clog-ordered-list)
  (:documentation "Get/Setf list list-kind.

LIST-KIND-TYPE -
  :disc :armenian :circle :cjk-ideographic
  :decimal :decimal-leading-zero :georgian :hebrew
  :hiragana :hiragana-iroha :katakana
  :katakana-iroha :lower-alpha :lower-greek
  :lower-latin :lower-roman :none :square
  :upper-alpha :upper-latin :upper-roman"))

(defmethod list-kind ((obj clog-ordered-list))
  (style obj "list-style-type"))

(defgeneric (setf list-kind) (value clog-ordered-list)
  (:documentation "Set list-kind VALUE for  CLOG-ORDERED-LIST"))

(defmethod (setf list-kind) (value (obj clog-ordered-list))
  (setf (style obj "list-style-type") value))

;;;;;;;;;;;;;;;;;;;
;; list-location ;;
;;;;;;;;;;;;;;;;;;;

(deftype list-location-type () '(member :inside :outside))

(defgeneric list-location (clog-ordered-list)
  (:documentation "Get/Setf list list-location (:inside or :outside).
Default is outside."))

(defmethod list-location ((obj clog-ordered-list))
  (style obj "list-style-position"))

(defgeneric (setf list-location) (value clog-ordered-list)
  (:documentation "Set list-location VALUE for CLOG-ORDERED-LIST"))

(defmethod (setf list-location) (value (obj clog-ordered-list))
  (setf (style obj "list-style-position") value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-unordered-list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-unordered-list (clog-element)()
  (:documentation "CLOG Unordered-List Objects."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-unordered-list ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-unordered-list (clog-obj
                                   &key style
                                     hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Unordered-List as child of CLOG-OBJ
and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-unordered-list ((obj clog-obj) &key (hidden nil)
                                                   (style nil)
                                                   (class nil)
                                                   (html-id nil)
                                                   (auto-place t))
  (create-child obj (format nil "<ul~@[~A~]~@[~A~]/>"
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style)))
                :clog-type  'clog-unordered-list
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-list-item
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-list-item (clog-element)()
  (:documentation "CLOG List-Item Objects."))

;;;;;;;;;;;;;;;;;;;;;;
;; create-list-item ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-list-item (clog-obj &key content
                                         style hidden class html-id auto-place)
  (:documentation "Create a new CLOG-List-Item as child of CLOG-OBJ
and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-list-item ((obj clog-obj) &key (content "")
                                              (style nil)
                                              (hidden nil)
                                              (class nil)
                                              (html-id nil) (auto-place t))
  (create-child obj (format nil "<li~@[~A~]~@[~A~]>~A</li>"
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style))
                            content)
                :clog-type  'clog-list-item
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;;;;;;
;; item-value ;;
;;;;;;;;;;;;;;;;

(defgeneric item-value (clog-list-item)
  (:documentation "Get/Setf list item-value."))

(defmethod item-value ((obj clog-list-item))
  (property obj "value"))

(defgeneric (setf item-value) (value clog-list-item)
  (:documentation "Set item-value VALUE for  CLOG-LIST-ITEM"))

(defmethod (setf item-value) (value (obj clog-list-item))
  (setf (property obj "value") value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-definition-list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-definition-list (clog-element)()
  (:documentation "CLOG Definition-List Objects."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-definition-list ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-definition-list (clog-obj
                                    &key style
                                      hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Definition-List as child of CLOG-OBJ
and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-definition-list ((obj clog-obj) &key (style nil)
                                                    (hidden nil)
                                                    (class nil)
                                                    (html-id nil)
                                                    (auto-place t))
  (create-child obj (format nil "<dl~@[~A~]~@[~A~]/>"
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style)))
                :clog-type  'clog-definition-list
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-term
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-term (clog-element)()
  (:documentation "CLOG Term Objects."))

;;;;;;;;;;;;;;;;;
;; create-term ;;
;;;;;;;;;;;;;;;;;

(defgeneric create-term (clog-obj
                         &key content
                           style hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Term as child of CLOG-OBJ
and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-term ((obj clog-obj)
                        &key (style nil)
                          (hidden nil)
                          (content "")
                          (class nil)
                          (html-id nil) (auto-place t))
  (create-child obj (format nil "<dt~@[~A~]~@[~A~]>~A</dt>"
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style))
                            content)
                :clog-type  'clog-term
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-description
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-description (clog-element)()
  (:documentation "CLOG Description Objects."))

;;;;;;;;;;;;;;;;;;;;;;;;
;; create-description ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-description (clog-obj
                                &key content
                                  style hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Description as child of CLOG-OBJ
and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-description ((obj clog-obj)
                               &key (content "")
                                 (style nil)
                                 (hidden nil)
                                 (class nil)
                                 (html-id nil)
                                 (auto-place t))
  (create-child obj (format nil "<dd~@[~A~]~@[~A~]>~A</dd>"
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style))
                            content)
                :clog-type  'clog-description
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-table (clog-element)()
  (:documentation "CLOG Table Objects."))

;;;;;;;;;;;;;;;;;;
;; create-table ;;
;;;;;;;;;;;;;;;;;;

(defgeneric create-table (clog-obj &key style hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Table as child of CLOG-OBJ
and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-table ((obj clog-obj)
                         &key (style nil) (hidden nil)
                           (class nil) (html-id nil) (auto-place t))
  (create-child obj (format nil "<table~@[~A~]~@[~A~]/>"
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style)))
                :clog-type  'clog-table
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-table-row
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-table-row (clog-element)()
  (:documentation "CLOG Table-Row Objects."))

;;;;;;;;;;;;;;;;;;;;;;
;; create-table-row ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-table-row (clog-obj &key style
                                         hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Table-Row as child of CLOG-OBJ
and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-table-row ((obj clog-obj)
                             &key (style nil) (hidden nil)
                               (class nil) (html-id nil) (auto-place t))
  (create-child obj (format nil "<tr~@[~A~]~@[~A~]/>"
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style)))
                :clog-type  'clog-table-row
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-table-column
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-table-column (clog-table-row)()
  (:documentation "CLOG Table-Column Objects."))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-table-column ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-table-column (clog-obj &key content
                                            column-span
                                            row-span
                                            style
                                            hidden
                                            class
                                            html-id
                                            auto-place)
  (:documentation "Create a new CLOG-Table-Column as child of CLOG-OBJ
and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-table-column ((obj clog-obj) &key (content "")
                                                 (column-span 1)
                                                 (row-span 1)
                                                 (style nil)
                                                 (hidden nil)
                                                 (class nil)
                                                 (html-id nil)
                                                 (auto-place t))
  (create-child obj (format nil "<td colspan=~A rowspan=~A~@[~A~]~@[~A~]>~A</td>"
                            column-span
                            row-span
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style))
                            content)
                :clog-type  'clog-table-column
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-table-heading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-table-heading (clog-table-row)()
  (:documentation "CLOG Table-Heading Objects."))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-table-heading ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-table-heading (clog-obj &key content
                                             column-span
                                             row-span
                                             style
                                             hidden
                                             class
                                             html-id
                                             auto-place)
  (:documentation "Create a new CLOG-Table-Heading as child of CLOG-OBJ
and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-table-heading ((obj clog-obj) &key (content "")
                                                  (column-span 1)
                                                  (row-span 1)
                                                  (style nil)
                                                  (hidden nil)
                                                  (class nil)
                                                  (html-id nil)
                                                  (auto-place t))
  (create-child obj (format nil "<th colspan=~A rowspan=~A~@[~A~]~@[~A~]>~A</th>"
                            column-span
                            row-span
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style))
                            content)
                :clog-type  'clog-table-heading
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-table-head
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-table-head (clog-table)()
  (:documentation "CLOG Table-Head Objects."))

;;;;;;;;;;;;;;;;;;;;;;;
;; create-table-head ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-table-head (clog-obj &key style
                                          hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Table-Head as child of CLOG-OBJ
and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-table-head ((obj clog-obj)
                              &key (style nil) (hidden nil)
                                (class nil) (html-id nil) (auto-place t))
  (create-child obj (format nil "<thead~@[~A~]~@[~A~]/>"
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style)))
                :clog-type  'clog-table-head
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-table-body
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-table-body (clog-table)()
  (:documentation "CLOG Table-Body Objects."))

;;;;;;;;;;;;;;;;;;;;;;;
;; create-table-body ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-table-body (clog-obj &key style
                                          hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Table-Body as child of CLOG-OBJ
and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-table-body ((obj clog-obj)
                              &key (style nil) (hidden nil)
                                (class nil) (html-id nil) (auto-place t))
  (create-child obj (format nil "<tbody~@[~A~]~@[~A~]/>"
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style)))
                :clog-type  'clog-table-body
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-table-caption
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-table-caption (clog-table)()
  (:documentation "CLOG Table-Caption Objects."))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-table-caption ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-table-caption (clog-obj
                                  &key content
                                    style hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Table-Caption as child of CLOG-OBJ
and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-table-caption ((obj clog-obj)
                                 &key (content "")
                                   (style nil)
                                   (hidden nil)
                                   (class nil)
                                   (html-id nil)
                                   (auto-place t))
  (create-child obj (format nil "<caption~@[~A~]~@[~A~]/>~A</caption>"
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style))
                            content)
                :clog-type  'clog-table-caption
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-table-footer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-table-footer (clog-table)()
  (:documentation "CLOG Table-Footer Objects."))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-table-footer ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-table-footer (clog-obj &key style hidden
                                            class html-id auto-place)
  (:documentation "Create a new CLOG-Table-Footer as child of CLOG-OBJ
and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-table-footer ((obj clog-obj)
                                &key (style nil) (hidden nil)
                                  (class nil) (html-id nil) (auto-place t))
  (create-child obj (format nil "<tfoot~@[~A~]~@[~A~]/>"
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style)))
                :clog-type  'clog-table-footer
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-table-column-group
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-table-column-group (clog-table)()
  (:documentation "CLOG Table-Column-Group Objects."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-table-column-group ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-table-column-group (clog-obj
                                       &key style hidden
                                         class html-id auto-place)
  (:documentation "Create a new CLOG-Table-Column-Group as child of CLOG-OBJ
and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-table-column-group ((obj clog-obj)
                                      &key (style nil) (hidden nil)
                                        (class nil) (html-id nil) (auto-place t))
  (create-child obj (format nil "<colgroup~@[~A~]~@[~A~]/>"
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style)))
                :clog-type  'clog-table-column-group
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-table-column-group-item
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-table-column-group-item (clog-table-column-group)()
  (:documentation "CLOG Table-Column-Group-Item Objects."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create-table-column-group-item ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric create-table-column-group-item (clog-obj
                                            &key column-span
                                              style hidden
                                              class html-id auto-place)
  (:documentation "Create a new CLOG-Table-Column-Group-Item as child of CLOG-OBJ
and if :AUTO-PLACE (default t) place-inside-bottom-of CLOG-OBJ"))

(defmethod create-table-column-group-item ((obj clog-obj)
                                           &key (column-span 1)
                                             (style nil)
                                             (hidden nil)
                                             (class nil)
                                             (html-id nil)
                                             (auto-place t))
  (create-child obj (format nil "<col span=~A~@[~A~]~@[~A~]/>"
                            column-span
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style)))
                :clog-type  'clog-table-column-group-item
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-details
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-details (clog-element)()
  (:documentation "CLOG Details Objects."))

;;;;;;;;;;;;;;;;;;;;
;; create-details ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric create-details (clog-obj &key content
                                       style hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Details as child of CLOG-OBJ with :CONTENT
(default \"\") and if :AUTO-PLACE (default t) place-inside-bottom-of
CLOG-OBJ. If hidden is true visiblep is set to nil."))

(defmethod create-details ((obj clog-obj) &key (content "")
                                            (style nil)
                                            (hidden nil)
                                            (class nil)
                                            (html-id nil)
                                            (auto-place t))
  (create-child obj (format nil "<details~@[~A~]~@[~A~]>~A</details>"
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style))
                            content)
                :clog-type  'clog-details
                :html-id    html-id
                :auto-place auto-place))

;;;;;;;;;;;;;;;;;;;
;; details-openp ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric details-openp (clog-details)
  (:documentation "Get/Setf details-openp. Will show details "))

(defmethod details-openp ((obj clog-details))
  (unless (equalp (attribute obj "open") "undefined")
    t))

(defgeneric (setf details-openp) (value clog-details)
  (:documentation "Set details-openp VALUE for CLOG-DETAILS"))

(defmethod (setf details-openp) (value (obj clog-details))
  (if value
      (setf (attribute obj "open") t)
      (remove-attribute obj "open")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-summary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-summary (clog-element)()
  (:documentation "CLOG Summary Objects."))

;;;;;;;;;;;;;;;;;;;;
;; create-summary ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric create-summary (clog-obj &key content
                                       style hidden class html-id auto-place)
  (:documentation "Create a new CLOG-Summary as child of CLOG-OBJ with :CONTENT
(default \"\") and if :AUTO-PLACE (default t) place-inside-bottom-of
CLOG-OBJ. If hidden is true visiblep is set to nil. Use inside a CLOG-DETAILS
object for drop reveal."))

(defmethod create-summary ((obj clog-obj) &key (content "")
                                            (style nil)
                                            (hidden nil)
                                            (class nil)
                                            (html-id nil)
                                            (auto-place t))
  (create-child obj (format nil "<summary~@[~A~]~@[~A~]>~A</summary>"
                            (when class
                              (format nil " class='~A'"
                                      (escape-string class :html t)))
                            (when (or hidden style)
                              (format nil " style='~@[~a~]~@[~a~]'"
                                      (when hidden "visibility:hidden;")
                                      style))
                            content)
                :clog-type  'clog-summary
                :html-id    html-id
                :auto-place auto-place))
