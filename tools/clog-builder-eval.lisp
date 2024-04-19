(in-package :clog-tools)

(defun on-open-console (obj)
  (let ((app (connection-data-item obj "builder-app-data")))
    (if (console-win app)
        (progn
          (setf (hiddenp (console-win app)) nil)
          (window-focus (console-win app)))
        (let* ((win (on-open-file obj :title "CLOG Builder Console"
                                      :is-console t
                                      :editor-use-console-for-evals t)))
          (set-on-window-can-close win (lambda (obj)
                                         (setf (hiddenp obj) t)
                                         nil))
          (setf (console-win app) win)))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; console-out-stream ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defclass console-out-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((clog-obj :reader   clog-obj                :initarg :clog-obj)
   (win      :accessor win       :initform nil :initarg :win)
   (ace      :accessor ace       :initform nil)
   (col      :accessor col       :initform 0))
  (:documentation "console-in-stream and console-out-stream when used together
provide an interactive console.)"))

(defmethod trivial-gray-streams:stream-write-char ((stream console-out-stream) character)
  (unless (win stream)
    (setf (win stream) (on-open-console (clog-obj stream))))
  (unless (ace stream)
    (setf (ace stream) (window-param (win stream)))
    (setf (clog-ace:mode (ace stream)) "ace/mode/plain_text")) ; mode turns off autoindent
  (js-execute (ace stream) (format nil "~A.renderer.scrollToLine(Number.POSITIVE_INFINITY)"
                                   (clog-ace::js-ace (ace stream))))
  (js-execute (ace stream) (format nil "~A.navigateFileEnd()"
                                   (clog-ace::js-ace (ace stream))))
  (js-execute (ace stream) (format nil "~A.insert(String.fromCharCode(~A),true)"
                                   (clog-ace::js-ace (ace stream))
                                   (char-code character)))
  (if (eql character #\linefeed)
      (setf (col stream) 0)
      (incf (col stream))))

(defmethod trivial-gray-streams:stream-line-column ((stream console-out-stream))
  (col stream))

;;;;;;;;;;;;;;;;;;;;;;;
;; console-in-stream ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defclass console-in-stream (trivial-gray-streams:fundamental-character-input-stream)
  ((clog-obj :reader   clog-obj  :initarg  :clog-obj)
   (buffer   :accessor buffer-of :initform "")
   (index    :accessor index     :initform 0))
  (:documentation "console-in-stream and console-out-stream when used together
provide an interactive console.)"))

(defmethod trivial-gray-streams:stream-read-char ((stream console-in-stream))
  (when (eql (index stream) (length (buffer-of stream)))
    (setf (buffer-of stream) "")
    (setf (index stream) 0))
  (when (eql (index stream) 0)
    (input-dialog (clog-obj stream) "Console Input:"
                  (lambda (result)
                    (setf (buffer-of stream) (format nil "~A~A~%" (buffer-of stream) result)))
                  :time-out 999
                  :modal nil))
  (when (< (index stream) (length (buffer-of stream)))
    (prog1
        (char (buffer-of stream) (index stream))
      (incf (index stream)))))

(defmethod trivial-gray-streams:stream-unread-char ((stream console-in-stream) character)
  (decf (index stream)))

(defmethod trivial-gray-streams:stream-line-column ((stream console-in-stream))
  nil)

;; Lisp code evaluation utilities
(defun capture-eval (form &key (capture-console t)
                            (capture-result t)
                            (capture-result-form "=>~A~%")
                            (eval-form "~A~%=>~A~%")
                            (clog-obj nil)
                            (private-console-win nil)
                            (eval-in-package "clog-user"))
  "Capture lisp evaluaton of FORM."
  (let (console
        (result (make-array '(0) :element-type 'base-char
                                 :fill-pointer 0 :adjustable t))
        eval-result)
    (with-output-to-string (stream result)
      (with-open-stream (out-stream (make-instance 'dialog-out-stream))
        (with-open-stream (in-stream (make-instance 'dialog-in-stream :clog-obj clog-obj :source out-stream))
          (labels ((reset-ace ()
                     (when (typep console 'console-out-stream)
                       (setf (ace console) nil)))
                   (my-debugger (condition encapsulation)
                     (if clog-obj
                         (handler-case
                             (let ((restart (one-of-dialog clog-obj condition (compute-restarts)
                                                           :title "Available Restarts")))
                               (reset-ace)
                               (when restart
                                 (let ((*debugger-hook* encapsulation))
                                   (invoke-restart-interactively restart))))
                           (end-of-file () ; no reset chosen
                             (reset-ace)))
                         (format t "Error - ~A~%" condition))))
            (unless (stringp form)
              (let ((r (make-array '(0) :element-type 'base-char
                                        :fill-pointer 0 :adjustable t)))
                (with-output-to-string (s r)
                  (print form s))
                (setf form r)))
            (setf console (if capture-console
                              stream
                              (make-instance 'console-out-stream :clog-obj clog-obj :win private-console-win)))
            (let* ((*query-io*             (make-two-way-stream in-stream out-stream))
                   (*standard-output*      console)
                   (*standard-input*       (make-instance 'console-in-stream :clog-obj clog-obj))
                   (*error-output*         console)
                   (*debugger-hook*        (if clog-connection:*disable-clog-debugging*
                                               *debugger-hook*
                                               #'my-debugger))
                   (*default-title-class*  *builder-title-class*)
                   (*default-border-class* *builder-border-class*)
                   (*package*              (find-package (string-upcase eval-in-package))))
              (setf eval-result (eval (read-from-string (format nil "(progn ~A)" form))))
              (unless capture-result
                (format console capture-result-form eval-result))
              (when (typep console 'console-out-stream)
                (close console))
              (close *query-io*)
              (values
               (format nil eval-form result eval-result)
               *package*
               eval-result))))))))

(defun do-eval (obj form-string cname &key (package "clog-user") (test t) custom-boot)
  "Render, evalute and run code for panel"
  (let* ((result (capture-eval (format nil "~A~% (clog:set-on-new-window~
                                               (lambda (body)~
                                                 (clog:debug-mode body)~
                                                 ~A
                                                 (create-~A body)) ~A:path \"/test\")"
                                       form-string
                                       (if custom-boot
                                           ""
                                           "(clog-gui:clog-gui-initialize body)
                                            (clog-web:clog-web-initialize body :w3-css-url nil)")
                                       cname
                                       (if custom-boot
                                           (format nil ":boot-file \"~A\" " custom-boot)
                                           ""))
                               :eval-in-package package)))
    (when test
      (if *clogframe-mode*
          (open-browser :url (format nil "http://127.0.0.1:~A/test" *clog-port*))
          (open-window (window (connection-body obj)) "/test")))
    (on-open-file obj :title-class "w3-yellow" :title "test eval" :text result)))
