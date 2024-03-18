;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) David Botton                                                      ;;;;
;;;;                                                                       ;;;;
;;;; clog-document.lisp                                                    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - clog-document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-document (clog-obj)
  ((document-element
    :reader document-element
    :initarg :document-element)
   (head-element
    :reader head-element
    :initarg :head-element)
   (body-element
    :reader body-element))
  (:documentation "CLOG Document Objects encapsulate the document."))

;;;;;;;;;;;;;;;;;;;;;;;;
;; make-clog-document ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-clog-document (connection-id)
  "Construct a new clog-document. (Private)"
  (make-instance
   'clog-document :connection-id connection-id :html-id "document"
                  :document-element (make-instance 'clog-element
                                                   :connection-id connection-id
                                                   :html-id       "documentElement")
                  :head-element (make-instance 'clog-element
                                               :connection-id connection-id
                                               :html-id "head")))
;;;;;;;;;;;;;;;;;;;;;;
;; document-element ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric document-element (clog-document)
  (:documentation "Reader for Document Element object"))

;;;;;;;;;;;;;;;;;;
;; head-element ;;
;;;;;;;;;;;;;;;;;;

(defgeneric head-element (clog-document)
  (:documentation "Reader for Head Element object"))

;;;;;;;;;;;;;;;;;;
;; body-element ;;
;;;;;;;;;;;;;;;;;;

(defgeneric document-element (clog-document)
  (:documentation "Reader for Body Element object"))

;;;;;;;;;;;;;;
;; set-body ;;
;;;;;;;;;;;;;;

(defgeneric set-body (clog-document body)
  (:documentation "Set the body slot after creating the
clog-document object. (Private)"))

(defmethod set-body ((obj clog-document) body)
  (setf (slot-value obj 'body-element) body))

;;;;;;;;;;;;
;; domain ;;
;;;;;;;;;;;;

(defgeneric domain (clog-document)
  (:documentation "Get domain."))

(defmethod domain ((obj clog-document))
  (query obj "domain"))

;;;;;;;;;;;;;;;;;;;;
;; input-encoding ;;
;;;;;;;;;;;;;;;;;;;;

(defgeneric input-encoding (clog-document)
  (:documentation "Get input encoding."))

(defmethod input-encoding ((obj clog-document))
  (query obj "inputEncoding"))

;;;;;;;;;;;;;;;;;;;
;; last-modified ;;
;;;;;;;;;;;;;;;;;;;

(defgeneric last-modified (clog-document)
  (:documentation "Get last modified."))

(defmethod last-modified ((obj clog-document))
  (query obj "lastModified"))

;;;;;;;;;;;;;
;; referer ;;
;;;;;;;;;;;;;

(defgeneric referer (clog-document)
  (:documentation "Get referer."))

(defmethod referer ((obj clog-document))
  (query obj "referer"))

;;;;;;;;;;;
;; title ;;
;;;;;;;;;;;

(defgeneric title (clog-document)
  (:documentation "Get/setf title."))

(defmethod title ((obj clog-document))
  (query obj "title"))

(defgeneric (setf title) (value clog-document))

(defmethod (setf title) (value (obj clog-document))
  (execute obj
           (format nil "title='~A'" (escape-string value)))
  value)

;;;;;;;;;;;;;;;;;;
;; document-url ;;
;;;;;;;;;;;;;;;;;;

(defgeneric document-url (clog-document)
  (:documentation "Get url."))

(defmethod document-url ((obj clog-document))
  (query obj "url"))

;;;;;;;;;;;;;;;;;
;; ready-state ;;
;;;;;;;;;;;;;;;;;

(defgeneric ready-state (clog-document)
  (:documentation "Get ready-state. Returns 'loading'|'interactive'|'complete'.
The state would only change if the boot-page was still loading css, graphics, etc.
Under normal circumstance there is no need in CLOG to check if the state is ready
as on-new-window is only called once one can interact with page. See also
SET-ON-READY-STATE-CHANGE"))

(defmethod ready-state ((obj clog-document))
  (query obj "readyState"))

;;;;;;;;;;;;;;;;;;;;;;
;; visibility-state ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric visibility-state (clog-document)
  (:documentation "Get visibility-state. Returns the string 'visible' if
browser is in non-minmized state and is partialy visible and 'hidden' is
browser is minimized or no part of page is visible including an OS screen
lock."))

(defmethod visibility-state ((obj clog-document))
  (query obj "visibilityState"))

;;;;;;;;;;;;;;
;; load-css ;;
;;;;;;;;;;;;;;

(defgeneric load-css (clog-document css-url &key load-only-once)
  (:documentation "Load css from CSS-URL. If LOAD-ONLY-ONCE load-css
returns t if load-css previously called otherwise loads the css and
returns css-url."))

(defmethod load-css ((obj clog-document) css-url &key (load-only-once t))
  (let ((loaded (connection-data-item obj (format nil "clog-~A" css-url))))
    (cond ((not (and load-only-once loaded))
           (jquery-execute (head-element obj)
                           (format nil "append('<link rel=\"stylesheet\" href=\"~A\" type=\"text/css\">')"
                                   (escape-string css-url)))
           (setf (connection-data-item obj (format nil "clog-~A" css-url)) t)
           css-url)
          (t
           t))))

;;;;;;;;;;;;;;;;;
;; load-script ;;
;;;;;;;;;;;;;;;;;

(defgeneric load-script (clog-document script-url &key wait-for-load
                                                    wait-timeout
                                                    load-only-once)
  (:documentation "Load script from SCRIPT-URL. If WAIT-FOR-LOAD
is t, load-script will not return until script load is completed or
WAIT-TIMEOUT passes and load-script returns nil otherwise script-url.
If LOAD-ONLY-ONCE is t first checks if previously loaded with load-script."))

(defmethod load-script ((obj clog-document) script-url &key (wait-for-load t)
                                                         (wait-timeout 3)
                                                         (load-only-once t))
  (let ((loaded (connection-data-item obj (format nil "clog-~A" script-url))))
    (cond ((not (and load-only-once loaded))
           (let ((sem (bordeaux-threads:make-semaphore)))
             (flet ((on-load (obj url)
                      (declare (ignore obj))
                      (when (equalp url script-url)
                        (bordeaux-threads:signal-semaphore sem))))
               (when wait-for-load
                 (set-on-load-script obj #'on-load :one-time t))
               ;; After we load the script from src we then fire the
               ;; custom on-load-script event in the next line of
               ;; script after the load as scripts are loaded
               ;; synchronously.
               (js-execute obj
                (format nil "$.getScript('~A', function() {~
                            $(clog['document']).trigger('on-load-script',~
                                                            '~A')})"
                        (escape-string script-url)
                        (escape-string script-url)))
               (cond (load-only-once
                      (when (bordeaux-threads:wait-on-semaphore
                             sem :timeout wait-timeout)
                        (setf (connection-data-item obj (format nil "clog-~A"
                                                                script-url)) t)
                        script-url))
                     (t
                      (setf (connection-data-item obj (format nil "clog-~A"
                                                              script-url)) t)
                      script-url)))))
          (t
           t))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-load-script ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-load-script (clog-document handler
                                    &key cancel-event one-time)
  (:documentation "Set a HANDLER for script load complete on CLOG-document.
the handler (clog-obj data) data is the script-url used to load it.
The handler should be installed on the document before calling load-script."))

(defmethod set-on-load-script ((obj clog-document) handler
                               &key
                                 (cancel-event nil)
                                 (one-time     nil))
  (set-event obj "on-load-script"
             (when handler
               (lambda (data)
                 (funcall handler obj data)))
             :call-back-script "+data"
             :cancel-event cancel-event
             :one-time     one-time))

;;;;;;;;;
;; put ;;
;;;;;;;;;

(defgeneric put (clog-document message)
  (:documentation "Write text to browser document object."))

(defmethod put ((obj clog-document) message)
  (execute obj (format nil "write('~A')" (escape-string message))))

;;;;;;;;;;;;;;
;; put-line ;;
;;;;;;;;;;;;;;

(defgeneric put-line (clog-document message)
  (:documentation "Write text to browser document object with new-line."))

(defmethod put-line ((obj clog-document) message)
  (execute obj (format nil "writeln('~A')" (escape-string message))))

;;;;;;;;;;;;
;; put-br ;;
;;;;;;;;;;;;

(defgeneric put-br (clog-document message)
  (:documentation "Write text to browser document object with <\br>new-line."))

(defmethod put-br ((obj clog-document) message)
  (execute obj (format nil "writeln('~A<\br>')" (escape-string message))))

;;;;;;;;;;;;;;
;; new-line ;;
;;;;;;;;;;;;;;

(defgeneric new-line (clog-document)
  (:documentation "Write to browser document <\br>new-line."))

(defmethod new-line ((obj clog-document))
  (execute obj (format nil "writeln('<\br>')")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-full-screen-change ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-full-screen-change (clog-document
                                       on-full-screen-change-handler)
  (:documentation "Set the ON-FULL-SCREEN-CHANGE-HANDLER for CLOG-OBJ.
If ON-FULL-SCREEN-CHANGE-HANDLER is nil unbind the event."))

(defmethod set-on-full-screen-change ((obj clog-document) handler)
  (set-on-event obj "fullscreenchange" handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-visibility-change ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-visibility-change (clog-document
                                       on-visibility-change-handler)
  (:documentation "Set the ON-VISIBILITY-CHANGE-HANDLER for CLOG-OBJ.
If ON-VISIBILITY-CHANGE-HANDLER is nil unbind the event. This event
is fired when the page visibility has changed such as a tab being
switch of browser minimized. Use the VISIBILITY-STATE property to
identify chage."))

(defmethod set-on-visibility-change ((obj clog-document) handler)
  (set-on-event obj "visibilitychange" handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-ready-state-change ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric set-on-ready-state-change (clog-document
                                       on-ready-state-change-handler)
  (:documentation "Set the ON-READY-STATE-CHANGE-HANDLER for CLOG-OBJ.
If ON-READY-STATE-CHANGE-HANDLER is nil unbind the event."))

(defmethod set-on-ready-state-change ((obj clog-document) handler)
  (set-on-event obj "readystatechange" handler))
