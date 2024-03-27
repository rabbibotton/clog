(in-package :clog-tools)

;; Lisp code evaluation utilities

(defun capture-eval (form &key (clog-obj nil) (eval-in-package "clog-user"))
  "Capture lisp evaluaton of FORM."
  (let ((result (make-array '(0) :element-type 'base-char
                                 :fill-pointer 0 :adjustable t))
        (eval-result))
    (with-output-to-string (stream result)
      (labels ((my-debugger (condition encapsulation)
                 (declare (ignore encapsulation))
                 (if clog-obj
                     (clog-web-alert (connection-body clog-obj) "Error"
                                     (format nil "~&Error: ~A" condition)
                                     :time-out 3))
                 (format t "~&Error: ~A" condition)))
        (unless (stringp form)
          (let ((r (make-array '(0) :element-type 'base-char
                                    :fill-pointer 0 :adjustable t)))
            (with-output-to-string (s r)
              (print form s))
            (setf form r)))
        (let* ((*standard-output* stream)
               (*error-output* stream)
               (*debugger-hook* #'my-debugger)
               (*package* (find-package (string-upcase eval-in-package))))
          (setf eval-result (eval (read-from-string (format nil "(progn ~A)" form))))
          (values
           (format nil "~A~%=>~A~%" result eval-result)
           *package*))))))

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

