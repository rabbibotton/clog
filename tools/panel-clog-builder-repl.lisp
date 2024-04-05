;;;; CLOG Builder generated code - modify original .clog file and rerender
(in-package :clog-tools)
(defclass clog-builder-repl (clog:clog-panel)
          ((terminal :reader terminal) (package-div :reader package-div)))
(defun create-clog-builder-repl
       (clog-obj &key (hidden nil) (class nil) (html-id nil) (auto-place t))
  (let ((panel
         (change-class
          (clog:create-div clog-obj :content
                           "<input type=\"TEXT\" value=\"clog-user\" style=\"box-sizing: content-box; position: absolute; inset: 0px 0px 338.5px; height: 25px;\" placeholder=\"Current Package\" class=\"w3-black\" id=\"CLOGB3921332482\" data-clog-name=\"package-div\"><div style=\"--pixel-density: 1; --char-width: 6.60156; box-sizing: content-box; position: absolute; inset: 30px 0px 0px;\" class=\"terminal\" id=\"CLOGB3921332483\" data-clog-name=\"terminal\"></div>"
                           :hidden hidden :class class :html-id html-id
                           :auto-place auto-place)
          'clog-builder-repl)))
    (setf (slot-value panel 'terminal)
            (attach-as-child clog-obj "CLOGB3921332483" :clog-type
             'clog-terminal:clog-terminal-element :new-id t))
    (setf (slot-value panel 'package-div)
            (attach-as-child clog-obj "CLOGB3921332482" :clog-type
             'clog:clog-form-element :new-id t))
    (let ((target (terminal panel)))
      (declare (ignorable target))
      (clog-terminal:attach-clog-terminal target :greetings
                                          "CLOG Builder REPL")
      (clog-terminal:prompt target "> "))
    (clog-terminal:set-on-command (terminal panel)
                                  (lambda (target data)
                                    (declare (ignorable target data))
                                    (multiple-value-bind (result new-package)
                                        (capture-eval data :clog-obj panel
                                         :capture-console nil :capture-result
                                         nil :eval-in-package
                                         (text-value (package-div panel)))
                                      (setf (text-value (package-div panel))
                                              (string-downcase
                                               (package-name new-package)))
                                      (clog-terminal:echo target result))))
    panel))