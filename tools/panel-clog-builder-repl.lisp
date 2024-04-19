;;;; CLOG Builder generated code - modify original .clog file and rerender
(in-package :clog-tools)
(defclass clog-builder-repl (clog:clog-panel)
          ((send-to-repl :reader send-to-repl) (status :reader status)
           (playground :reader playground) (terminal :reader terminal)
           (package-div :reader package-div)))
(defun create-clog-builder-repl
       (clog-obj &key (hidden nil) (class nil) (html-id nil) (auto-place t))
  (let ((panel
         (change-class
          (clog:create-div clog-obj :content
                           "<input type=\"TEXT\" value=\"clog-user\" style=\"box-sizing: content-box; position: absolute; inset: 0px 0px 338.5px; height: 25px;\" placeholder=\"Current Package\" class=\"w3-black\" id=\"CLOGB3922519800\" data-clog-name=\"package-div\"><div style=\"--pixel-density: 1; --char-width: 6.60156; box-sizing: content-box; position: absolute; top: 30px; right: 0px; left: 0px; height: 170px;\" class=\"terminal\" id=\"CLOGB3922519801\" data-clog-name=\"terminal\"></div><div style=\"border: thin solid black; box-sizing: content-box; position: absolute; inset: 200px 0px 22px;\" class=\" ace_editor ace_hidpi ace-terminal-theme ace-tm\" id=\"CLOGB3922519802\" data-clog-name=\"playground\"></div><div style=\"box-sizing: content-box; position: absolute; left: 0px; bottom: 0px; right: 0px; height: 20px;\" class=\"w3-black w3-tiny w3-border\" id=\"CLOGB3922519803\" data-clog-name=\"status\">status</div><button id=\"CLOGB39225198357\" style=\"box-sizing: content-box; position: absolute; top: 185px; width: 10px; height: 18px; right: 0px;\" class=\"w3-tiny\" data-clog-name=\"send-to-repl\">&gt;</button>"
                           :hidden hidden :class class :html-id html-id
                           :auto-place auto-place)
          'clog-builder-repl)))
    (setf (slot-value panel 'send-to-repl)
            (attach-as-child clog-obj "CLOGB39225198357" :clog-type
             'clog:clog-button :new-id t))
    (setf (slot-value panel 'status)
            (attach-as-child clog-obj "CLOGB3922519803" :clog-type
             'clog:clog-div :new-id t))
    (setf (slot-value panel 'playground)
            (attach-as-child clog-obj "CLOGB3922519802" :clog-type
             'clog-ace:clog-ace-element :new-id t))
    (setf (slot-value panel 'terminal)
            (attach-as-child clog-obj "CLOGB3922519801" :clog-type
             'clog-terminal:clog-terminal-element :new-id t))
    (setf (slot-value panel 'package-div)
            (attach-as-child clog-obj "CLOGB3922519800" :clog-type
             'clog:clog-form-element :new-id t))
    (let ((target (terminal panel)))
      (declare (ignorable target))
      (clog-terminal:attach-clog-terminal target :greetings
                                          "CLOG Builder REPL - (clog-repl) or (clog-builder-repl) for GUI using *body*")
      (clog-terminal:prompt target "> ")
      (repl-on-create panel target))
    (let ((target (playground panel)))
      (declare (ignorable target))
      (clog-ace:attach-clog-ace target)
      (setf (clog-ace:theme target) "ace/theme/terminal")
      (setf (clog-ace:mode target) "ace/mode/lisp")
      (setf (clog-ace:tab-size target) 2))
    (clog-terminal:set-on-command (terminal panel)
                                  (lambda (target data)
                                    (declare (ignorable target data))
                                    (repl-on-commmand panel target data)))
    (clog:set-on-click (send-to-repl panel)
                       (lambda (target)
                         (declare (ignorable target))
                         (repl-on-send-to-repl panel target)))
    panel))