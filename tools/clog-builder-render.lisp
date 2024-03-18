(in-package :clog-tools)

;; Code rendering utlities

(defun render-clog-code (content hide-loc)
  "Render panel to clog code and add tp CW window"
  (let* ((app      (connection-data-item content "builder-app-data"))
         (panel-id (html-id content))
         (package  (attribute content "data-in-package"))
         (slots    (attribute content "data-custom-slots"))
         (cname    (attribute content "data-clog-name"))
         cmembers vars creates events)
    (unless (or (equal slots "")
                (equal slots "undefined"))
      (push slots cmembers))
    (maphash (lambda (html-id control)
               (declare (ignore html-id))
               (place-inside-bottom-of hide-loc
                                       (get-placer control)))
             (get-control-list app panel-id))
    ;; crawl tree
    ;; Insure that on-setup/on-create follow order in tree
    (labels ((add-siblings (control)
               (let (dct)
                 (loop
                   (unless control (return))
                   (when (equal (html-id control) "undefined") (return))
                   (setf dct (attribute control "data-clog-name"))
                   (unless (equal dct "undefined")
                     (setf control (get-from-control-list app panel-id (html-id control)))
                     (when control
                       (let ((vname (attribute control "data-clog-name"))
                             (control-record (control-info (attribute control "data-clog-type"))))
                         (unless (and (>= (length vname) 5)
                                      (equalp (subseq vname 0 5) "none-"))
                           ;; Add to members of the panel's class for each control
                           (push (format nil
                                         "    \(~A :reader ~A\)~%"
                                         vname
                                         vname)
                                 cmembers)
                           ;; On instance of class, set member value for each control
                           (push (format nil
                                         "    \(setf (slot-value panel '~A\) ~
                                            \(attach-as-child clog-obj \"~A\" :clog-type \'~A\ :new-id t)\)~%"
                                         vname
                                         (html-id control)
                                         (format nil "~S" (getf control-record :clog-type)))
                                 vars)
                           ;; On instance of class, set handers defined for each control
                           (dolist (event (getf control-record :events))
                             ;; Set regular handlers
                             (let ((handler (attribute control (format nil "data-~A" (getf event :name)))))
                               (unless (or (equalp handler "undefined")
                                           (equal handler ""))
                                 (unless (equalp (getf event :name) "on-create")
                                   (let ((event-package (or (getf event :package) "clog")))
                                     (push (format nil
                                                   "    \(~A:set-~A \(~A panel\) \(lambda \(~A\) \(declare \(ignorable ~A\)\) ~A\)\)~%"
                                                   event-package
                                                   (getf event :name)
                                                   vname
                                                   (getf event :parameters)
                                                   (getf event :parameters)
                                                   handler)
                                           events))))))
                           ;; Set on-create (from user in builder) and on-setup (from control-record)
                           (let ((handler (attribute control "data-on-create")))
                             (when (equalp handler "undefined")
                               (setf handler ""))
                             (when (getf control-record :on-setup)
                               (setf handler (format nil "~A~A"
                                                     (funcall (getf control-record :on-setup)
                                                              control control-record)
                                                     handler)))
                             (unless (equal handler "")
                               (push (format nil
                                             "    \(let \(\(target \(~A panel\)\)\) ~
                                             \(declare \(ignorable target\)\) ~
                                              ~A\)~%"
                                             vname
                                             handler)
                                     creates)))))
                       (add-siblings (first-child control))))
                   (when control
                     (setf control (next-sibling control)))))))
      (add-siblings (first-child content)))
    (let ((result (format nil
                          "\(in-package :~A)
\(defclass ~A \(clog:clog-panel\)
  \(~{~A~}\)\)
\(defun create-~A \(clog-obj &key \(hidden nil\) \(class nil\) \(html-id nil\) \(auto-place t\)\)
  \(let \(\(panel \(change-class \(clog:create-div clog-obj :content \"~A\"
         :hidden hidden :class class :html-id html-id :auto-place auto-place\) \'~A\)\)\)
~{~A~}~{~A~}~{~A~}    panel\)\)~%"
                           (string-downcase package)
                          cname     ;;defclass
                          cmembers
                          cname     ;;defun
                          (ppcre:regex-replace-all "\""
                                                    (js-query content
                                                              (format nil
                                                                      "var z=~a.clone();~
    z.find('*').each(function(){~
      var m=$(this).attr('data-clog-name');
      if($(this).attr('data-clog-composite-control') == 't'){$(this).text('')}~
      if($(this).attr('data-clog-composite-control') == 'b'){$(this).html($(this).attr('data-original-html'))}~
      for(n in $(this).get(0).dataset){delete $(this).get(0).dataset[n]}~
      if(m){$(this).attr('data-clog-name', m);}~
    });~
    z.html()"
                                                                      (jquery content)))
                                                    "\\\"")
                          cname
                          vars
                          (reverse creates)   ; Insure that on-setup/on-create follow order in tree
                          (reverse events))))
      (maphash (lambda (html-id control)
                 (declare (ignore html-id))
                 (place-after control (get-placer control)))
               (get-control-list app panel-id))
      ;; prety print the code
      (let ((r (make-array '(0) :element-type 'base-char
                                :fill-pointer 0 :adjustable t)))
        (with-output-to-string (s r)
          (with-input-from-string (n result)
            (let ((*standard-output* s)
                  (*print-case*      :downcase))
              (format t ";;;; CLOG Builder generated code - modify original .clog file and rerender")
              (loop
                (let ((l (read n nil)))
                  (unless l (return))
                  (pprint l))))))
        r))))
