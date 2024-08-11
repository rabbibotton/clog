(in-package :clog-tools)

;; Code rendering utlities

(defun pprint-as-html (content)
  (js-query content (format nil "html_beautify ('~A',{'wrap_line_length':'80',~
     'extra_liners':'div,form,input,button,select,textarea,ol,ul,table,style,datalist'})"
                            (escape-string (remove-clog-attributes (inner-html content))))))

(defun remove-clog-attributes (html)
  (let ((root (plump:parse html)))
    (html-remove-attributes root '("data-clog-type"
                                   "data-original-html"
                                   "data-clog-composite-control"
                                   "data-clog-custom-create"))
    (with-output-to-string (str)
      (plump:serialize root str))))

(defun html-remove-attributes (root attrs)
  (let ((v (plump:child-elements root)))
    (dotimes (n (length v))
      (if (equalp (plump:tag-name (aref v n)) "data")
          (plump:remove-child (aref v n))
          (progn
            (when (plump:child-elements (aref v n))
              (let ((composite (plump:attribute (aref v n) "data-clog-composite-control")))
                (if composite
                    (progn
                      (if (equalp composite "b")
                          (let* ((new-html     (plump:parse (plump:attribute (aref v n) "data-original-html")))
                                 (new-children (plump:child-elements new-html)))
                            (plump:clear (aref v n))
                            (dotimes (cn (length new-children))
                              (plump:append-child (aref v n) (aref new-children cn))))
                          (plump:clear (aref v n))))
                    (html-remove-attributes (aref v n) attrs))))
            (maphash (lambda (k v)
                       (declare (ignore v))
                       (unless (ppcre:scan "^data-clog-name" k)
                         (when (ppcre:scan "^data-clog-" k)
                           (pushnew k attrs :test 'equalp)))
                       (when (ppcre:scan "^data-on-" k)
                         (pushnew k attrs :test 'equalp)))
                     (plump:attributes (aref v n)))
            (mapcar (lambda (attr)
                      (plump:remove-attribute (aref v n) attr))
                    attrs))))))

(defun render-clog-code (content hide-loc)
  "Render panel to clog code and add to window"
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
                       (let* ((vname (attribute control "data-clog-name"))
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
                                         (if (eq (getf control-record :create-type) :custom-panel)
                                             (format nil "~A" (attribute control "data-clog-custom-type"))
                                             (format nil "~S" (getf control-record :clog-type))))
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
                           (let ((resizeh (attribute control "data-on-resize"))
                                 (handler (attribute control "data-on-create")))
                             (when (equalp handler "undefined")
                               (setf handler ""))
                             (when (equalp resizeh "undefined")
                               (setf resizeh ""))
                             (when (getf control-record :on-setup)
                               (setf handler (format nil "~A~A"
                                                     (funcall (getf control-record :on-setup)
                                                              control control-record)
                                                     handler)))
                             (unless (equal resizeh "")
                               (push (format nil
                                             "   \(set-on-resize \(window \(connection-body panel\)\)
                                                                \(lambda \(obj\)
                                                                  \(declare \(ignore obj\)\)
                                                                  \(jquery-trigger \(~A panel\) \"resize\"\)\)\)"
                                             vname)
                                     creates))
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
\(defun create-~A \(clog-obj &key hidden class style html-id \(auto-place t\)\)
  \(let \(\(panel \(change-class \(clog:create-div clog-obj :content \"~A\"
         :hidden hidden :class class :style style :html-id html-id :auto-place auto-place\) \'~A\)\)\)
~{~A~}~{~A~}~{~A~}    panel\)\)~%"
                          (string-downcase package)
                          cname     ;;defclass
                          cmembers
                          cname     ;;defun
                          (ppcre:regex-replace-all "\""
                                                   (pprint-as-html content)
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
            (let ((*standard-output*    s)
                  (*print-right-margin* *builder-render-right-margin* )
                  (*print-case*         *builder-render-case*))
              (format t ";;;; CLOG Builder generated code - modify original .clog file and rerender")
              (loop
                (let ((l (read n nil)))
                  (unless l (return))
                  (pprint l))))))
        r))))
