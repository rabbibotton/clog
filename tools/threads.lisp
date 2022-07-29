(in-package "CLOG-TOOLS")
(defclass thread-list (clog:clog-panel) ((w3-table-1 :reader w3-table-1)))
(defun create-thread-list
       (clog-obj &key (hidden nil) (class nil) (html-id nil) (auto-place t))
  (let ((panel
         (change-class
          (clog:create-div clog-obj :content
                           "<table class=\"w3-table w3-striped w3-border w3-bordered w3-hoverable w3-small\" style=\"box-sizing: content-box; position: static; inset: 0px 0px 0px 1px; width: 100%; height: 100%;\" id=\"CLOGB3868051588\" data-clog-name=\"w3-table-1\"></table>"
                           :hidden hidden :class class :html-id html-id
                           :auto-place auto-place)
          'thread-list)))
    (setf (slot-value panel 'w3-table-1)
            (attach-as-child clog-obj "CLOGB3868051588" :clog-type
             'clog:clog-table :new-id t))
    (let ((target (w3-table-1 panel)))
      (declare (ignorable target))
      (loop
       (let ((threads (swank:list-threads)))
         (dolist (thread threads)
           (let ((tr (create-table-row target)))
             (create-table-column tr :content (second thread))
             (create-table-column tr :content (third thread)))))
       (sleep 1)
       (setf (inner-html target) "")
       (unless (visiblep target) (return))))
    panel))
