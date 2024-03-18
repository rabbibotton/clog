(in-package :clog-tools)

;; Control-List utilities

(defun init-control-list (app panel-id)
  "Initialize new control list for PANEL-ID on instance of APP."
  (setf (gethash panel-id (control-lists app)) (make-hash-table :test #'equalp)))

(defun destroy-control-list (app panel-id)
  "Destroy the control-list on PANEL-ID"
  (remhash panel-id (control-lists app)))

(defun get-control-list (app panel-id)
  "Rerieve the control-list hash table on PANEL-ID"
  (let ((h (gethash panel-id (control-lists app))))
    (if h
	h
	(make-hash-table* :test #'equalp)))) ;; return empty hash to avoid map fails

(defun add-to-control-list (app panel-id control)
  "Add a CONTROL on to control-list on PANEL-ID"
  (let ((html-id (format nil "~A" (html-id control))))
    (setf (gethash html-id (get-control-list app panel-id)) control)))

(defun get-from-control-list (app panel-id html-id)
  "Get control identified my HTML-ID from control-list on PANEL-ID"
  (gethash html-id (get-control-list app panel-id)))

(defun remove-from-control-list (app panel-id html-id)
  "Remove a control identified by HTML-ID from control-list on PANEL-ID"
  (remhash html-id (get-control-list app panel-id)))

(defun remove-deleted-from-control-list (app panel-id)
  "Remove any deleted control from control-list"
  (maphash (lambda (html-id control)
             (when (equalp (js-query control (format nil "$.contains(document.documentElement, ~A)"
                                                     (clog::script-id control))) "false")
               (remove-from-control-list app panel-id html-id)))
           (get-control-list app panel-id)))
