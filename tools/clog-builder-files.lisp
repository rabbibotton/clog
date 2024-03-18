(in-package :clog-tools)

;; Local file utilities

(defun read-file (infile &key clog-obj)
  "Read local file named INFILE"
  (handler-case
      (with-open-file (instream infile :direction :input :if-does-not-exist nil)
	(when instream
	  (let* ((len    (file-length instream))
		 (string (make-string len))
		 (pos    (read-sequence string instream)))
            (subseq string 0 pos))))
    (error (condition)
      (if clog-obj
	  (alert-toast clog-obj "File Error" (format nil "Error: ~A" condition))
	  (format t "Error: ~A" condition)))))

(defun write-file (string outfile &key clog-obj (action-if-exists :rename))
  "Write local file named OUTFILE"
   (check-type action-if-exists (member nil :error :new-version :rename :rename-and-delete
                                            :overwrite :append :supersede))
  (handler-case
      (with-open-file (outstream outfile :direction :output :if-exists action-if-exists)
	(when outstream
	  (write-sequence string outstream)))
    (error (condition)
      (if clog-obj
	  (alert-toast clog-obj "File Error" (format nil "Error: ~A" condition))
	  (format t "Error: ~A" condition)))))
