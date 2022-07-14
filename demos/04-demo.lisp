;;;; Demo 4 - CMS Website

(defpackage #:clog-demo-4
  (:use #:cl #:clog #:clog-web)
  (:export start-demo))

(in-package :clog-demo-4)

;; Site Configuration
(defparameter side-panel-size 200 "Size of menu")
(defparameter sysop-password "admin")

(defvar *sql-connection*)

(defclass app-data ()
  ((head
    :accessor head)
   (side
    :accessor side)
   (main
    :accessor main)
   (sysop
    :initform nil
    :accessor sysop)))

(defun create-web-frame (body app)
  (setf (title (html-document body)) "Demo 4")
  ;; +----------------------------------------------+
  ;; |             header area (head)               |
  ;; +----------------------------------------------+
  ;; |          |                                   |
  ;; |   menu   |          content area             |
  ;; |  (side)  |             (main)                |
  ;; |          |                                   |
  ;; +----------+-----------------------------------+
  ;; create 3 pain site
  ;;
  ;; Header
  (setf (head app) (create-web-panel body :content "<h3>Demo 4:</h3><p>A simple Lisp CMS</p>"
                                          :class   "w3-yellow"))
  ;; Sidebar
  (setf (side app) (create-web-sidebar body))
  (setf (box-width (side app)) (unit :px side-panel-size))
  (add-card-look (side app))
  ;; Main
  (setf (main app) (create-web-content body))
  (set-margin-side (main app) :left (unit :px (+ side-panel-size 10)))
  (create-web-container (main app)))

(defun insert-content (app new-page text-area)
  (dbi:do-sql
   *sql-connection*
   "insert into config (menu, main) values (?, ?)"
    (list (escape-string (value new-page)) (escape-string (value text-area))))
  (reset-menu app)
  (route-content app (escape-string (value new-page))))

(defun new-content (app)
  (setf (inner-html (main app)) "")
  (let ((new-page  (create-form-element (main app) :text :value "New Title"))
        (tmp       (create-br (main app)))
        (text-area (create-text-area (main app) :rows 10 :columns 40)))
    (declare (ignore tmp))
    (create-br (main app))
    (set-on-click (create-button (main app) :content "Insert")
                  (lambda (obj)
                    (declare (ignore obj))
                    (insert-content app new-page text-area)))))

(defun update-content (app page text-area)
  (dbi:do-sql
   *sql-connection*
    "update config set main= ? where menu= ?"
    (list (escape-string (value text-area)) page))
  (route-content app page))

(defun delete-content (app page)
  (dbi:do-sql
   *sql-connection*
    "delete from config where menu= ?" (list page))
  (reset-menu app)
  (route-content app "Home"))

(defun edit-content (app page)
  (setf (inner-html (main app)) "")
  (let ((contents (dbi:fetch-all
                   (dbi:execute
                    (dbi:prepare
                     *sql-connection*
                     "select main from config where menu= ?")
                    (list page)))))
    (dolist (content contents)
      (let ((text-area (create-text-area (main app) :rows 10 :columns 40
                                                    :value (second content))))
        (create-br (main app))
        (set-on-click (create-button (main app) :content "Update")
                      (lambda (obj)
                        (declare (ignore obj))
                        (update-content app page text-area)))
        (unless (equal page "Home")
          (set-on-click (create-button (main app) :content "Delete")
                        (lambda (obj)
                          (declare (ignore obj))
                          (delete-content app page))))))))

(defun route-content (app page)
  (setf (inner-html (main app)) "")
  (let ((contents (dbi:fetch-all
                   (dbi:execute
                    (dbi:prepare
                     *sql-connection*
                     "select main from config where menu= ?")
                    (list page)))))
    (dolist (content contents)
      (setf (inner-html (main app)) (second content))
      (create-br (main app))
      (create-br (main app))
      (when (sysop app)
        (set-on-click (create-a (main app) :content "edit")
                      (lambda (obj)
                        (declare (ignore obj))
                        (edit-content app page)))))))
(defun id-me (app)
  (setf (inner-html (main app)) "")
  (clog-web-form (main app) "Validate:"
                 '(("Password" "pass" :password))
                 (lambda (res)
                   (if (equal (second (first res)) sysop-password)
                       (progn
                         (setf (sysop app) t)
                         (reset-menu app)
                         (setf (inner-html (main app)) "You are logged in."))
                       (setf (inner-html (main app)) "Invalid password.")))))

(defun reset-menu (app)
  (setf (inner-html (side app)) "")
  (let ((menu-items (dbi:fetch-all
                     (dbi:execute
                      (dbi:prepare *sql-connection*
                                   "select menu from config")))))
    (dolist (menu-item menu-items)
      (set-on-click
       (create-web-sidebar-item (side app) :content (second menu-item))
       (lambda (obj)
         (declare (ignore obj))
         (route-content app (second menu-item))))))
  (create-br (side app))
  (if (sysop app)
      (progn
        (set-on-click (create-a (side app) :content "new")
                      (lambda (obj)
                        (declare (ignore obj))
                        (new-content app)))
        (create-br (side app))
        (set-on-click (create-a (side app) :content "logout")
                      (lambda (obj)
                        (declare (ignore obj))
                        (setf (sysop app) nil)
                        (reset-menu app)
                        (route-content app "Home"))))
      (set-on-click (create-a (side app) :content "login")
                    (lambda (obj)
                      (declare (ignore obj))
                      (id-me app)))))

(defun on-new-window (body)
  (set-html-on-close body "Connection Lost")
  ;; Create an app-data object for every connection
  (let ((app (make-instance 'app-data)))
    (setf (connection-data-item body "app-data") app)
    (clog-web-initialize body)
    (create-web-frame body app)
    (reset-menu app)
    (route-content app "Home")))

(defun start-demo ()
  "Start dynamic website demo."
  ;; The demo database is created in the clog dir
  (let ((db-dir (merge-pathnames "demo4.db" (clog:clog-install-dir))))
    (setf *sql-connection* (dbi:connect :sqlite3 :database-name db-dir))
    (format t "Database location: ~A~%" db-dir))
  (handler-case
      (dbi:fetch (dbi:execute (dbi:prepare *sql-connection* "select * from config")))
    (error ()
      (print "First run creating config.")
      (dbi:do-sql
       *sql-connection*
       "create table config (menu varchar, main varchar)")
      (dbi:do-sql
       *sql-connection*
       "insert into config (menu, main) values ('Home', '<b>Hello welcome.</b>')")
      (dbi:do-sql
       *sql-connection*
       "insert into config (menu, main) values ('Page2', '<i>Customize Me.</i>')")))
  (initialize 'on-new-window)
  (open-browser))

(defun stop-demo ()
  "Shutdown demo and close databases."
  (dbi:disconnect *sql-connection*)
  (shutdown))
