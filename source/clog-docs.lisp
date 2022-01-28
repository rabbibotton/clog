;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2021 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-docs.lisp                                                        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :clog)

;;; Various defsections defined in clog.lisp that are specific to
;;; documentation and documentation helpers

;;;;;;;;;;;;;;;;
;; load-world ;;
;;;;;;;;;;;;;;;;

(defun load-world ()
  "Load source files for creating documentation"
  (load "source/clog.lisp")
  (load "source/clog-utilities.lisp")
  (load "source/clog-base.lisp")
  (load "source/clog-element.lisp")
  (load "source/clog-element-common.lisp")
  (load "source/clog-style.lisp")
  (load "source/clog-canvas.lisp")
  (load "source/clog-form.lisp")
  (load "source/clog-multimedia.lisp")  
  (load "source/clog-window.lisp")
  (load "source/clog-document.lisp")
  (load "source/clog-location.lisp")
  (load "source/clog-navigator.lisp")
  (load "source/clog-body.lisp")
  (load "source/clog-system.lisp")
  (load "source/clog-panel.lisp")
  (load "source/clog-gui.lisp")
  (load "source/clog-web.lisp")    
  (load "source/clog-docs.lisp")
  (load "source/clog-helpers.lisp"))

;;;;;;;;;;;;;;;;;;;;
;; make-mark-down ;;
;;;;;;;;;;;;;;;;;;;;

(defun make-mark-down ()
  "Create manual in Mark Down format"
  (load-world)
  (describe clog:@CLOG-MANUAL))

;;;;;;;;;;;;;;;
;; make-html ;;
;;;;;;;;;;;;;;;

(defun make-html ()
  "Create manual in HTML"
  (load-world)
  (mgl-pax:update-asdf-system-html-docs clog:@CLOG-MANUAL :clog))

;;;;;;;;;;;;;;;;
;; make-world ;;
;;;;;;;;;;;;;;;;

(defun make-world ()
  (make-html)
  (asdf:compile-system :clog))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports - clog documentation sections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defsection @clog-getting-started (:title "CLOG Getting Started")
"CLOG - The Common Lisp Omnificent GUI

David Botton <david@botton.com>

License BSD 3-Clause License

The Common Lisp Omnificent GUI, CLOG for short, uses web technology to
produce graphical user interfaces for applications locally or remotely.
CLOG can take the place, or work alongside, most cross-platform GUI
frameworks and website frameworks. The CLOG package starts up the
connectivity to the browser or other websocket client (often a browser
embedded in a native template application.)

STATUS: CLOG is complete and all work is on higher order additions, 
such as full desktop over the web, database tools,etc. See below for
some enhacements being worked on. CLOG is actually based on GNOGA, a
framework I wrote for Ada in 2013 and used in commercial production
code for the last 8+ years, i.e. the techiniques CLOG uses are solid
and proven.

CLOG is being actively extended daily. Check the github discussion
boards for the latest.

Some potential applications for CLOG:

* Cross-platform GUIs and Reports
* Secure websites and complex interactive web applications
* Mobile software
* Massive multiplayer online games
* Monitoring software for embedded systems
* A fun way to teach programming and advanced multi-tasking
  parallel programming techniques. (CLOG is a parallel GUI)
* And the list goes on

The key to CLOG is the relationship it forms with a Browser window
or Browser control compiled to native code. CLOG uses websockets
for communications and the browser to render a GUI that maintains
an active soft realtime connection. For most CLOG applications all
programming logic, events and decisions are done on the server
which can be local, or remote over the web.

CLOG is developed on an M1 MacBook with ECL and SBCL,
it is tested fairly regulary with SBCL on Linux, Windows and
Intel MacBook. It should in theory work on any system with
Quicklisp and CLACK.

CLOG is in Quicklisp (ql:quickload :clog), but you may want to live on the bleeding edge
and use Ultralisp or clone the github repo into your
~/common-lisp directory (or other quicklisp/asdf findable
directory):

```
cd ~/common-lisp
git clone https://github.com/rabbibotton/clog.git
```

To load this package and work through tutorials (assuming you
have Quicklisp configured.)

Note: If using portacle for Windows you will need to
update Quicklisp use (ql:update-dist \"quicklisp\")
You will also likely need to copy the sqlite3 dll from
https://www.sqlite.org/download.html to portacle\win\lib

1. Start emacs then M-x slime
2. In the REPL, run:

```
CL-USER> (ql:quickload :clog)
CL-USER> (clog:run-tutorial 1)
```

Tip for Windows WSL linux user. Install \"sudo apt install xdg-utils\"
to install xdg-open so that run-tutorial uses the windows browser.

To see where the source, tutorial and demo files are:

```
CL-USER> (clog:clog-install-dir)
```

You can the run the demos with:

```
CL-USER> (ql:quickload :clog)
CL-USER> (clog:run-demo 1)
```

The clog-db-admin tool can be run with:

```
CL-USER> (ql:quickload :clog/tools)
CL-USER> (clog-tools:clog-db-admin)
```

The clog-builder GUI Builder tool can be run with:

```
CL-USER> (ql:quickload :clog/tools)
CL-USER> (clog-tools:clog-builder)
```

You can also open a \"clog-repl\" window in your browser to play
from the common-lisp repl:

```
CL-USER> (in-package clog-user)
CLOG-USER> (clog-repl)
CLOG-USER> (setf (background-color *body*) \"beige\")
CLOG-USER> (create-div *body* :content \"Hello World!\")
```

The clog-repl URL is http://127.0.0.1/repl *body* will always refer to the
last access of that URL.

To open a browser with the CLOG manual:

```
CL-USER> (clog:open-manual)
```

Work your way through the tutorials. You will see how quick and easy it is
to be a CLOGer. The next section also covers the basic programming concepts
needed for mastering CLOG.")

(defsection @clog-programming-basics (:title "CLOG Programming Basics")
  "
* Prerequisites
    - You don't have to be an expert in Common Lisp but should know the basics
    - You _don't_ need to know JavaScript
    - You don't need to know HTML but it helps unless someone else is doing the
      design work.
    - You have installed CLOG and (ql:quickload :clog) is working for you.

* Simple REPL techniques Tutorial

We first need to load CLOG

```lisp
CL-USER> (ql:quickload :clog)
To load \"clog\":
  Load 1 ASDF system:
    clog
; Loading \"clog\"
................................................
(:CLOG)
```

Next, we tell clog to start a clog-repl:

```lisp
CL-USER> (clog:clog-repl)
Hunchentoot server is started.
Listening on 0.0.0.0:8080.
HTTP listening on    : 0.0.0.0:8080
HTML Root            : ~/common-lisp/clog/static-files/
Boot file for path / : /debug.html

Use clog-user:*body* to access the clog-repl window.
NIL
```

At this point CLOG should open a browser window to
 http://127.0.0.1:8008/repl

We can now enter the clog-user package and hack a way.

```lisp
CL-USER> (in-package clog-user)
#<\"CLOG-USER\" package>
CLOG-USER> (setf (background-color *body*) :red)
```

Something more than an empty lambda function is needed to do more. The
tutorials are a good place to start with make CLOG apps in code, so
here we are going to demonstrate the concepts using some REPL tricks
to help developing CLOG apps in general.

(From here on, we will leave out the prompts and responses in our quotes of
code.)

```lisp
(create-div *body* :content \"Hello World\")
```

If you have the browser on the screen you will see the results immediately. Try
this line and you can watch it happen:

```lisp
(dotimes (n 10) (create-div *body* :content (format nil \"Line ~A - Hello World\" n)) (sleep .3))
```

We can also set and respond to events and set properties etc:

```lisp
(let ((tmp (create-button *body* :content \"Click Me\")))
  (set-on-click tmp (lambda (obj)(setf (hiddenp tmp) t))))
```

Since we already initialized CLOG let's use SET-ON-NEW-WINDOW to change our
on-new-window handler (handler is just a made up name for a function that
will handle an event).

```lisp
CLOG-USER> (set-on-new-window (lambda (body) (create-div body :content \"Hello World!\")))
```

Now any new window opened will not be using CLOG REPL but instead will execute our handler.

Important take aways to using CLOG from the REPL:

1. You will need to pass to a global from the running system whatever you want to tinker
with in the live system from the REPL.
2. Any time you recompile the on-new-window handler or want to use a different one
you will need to use SET-ON-NEW-WINDOW.
3. Similarily with all events, any time an event handler is recompiled or want to
change the even hander, set-on-* function will need to be called.")


(defsection @clog-event-data (:title "CLOG Event Data")
"
Some events in CLOG return in addition to the target event, event data.
The data is passed in the second argument to the event handler as a 
property list. To retrieve the data use (getf data :property) the available
properties (to use for :property) are based on the event type.

Events in clog-base

     :event-type   :mouse
     :x            x relative to the target
     :y            y relative to the target
     :screen-x     x relative to the users screen
     :screen-y     y relative to the users screen
     :which-button which mouse button clicked
     :alt-key      t or nil if alt-key held down
     :ctrl-key     t or nil if ctrl-key held down
     :shift-key    t or nil if shift-key held down
     :meta-key     t or nil if meta-key held down

     :event-type   :pointer
     :x            x relative to the target
     :y            y relative to the target
     :screen-x     x relative to the users screen
     :screen-y     y relative to the users screen
     :which-button which mouse button clicked
     :alt-key      t or nil if alt-key held down
     :ctrl-key     t or nil if ctrl-key held down
     :shift-key    t or nil if shift-key held down
     :meta-key     t or nil if meta-key held down

     :event-type     :touch
     :x              x relative to the target
     :y              y relative to the target
     :screen-x       x relative to the users screen
     :screen-y       y relative to the users screen
     :number-fingers number of fingers being used
     :alt-key        t or nil if alt-key held down
     :ctrl-key       t or nil if ctrl-key held down
     :shift-key      t or nil if shift-key held down
     :meta-key       t or nil if meta-key held down

     :event-type :keyboard
     :key        String of key pressed, with out modifiers like ctrl characters
     :key-code   The utf-16 value of :key
     :char-code  UTF-8 representation for key pressed when possible - deprecated
     :alt-key    t or nil if alt-key held down
     :ctrl-key   t or nil if ctrl-key held down
     :shift-key  t or nil if shift-key held down
     :meta-key   t or nil if meta-key held down

Events in clog-window

     :event-type :storage
     :key        local storage key that was updated (even in another window)
     :old-value  old key value
     :value      new key value


")

(defsection @clog-internals (:title "CLOG Framework internals and extensions")
"
** Introduction to Internals **

This section on internals is not intended for general use of CLOG. It is
for those looking to maint or extend CLOG, or those creating plugins.

** The Client Side and the Server Side **

All objects created in CLOG have a server side and a client side
representation, at least at the time of their creation. The server
side representation is a CLOG-obj or one of its descendants that is
returned by one of the many create-* functions. The client side
representation is the DOM element (or other JavaScript object) itself
stored in the clog array keyed by the html-id clog[html-id].

** Client Side Scripting **

Executing code on the client side is done in one of three ways:

 1. The connection - Using the clog-connection package execute or query
 2. The DOM object - Using the clog-obj execute or query
 3. The jQuery wrapper - Using the clog-obj jquery-execute or jquery-query

Query time outs are set in clog-connect:*query-time-out* by default 3
seconds.


** Responding to new JavaScript DOM events **

If there is no data for the event just changing the name of the event is
sufficient in this example:

```lisp
(defmethod set-on-click ((obj clog-obj) handler)
  (set-event obj \"click\"
	     (when handler
	       (lambda (data)
		 (declare (ignore data))
		 (funcall handler obj)))))
```

If there is data for the event an additional string containing the needed
JavaScript to return the even data and a function to parse out the data.

Replace the event name with the correct name, parse-keyboard-even with the
parse function and the string containing the needed JavaScript replaces
keyboard-event-script:

* The event handlers setter

```lisp
(defmethod set-on-key-down ((obj clog-obj) handler)
  (set-event obj \"keydown\"
	     (when handler
	       (lambda (data)
		 (funcall handler obj (parse-keyboard-event data))))
	     :call-back-script keyboard-event-script))   
```

* The script

```lisp
(defparameter keyboard-event-script
  \"+ e.keyCode + ':' + e.charCode + ':' + e.altKey + ':' + e.ctrlKey + ':' +
     e.shiftKey + ':' + e.metaKey\")
```

* The event parser

```lisp
(defun parse-keyboard-event (data)
  (let ((f (ppcre:split \":\" data)))
    (list
     :event-type :keyboard
     :key-code   (parse-integer (nth 0 f) :junk-allowed t)
     :char-code  (parse-integer (nth 1 f) :junk-allowed t)
     :alt-key    (js-true-p (nth 2 f))
     :ctrl-key   (js-true-p (nth 3 f))
     :shift-key  (js-true-p (nth 4 f))
     :meta-key   (js-true-p (nth 5 f)))))
```

")
