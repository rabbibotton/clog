;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2021 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-docs.lisp                                                        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :clog)

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

STATUS: CLOG is complete enough for most uses. See below for some
enhacements bing worked on, CLOG is actually based on GNOGA, a
framework I wrote for Ada in 2013 and used in commercial production
code for the last 8 years, i.e. the techiniques CLOG uses are solid
and proven.

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
which can be local or remote over the web.

CLOG is developed on an M1 MacBook with ECL, it is tested fairly
regulary with SCBL on Linux, Windows and Intel MacBook. It should
in theory work on any system Quicklisp and CLACK will load on to.

CLOG will be in Quicklisp in the next update, but because I am still 
adding code daily, it is currently preferable to clone the github repo
into your ~/common-lisp directory:

```
cd ~/common-lisp
git clone https://github.com/rabbibotton/clog.git
```

To load this package and work through tutorials (assuming you
have Quicklisp configured):

1. Start emacs then M-x slime
2. In the REPL, run:

```
CL-USER> (ql:quickload :clog)
CL-USER> (clog-user:run-tutorial 1)
```

To see where the source files are:

```
CL-USER> (clog:clog-install-dir)
```

You can the load the demos with:

```
CL-USER> (load \"path to clog/demos/01-snake.lisp\")
CL-USER> (clog-user:start-demo)
```

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

Next, we need to use the INITIALIZE function to tell CLOG to start up the web
server, what to do when someone connects and where the static HTML files
are located.

```lisp
CL-USER> (clog:initialize (lambda (body)()) :static-root #P\"~/common-lisp/clog/static-files/\")
Hunchentoot server is started.
Listening on 0.0.0.0:8080.
HTTP listening on    : 0.0.0.0:8080
HTML Root            : /Users/dbotton/common-lisp/clog/static-files/
Boot file for path / : /boot.html
NIL
```

At this point our CLOG app doese very little. To see our CLOG app so far go to
http://127.0.0.1:8008  or in most common-list configurations you can use:

```lisp
CL-USER> (clog:open-browser)
```

Something more than an empty lambda function is needed to do more. The
tutorials are a good place to start with make CLOG apps in code, so
here we are going to demonstrate the concepts using some REPL tricks
to help developing CLOG apps in general.

We need to give ourselves easier access to CLOG and or an app we are
working one. Let's create a package that uses CLOG and of course
common lisp \"cl\" we will call it \"clog-user\".

```lisp
CL-USER> (defpackage #:clog-user
  (:use #:cl #:clog))
(in-package :clog-user)
#<\"CLOG-USER\" package>
CLOG-USER> 
```

Since we already initialized CLOG let's use SET-ON-NEW-WINDOW to change our
on-new-window handler (handler is just a made up name for a function that
will handle an event).

```lisp
CLOG-USER> (set-on-new-window (lambda (body) (create-div body :content \"Hello World!\")))
```

Now go ahead and resresh our browser and you should see the famous first words
of every app.

This of though is still not very REPL like, CLOG is a 'live' connection to a
browser. So lets redo our on-new-window handler to give us access to the
browser in the REPL.

```lisp
CLOG-USER> (defparameter *body* nil)
*BODY*
CLOG-USER> (set-on-new-window (lambda (body) (setf *body* body)))
```

Reset your browser again (or navigate to http://127.0.0.1:8080 and let's have
some fun.

(From here on, we will leave out the promps and responses in our quotes of
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

From clog-base

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
     :key-code   A key code sometimes called a scan code for the key pressed
     :char-code  UTF-8 representation for key pressed when possible
     :alt-key    t or nil if alt-key held down
     :ctrl-key   t or nil if ctrl-key held down
     :shift-key  t or nil if shift-key held down
     :meta-key   t or nil if meta-key held down

From clog-window

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
