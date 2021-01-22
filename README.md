# CLOG - The Common Lisp Omnificent GUI

## David Botton <david@botton.com>

### License BSD 3-Clause License

View the HTML Documentation:

https://rabbibotton.github.io/clog/clog-manual.html


The Common Lisp Omnificient GUI, CLOG for short, uses web technology to
produce graphical user interfaces for applications locally or remotely.
CLOG can take the place, or work along side, most cross platform GUI
frameworks and website frameworks. The CLOG package starts up the
connectivity to the browser or other websocket client (often a browser
embedded in a native template application.)

STATUS: CLOG is complete enough for most uses, there are a few loose
ends (multimedia, client side storage, integrations with databases),
but CLOG is actually based on a framework I wrote for Ada, GNOGA, in
2013 and used in commercial production code for the last 6 years,
i.e. the techiniques it uses are solid and proven.

Some of the things CLOG can be used for:

* Cross platform GUIs and Reports
* Secure websites and complex interactive web applications
* Write mobile software
* Write massive multiplayer online games
* Monitoring software for embedded systems
* A fun way to teaching programming and advanced multi-tasking
  parallel programming techniques. (CLOG is a parallel GUI)
* And the list goes on

The key to CLOG is the relationship it forms with a Browser window
or Browser control compiled to native code. CLOG uses websockets
for communications and the browser to render a GUI that maintanes
an active soft realtime connection. For most CLOG applications all
programming logic, events and decisions are done on the server
which can be local or remote over the web.

CLOG is developed on an M1 MacBook with ECL, it is tested fairly
regulary with SCBL on Linux, Windows and Intel MacBook. It should
in theory work on any system QuickLisp and CLACK will load on to.

CLOG will be in QuickSlip in the next update, but a good idea,
since I am still adding code daily, is to cloan the github repo
in to your ~/common-lisp directory:

```
cd ~/common-lisp
git clone https://github.com/rabbibotton/clog.git
```

To load this package and work through tutorials (assuming you
have QuickSlip configured):

1. cd to the CLOG dir (the dir should be one used by QuickLisp ex. ~/common-lisp/)
2. Start emacs/slime or your common lisp "repl" in _that_ directory.
3. In the REPL run:

CL-USER> (ql:quickload :clog)
CL-USER> (load "~/common-lisp/clog/tutorial/01-tutorial.lisp")
CL-USER> (clog-user:start-tutorial)

Work your way through the tutorials. You will see how quick and easy it is
to be a CLOGer.

Here is a sample CLOG app:

```lisp
(defpackage #:clog-user               ; Setup a package for our work to exist in
  (:use #:cl #:clog)                  ; Use the Common Lisp language and CLOG
  (:export start-tutorial))           ; Export as public the start-tutorial function

(in-package :clog-user)               ; Tell the "reader" we are in the clog-user package


;; Define our CLOG application
(defun on-new-window (body)           ; Define the function called on-new-window
  "On-new-window handler."            ; Optional docstring to describe function

  (let ((hello-element                ; hello-element is a local variable that
                                      ; will be bound to our new CLOG-Element
      
      ;; This application simply creates a CLOG-Element as a child to the
      ;; CLOG-body object in the browser window.

      ;; A CLOG-Element represents a block of HTML (we will see later ways to
      ;; directly create buttons and all sorts of HTML elements in more lisp
      ;; like ways with no knowledge of HTML or javascript. 
      (create-child body "<h1>Hello World! (click me!)</h1>")))

    (set-on-click hello-element      ; Now we set a function to handle clicks
          (lambda (obj)              ; In this case we use an anonymous function
            (setf (color hello-element) "green")))

   (run body))) ; Keep our thread alive until connection closes
                ; and prevent garbage collection of our CLOG-Objects
                ; until no longer needed.
            
;; To see all the events one can set and the many properties and styles that
;; exist, take a look through the CLOG manual or the file clog-element.lisp


(defun start-tutorial ()   ; Define the function called start-tutorial
  "Start turtorial."       ; Optional docstring to describe function

  ;; Initialize the CLOG system
  (initialize #'on-new-window)
  ;; Set the function on-new-window to execute
  ;; everytime a browser connection to our app.
  ;; #' tells common lisp to pass the function
  ;; to intialize and not to execute it.


  ;; Open a browser to http://12.0.0.1:8080 - the default for CLOG apps
  (open-browser))
```

Work in progress:
(Add an enhacement request if want to see some feature specificly not
 covered yet.)

Tutorial Summary

- 01-tutorial.lisp - Hello World
- 02-tutorial.lisp - Closures in CLOG
- 03-tutorial.lisp - Events fire in parallel
- 05-tutorial.lisp - The event target, reusing event handlers
- 05-tutorial.lisp - Using connection-data-item
- 06-tutorial.lisp - Tasking and events
- 07-tutorial.lisp - My first CLOG video game (and handling disconnects)
- 08-tutorial.lisp - Mice Love Containers
- 09-tutorial.lisp - Tabs, pannels and forms
- 10-tutorial.lisp - Canvas
- 11-tutorial.lisp - Attaching to existing HTML
- 12-tutorial.lisp - Running a website in CLOG (routing)
- 13-tutorial/     - Flying Solo - A minimalist CLOG project
- 14-tutorial.lisp - Local (persistent) and Session client side storage

Demo Summary

- 01-snake-game.lisp - Sparkey the Snake Game


Enhancements being worked on now:

- Multimedia - HTML 5 Audio and Video

- CLOG higher level containers and GUI widgets

- Database bindings and server side APIs
  - Current CL packages
  - Direct bidings to widgets ete.

- CLOG Devtools
  - Generate application scaffolding
  - GUI Builder
    - Grid style
    - Page style
  - Electron for native GUIs
  
- Plugin API 
  - General CL systems
  - Widgets created from JavaScript code
