# CLOG - The Common Lisp Omnificent GUI

## David Botton <david@botton.com>

### License BSD 3-Clause License

####
The Common Lisp Omnificient GUI, CLOG for short, uses web technology
to produce graphical user interfaces for applications locally or
remotely, ie as web applications.

View the HTML Documentation:

https://htmlpreview.github.io/?https://github.com/rabbibotton/clog/blob/main/doc/clog-manual.html


To load this package and use the tests:

1. cd to the CLOG dir (the dir should be one used by QuickLisp lime ~/common-lisp)
2. Start emacs/slime or your common lisp repl in that directory.
3. In the REPL run (ql:quickload :clog)
4. Then (load "~/common-lisp/clog/test/test-clog.lisp) (clog-test:test)

Sample CLOG app with code base so far:

```
(defpackage #:clog-user
  (:use #:cl #:clog)
  (:export hello))

(in-package :clog-user)

(defun hello ()
  "Simple Hello world using CLOG."

  ;; Initialize the CLOG system
  (clog:initialize
   (lambda (win)
     (clog:set-on-click
      (clog:create-child win "<h1>Hello World!</H1>")
      (lambda ()
	(clog:create-child win "<p>You Clicked me!</p>")))))

  (clog:open-browser))
```


Status:

- Connection methods
  - Websockets - DONE
  - AJAX/HTML - to do (In 2021 are there browsers not supporting Websockets?)
  - Long Poll - to do (Needed for websites for webcrawlers and firewalls) 
  - Direct API access to native browser components - to do (not needed but games, soft real-time apps, etc would be quicker) 

- HTML bindings and Browser
  - Base system for bindings - DONE
  - Event system - DONE
  - General DOM (Window, Screen, Document, Location, Navigator) - DONE
  - Base Elements (HTML Elements)
  - Canvas - HTML 5 Canvas bindings
  - SVG - HTML SVG vector graphics
  - Multimedia - HTML 5 Audio and Video
  - Styles - CSS Style blocks

- CLOG higher level containers and GUI widgets - to do

- Database bindings and server side APIs - to do
  - Current CL packages
  - Direct bidings to widgets ete.

- CLOG Devtools - to do
  - Generate application scaffolding
  - GUI Builder
	- Grid style
	- Page style
  - Electron for native GUIs
  
- Plugins - to do
  - General CL systems
  - Widgets
  
- Documentation - Auto Generated - DONE
