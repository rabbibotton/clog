To run a tutorial load clog:

```
CL-USER> (ql:quickload :clog)
To load "clog":
  Load 1 ASDF system:
    clog
; Loading "clog"
...........................
(:CLOG)
```

Load the tutorial:

```
CL-USER> (load "/Users/dbotton/common-lisp/clog/tutorial/01-tutorial.lisp")
#P"/Users/dbotton/common-lisp/clog/tutorial/01-tutorial.lisp"
```

Start the tutorial:

```
CL-USER> (clog-user:start-tutorial)
Hunchentoot server is started.
Listening on 0.0.0.0:8080.
HTTP listening on : 0.0.0.0:8080
HTML Root         : static-files/
Boot file default : /boot.html
```

Most demos startup a browser, if not use http://127.0.0.1:8080


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
- 15-tutorial.lisp - Multi-media
- 16-tutorial.lisp - Bootstrap 4, Loading css files and javascript
- 17-tutorial.lisp - W3.CSS layout example and Form submit methods
- 18-tutorial.lisp - Drag and Drop
- 19-tutorial.lisp - Using JavaScript components
- 20-tutorial.lisp - New CLOG plugin from JavaScript component
