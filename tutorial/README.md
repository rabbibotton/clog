To run a tutorial, start emacs/slime or your CL Lisp in the common-lisp/clog directory:

```
CL-USER> (ql:quickload :clog)
To load "clog":
  Load 1 ASDF system:
    clog
; Loading "clog"
...........................
(:CLOG)
```

Load the demo:

```
CL-USER> (load "/Users/dbotton/common-lisp/clog/tutorial/01-tutorial.lisp")
#P"/Users/dbotton/common-lisp/clog/tutorial/01-tutorial.lisp"
```

Start the demo:

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
