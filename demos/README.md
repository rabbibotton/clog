

To run a demo, start emacs/slime or your CL Lisp in the common-lisp/clog directory:

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
CL-USER> (load "/Users/dbotton/common-lisp/clog/demos/01-demo.lisp")
#P"/Users/dbotton/common-lisp/clog/demos/01-demo.lisp"
```

Start the demo:

```
CL-USER> (clog-user:start-demo)
Hunchentoot server is started.
Listening on 0.0.0.0:8080.
HTTP listening on : 0.0.0.0:8080
HTML Root         : static-files/
Boot file default : /boot.html
```

Most demos startup a browser, if not use http://127.0.0.1:8080

Demo Summary

- 01-demo.lisp - Sparkey the Snake Game
- 02-demo.lisp - Chat - Private instant messenger
