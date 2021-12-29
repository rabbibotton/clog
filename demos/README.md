

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
CL-USER> (clog:run-demo 1)
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
- 03-demo.lisp - IDE - A very simple common lisp IDE
                       (see source if editor dosen't load)
- 04-demo.lisp - CMS Website