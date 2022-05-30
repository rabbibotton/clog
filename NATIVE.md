
# Creating Native Applications with CLOG

## Using Ceramic Mac/Windows/Linux

The documentation for ceramic is at:
http://ceramic.github.io/

1. (ql:quickload :ceramic)
2. (ceramic:setup)

Now that ceramic is installed. We create a new project using CLOG Builder:

1. (ql:quickload :clog/tools)
2. (clog-tools:clog-builder)

In CLOG Builder:

1. Choose Builder->New Application Template
2. Choose a template for your project - I will use CLOG-GUI
3. Give the new project a name - I am using elect as the project name
4. Choose the default directory ~/common-lisp or another that works for you

In the REPL let's load the new project:

1. Let's open ~/common-lisp/elect/elect.asd
2. Add to depends-on #:ceramic
3. Let's open ~/common-lisp/elect/elect.lisp
4. Replace start-app with:

```
(defvar *window* nil)

(defun start-app (&key (port 8080))
  (ceramic:start)
  (initialize 'on-new-window
	      :port port
	      :static-root (ceramic:resource-directory 'www))
  (setf *window*
        (ceramic:make-window :url (format nil "http://127.0.0.1:~D/" port)))
  (ceramic:show *window*))

(ceramic:define-resources :elect ()
  (www #p"www/"))
  
(ceramic:define-entry-point :elect ()
  (start-app))
```

5. I suggest starting from scratch at this point: M-x slime-restart-inferior-lisp
6. (ql:quickload :elect)
7. (elect:start-app)

That should start up a native application with your CLOG app

To package you applicaton use:

1. (ceramic:bundle :elect)

