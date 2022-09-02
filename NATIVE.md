# Creating Native Applications with CLOG

Simple solution:

Common to all solutions is compiling you CLOG application to an executable.
See demos/make-snake and the source of the 01-demo.lisp file.

make-snake produces a native app that launches a browser. When snake game
exist the executable quits.

More advanced solutions:

1. Open app using chrome in app mode (not exactly native but looks it)

2. Use MacGap on Mac (best native option for Mac)

3. Native app using (ql:quickload :ceramic) (works well for Windows and Linux)

4. Native iOS and Android using Cordova and ECL


## 1 Open app using chrome in app mode

To open a chrome window in app mode use -app="URL" for example
to start the builder as an app:

On Mac:

/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome -app="http://127.0.0.1:8080/builder"

On Linux:

google-chrome-stable --new-window --app="http://127.0.0.1:8080/builder"

On Windows

chrome --new-window --app="http://127.0.0.1:8080/builder"

## 2 Use MacGap and XCode

https://github.com/MacGapProject/MacGap2

```
git clone https://github.com/MacGapProject/MacGap2.git
open MacGap2/MG.xcodeproj/
```
In public/index.html in Xcode you use: MacGap.launch(appName)
to launch your app. Then connect to it on the port you have chosen.

## 3 Using Ceramic Mac/Windows/Linux

The documentation for ceramic is at:
http://ceramic.github.io/

Complete project at - https://github.com/rabbibotton/elect

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

5. We need to add to the botton of on-new-window code to shutdown app.

```
  (clog:run body) ; wait while body is running
  (ceramic:quit)  ; quit ceramic/electron
  (clog:shutdown) ; shutdown clog
```

6. I suggest starting from scratch at this point: M-x slime-restart-inferior-lisp
7. (ql:quickload :elect)
8. (elect:start-app)

That should start up a native application with your CLOG app

To package you applicaton use:

1. (ceramic:bundle :elect)


## 4 Native iOS and Android using Cordova and ECL

https://cordova.apache.org/