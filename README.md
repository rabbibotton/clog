# CLOG - The Common Lisp Omnificent GUI

## David Botton <david@botton.com>

### License BSD 3-Clause License

- [Learn to program Common Lisp and CLOG here - LEARN.md](LEARN.md)

---

![Image of CLOG](https://rabbibotton.github.io/images/clog.png)

The Common Lisp Omnificent GUI, CLOG for short, uses web technology to
produce graphical user interfaces for applications locally or remotely.
CLOG can take the place, or work alongside, most cross-platform GUI
frameworks and website frameworks. The CLOG package starts up the
connectivity to the browser or other websocket client (often a browser
embedded in a native application.)

- [CLOG - Reference Manual](https://rabbibotton.github.io/clog/clog-manual.html)

STATUS: CLOG and CLOG Builder 2.3 released. CLOG API Stable 4 years

The CLOG Builder is in 100% portable Common Lisp using the CLOG Framework.

CLOG tech was invented (first in Ada) and has been running in commercial
production code and products since 2013 and in Common Lisp since 2022. CLOG
is used in commerical products, websites, and other opensource projects.

CLOG is being actively extended, the core API is stable and proven,
the CLOG Builder is rich in features and is a full featured general purpose
IDE for Common Lisp and web development (including support for working on
JavaScript, HTML and more) and includes a GUI Builder for the CLOG Framework.

Check the github discussion boards for the latest on the project and support.

Consider sponsoring work on CLOG at https://github.com/sponsors/rabbibotton
it really does help!

Some potential applications for CLOG:

* Cross-platform GUIs and Reports
* Secure websites and complex interactive web applications
* Desktop business software (CLOG Runs native on Windows, Mac and Linux)
* Mobile software (CLOG Runs native on Android and iOS)
* Massive multiplayer online games
* Monitoring software for embedded systems
* A fun way to teach programming and advanced multi-tasking
  parallel programming techniques. (CLOG is a parallel GUI)
* And the list goes on

The key to CLOG is the relationship it forms with a browser window
or browser control compiled to native code. CLOG uses websockets
for communications and the browser to render a GUI that maintains
an active soft realtime connection. For most CLOG applications all
programming logic, events and decisions are done on the server
which can be local, or remote over the web.

CLOG is developed with ECL, CCL and SBCL, it is tested fairly
regulary on Linux, Windows, Android, Rasperry Pi, and Mac. It
is also know to work with the commercial Common Lisps as well.

CLOG works with QuickLisp and with OCICL distribution for builds.

To install Common Lisp with QuikLisp:

* [Install Common-Lisp for MacOS](MACOS.md)
* [Install Common-Lisp for Linux](LINUX.md)
* [Install Common-Lisp For Android (Termux)](ANDROID-TERMUX.md)
* [Install Common-Lisp for Win64](WINDOWS.md)

For more advanced users:

* [Install and Using OCICL on all platforms](OCICL.md)

For those new to Common Lisp or just want to quickly try CLOG use ther
simple _EZ_ standalone versions:

* https://github.com/rabbibotton/clog-win64-ez/releases
* https://github.com/rabbibotton/clog-linux-ez/releases
* https://github.com/rabbibotton/clog-linux-arm-ez/releases
* https://github.com/rabbibotton/clog-mac-ez/releases

Unzip, double click setup.bat or ./setup
Run builder.exe or ./builder
Update (almost daily :) use update.bat or ./update

As QuickLisp most frequently used - here are instructions:

CLOG v1 is in QuickLisp (ql:quickload :clog), therefore one should
add the UltraLisp distribution to use CLOG v2 (alternatively use git)

To add UltraLisp to QuickLisp:

```
(ql-dist:install-dist "http://dist.ultralisp.org/"
                      :prompt nil)
```

You still need to update often the UltraLisp and QuickLisp distros with:

```
(ql:update-all-dists)
```
If using the CLOG Builder Options -> Update CLOG Builder will do this for you.


Q U I C K   S T A R T
=====================


1) To get started load CLOG and then can load and run the builder:

```
sbcl --eval "(ql:quickload :clog/tools)" --eval "(clog-tools:clog-builder)"
```
or if using OCICL see [Install and Using OCICL on all platforms](OCICL.md)
or if using emacs M-x slime then (ql:quickload :clog/tools)(clog-tools:clog-builder)


2) In a REPL in the builder Tools -> CLOG Builder REPL
(can _also_ do all in the slime/sly REPL in emacs)

```
CL-USER> (clog:run-tutorial 1)
```

_Tips for Windows WSL linux_ Install "sudo apt install xdg-utils wslu" to
install xdg-open so that run-tutorial uses the windows browser. Also create
or add to your %UserProfile% (home dir) a file .wslconfig with
```
[wsl2]
networkingMode=mirrored
```


To see where the source, tutorial and demo files are:

```
CL-USER> (clog:clog-install-dir)
```

Also in CLOG Builder use HELP -> Tutorials Dir


You can the run the demos with:

```
CL-USER> (clog:run-demo 1)
```

You can also open a "clog-repl" browser window to play
from the common-lisp repl:

```
CL-USER> (in-package clog-user)
CLOG-USER> (clog-repl)
CLOG-USER> (setf (background-color *body*) "beige")
CLOG-USER> (create-div *body* :content "Hello World!")
```

The clog-repl URL is http://127.0.0.1:8080/repl ``*body*`` will always refer
to the last access of that URL.

To open the CLOG manual (or in the builder it is under Help):

```
CL-USER> (clog:open-manual)
```

Work your way through the tutorials. You will see how quick and easy it is
to be a CLOGer.

F i r s t   C L O G   A p p
===========================

** 1 Smallest CLOG App - in a REPL

```lisp
(ql:quickload :clog)
(in-package :clog-user)
(initialize (lambda (body) (create-div body :content "Hello World")))
```

Open browser to http://127.0.0.1:8080/ 
Or launch with (open-browser)

** 2 Minimal CLOG App - Quick Lisp

(in common-lisp or another asdf reachable location)

```
cd ~/common-lisp
mkdir agui
cd agui
```

Create a project file agui.asd

```lisp
(asdf:defsystem #:agui
  :description "Hello World"
  :author "some@one.com"
  :license  "BSD"
  :version "0.0.0"
  :serial t
  :depends-on (#:clog)
  :components ((:file "agui")))
```

Create the agui.lisp file

```lisp
(defpackage #:agui
  (:use #:cl #:clog)
  (:export start-app))

(in-package :agui)

(defun on-new-window (body)
  (create-div body :content "Hello World"))

(defun start-app ()
  (initialize 'on-new-window)
  (open-browser))
```

** 3 Minimal CLOG App - OCICL - Best Method

To install ocicl -

https://docs.google.com/presentation/d/16egmnPGA88RVOl0zhurxw6uLnA86v-lBe_SHZA7CWOg

Can be anywhere on your system any OS

```
mkdkir helloworld
cd helloworld
ocicl setup > init
ocicl install clog
```

(If on Windows you will need the open source dlls in any project dir:
  https://rabbibotton.github.io/clog/clogframe.zip )

Run bash run-ocicl or run-ocicl.bat on Windows

Edit helloworld.lisp with bash edit-ocicl or edit-ocicl.bat

M o r e   I n f o
=================

![Image of clog-builder](https://rabbibotton.github.io/images/clog-builder.png)
![Image of clog-builder-web](https://rabbibotton.github.io/images/cb-web.png)
![Image of demo1](https://rabbibotton.github.io/images/clog-demo1.png)
![Image of demo2](https://rabbibotton.github.io/images/clog-demo2.png)
![Image of demo3](https://rabbibotton.github.io/images/clog-demo3.png)
![Image of clog-db-admin](https://rabbibotton.github.io/images/clog-db-admin.png)
![Image of clog-web-containers](https://rabbibotton.github.io/images/clog-web-containers.png)


Here is a very simple sample CLOG app (from Tutorial 1):

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

      ;; A CLOG-Element represents a block of HTML (we will later see ways to
      ;; directly create buttons and all sorts of HTML elements in more
      ;; lisp-like ways with no knowledge of HTML or JavaScript.
      (create-child body "<h1>Hello World! (click me!)</h1>")))

    (set-on-click hello-element      ; Now we set a function to handle clicks
          (lambda (obj)              ; In this case we use an anonymous function
            (setf (color hello-element) "green"))))))

;; To see all the events one can set and the many properties and styles that
;; exist, refer to the CLOG manual or the file clog-element.lisp


(defun start-tutorial ()   ; Define the function called start-tutorial
  "Start tutorial."        ; Optional docstring to describe function

  ;; Initialize the CLOG system
  (initialize #'on-new-window)
  ;; Set the function on-new-window to execute
  ;; every time a browser connection to our app.
  ;; #' tells Common Lisp to pass the function
  ;; to intialize and not to execute it.


  ;; Open a browser to http://12.0.0.1:8080 - the default for CLOG apps
  (open-browser))
```

Other samples of CLOG on the web:

- [Moldable Inspectors](https://codeberg.org/khinsen/clog-moldable-inspector)
- [CLOG + cl-collider](https://github.com/byulparan/clog-collider-experience)
- [CLOG on iOS and Android](https://www.reddit.com/r/lisp/comments/tl46of/would_it_be_cool_to_run_a_clog_app_on_mobile_you/)
- [Learn CLOG Dashboard](https://gist.github.com/mmontone/3a5a8a57675750e99ffb7fa64f40bc39#file-clog-learn-lisp)
- [CLOGPower.com](http://clogpower.com)

Get started learning CLOG and the CLOG Builder with
* [Learn CLOG](LEARN.md)

Tool Summary

- clog-builder  - Rapid visual interactive development for Web and GUIs
- clog-db-admin - SQLite3 admin tool

High Order Extensions to CLOG

- clog-gui - Desktop over the web
  - Menus
  - Windowing system
  - Modal windows, Keep-on-top windows
  - File Load / Save dialogs
  - Alert, Input and Confirmation dialogs
  - Form dialogs

- clog-web - Webpage creation
  - Auto column layouts
  - 12 Point Grid System layouts
  - Content containers
  - Panels
  - Sidebar menus
  - Compositor containers
  - Menus
  - Alerts

- clog-web-site - Instant themed websites with plugins:
  - clog-web-page    - create a theme based page
  - clog-web-dbi     - database driven websites (uses clog-auth)
  - clog-web-forms   - Instant web forms
  - clog-web-themes  - basic themes for clog based websites
  - clog-web-content - database driven content,tags, comments (in progress)
  - clog-web-blog    - instant blogs (in progress)
  - clog-web-cart    - instant shopping carts (future)

- clog-panels - Quick application layouts

- clog-tree - Collapsable tree control

- clog-presentations - bi-directional linking of Lisp Objects and CLOG
                       Objects

- clog-jquery - DOM queries

- clog-data - Move data to and from groups of controls
  -  SQL writer helpers for basic SQL
  -  CLOG-Database - Database control for CLOG Builder
  -  CLOG-One-Row  - One row at a time table access auto
                     binds to controls in CLOG Builder
  -  CLOG-Lookup   - Version of the select control (dropdown and listbox)
                     that are database connected
  -  CLOG-DB-Table - Version of html table that are database connected

- clog-auth - Authentication and authorization framework

- clog-plugin - Custom Control Plug-in template for Builder and CLOG
