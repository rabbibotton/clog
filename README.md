# CLOG - The Common Lisp Omnificent GUI

## David Botton <david@botton.com>

### License BSD 3-Clause License

View the HTML Documentation:

https://rabbibotton.github.io/clog/clog-manual.html

View a short video on reddit about CLOG Builder

https://www.reddit.com/r/lisp/comments/sd9wf1/clog_builder_cl_web_3_awesome_lang_awesome_tool/


[LEARN COMMON-LISP AND CLOG - LEARN.md](LEARN.md)

---

![Image of CLOG](https://rabbibotton.github.io/images/clog.png)

The Common Lisp Omnificent GUI, CLOG for short, uses web technology to
produce graphical user interfaces for applications locally or remotely.
CLOG can take the place, or work alongside, most cross-platform GUI
frameworks and website frameworks. The CLOG package starts up the
connectivity to the browser or other websocket client (often a browser
embedded in a native template application.)

[CLOG - Technical Overview and Purpose](CONCEPT.md)

STATUS: CLOG 1.2 and CLOG Builder 1.1 released.

CLOG is actually based on GNOGA, a framework I wrote for Ada in 2013
and used in commercial production code for the last 9+ years, i.e. the
techiniques CLOG uses are solid and proven.

CLOG is being actively extended daily. Check the github discussion
boards for the latest.

Some potential applications for CLOG:

* Cross-platform GUIs and Reports
* Secure websites and complex interactive web applications
* Mobile software (CLOG Runs native on Android and iOS)
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
which can be local, or remote over the web.

CLOG is developed with ECL and SBCL, it is tested fairly regulary on
 Linux, Windows, Rasberry Pi (running Ubuntu), M1 and Intel Mac. It
 should in theory work out of the box on any system with Quicklisp
 (although you could hand install) and CLACK (easily switched out
 and the ecl Android/iPhone branch doesn't use).

CLOG is in QuickLisp (ql:quickload :clog), but you likely want to live
on the bleeding edge and use UltraLisp or clone the github repo into
~/common-lisp directory [or other quicklisp/asdf findable directory
(push "path/to/dir/of/projects" ql:*local-project-directories*) ]:

```
cd ~/common-lisp
git clone https://github.com/rabbibotton/clog.git
```

To load this package and work through tutorials (assuming you
have Quicklisp configured.)

Note: If using portacle for Windows you will need to
update Quicklisp use (ql:update-dist "quicklisp")
You will also likely need to copy the sqlite3 dll from
https://www.sqlite.org/download.html to portacle\win\lib
Consider a custom [install on windows](WINDOWS.md)

1. Start emacs then M-x slime
2. In the REPL, run:

```
CL-USER> (ql:quickload :clog)
CL-USER> (clog:run-tutorial 1)
```

Tip for Windows WSL linux user. Install "sudo apt install xdg-utils" to
install xdg-open so that run-tutorial uses the windows browser.

To see where the source, tutorial and demo files are:

```
CL-USER> (clog:clog-install-dir)
```

You can the run the demos with:

```
CL-USER> (ql:quickload :clog)
CL-USER> (clog:run-demo 1)
```

The clog-db-admin tool can be run with:

```
CL-USER> (ql:quickload :clog/tools)
CL-USER> (clog-tools:clog-db-admin)
```

The CLOG Builder tool can be run with:

```
CL-USER> (ql:quickload :clog/tools)
CL-USER> (clog-tools:clog-builder)
```

You can also open a "clog-repl" window in your browser to play
from the common-lisp repl:

```
CL-USER> (in-package clog-user)
CLOG-USER> (clog-repl)
CLOG-USER> (setf (background-color *body*) "beige")
CLOG-USER> (create-div *body* :content "Hello World!")
```

The clog-repl URL is http://127.0.0.1/repl *body* will always refer to the
last access of that URL.

To open a browser with the CLOG manual:

```
CL-USER> (clog:open-manual)
```

Work your way through the tutorials. You will see how quick and easy it is
to be a CLOGer.


![Image of clog-builder](https://rabbibotton.github.io/images/clog-builder.png)
![Image of clog-builder-web](https://rabbibotton.github.io/images/cb-web.png)
![Image of demo1](https://rabbibotton.github.io/images/clog-demo1.png)
![Image of demo2](https://rabbibotton.github.io/images/clog-demo2.png)
![Image of demo3](https://rabbibotton.github.io/images/clog-demo3.png)
![Image of clog-db-admin](https://rabbibotton.github.io/images/clog-db-admin.png)
![Image of clog-web-containers](https://rabbibotton.github.io/images/clog-web-containers.png)


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

[CLOG + cl-collider](https://github.com/byulparan/clog-collider-experience)
[CLOG on iOS and Android](https://www.reddit.com/r/lisp/comments/tl46of/would_it_be_cool_to_run_a_clog_app_on_mobile_you/)
[Learn CLOG Dashboard](https://gist.github.com/mmontone/3a5a8a57675750e99ffb7fa64f40bc39#file-clog-learn-lisp)

CLOG Builder Tutorials

1. Chat App
    https://www.reddit.com/r/lisp/comments/sj1tv5/clog_builder_tutorial_1_a_chat_app_from_start_to/
2. Building a Web Page
    https://www.reddit.com/r/lisp/comments/sn8j77/clog_builder_tutorial_2_building_a_web_page/
3. Importing HTML in to Builder, Adding Pages and Hand Coding
    https://www.reddit.com/r/lisp/comments/snvv0w/clog_builder_tutorial_3_importing_html_adding/
4. CLOS-CONTACT - Using database controls demos a contact manager app in clog.
    https://www.reddit.com/r/lisp/comments/t61sib/clog_builder_tutorial_4_a_complete_database_app/

CLOG Tutorials

- [01-tutorial.lisp](tutorial/01-tutorial.lisp) - Hello World
- [02-tutorial.lisp](tutorial/02-tutorial.lisp) - Closures in CLOG
- [03-tutorial.lisp](tutorial/03-tutorial.lisp) - Events fire in parallel
- [04-tutorial.lisp](tutorial/04-tutorial.lisp) - The event target, reusing event handlers
- [05-tutorial.lisp](tutorial/05-tutorial.lisp) - Using connection-data-item
- [06-tutorial.lisp](tutorial/06-tutorial.lisp) - Tasking and events
- [07-tutorial.lisp](tutorial/07-tutorial.lisp) - My first CLOG video game (and handling disconnects)
- [08-tutorial.lisp](tutorial/08-tutorial.lisp) - Mice Love Containers
- [09-tutorial.lisp](tutorial/09-tutorial.lisp) - Tabs, panels, and forms
- [10-tutorial.lisp](tutorial/10-tutorial.lisp) - Canvas
- [11-tutorial.lisp](tutorial/11-tutorial.lisp) - Attaching to existing HTML
- [12-tutorial.lisp](tutorial/12-tutorial.lisp) - Running a website in CLOG (routing)
- [13-tutorial/](tutorial/13-tutorial) - Flying Solo - A minimalist CLOG project
- [14-tutorial.lisp](tutorial/14-tutorial.lisp) - Local (persistent) and Session client-side storage
- [15-tutorial.lisp](tutorial/15-tutorial.lisp) - Multi-media
- [16-tutorial.lisp](tutorial/16-tutorial.lisp) - Bootstrap 4, Loading css files and javascript
- [17-tutorial.lisp](tutorial/17-tutorial.lisp) - W3.CSS layout example and Form submit methods
- [18-tutorial.lisp](tutorial/18-tutorial.lisp) - Drag and Drop
- [19-tutorial.lisp](tutorial/19-tutorial.lisp) - Using JavaScript components
- [20-tutorial.lisp](tutorial/20-tutorial.lisp) - New CLOG plugin from JavaScript component
- [21-tutorial.lisp](tutorial/21-tutorial.lisp) - New CLOG plugin in Common-Lisp
- [22-tutorial.lisp](tutorial/22-tutorial.lisp) - CLOG GUI Menus and Desktop Look and Feel
- [23-tutorial.lisp](tutorial/23-tutorial.lisp) - Using semaphores to wait for input
- [24-tutorial.lisp](tutorial/24-tutorial.lisp) - CLOG WEB containers
- [25-tutorial.lisp](tutorial/25-tutorial.lisp) - A "local" web app using CLOG WEB
- [26-tutorial.lisp](tutorial/26-tutorial.lisp) - A web page and form with CLOG WEB
- [27-tutorial.lisp](tutorial/27-tutorial.lisp) - Panel Box Layouts
- [28-tutorial/](tutorial/28-tutorial) - CLOG Builder Hello - A minimalist CLOG Builder project
- [29-tutorial.lisp](tutorial/29-tutorial.lisp) - Presentations (and jQuery) - linking lisp objects to clog objects
- [30-tutorial.lisp](tutorial/30-tutorial.lisp) - Instant websites - clog-web-site
- [31-tutorial.lisp](tutorial/31-tutorial.lisp) - Database and Authority based websites - clog-web-dbi and clog-auth
- [32-tutorial.lisp](tutorial/32-tutorial.lisp) - Database Managed Content websites - clog-web-content

CLOG Demos

- [01-demo.lisp](demos/01-demo.lisp) - Sparkey the Snake Game
- [02-demo.lisp](demos/02-demo.lisp) - Chat - Private instant messenger
- [03-demo.lisp](demos/03-demo.lisp) - IDE - A very simple common lisp IDE
- [04-demo.lisp](demos/04-demo.lisp) - CMS Website - A very simple database driven website

Tool Summary

- clog-db-admin - SQLite3 admin tool
- clog-builder  - Rapid visual interactive development for Web and GUIs

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

(See [FUTURE.md](FUTURE.md) for future plans)
