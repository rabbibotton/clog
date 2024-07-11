# Creating Native Applications with CLOG

## The Simple solutions:

1. Common to all solutions is compiling you CLOG application to an executable.
See demos/make-snake and the source of the 01-demo.lisp file.

make-snake produces a native app that launches a browser. When snake game
exist the executable quits.

2. You can use CLOGFrame - see clogframe/README.md and the built in
browser control on your OS.


## Other solutions:

1. Use the Ceramic project that uses Electron (win,linux,mac)

2. Chrome app mode

3. Use MacGap on Mac

4. Native iOS and Android using Cordova and ECL


## 1 Open app using chrome in app mode

Sample project https://github.com/rabbibotton/elect 

## 2 Open app using chrome in app mode

To open a chrome window in app mode use -app="URL" for example
to start the builder as an app:

On Mac:

/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome -app="http://127.0.0.1:8080/builder"

On Linux:

google-chrome-stable --new-window --app="http://127.0.0.1:8080/builder"

On Windows

chrome --new-window --app="http://127.0.0.1:8080/builder"

## 3 Use MacGap and XCode

https://github.com/MacGapProject/MacGap2

```
git clone https://github.com/MacGapProject/MacGap2.git
open MacGap2/MG.xcodeproj/
```
In public/index.html in Xcode you use: MacGap.launch(appName)
to launch your app. Then connect to it on the port you have chosen.


## 4 Native iOS and Android using Cordova and ECL

https://cordova.apache.org/
