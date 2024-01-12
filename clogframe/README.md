## CLOGFrame

CLOG Frame uses the native browser control on your platform to create
a simple app the displays a frame ready to recieve your app. (see
the code used in demos/01-demo.lisp for some tips)

```
clogframe[.exe] "Window Title" 8080 640 480
                               port  w x h
```

in the clogframe directory run:


On Mac (all requirements for build part of OS):
```
./make-mac
./make-snake
./make-builder
```
On Linux

Development: apt install g++ libgtk-3-dev libwebkit2gtk-4.0-dev
Production: apt install libgtk-3-0 libwebkit2gtk-4.0-37
```
./make-linux
./make-snake
./make-builder
```
On Windows

to do (see https://github.com/webview/webview)

On All Platfroms:

This will create three executable clogframe, snake and builder. clogframe
must be in the same directory wtih the other executables. Then
run:
```
./snake
```
or
run:
```
./builder
```
